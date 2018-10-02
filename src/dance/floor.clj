(ns dance.floor
  (:use clojure.pprint)
  (:require [clojure.set :as set]
            [arity.core :refer [arities fake-arities]]
            [shuriken.associative :refer [merge-with-plan map-intersection]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [flatland.ordered.map :refer [ordered-map]]))

;; TODO: backport this multi-sym version to shuriken.
;; We need to copy it over here because it's in shuriken.macro which already
;; depends on dance.core.
(defn is-form? [sym-or-syms expr]
  (let [syms (-> sym-or-syms
                 (when-not-> coll? list)
                 set)]
    (boolean (and (seq? expr) (-> expr first syms)))))

(defn- chain-dance-fns [f & more-fns]
  (->> (map apply| more-fns)
       (concat [f])
       (apply ->|)))

(defn- reverse-chain-dance-fns [& fns]
  (apply chain-dance-fns (reverse fns)))

;; TODO: rewrite warp with interleave
(defmacro ^:private contextualize [name f]
  `(def ^:private ~name
     (warp| ~f (fn
                 ([g# [form# ctx#]] (g# form# ctx#))
                 ([g# form# ctx#]   (g# form# ctx#))))))

(contextualize ctx-comp arity-comp)
(contextualize ctx->|   ->|)

(defn- ctx-and|
  [& fns]
  (fake-arities
    (->> fns (mapcat arities) distinct sort)
    #(loop [[f & more] fns]
       (let [[result ctx] (apply f %&)]
         (if (and result (seq more))
           (recur more)
           [result ctx])))))

(defn- reverse-ctx-and| [& fns]
  (apply ctx-and| (reverse fns)))

(declare merge-dances empty-dance)

(def ^:private dance-merge-plan
  {:before-all ctx->|
   :before ctx->|
   :pre?  ctx-and|         :pre   ctx->|
   :walk? ctx-and|
   :post? reverse-ctx-and| :post  ctx-comp
   :after ctx-comp
   :after-all ctx-comp
   :context merge
   :scoped #(set/union (set %1) (set %2))
   :debug-context-whitelist #(set/union (set %1) (set %2))
   :debug-context-blacklist #(set/union (set %1) (set %2))
   ;; :debug, :return, :debug-depth and :step are merged normally
   ;; (rightmost preference).
   })

(defn adapt-dance-fns [dance]
  (->> dance
       (map (fn [[k f]]
              (if (#{:walk? :pre? :pre :post? :post :before :after
                     :before-all :after-all}
                     k)
                [k (context| f)]
                [k f])))
       (into {})))

(defn dance-name [dance]
  (-> dance meta :dance.core/dance-name))
(defmacro with-dance-name [dance dance-name]
  `(vary-meta ~dance assoc :dance.core/dance-name ~dance-name))

;; TODO: keep subdance ?
(defn subdances [dance]
  (-> dance meta :dance.core/subdances))
(defmacro with-subdances [dance subdances]
  `(vary-meta ~dance assoc :dance.core/subdances ~subdances))

(defn atomic-dance? [dance]
  (-> dance subdances empty?))

(defn all-subdances [dance]
  (if (atomic-dance? dance)
    [dance]
    (mapcat all-subdances (subdances dance))))

(defn defdance*
  [nme dances]
  `(def ~nme
     ))

(defmacro defdance [nme & body]
  (let []
    (shuriken.core/debug
      `(do ~@(when (seq impl-dance)
               [`(defdance ~impl-name ~impl-dance)])
           (let [[dependent-dances# dances#]
                 ~(if (seq impl-dance)
                    `(let [ds# ~dependent-dances]
                       [ds# (concat ds#  [~impl-name])])
                    `(let [ds# ~dependent-dances]
                       [ds# ds#]))]
             (def ~nme
               (-> (apply merge-dances dances#)
                   (with-dance-name '~nme)
                   (with-subdances dependent-dances#))))))))

(defn ^:no-doc emit-defdance [nme dependent-dances impl-dance]
  (let [dependent-dances (vec dependent-dances)]))

(defmacro defdance [nme & body]
  (let [[dependent-dances impl-dance] (split-with (not| keyword?) body)

        impl-dance (-> (->> (partition 2 impl-dance)
                            (map vec)
                            seq
                            (into {}))
                       (with-dance-name impl-name))]
    (if-not (empty? impl-dance)
      (let [impl-name (-> nme name (str "*") symbol)]
        `(do ~(emit-defdance impl-name [] impl-dance)
             ~(emit-defdance nme (concat dependent-dances [impl-dance])))))))

(defn merge-dances
  "Merges multiples dances together. A dance is a hash of arguments
  to [[dance]]."
  [& dances]
  (let [dances (map adapt-dance-fns dances)]
    (-> (apply merge-with-plan dance-merge-plan dances)
        (with-subdances dances))))

; (defn order-dance [dance constraints]
;   (let [dances (all-subdances dance)]
;     (-> (->> (order dances constraints)
;              (apply merge-with-plan dance-merge-plan))
;         (with-subdances))))

;; TODO: remove form & subform arguments (here for debugging)
;; TODO: make a :scoped dance ? And set its :after rule to be :> :all via
;; shuriken.sequence/order ?
(defn- handle-scoped [prev-ctx new-ctx]
  (let [prev-scoped    (-> prev-ctx :scoped set)
        new-scoped     (-> new-ctx  :scoped set)
        newly-scoped   (set/difference new-scoped prev-scoped)
        newly-unscoped (set/difference prev-scoped new-scoped)
        same-scoped    (set/intersection new-scoped prev-scoped)
        all-ctx-keys   (set/union (set (keys prev-ctx)) (set (keys new-ctx)))
        all-scoped     (set/union prev-scoped new-scoped)
        same-unscoped  (set/difference all-ctx-keys all-scoped)
        m (merge prev-ctx new-ctx)
        w prev-ctx
        s select-keys
        entries-to-keep    (s m (set/union newly-unscoped same-unscoped))
        entries-to-restore (s w (set/union newly-scoped same-scoped))]
    (merge entries-to-keep
           entries-to-restore)))

(defn step [form {:keys [initial-acc initial-context wrap inner outer]}]
  (let [[result context]
        (reduce (fn [[acc prev-ctx] subform]
                  (let [[result new-ctx] (inner subform prev-ctx)
                        new-ctx (handle-scoped prev-ctx new-ctx)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                form)]
    (outer (wrap result) context)))

(defn step-indexed [form {:keys [initial-acc initial-context wrap inner outer]}]
  (let [indexes (if (record? form)
               (map key form)
               (range))
        [result context]
        (reduce (fn [[acc prev-ctx] [i subform]]
                  (let [ctx (assoc prev-ctx :index i)
                        [result new-ctx] (inner subform ctx)
                        new-ctx (-> (handle-scoped prev-ctx new-ctx)
                                    (assoc :index i))]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                (into (ordered-map)
                      (map #(vector %1 %2) indexes form)))]
    (outer (wrap result) context)))

(defn map-entry [x]
  (if (map-entry? x)
    x
    (clojure.lang.MapEntry/create (first x) (second x))))

(defn context-walk [step inner outer form context]
  (let [marche (fn [& {:as opts}]
                 (step form (merge {:initial-acc []
                                    :initial-context context
                                    :wrap identity
                                    :inner inner :outer outer}
                                   opts)))]
    (cond
      (list? form)      (marche :wrap #(apply list %))
      (map-entry? form) (marche :wrap map-entry)
      (seq? form)       (doall (marche :wrap seq))
      (record? form)    (marche :initial-acc form)
      (coll? form)      (marche :wrap #(into (empty form) %))
      :else             (outer form context))))

(def empty-dance
  (adapt-dance-fns
    {:walk?       any?
     :pre?        any?       :pre       identity
     :post?       any?       :post      identity
     :before      identity   :after     identity
     :before-all  identity   :after-all identity
     :context     nil        :return    :form
     :scoped      #{:scoped} :step      step
     :debug-context-whitelist #{}
     :debug-context-blacklist #{}}))
