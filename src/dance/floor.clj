(ns dance.floor
  (:require [clojure.set :as set]
            [arity.core :refer [arities fake-arities]]
            [shuriken.associative :refer [merge-with-plan map-intersection]]
            [threading.core :refer :all]
            [weaving.core :refer :all :exclude [arity-comp]]
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

(defn- arity-comp
  "Composes functions like `comp` but preserves arity."
  ([]  identity)
  ([f] f)
  ([f & more-fns]
   (let [fns (cons f more-fns)]
     (fake-arities (-> fns last arities)
                   #(apply (apply comp fns)
                           %&)))))

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
  (-> dance meta :dance.core/name))
(defmacro with-dance-name [dance-name dance]
  (if dance-name
    `(vary-meta ~dance assoc :dance.core/name ~dance-name)
    dance))

(defn dance-impl [dance]
  (-> dance meta :dance.core/impl))
(defmacro with-dance-impl [impl dance]
  `(let [impl# ~impl
         d# ~dance]
     (if (seq impl#)
       (vary-meta d# assoc :dance.core/impl impl#)
       d#)))

(defn dependent-dances [dance]
  (-> dance meta :dance.core/dependent-dances))
(defmacro with-dependent-dances [dependent-dances dance]
  `(let [ds# ~dependent-dances
         d# ~dance]
     (if (seq ds#)
       (vary-meta d# assoc :dance.core/dependent-dances ds#)
       d#)))

(defn subdances [dance]
  (vec (concat (dependent-dances dance)
               (when-let [impl (dance-impl dance)]
                 [impl]))))

(defn atomic-dance? [dance]
  (-> dance
      (if-> dance-name
        (and-> (-> dependent-dances empty?)
               (not-> dance-impl dance-name))
        (and-> (not-> dance-impl)
               (not-> dependent-dances)))
      boolean))

(defn atomic-dances [dance]
  (->> (tree-seq (not| atomic-dance?) subdances dance)
       rest
       (filter atomic-dance?)))

(defn ^:no-doc emit-defdance* [nme docstring dependent-dances impl]
  `(def ~nme ~@(when docstring [docstring])
     (let [ds# ~dependent-dances
           impl# ~impl]
       (->> (apply merge-dances (concat ds# (when (seq impl#) [impl#])))
            (with-dance-name '~nme)
            (with-dance-impl impl#)
            (with-dependent-dances ds#)))))

(defmacro defdance [nme & args]
  (let [[docstring body] (-> args (if-> (-> first string?)
                                    (juxt-> first rest)
                                    (juxt-> (<- nil) identity)))
        [dependent-dances impl-dance] (split-with (not| keyword?) body)
        dependent-dances (vec dependent-dances)
        impl-name (-> nme name (str "*") symbol)
        impl-dance? (->> (partition 2 impl-dance)
                         (map vec)
                         seq)
        impl-dance (into {} impl-dance?)]
    `(do ~@(when impl-dance?
             [(emit-defdance* impl-name (format "See [[%s]]." nme)
                              []
                              impl-dance)])
         ~(emit-defdance* nme docstring
                          dependent-dances
                          (if impl-dance? impl-name nil)))))

(defn ^:no-doc dance-collect* [when what at into accumulate position form ctx]
  [form (when-> ctx (<- ((context| when) form ctx))
          (update
            at (fnil (fn [x]
                       (accumulate
                         x (first ((context| what)
                                   form ctx))))
                     into)))])

;; TODO: use something better than (context| when).
(defmacro def-collecting-dance [nme & args]
  (let [[docstring body] (-> args (if-> (-> first string?)
                                    (juxt-> first rest)
                                    (juxt-> (<- nil) identity)))
        [dependent-dances impl-dance] (split-with (not| keyword?) body)

        [& {:keys [when what at into accumulate position]
            :or {into []
                 accumulate conj
                 position :post}
            :as more-opts}]
        impl-dance

        position-sym (gensym "position-")
        at-sym (gensym "at-")
        collecting-dance
        (-> more-opts
            (dissoc :when :what :at :into :accumulate :position)
            (merge {(keyword position)
                    `(fn [form# ctx#]
                       (dance-collect*
                         ~when ~what ~at ~into ~accumulate ~position
                         form# ctx#))
                    :return [:context at]}))]
    `(defdance ~nme ~@(or (and docstring [docstring]) [])
       ~@dependent-dances
       ~@(apply concat collecting-dance))))

;; TODO: should have with-dependent-dances here but tests fail.
(defn merge-dances
  "Merges multiples dances together. A dance is a hash of arguments
  to [[dance]]."
  [& dances]
  (let [dances (map adapt-dance-fns dances)]
    (apply merge-with-plan dance-merge-plan dances)))

;; TODO: find a way to order dances (i.e. to order a tree)
; (defn order-dance [dance constraints]
;   (let [dances (atomic-dances dance)]
;     (-> (->> (order dances constraints)
;              (apply merge-with-plan dance-merge-plan)
;              (with-subdances dances)))))

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
