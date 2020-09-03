(ns dance.floor
  (:use clojure.pprint)
  (:require [clojure.set :as set]
            [arity.core :refer [arities fake-arities]]
            [shuriken.associative :refer [merge-with-plan continue|
                                          map-intersection]]
            [shuriken.string :refer [adjust]]
            [threading.core :refer :all]
            [weaving.core :refer :all :exclude [arity-comp]]
            [flatland.ordered.map :refer [ordered-map]]
            [dance.debug]
            [potemkin :refer [import-vars]]))

;; TODO: drop dependency on potemkin/import-vars and move it to
;; shuriken.namespace (importing the 'import-vars' var)
(import-vars [dance.debug
              with-debugs
              is-form?
              debug-it
              should-debug?
              debug-print
              changed?
              changed-ctx?
              debug-changed
              debug-changed-ctx
              step-lbl
              empty-lbl])

;; The essential. See the bottom of this file for the implementations.
(declare merge-dances default-merge-plan empty-dance)

;; TODO: move to threading.core
(defmacro apply-> [expr form]
  (let [form (when-not-> form coll? list)
        [head & args] form]
    `(apply ~head ~expr ~@args)))

;; TODO: move to shuriken
(defn same
  ([x] x)
  ([x & more] (concat [x] more)))

(defn- unions [a b]
  (set/union (set a) (set b)))

(defn- chain-dance-fns [f & more-fns]
  (->> (map apply| more-fns)
       (concat [f])
       (apply ->|)))

(def dance-fns
  #{:walk? :pre? :pre :post? :post :before :after
    :before-all :after-all :init})

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

(defmacro def-ctx-boolean-combinators [& operators]
  (for [op operators
          :let [name (symbol (str "ctx-" op "|"))]]
    `(defn- ~name [& fns#]
       (fake-arities
         (->> fns# (mapcat arities) distinct sort)
         (fn [form# ctx#]
           (loop [form# form#
                  ctx# ctx#
                  [f# & more#] fns#]
             (let [[new-form# new-ctx#] (f# form# ctx#)]
               (if (~op new-form# (seq more#))
                 (recur new-form# new-ctx# more#)
                 [new-form# new-ctx#]))))))))

(def-ctx-boolean-combinators and or)

(defn- reverse-ctx-and| [& fns]
  (apply ctx-and| (reverse fns)))

(defn adapt-dance-fns [dance]
  (->> dance
       (map (fn [[k f]]
              (if (dance-fns k)
                [k (context| f)]
                [k f])))
       (into {})))

(def default-merge-plan
  {:init ctx->|

   :before-all ctx->|
   :before ctx->|
   :pre?  ctx-and|         :pre   ctx->|
   :walk? ctx-and|         :walk  continue|
   :post? reverse-ctx-and| :post  ctx-comp
   :after ctx-comp
   :after-all ctx-comp

   :context    merge
   :scoped     unions
   :dance-args unions

   :debug-context-whitelist unions
   :debug-context-blacklist unions
   :debug?                  ctx-or|

   :step continue| :step-in continue| :step-out continue|
   :handle-scope continue|

   :merge-plan            merge
   :precontext-merge-plan merge

   ;; :return, :debug, :debug-context-style are merged normally
   ;; (rightmost preference).
   })

(def ^:dynamic *base-merge-plan* default-merge-plan)

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
  `(let [dd# ~dependent-dances
         d# ~dance]
     (if (seq dd#)
       (vary-meta d# assoc :dance.core/dependent-dances dd#)
       d#)))

(defn subdances [dance]
  (if (sequential? dance)
    (vec (distinct (mapcat subdances dance)))
    (vec (concat (dependent-dances dance)
                 (when-let [impl (dance-impl dance)]
                   [impl])))))

(defn atomic-dance? [dance]
  (-> dance
      (if-> dance-name
        (and-> (-> dependent-dances empty?)
               (not-> dance-impl dance-name))
        (and-> (not-> dance-impl)
               (not-> dependent-dances)))
      boolean))

(defn atomic-dances [dance]
  (if (sequential? dance)
    (distinct (mapcat atomic-dances dance))
    (if (atomic-dance? dance)
      (list dance)
      (->> (tree-seq (not| atomic-dance?) subdances dance)
             rest
             (filter atomic-dance?)
             distinct))))

(defn ^:no-doc emit-defdance* [nme docstring dependent-dances impl]
  `(def ~nme ~@(when docstring [docstring])
     (let [dd# ~dependent-dances
           ad# (distinct (mapcat atomic-dances dd#))
           impl# ~impl]
       (->> (apply merge-dances (concat ad# (when (seq impl#) [impl#])))
            (with-dance-name (symbol (str *ns*) ~(str nme)))
            (with-dance-impl impl#)
            (with-dependent-dances dd#)))))

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

;; TODO: find a way to order dances (i.e. to order a tree)
; (defn order-dance [dance constraints]
;   (let [dances (atomic-dances dance)]
;     (-> (->> (order dances constraints)
;              (apply merge-with-plan *base-merge-plan*)
;              (with-subdances dances)))))

;; TODO: remove form & subform arguments (here for debugging)
;; TODO: make a :scoped dance ? And set its :after rule to be :> :all via
;; shuriken.sequence/order ?
(defn standard-handle-scope [form subform prev-ctx new-ctx]
  (let [prev-scoped    (-> prev-ctx :scoped set)
        new-scoped     (-> new-ctx  :scoped set)
        newly-scoped   (set/difference new-scoped prev-scoped)
        newly-unscoped (set/difference prev-scoped new-scoped)
        same-scoped    (set/intersection new-scoped prev-scoped)
        all-ctx-keys   (set (concat (keys prev-ctx) (keys new-ctx)))
        all-scoped     (set/union prev-scoped new-scoped)
        same-unscoped  (set/difference all-ctx-keys all-scoped)
        s select-keys
        entries-to-keep    (s new-ctx  (set/union newly-unscoped same-unscoped))
        entries-to-restore (s prev-ctx (set/union newly-scoped same-scoped))]
    (let [f #((if (map? %)
                shuriken.associative/remove-keys
                remove)
               #{:index :depth :debug-depth :results :next-parent :ancestors
                 :parsed-fn-form :parent :precontext-merge-plan}
               %)]
      ; (println "form:" form)
      ; (println "subform:" subform)
      (when (or (= subform '(•== [k]))
                (= subform '[k (•== [])]))
        (println "SUBFORM" subform)
        (println "prev-ctx")
        (pprint (f prev-ctx))
        (newline)
        (println "new-ctx")
        (pprint (f new-ctx))
        (newline)(newline)
        (println "newly-scoped      " (f newly-scoped))
        (println "newly-unscoped    " (f newly-unscoped))
        (println "same-scoped       " (f same-scoped))
        (println "all-ctx-keys      " (f all-ctx-keys))
        (println "all-scoped        " (f all-scoped))
        (println "same-unscoped     " (f same-unscoped))
        (newline)
        (println "entries-to-keep")
        (pprint (f entries-to-keep))
        (newline)
        (println "entries-to-restore")
        (pprint (f entries-to-restore))
        (newline)
        (println "---------------------------------")))
    (merge entries-to-keep
           entries-to-restore)))

(defn step [form {:keys [initial-acc initial-context wrap
                         step-in step-out handle-scope]}]
  (let [[result context]
        (reduce (fn [[acc prev-ctx] subform]
                  (let [[result new-ctx] (step-in subform prev-ctx)
                        new-ctx (handle-scope form subform prev-ctx new-ctx)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                form)]
    (step-out (wrap result) context)))

(defn step-indexed [form {:keys [initial-acc initial-context wrap
                                 step-in step-out handle-scope]}]
  (let [indexes (if (record? form)
               (map key form)
               (range))
        [result context]
        (reduce (fn [[acc prev-ctx] [i subform]]
                  (let [ctx (assoc prev-ctx :index i)
                        [result new-ctx] (step-in subform ctx)
                        new-ctx (-> (handle-scope
                                      form subform prev-ctx new-ctx)
                                    (assoc :index i))]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                (into (ordered-map)
                      (map #(vector %1 %2) indexes form)))]
    (step-out (wrap result) context)))

(defn map-entry [x]
  (if (map-entry? x)
    x
    (clojure.lang.MapEntry/create (first x) (second x))))

(defn context-walk [{:keys [step step-in step-out] :as dance-opts}
                    form context]
  (let [marche (fn [& {:as marche-opts}]
                 (step form (merge {:initial-acc []
                                    :initial-context context
                                    :wrap identity
                                    :step-in (fn [form ctx]
                                               (step-in form (assoc dance-opts
                                                               :context ctx)))
                                    :step-out step-out
                                    :handle-scope (:handle-scope dance-opts)}
                                   marche-opts)))]
    (cond
      (list? form)      (marche :wrap #(apply list %))
      (map-entry? form) (marche :wrap map-entry)
      (seq? form)       (doall (marche :wrap seq))
      (record? form)    (marche :initial-acc form)
      (coll? form)      (marche :wrap #(into (empty form) %))
      :else             (step-out form context))))

;; TODO: should have with-dependent-dances here but tests fail.
(defn merge-dances
  "Merges multiples dances together. A dance is a hash of arguments
  to [[dance]]."
  [& dances]
  (let [base-merger (:merge-plan *base-merge-plan*)
        mergers (map #(select-keys % [:merge-plan]) dances)
        merger (->> (cons {:merge-plan base-merger} mergers)
                    (apply base-merger)
                    :merge-plan)
        merge-plan (assoc *base-merge-plan* :merge-plan merger)
        dances (map adapt-dance-fns dances)]
    (apply merge-with-plan merge-plan dances)))


(defdance depth-dance
  "A dance that keeps track of the depth of the traversal.
  Context key: :depth."
  :scoped [:depth]
  :before (fn [x ctx] [x (update ctx :depth (fnil inc -1))]))

(defn debug-context? [dance]
  (-> dance
      (if-> (contains? :debug-context)
        :debug-context
        (or-> (-> :return #{:context :form-and-context})
              (contains? :context)
              (contains? :debug-context-whitelist)
              (contains? :debug-context-blacklist)
              (-> (select-keys dance-fns)
                  vals
                  (->> (mapcat arities)
                       (some #(= % 2))))))))


;; TODO: backport these to threading.core
(defmacro ^:private when-let-> [expr & body]
  (let [[bindings & body] body
        form (first bindings)]
    `(let-> ~expr ~bindings
       (when-> (<- ~form)
         ~@body))))

(defmacro ^:private <-> [expr & body]
  `(<- ~expr (-> ~@body)))

(defmacro ^:private <-• [expr & body]
  `(<- ~expr (-• ~@body)))


(defn prepare-scoped [dance ctx]
  [dance (update ctx :scoped #(unions (:scoped dance) %))])

(defn prepare-debug [dance ctx]
  [(when-> dance debug-context?
     (assoc :debug-context true))
   (-> ctx
       (when-> (<- (:debug dance))
         (update :scoped conj :depth))
       (when-let-> [dd (<- (:debug-depth dance))]
         (assoc :debug-depth dd)))])

(defn prepare-dance-args [dance ctx]
  ;; TODO: one day: deep-merge with plan
  [dance (merge (select-keys dance (:dance-args dance))
                ctx)])

(defn apply->| [f & fns]
  (apply ->| f (map apply| fns)))

;; TODO: do something more generic about this, like first|, second| and third|
(defn form| [& fns]
  (fn [form ctx]
    [((apply ->| fns) form) ctx]))

(defn ctx| [& fns]
  (fn [form ctx]
    [form ((apply ->| fns) ctx)]))
