(ns dance.choreography
  (:use clojure.pprint)
  (:require [clojure.set :as set]
            [shuriken.associative :refer [getsoc]]
            [shuriken.destructure :refer [deconstruct]]
            [shuriken.sequential :refer [get-nth update-nth update-nth-in]]
            [shuriken.spec :refer [conform!]]
            [clojure.spec.alpha :refer [conform]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [dance.floor :refer :all]))

(defdance ancestors-dance
  "A dance that keeps track of the parent nodes, in ascending genealogic
  order (parent, grand-parent, ...).
  Context keys: :ancestors (:next-parent)."
  :scoped [:ancestors :next-parent]
  :context {:ancestors '()}
  :before (fn [form ctx]
            [form (-> ctx
                      (when-> :next-parent
                        (update :ancestors conj (:next-parent ctx)))
                      (assoc :next-parent form))]))

;; TODO: use fun-map ?
(defdance parent-dance
  "A dance that keeps track of the parent node.
  Context keys: :parent."
  ancestors-dance
  :scoped [:parent]
  :before (fn [form ctx]
            [form (assoc ctx :parent (-> ctx :ancestors first))]))

;; TODO: document
(defdance original-form-dance
  :scoped [:original-form]
  :before (fn [form ctx]
            [form (assoc ctx :original-form form)]))

;; TODO: does it work with sets ?
;; TODO: use :scoped [:index] ?
(defdance indexed-dance
  "A dance that keeps track of the index the current nodes resides at in
  its parent node. The index can be either a number for list/vectors or
  anything else for maps.
  Context key: :index."
  :step (ø| step-indexed)
  :scoped [:index])

(defdance path-dance
  "A dance that keeps track of the path from the root of the datastructure to
  the current node.
  To use a path to find back the node it leads to, use shuriken.core/get-nth-in
  and sister functions since contrary to get-in, assoc, etc ..., they support
  traversing sequences.
  Context key: :path."
  indexed-dance
  :scoped [:path]
  :before (fn [form ctx]
            (let [conj-path (fn [ctx k]
                              (update ctx :path
                                      (comp vec concat)
                                      (if-let [i (get ctx k)]
                                        [i] [])))
                  new-ctx (cond
                            (map-entry? form)
                            (assoc ctx :map-index (key form))

                            (contains? ctx :map-index)
                            (case (:index ctx)
                              0 ctx
                              1 (-> (conj-path ctx :map-index)
                                    (dissoc :map-index)))

                            :else (conj-path ctx :index))]
              [form new-ctx])))

(def-collecting-dance leafs-collecting-dance
  path-dance
  :accumulate #(into %1 [%2])
  :into {}
  :when (not| coll?)
  :what (fn [form ctx]
          [[(:path ctx) form]])
  :at :leafs)

(defn splice [form]
  [::splice form])

(defn- splice? [form]
  (and (vector? form) (-> form first #{::splice})))

(defdance splicing-dance
  :post (fn [form]
          (if-not (and (sequential? form)
                       (some splice? form))
            form
            (let [spliced (reduce (fn [acc x]
                                    (if (splice? x)
                                      (into acc (second x))
                                      (conj acc x)))
                                  []
                                  form)]
              (if (vector? form)
                spliced
                (reverse (into '() spliced)))))))

(defdance macroexpanding-dance
  :before macroexpand)

(def ^:no-doc quoted?
  (|| is-form? 'quote))

(defdance reluctant-macroexpanding-dance
  :before (when| (not| quoted?) macroexpand))

;; TODO: move to shuriken.associative
(defn merge-with-plan
  "Like `merge-with` except that the combination fn of a specific pair
  of entries is determined by looking up their key in `plan`. If not
  found, falls back to the function found under key `:else` or if not
  provided to a function that returns the value in the right-most map,
  thus providing the behavior of `merge`.
  In addition to a map, `plan` can also be a function accepting a key
  and returning a combination fn for the two values to merge."
  [plan & maps]
    (when (some identity maps) ;; TODO: what the hell ?
      (let [merge-entry (fn [m e]
                          (let [k (key e) v (val e)]
                            (if (contains? m k)
                              (let [else-f (get plan :else  #(identity %2))
                                    f (get plan k else-f)]
                                (assoc m k (f (get m k) v)))
                              (assoc m k v))))
            merge2 (fn [m1 m2]
                     (reduce merge-entry (or m1 {}) (seq m2)))]
        (reduce merge2 maps))))

(defn last-in-coll? [ctx form]
  (-> ctx :parent last (= form)))


(defn get-precontext [ctx]
  (-> ctx :precontexts first))

(defn wrap-in-precontext [x]
  {:precontext x})

(defn- list-or-map-instead-of-nil [pth _coll]
  (if (-> pth last integer?)
    '()
    {}))

;; Generic
(defn update-nth-next-context [ctx n f & args]
  (apply update-nth-in
         list-or-map-instead-of-nil
         ctx [:precontexts 0]
         (if (> n 0)
           #(apply update-nth-next-context %1 (dec n) f %&)
           f)
         args))

(defn update-in-nth-next-context [ctx n k-or-ks f & args]
  (apply update-nth-in
         list-or-map-instead-of-nil
         ctx [:precontexts 0]
         (if (> n 0)
           #(apply update-in-nth-next-context %1 (dec n) k-or-ks f %&)
           (let [ks (when-not-> k-or-ks sequential? vector)]
             #(apply update-nth-in %1 ks f %&)))
         args))

(defn add-to-nth-next-context [ctx n m]
  (update-nth-next-context ctx n merge m))

;; Next context
(defn update-next-context [ctx f & args]
  (apply update-nth-next-context ctx 0 f args))

(defn update-in-next-context [ctx k-or-ks f & args]
  (apply update-in-nth-next-context ctx 0 k-or-ks f args))

(defn add-to-next-context [ctx m]
  (add-to-nth-next-context ctx 0 m))

;; Second next context
(defn update-second-next-context [ctx f & args]
  (apply update-nth-next-context ctx 1 f args))

(defn update-in-second-next-context [ctx k-or-ks f & args]
  (apply update-in-nth-next-context ctx 1 k-or-ks f args))

(defn add-to-second-next-context [ctx m]
  (apply add-to-nth-next-context ctx 1 m))



(defn update-right-context [form ctx f & args]
  (when-not-> ctx (last-in-coll? form)
    (apply-> (update-next-context f args))))

(defn update-in-right-context [form ctx k-or-ks f & args]
  (when-not-> ctx (last-in-coll? form)
    (apply-> (update-in-next-context k-or-ks f args))))

(defn add-to-right-context [form ctx m]
  (when-not-> ctx (last-in-coll? form)
    (add-to-next-context m)))

(def ^:private rdistinct     (comp reverse distinct reverse))
(def ^:private fusion-locals (comp vec rdistinct concat))

;; TODO: move to shuriken.sequence
(defn pad
  "Returns [a b] such that both sequences have the same length."
  ([a b] (pad nil a b))
  ([padding a b]
   (if (> (count a) (count b))
     [a (concat b (repeat (- (count a) (count b)) padding))]
     [(concat a (repeat (- (count b) (count a)) padding)) b])))

(defn- fusion-precontexts [a b]
  ;; TODO: that should depend on the context.
  (apply map #(merge-with-plan {:locals fusion-locals}
                               %1 %2)
         (pad a b)))

(defn- pop-precontext [ctx]
  (update ctx :precontexts rest))

(defn- push-precontext [ctx]
  (update ctx :precontexts (|| cons {})))

(defn- prepare-to-absorb-context [ctx]
  (update ctx :precontext-to-absorb
          #(merge-with-plan
             (merge (:precontext-merge-plan (get-precontext ctx))
                    (:precontext-merge-plan %))
             %
             (get-precontext ctx))))

(defn- absorb-precontext [ctx]
  (-> (merge-with-plan (:precontext-merge-plan ctx)
                       ctx
                       (:precontext-to-absorb ctx))
      (dissoc :precontext-to-absorb)))

(defdance precontext-dance
  parent-dance
  :dance-args [:precontext-merge-plan]
  :merge-plan {:precontext-merge-plan merge}
  :precontext-merge-plan {:precontexts fusion-precontexts}
  ; :handle-scope (fn [original form subform prev-ctx new-ctx]
  ;                  (original form subform
  ;                            prev-ctx
  ;                            new-ctx
  ;                            #_(merge-with-plan (:precontext-merge-plan prev-ctx)
  ;                                             prev-ctx
  ;                                             (:precontext-to-absorb new-ctx))
  ;                            #_(dissoc new-ctx :precontext-to-absorb)))
  :step (fn [original form opts]
          (-> opts
              (update :initial-context (->| absorb-precontext
                                            push-precontext))
              (->> (original form))
              (as-> [result ctx]
                [result (-> ctx
                            prepare-to-absorb-context
                            pop-precontext)])))
  ; :before (ctx| absorb-precontext
  ;               push-precontext)
  ; :after  (ctx| prepare-to-absorb-context
  ;               pop-precontext)
  )

(defn- add-locals-to-current-context [ctx locals]
  (update ctx :locals fusion-locals locals))

; (defn- add-locals-to-next-context [ctx locals]
;   (update-in-next-context
;     ctx :locals fusion-locals
;     ;; TODO: remove filter
;     (filter #(= (count (str %)) 1)
;             locals)))

(defn- add-locals-to-second-next-context [ctx locals]
  (update-in-second-next-context ctx :locals fusion-locals locals))

; (defn- add-locals-to-right-context [form ctx locals]
;   (update-in-right-context
;     form ctx :locals fusion-locals
;     ;; TODO: remove filter
;     (filter #(= (count (str %)) 1)
;             locals)))

(defn- make-locals-scoped [ctx]
  (println "------> LOCALS SCOPED")
  (update ctx :scoped #(set/union (set %) #{:locals})))

(defn- make-locals-unscoped [ctx]
  (println "------> LOCALS UNSCOPED")
  (update ctx :scoped disj :locals))

;; -- Top level
(defdance ^:private locals-tracking-dance--top-level
  :context {:locals []}
  :scoped [:locals :binding-form?] ;; the main trick: :scoped is scoped
  ;; TODO: use commented version (bugs idontknowwhy)
  :before ;(ctx| (when| (|| is-form? '[let* loop* letfn* fn* reify*])
          ;        make-locals-scoped))
  (fn [form ctx]
    [form (when-> ctx (<- (is-form? '[let* loop* letfn* fn* reify*] form))
            make-locals-scoped)]))

;; TODO: move to shuriken.sequential
(defn third  [x] (-> x next next first))
(defn fourth [x] (-> x next next next first))
(defn fifth  [x] (-> x next next next next first))

;; -- let, loop & letfn: binding vector
(defdance ^:private locals-tracking-dance--let*-loop*-letfn*-binding-vec
  :before
  (fn [form ctx]
    ;; When the form is a child form of a binding form
    [form (when-> ctx (-> :parent (and-> (->> (is-form? '[let* loop* letfn*]))
                                         (-> second  (= form))))
            make-locals-unscoped)]))

;; -- let, loop & letfn: binding syms & exprs
(defdance ^:private locals-tracking-dance--let*-loop*-letfn*-binding-sym
  :before
  (fn [form ctx]
    [form (when-> ctx (and->
                        ;; When we're processing elements of a binding vector
                        (->> :ancestors second (is-form? '[let* loop* letfn*]))
                        (>-args (-> (= (-> :ancestors second second)
                                       (-> :ancestors first)))))
            ;; when these are binding syms, not exprs
            (if-> (-> :index even?)
              (-> (assoc :binding-form? true)
                  (add-locals-to-second-next-context (deconstruct form)))
              ;; when these are binding exprs of a letfn form
              (when-> (->> :ancestors second (is-form? 'letfn))
                )))]))

;; -- fn
; (defdance ^:private locals-tracking-dance--fn*-single-body
;   :before
;   (fn [form ctx]
;     [form (if (is-form? 'fn* form)
;             (let [parsed (conform! :shuriken.spec/fn-form form)]
;               (if (-> parsed :bodies count (= 1))
;                 (add-locals-to-current-context
;                   ctx (concat (keep identity [(:name parsed)])
;                               (->> parsed :bodies first :args deconstruct)))
;                 ctx))
;             ctx)]))

(require '[clojure.tools.macro :refer [symbol-macrolet]])
(defmacro lay [[sym expr & more-bindings] & body]
  (let [delay-sym (gensym (str "laid-" sym "-"))]
    `(let [~delay-sym (delay ~expr)]
       (symbol-macrolet [~sym (deref ~delay-sym)]
         ~@(if (empty? more-bindings)
             body
             `[(lay ~more-bindings ~@body)])))))

; (lay [a      (println "aaa")
;       parent (println "parent")
;       b      (do parent (println "bbb"))]
;   b
;   a)

;; TODO: move to shuriken.sequential
(defn compact [s]
  (remove nil? s))

(defdance ^:private locals-tracking-dance--fn*-single-body
  :before
  (fn [form ctx]
    (lay [parent        (->> ctx :ancestors first)
          form-is-fn    (is-form? 'fn* form)
          parsed-fn     (conform! :shuriken.spec/fn-form form)
          parsed-body   (-> (conform :shuriken.spec/args+body form)
                            (when-> #{:clojure.spec.alpha/invalid}
                              (<- nil)))
          first-body    (when form-is-fn (-> parsed-fn :bodies first))
          single-body   (:single-body first-body)
          body-of-multi (and (is-form? 'fn* parent)
                             (seq? form)
                             (not single-body))
          parsed        (cond form-is-fn     parsed-fn
                              body-of-multi  parsed-body)
          single-args   (when single-body (:args first-body))
          multi-args    (:args parsed)
          args          (or single-args multi-args)
          single-name   (:name parsed)
          multi-name    (when parsed
                          (some->> parent (conform! :shuriken.spec/fn-form)
                                   :name))
          name          (if single-args single-name multi-name)
          locals        (concat (compact [name])
                                (deconstruct args))]
      #_(when true
        (println "==> FORM       " form)
        (println "==> PARSED     " parsed)
        (println "=>> SINGLE ARGS" single-args)
        (println "=>> MULTI ARGS " multi-args)
        (println "==> ARGS       " args)
        (println "==> SINGLE NAME" single-name)
        (println "==> MULTI NAME " multi-name)
        (println "==> NAME       " name)
        (println "==> LOCALS     " locals))
      [form (when-> ctx (<- args)
              (add-locals-to-current-context locals))])))

; (defdance ^:private locals-tracking-dance--fn*-multiple-bodies
;   :scoped [:parsed-fn-form]
;   :before
;   (fn [form ctx]
;     [form (let [parent (->> ctx :ancestors first)]
;             (if (and (is-form? 'fn* parent) (seq? form))
;               (let [parsed (conform! :shuriken.spec/fn-form parent)]
;                 (if (-> parsed :bodies count (> 1))
;                   (add-locals-to-current-context
;                     ctx
;                     (concat (keep identity [(:name parsed)])
;                             (-> (conform! :shuriken.spec/args+body form)
;                                 :args deconstruct)))
;                   ctx))
;               ctx))]))

;;; -- reify
; (defdance ^:private locals-tracking-dance--reify*
;   :before
;   (fn [form ctx]
;     [form
;      (if-> ctx (and-> (<- (seq? form))
;                       (->> :ancestors first (is-form? 'reify*)))
;        make-locals-scoped
;        (when-> (and-> (<- (vector? form))
;                       (->> :ancestors second (is-form? 'reify*)))
;          (-> make-locals-unscoped
;              (as-> $ (add-locals-to-next-context
;                        form $ (deconstruct form))))))]))

(defdance ^:private locals-tracking-dance--let*-loop*-letfn*
  locals-tracking-dance--let*-loop*-letfn*-binding-vec
  locals-tracking-dance--let*-loop*-letfn*-binding-sym)

(defdance ^:private locals-tracking-dance--fn*
  locals-tracking-dance--fn*-single-body
  ; locals-tracking-dance--fn*-multiple-bodies
  )


;; TODO: handle catch blocks (the name of the exception)
;; TODO: deftype & defrecord fields are somehow local variables
(defdance locals-tracking-dance
  ; {:before (fn [form ctx] (pprint ctx) [form ctx])}
  reluctant-macroexpanding-dance
  ancestors-dance
  indexed-dance
  precontext-dance
  {:precontext-merge-plan {:locals fusion-locals}}

  locals-tracking-dance--top-level
  locals-tracking-dance--let*-loop*-letfn*
  locals-tracking-dance--fn*
  ; locals-tracking-dance--reify*
  )

(defdance free-symbols-tracking-dance
  "Accepts an optional :bound-sym? function in the context."
  locals-tracking-dance
  :scoped [:free-symbol?]
  :before-all (fn [form ctx]
                [form (update ctx :bound-sym?
                              #(when-> % (or-> nil? sequential?) set))])
  :before (fn [form ctx]
            [form
             (when-> ctx (<- (-> form
                                 (and->
                                   symbol?
                                   (<- (not (:binding-form? ctx)))
                                   (not-> (or->> special-symbol?
                                                 (.contains (:locals ctx))
                                                 ((:bound-sym? ctx))
                                                 resolve)))))
               (assoc :free-symbol? true))]))

(def-collecting-dance free-symbols-collecting-dance
  free-symbols-tracking-dance
  :accumulate (comp vec rdistinct conj)
  :when #(:free-symbol? %2)
  :what identity
  :at :free-symbols)
