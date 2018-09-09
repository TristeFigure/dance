(ns dance.choreography
  (:use clojure.pprint)
  (:require [shuriken.associative :refer [getsoc]]
            [shuriken.destructure :refer [deconstruct]]
            [shuriken.sequential :refer [get-nth]]
            [shuriken.spec :refer [conform!]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [dance.floor :refer :all]))

(def parent-dance
  "A dance that keeps track of the parent node.
  Context keys: :parent (:next-parent)."
  {:scoped [:parent :next-parent]
   :before (fn [form ctx]
             [form (assoc ctx
                     :parent (:next-parent ctx)
                     :next-parent form)])})

(def parents-dance
  "A dance that keeps track of the parent nodes, in genealogic order
  (parent, grand-parent, ...).
  Context keys: :parents (:next-parent)."
  {:scoped [:parents :next-parent]
   :context {:parents '()}
   :before (fn [form ctx]
             [form (-> ctx
                       (update :parents conj (:next-parent ctx))
                       (assoc :next-parent form))])})

(def depth-dance
  "A dance that keeps track of the depth of the traversal.
  Context key: :depth."
  {:scoped [:depth]
   :before    (fn [x ctx] [x (update ctx :depth (fnil inc -1))])
   :after-all (fn [x ctx] [x (dissoc ctx :depth)])})

;; TODO: does it work with sets ?
(def indexed-dance
  "A dance that keeps track of the index the current nodes resides at in
  its parent node. The index can be either a number for list/vectors or
  anything else for maps.
  Context key: :index."
  {:step  step-indexed
   :after (fn [form ctx]
            [form (dissoc ctx :index)])})

(def path-dance
  "A dance that keeps track of the path from the root of the datastructure to
  the current node.
  To use a path to find back the node it leads to, use shuriken.core/get-nth-in
  and sister functions since contrary to get-in, assoc, etc ..., they support
  traversing sequences.
  Context key: :path."
  (merge-dances
    indexed-dance
    {:scoped [:path]
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
                [form new-ctx]))}))

;; TODO: document the rest

(def leaf-collecting-dance
  (merge-dances
    path-dance
    {:pre? (not| coll?)
     :pre (fn [form ctx]
           (let [new-ctx (update ctx :leafs assoc (:path ctx) form)]
             [form new-ctx]))
     :after-all (fn [form ctx]
                  [(:leafs ctx) ctx])}))

(defn splice [form]
  [::splice form])

(defn- splice? [form]
  (and (vector? form) (-> form first #{::splice})))

(def splicing-dance
  {:post (fn [form]
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
                 (reverse (into '() spliced))))))})

(def not-processing-quoted-forms-dance
  (let [quoted?-nope #(not (is-form? 'quote %))]
    {:pre?  quoted?-nope
     :walk? quoted?-nope
     :post? quoted?-nope}))

(def reluctant-macroexpanding-dance
  (merge-dances
    not-processing-quoted-forms-dance
    {:before macroexpand}))

(defn- add-locals [ctx more-locals]
  (update ctx :next-locals concat more-locals))

(def ^:private locals-tracking-dance--let*-loop*
  (let [let*-loop*? #(or->> % (is-form? 'let*) (is-form? 'loop*))]
    {:context {:binding-locals []}
     :before  (fn [form ctx]
                [form (when-> ctx (<- (let*-loop*? form))
                        (update :binding-locals into [[]]))])
     :after   (fn [form ctx]
                [form (when-> ctx (<- (let*-loop*? form))
                        (update :binding-locals pop))])}))

(def ^:private locals-tracking-dance--let*-loop*-binding-expr
  {:before
   (fn [form ctx]
     [form
      (when-> ctx (and-> (-> :parents second
                             (or->> (is-form? 'let*) (is-form? 'loop*)))
                         (>-args (= (-> :parents second second)
                                    (-> :parents first)))
                         (-> :index even?))  ;; a binding sym, not a bound expr
        (•- (update :locals concat (-• :binding-locals (apply concat))))

        (assoc
          :locals (-> (concat (:locals ctx)
                              (->> ctx :binding-locals (apply concat))))))]


     #_[form
      (-> ctx
          (if-> (-> :parents first (and-> (or->> (is-form? 'let*)
                                                 (is-form? 'loop*))
                                          (-> second (= form))))
            (update :binding-locals #(vec (concat % [[]])))
            (assoc
              :locals (-> (concat (:locals ctx)
                                  (-> ctx :binding-locals (apply concat))
                                  (:next-locals ctx))
                          reverse distinct reverse)))
          (dissoc :next-locals))])})

(def ^:private locals-tracking-dance--single-bodied-fn*
  {:scoped [:parsed-fn-form]
   :before
   (fn [form ctx]
     [form
      (when-> ctx (<- (is-form? 'fn* form))
        (<- (let [[parsed ctx] (getsoc ctx :parsed-fn-form
                                       #(conform! :shuriken.spec/fn-form form))]
              (when-> ctx (<- (-> parsed :bodies count (= 1)))
                (add-locals (->> parsed :bodies first :args deconstruct))))))])}
  )

(def ^:private locals-tracking-dance--multiple-bodied-fn*
  {:scoped [:parsed-fn-form]
   :before
   (fn [form ctx]
     [form
      (when-> ctx (some->> :parents first (is-form? 'fn*))
        (<- (let [[parsed ctx] (getsoc ctx :parsed-fn-form
                                       #(conform! :shuriken.spec/fn-form
                                                  (-> ctx :parents first)))]
              (when-> ctx (and-> (<- (-> parsed :bodies count (> 1)))
                                 (<- (seq? form)))
                (add-locals (-> (conform! :shuriken.spec/args+body form)
                                :args deconstruct))))))])})

(def ^:private locals-tracking-dance--single-bodied-letfn*
  {:before
   (fn [form ctx]
     [form
      (-> ctx
          (when-> (and-> (->> :parents second (is-form? 'letfn*))
                         (<- (= (-> ctx :parents first)
                                (-> ctx :parents second second))))
            (add-locals (->> ctx :parents second
                             (conform! :shuriken.spec/letfn-spec)
                             :bodies first :args deconstruct))))])})

(def ^:private locals-tracking-dance--multiple-bodied-letfn*
  {:before
   (fn [form ctx]
     [form
      (-> ctx
          (when-> (and-> (-> :parents (get-nth 2) (->> (is-form? 'letfn*)))
                         (<- (= (-> ctx :parents second)
                                (-> ctx :parents (get-nth 2) second)))
                         (-> :parents first second seq?))
            (add-locals (-> (conform! :shuriken.spec/args+body form)
                            :args deconstruct))))])})

(def locals-tracking-dance
  (merge-dances
    reluctant-macroexpanding-dance
    parents-dance
    indexed-dance
    locals-tracking-dance--let*-loop*
    locals-tracking-dance--single-bodied-fn*
    locals-tracking-dance--multiple-bodied-fn*
    locals-tracking-dance--single-bodied-letfn*
    locals-tracking-dance--multiple-bodied-letfn*
    {:scoped [:locals :next-locals :original-locals]
     :before (fn [form ctx]
               [form
                (-> ctx
                    (if-> (and-> (-> :parents first (or->> (is-form? 'let*)
                                                           (is-form? 'loop*)))
                                 (-> :parents first second (= form)))
                      (update :binding-locals #(vec (concat % [[]])))
                      (assoc
                        :locals (-> (concat
                                      (:locals ctx)
                                      (->> ctx :binding-locals (apply concat))
                                      (:next-locals ctx))
                                    reverse distinct reverse)))
                    (dissoc :next-locals))])
     :after (fn [form ctx]
              [form
               (-> ctx
                   (when-> (and-> (-> :parents first (or->> (is-form? 'let*)
                                                            (is-form? 'loop*)))
                                  (-> :parents first second (= form)))
                     (update :binding-locals #(vec (concat % [[]])))))])}))

(def free-vars-collecting-dance
  "Accepts an optional :bound-syms sequence in the context."
  (merge-dances
    locals-tracking-dance
    {:context {:bound-syms []}
     :pre (fn [form ctx]
            [form
             (if (-> form
                     (and->
                       symbol?
                       (not->
                         (or->>
                           resolve
                           special-symbol?
                           (.contains (:locals ctx))
                           (.contains (:bound-syms ctx))))))
               (update ctx :free-vars concat [form])
               ctx)])
     :return-context true
     :after-all (fn [form ctx]
                  [(vec (:free-vars ctx)) nil])}))
