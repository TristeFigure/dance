(ns dance.floor
  (:require [clojure.set :as set]
            [arity.core :refer [arities fake-arities]]
            [shuriken.associative :refer [merge-with-plan]]
            [threading.core :refer :all]
            [weaving.core :refer :all]))

(defn is-form? [sym code]
  (boolean (and (seq? code) (-> code first #{sym}))))

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
   :scoped  concat
   :unscoped concat
   ;; :debug, :return-context, and :step are merged normally
   ;; (rightmost preference).
   })

(defn- adapt-dance-fns [dance]
  (->> dance
       (map (fn [[k f]]
              (if (#{:walk? :pre? :pre :post? :post :before :after
                     :before-all :after-all}
                     k)
                [k (context| f)]
                [k f])))
       (into {})))

(defn merge-dances
  "Merges multiples dances together. A dance is a hash of arguments
  to [[dance]]."
  [& dances]
  (merge empty-dance
         (apply merge-with-plan dance-merge-plan
                (map adapt-dance-fns dances))))

(defn adapt-dance-fns [dance]
  (->> dance
       (map (fn [[k f]]
              (if (#{:walk? :pre? :pre :post? :post :before :after
                     :before-all :after-all}
                     k)
                [k (context| f)]
                [k f])))
       (into {})))

(defn- handle-scoped-unscoped [scoped unscoped original-scope new-ctx]
  (-> (set scoped)
      (set/difference unscoped)
      (set/union (:scoped new-ctx))
      (set/difference (:unscoped new-ctx))
      (->> (map #(vector % (get original-scope
                                % ::not-found)))
           (into {})
           (merge new-ctx)
           (remove (comp #{::not-found} val))
           (into {}))))

(defn step [form {:keys [initial-acc initial-context context wrap
                         inner outer scoped unscoped]}]
  (let [[result context]
        (reduce (fn [[acc prev-ctx] subform]
                  (let [original-scope (select-keys prev-ctx scoped)
                        [result new-ctx] (inner subform prev-ctx)
                        _ (->> (set/union (set scoped)
                                                (:scoped new-ctx))
                                     (map #(vector % (get original-scope
                                                          % ::not-found)))
                                     (into {})
                                     (merge new-ctx)
                                     (remove (comp #{::not-found} val))
                                     (into {}))
                        new-ctx (handle-scoped-unscoped
                                  scoped unscoped original-scope new-ctx)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                form)]
    (outer (wrap result) context)))

(defn step-indexed [form {:keys [initial-acc initial-context context wrap
                                 inner outer scoped unscoped]}]
  (let [indexes (if (record? form)
               (map key form)
               (range))
        [result context]
        (reduce (fn [[acc prev-ctx] [i subform]]
                  (let [ctx (assoc prev-ctx :index i)
                        original-scope (select-keys prev-ctx scoped)
                        [result new-ctx] (inner subform ctx)
                        new-ctx (assoc new-ctx :index i)
                        new-ctx (handle-scoped-unscoped
                                  scoped unscoped original-scope new-ctx)]
                    [(conj acc result) new-ctx]))
                [initial-acc initial-context]
                (zipmap indexes form))]
    (outer (wrap result) context)))

(def empty-dance
  (adapt-dance-fns
    {:walk?      any?
     :pre?       any?     :pre            identity
     :post?      any?     :post           identity
     :before     identity :after          identity
     :before-all identity :after-all      identity
     :context    nil      :return-context false
     :scoped     #{}      :step           step}))
