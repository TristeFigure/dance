(ns dance.core-test
  (:require [clojure.test :refer :all]
            [dance.core :refer :all]
            [threading.core :refer :all]))

(defrecord DanceTestRecord [a b c])

(defmacro printed-lines [& body]
  `(-> (with-out-str ~@body)
       (clojure.string/split  #"\n")
       (->> (mapv #(clojure.string/replace % #"\s+$" "")))))

(deftest test-simple-dance
  (is (= {:a 2 :b 3 :c 4}
         (dance {:a 1 :b 2 :c 3}
                :pre? number?
                :post? number?
                :post  inc))))

(deftest test-complex-dance
  (is (= '[[+ 2 [- 3 [* 4 (/ 4 5) [dec 3]]]]
           {:cnt 3
            :scoped #{:scoped :depth}
            :debug-depth -1
            :depth 0}]
         (dance '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
                depth-dance
                :walk?          #(and (seq? %) (-> % first (not= 'dec)))
                :pre?           #(and (seq? %) (-> % first (not= '/)))
                :pre            vec
                :post?          (fn [form ctx]
                                  [(and (number? form)
                                        (-> ctx :depth (< 4)))
                                   ctx])
                :post           (fn [x ctx] [(inc x) (update ctx :cnt inc)])
                :context        {:cnt 0}
                :return         :form-and-context))))

;; TODO: move to threading.core
(defmacro ^:private <-> [expr & body]
  `(-> ~expr (<- (-> ~@body))))

(deftest test-scoped-context
  (is (= ["- Walking        : (1 [:a] 3)"
          "  Context        : {:scoped #{:y :scoped :depth :x}, :x 0}"
          "  - Walking        : 1"
          "    Context        : {:scoped #{:y :scoped :depth :x}, :x 0}"
          "  - Walking        : [:a]"
          "    Context        : {:y 456, :scoped #{:y :scoped :depth :x}, :x 123}"
          "    - Walking        : :a"
          "      Context        : {:y 456, :scoped #{:y :scoped :depth :x}, :x 123}"
          "  - Walking        : 3"
          "    Context        : {:scoped #{:y :scoped :depth :x}, :x 0}"]
         (printed-lines
           (dance '(1 [:a] 3)
                  :context {:x 0}
                  :scoped [:x :y]
                  :debug true
                  :debug-context-whitelist [:x :y :scoped]
                  :pre? (fn [form ctx]
                          (if (vector? form)
                            [form (assoc ctx :x 123 :y 456)]
                            [form ctx]))))))
  (is (= ["- Walking        : (1 [:a] 3)"
          "  Context        : {:scoped #{:y :scoped :depth :x}, :x 0}"
          "  - Walking        : 1"
          "    Context        : {:scoped #{:y :scoped :depth :x}, :x 0}"
          "  - Walking        : [:a]"
          "    Context        : {:y 456, :scoped #{:y :scoped :depth :x}, :x 123}"
          "    - Walking        : :a"
          "      Context        : {:y 456, :scoped #{:y :scoped :depth :x}, :x 123}"
          "  - Walking        : 3"
          "    Context        : {:scoped #{:y :scoped :depth :x}, :x 0}"]
         (printed-lines
           (dance '(1 [:a] 3)
                  :step step-indexed ;; The only difference with code from above
                  :context {:x 0}
                  :scoped [:x :y]
                  :debug true
                  :debug-context-whitelist [:x :y :scoped]
                  :pre? (fn [form ctx]
                          (if (vector? form)
                            [form (assoc ctx :x 123 :y 456)]
                            [form ctx]))))))
  (testing "unscoping"
    (let [result    ;u       ;s       ;u                  ;s  ;u  ;s
          (dance [1 [2 [3] 4 [5 [6] 7 [8 [9] 10 [11 12] 13] 14] 15] 16]
                 :scoped [:n]
                 :pre (fn [form ctx]
                        [form
                         (-> ctx
                             (when-> (<-> form (and-> sequential?
                                                      (-> first number?)))
                               (assoc :n (-> form first))
                               (when-> (<-> form first #{2 8})
                                 (update :scoped disj :n))
                               (when-> (<-> form first (= 5))
                                 (update :scoped conj :n)))
                             (•- (update :contexts (comp vec concat)
                                         [[form (-• (dissoc :contexts))]])))])
                 :return :context)
          expected
          [[[1 [2 [3] 4 [5 [6] 7 [8 [9] 10 [11 12] 13] 14] 15] 16]
            {:scoped #{:n :scoped}, :n 1}]
           [1 {:scoped #{:n :scoped}, :n 1}]
           [[2 [3] 4 [5 [6] 7 [8 [9] 10 [11 12] 13] 14] 15]
            {:n 2, :scoped #{:scoped}}]
           [2 {:n 2, :scoped #{:scoped}}]
           [[3] {:n 3, :scoped #{:scoped}}]
           [3 {:n 3, :scoped #{:scoped}}]
           [4 {:n 3, :scoped #{:scoped}}]
           [[5 [6] 7 [8 [9] 10 [11 12] 13] 14] {:n 5, :scoped #{:n :scoped}}]
           [5 {:n 5, :scoped #{:n :scoped}}]
           [[6] {:n 6, :scoped #{:n :scoped}}]
           [6 {:n 6, :scoped #{:n :scoped}}]
           [7 {:n 5, :scoped #{:n :scoped}}]
           [[8 [9] 10 [11 12] 13] {:n 8, :scoped #{:scoped}}]
           [8 {:n 8, :scoped #{:scoped}}]
           [[9] {:n 9, :scoped #{:scoped}}]
           [9 {:n 9, :scoped #{:scoped}}]
           [10 {:n 9, :scoped #{:scoped}}]
           [[11 12] {:n 11, :scoped #{:scoped}}]
           [11 {:n 11, :scoped #{:scoped}}]
           [12 {:n 11, :scoped #{:scoped}}]
           [13 {:n 11, :scoped #{:scoped}}]
           [14 {:n 11, :scoped #{:n :scoped}}]
           [15 {:n 3 :scoped #{:scoped}}]
           [16 {:n 3, :scoped #{:n :scoped}}]]]
      (is (= expected (:contexts result))))))

(deftest test-right-context-dance
  (is (= [[1 {}]
          [2 {:a 1, :b 2}]
          [3 {:a 1, :b 2}]
          [4 {:a 1, :b 2, :c 3, :d 5}]
          [[2 3 4] {:a 1, :b 2, :d 5}]
          [5 {:b 2, :d 5, :e 5, :x {}}]
          [6 {:b 2, :d 5, :e 5, :x {}}]
          [[5 6] {:b 2, :d 5, :e 5, :x {}}]
          [7 {:b 2, :d 5, :e 5, :g 7, :x {}}]
          [[1 [2 3 4] [5 6] 7] {:b 2, :d 5, :e 5, :g 7, :x {}}]]
         (dance [1 [2 3 4] [5 6] 7]
                right-context-dance
                :scoped [:a :c :f]
                :before
                (fn [form ctx]
                  [form (case form
                          1       (add-to-right-context ctx :a 1 :b 2)
                          [2 3 4] (add-to-right-context
                                    ctx :x (get-right-context ctx))
                          3       (-> ctx
                                      (add-to-right-context :c 3 :d 4)
                                      ;; TODO: remove update-sister-context ?
                                      (update-right-context :d inc))
                          4       (add-to-right-context ctx :e 5)
                          6       (add-to-right-context ctx :f 6 :g 7)
                          ctx)])
                :post
                (fn [form ctx]
                  [form (update
                          ctx :results concat
                          [[form
                            (select-keys ctx [:a :b :c :d :e :f :g :x])]])])
                :return [:context :results]))))

(deftest test-with-context-dance
  (is (= ["- Walking        : [1 #:dance.core{:with-context {:form [:a], :ctx {:x 123, :y 456}}} 3]"
          "  Context        : {:scoped #{:scoped :depth :x}}"
          "  - Walking        : 1"
          "    Context        : {:scoped #{:scoped :depth :x}}"
          "  - Walking        : [:a]"
          "    Context        : {:y 456, :scoped #{:scoped :depth :x}, :x 123}"
          "    - Walking        : :a"
          "      Context        : {:y 456, :scoped #{:scoped :depth :x}, :x 123}"
          "  - Walking        : 3"
          "    Context        : {:y 456, :scoped #{:scoped :depth :x}}"]
         (printed-lines
           (dance [1 (with-context [:a] {:x 123 :y 456}) 3]
                  with-context-dance
                  :scoped [:x]
                  :debug-context true
                  :debug-context-whitelist [:x :y :scoped]))))
  (is (= ["- Walking        : [1 #:dance.core{:with-context {:form [:a], :ctx {:x 123, :y 456, :scoped [:y]}}} 3]"
          "  Context        : {:debug-depth -1, :x 0, :scoped #{:scoped :depth :x}, :depth 0}"
          "  - Walking        : 1"
          "    Context        : {:debug-depth -1, :x 0, :scoped #{:scoped :depth :x}, :depth 1}"
          "  - Walking        : [:a]"
          "    Context        : {:debug-depth -1, :scoped [:y], :depth 1, :x 123, :y 456}"
          "    - Walking        : :a"
          "      Context        : {:debug-depth -1, :scoped [:y], :depth 2, :x 123, :y 456}"
          "      - Walking        : 3"
          "        Context        : {:debug-depth -1, :scoped [:y], :depth 3, :x 123}"]
         (printed-lines
           (dance [1 (with-context [:a] {:x 123 :y 456 :scoped [:y]}) 3]
                  with-context-dance
                  :context {:x 0}
                  :scoped [:x]
                  :debug-context true)))))

(deftest test-dance-on-record
  (let [result (dance (DanceTestRecord. 1 2 [3 4 5])
                      :post? number?
                      :post  inc)]
    (is (instance? DanceTestRecord result))
    (is (= result (DanceTestRecord. 2 3 [4 5 6])))))

(deftest test-break-dance!
  (is (= [3 4 :needle [5 6]]
         (dance [1 2 [3 4 :needle [5 6]]]
                :pre? coll?
                :pre #(or (when (.contains % :needle)
                             (break-dance! %))
                           %)))))

; (deftest test-backtrack
;   (testing "no params"
;     (is (= [2 [2] [[1] [[[1]]]]]
;            (dance [1 [1] [[1] [[[1]]]]]
;                   depth-dance
;                   :before (fn [x ctx]
;                             (if (-> ctx :depth (>= 2))
;                               (backtrack)
;                               [x ctx]))
;                   :post?  number?
;                   :post   inc))))
;   (testing "one param (form)"
;     (is (= [2 [101] [101 101]]
;            (dance [1 [1] [[1] [[[1]]]]]
;                   depth-dance
;                   :before (fn [x ctx]
;                             (if (-> ctx :depth (>= 2))
;                               (backtrack 100)
;                               [x ctx]))
;                   :post?  number?
;                   :post   inc))))
;   (testing "two params (form ctx)"
;     (is (= [[2 [101] [101 101]] {:abc :xyz}]
;            (dance [1 [1] [[1] [[[1]]]]]
;                   depth-dance
;                   :before (fn [x ctx]
;                             (if (-> ctx :depth (>= 2))
;                               (backtrack 100 {:abc :xyz})
;                               [x ctx]))
;                   :post?  number?
;                   :post   inc
;                   :return :form-and-context))))
;   (testing "anticipated backtracking"
;     (println "--->" (backtrack [3 4]))
;     (pprint (dance [1 2 (backtrack [3 4])]
;                    depth-dance
;                    :debug true
;                    :post?  number?
;                    :post   inc
;                    :return :form-and-context))))

(deftest test-locals-tracking-dance
  (let [form '(let [a (• 1)
                    [b c] (• [2 3])
                    e (• 4)
                    ee (• 44)
                    [d e & {:keys [f]}] (• [(+ a b) (+ a c) :f (+ b c)])
                    g (let [h (• 5)]
                        (• 6))
                    i (• 7)]
               (• 8)
               (loop [j (• 9)
                      k (• 10)
                      l (fn f1 [m]
                          (let [n (• 11)]
                            (• (inc n))))
                      o (fn [p] (• p))]
                 (• 12)
                 (fn
                   ([q] (• (inc q)))
                   ([r s] (let [z 0]
                            (• (+ r s)))))
                 (letfn [(t [u] (• (inc u)))
                         (v ([w] (• (inc w)))
                            ([x y] (• (+ x y))))]
                   (• (t (v 1 2))))))
        results (dance
                  form locals-tracking-dance
                  :pre
                  (fn [form ctx]
                    [form
                     (if-> ctx (<- (-> form (and-> seq? (-> first (= '•)))))
                       (update :results concat
                               [[(second form)
                                 (->> ctx :locals
                                      (filter #(-> % name count #{1 2})))]]))])
                  :return [:context :results])]
    (is (= '[[1 ()]
             [[2 3]                        (a)]
             [4                            (a b c)]
             [44                           (a b c e)]
             [[(+ a b) (+ a c) :f (+ b c)] (a b c e ee)]
             [5                            (a b c ee d e f)]
             [6                            (a b c ee d e f h)]
             [7                            (a b c ee d e f g)]
             [8                            (a b c ee d e f g i)]
             [9                            (a b c ee d e f g i)]
             [10                           (a b c ee d e f g i j)]
             [11                           (a b c ee d e f g i j k f1 m)]
             [(inc n)                      (a b c ee d e f g i j k f1 m n)]
             [p                            (a b c ee d e f g i j k l p)]
             [12                           (a b c ee d e f g i j k l o)]
             [(inc q)                      (a b c ee d e f g i j k l o q)]
             [(+ r s)                      (a b c ee d e f g i j k l o r s z)]
             [(inc u)                      (a b c ee d e f g i j k l z t u)]
             [(inc w)                      (a b c ee d e f g i j k l z t v w)]
             [(+ x y)                      (a b c ee d e f g i j k l z t v x y)]
             [(t (v 1 2))                  (a b c ee d e f g i j k l z t v)]]
           results))))

(deftest test-free-symbols-collecting-dance
  (let [form '(for [a [1 2 3]
                    :let [b (inc (+ a (+ c (+ d 100))))]]
               (+ a d e))]
    (is (= '[c d e] (dance form free-symbols-collecting-dance)))))

(deftest test-leafs-collecting-dance
  (let [form [1 2 [3 4] 5]]
    (is (= {[] [1 2 [3 4] 5]
            [0] 1
            [1] 2
            [2] [3 4]
            [2 0] 3
            [2 1] 4
            [3] 5}
           (dance form leafs-collecting-dance)))))
