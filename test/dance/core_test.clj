(ns dance.core-test
  (:require [clojure.test :refer :all]
            [dance.core :refer :all]))

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
  (is (= '[[+ 2 [- 3 [* 4 (/ 4 5) [dec 3]]]] {:cnt 3}]
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
                :return-context true))))

(deftest test-scoped-context
  (is (= ["- Walking        : (1 [:a] 3)"
          "  Context        : {:x 0, :depth 0}"
          "  - Walking        : 1"
          "    Context        : {:x 0, :depth 1}"
          "  - Walking        : [:a]"
          "    Context        : {:x 123, :depth 1, :y 456}"
          "    - Walking        : :a"
          "      Context        : {:x 123, :depth 2, :y 456}"
          "  - Walking        : 3"
          "    Context        : {:x 0, :depth 1}"]
         (printed-lines
           (dance '(1 [:a] 3)
                  :context {:x 0}
                  :scoped [:x :y]
                  :debug true
                  :pre? (fn [form ctx]
                          (if (vector? form)
                            [form (assoc ctx :x 123 :y 456)]
                            [form ctx]))))))
  (is (= ["- Walking        : (1 [:a] 3)"
          "  Context        : {:x 0, :depth 0}"
          "  - Walking        : 1"
          "    Context        : {:x 0, :depth 1, :index 0}"
          "  - Walking        : [:a]"
          "    Context        : {:x 123, :depth 1, :index 1, :y 456}"
          "    - Walking        : :a"
          "      Context        : {:x 123, :depth 2, :index 0, :y 456}"
          "  - Walking        : 3"
          "    Context        : {:x 0, :depth 1, :index 2}"]
         (printed-lines
           (dance '(1 [:a] 3)
                  :step step-indexed ;; The only difference with code from above
                  :context {:x 0}
                  :scoped [:x :y]
                  :debug true
                  :pre? (fn [form ctx]
                          (if (vector? form)
                            [form (assoc ctx :x 123 :y 456)]
                            [form ctx])))))))

(deftest test-with-context-dance
  (is (= ["- Walking        : [1 #:dance.core{:with-context {:form [:a], :ctx {:x 123, :y 456}}} 3]"
          "  Context        : {:depth 0}"
          "  - Walking        : 1"
          "    Context        : {:depth 1}"
          "  - Walking        : [:a]"
          "    Context        : {:depth 1, :x 123, :y 456}"
          "    - Walking        : :a"
          "      Context        : {:depth 2, :x 123, :y 456}"
          "  - Walking        : 3"
          "    Context        : {:depth 1, :y 456}"]
         (printed-lines
           (dance [1 (with-context [:a] {:x 123 :y 456}) 3]
                  with-context-dance
                  :scoped [:x]
                  ; :debug-context true
                  ))))
  (is (= ["- Walking        : [1 #:dance.core{:with-context {:form [:a], :ctx {:x 123, :y 456, :scoped [:y]}}} 3]"
          "  Context        : {:x 0, :depth 0}"
          "  - Walking        : 1"
          "    Context        : {:x 0, :depth 1}"
          "  - Walking        : [:a]"
          "    Context        : {:x 123, :depth 1, :y 456, :scoped [:y]}"
          "    - Walking        : :a"
          "      Context        : {:x 123, :depth 2, :y 456, :scoped [:y]}"
          "  - Walking        : 3"
          "    Context        : {:x 0, :depth 1, :scoped [:y]}"]
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

(deftest test-backtrack
  #_(testing "no params"
    (is (= [2 [2] [[1] [[[1]]]]]
           (dance [1 [1] [[1] [[[1]]]]]
                  depth-dance
                  :before (fn [x ctx]
                            (if (-> ctx :depth (>= 2))
                              (backtrack)
                              [x ctx]))
                  :post? number?
                  :post inc))))
  #_(testing "one param (form)"
    (is (= [2 [101] [101 101]]
           (dance [1 [1] [[1] [[[1]]]]]
                  depth-dance
                  :before (fn [x ctx]
                            (if (-> ctx :depth (>= 2))
                              (backtrack 100)
                              [x ctx]))
                  :post? number?
                  :post inc))))
  #_(testing "two params (form ctx)"
    (is (= [[2 [101] [101 101]] {:abc :xyz}]
           (dance [1 [1] [[1] [[[1]]]]]
                  depth-dance
                  :before (fn [x ctx]
                            (if (-> ctx :depth (>= 2))
                              (backtrack 100 {:abc :xyz})
                              [x ctx]))
                  :post? number?
                  :post inc
                  :return-context true))))
  #_(testing "anticipated backtracking"
    (println "--->" (backtrack [3 4]))
    #_(pprint (dance [1 2 (backtrack [3 4])]
                   depth-dance
                   :debug true
                   :post? number?
                   :post inc
                   :return-context true))))
