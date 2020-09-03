(ns dance.core-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]
            [dance.debug :reload true]
            [dance.floor :reload true]
            [dance.choreography :reload true]
            [dance.core :refer :all :reload true]
            [threading.core :refer :all]
            [weaving.core :refer :all]))

(defrecord DanceTestRecord [a b c])

(defmacro printed-lines [& body]
  `(-> (with-out-str ~@body)
       (clojure.string/split  #"\n")
       (->> (mapv #(clojure.string/replace % #"\s+$" "")))))

; (pprint (clojure.walk/macroexpand-all '(defdance a-dance {:pre inc})))

(defdance a-dance {:pre inc})
(defdance b-dance :pre dec)
(defdance c-dance :pre -)

(defdance abc-dance
  a-dance b-dance c-dance
  :pre (fn [x] (println x) x))


; ;; TODO: namespace dance names
(deftest test-dance-name
  (is (= 'dance.core-test/a-dance    (dance-name a-dance)))
  (is (= [nil]                       (map dance-name (subdances a-dance))))
  (is (= 'dance.core-test/b-dance    (dance-name b-dance)))
  (is (= ['dance.core-test/b-dance*] (map dance-name (subdances b-dance))))
  (is (= [nil]                       (map dance-name (subdances b-dance*))))
  (is (= 'dance.core-test/abc-dance  (dance-name abc-dance)))
  (is (= '[dance.core-test/a-dance
           dance.core-test/b-dance
           dance.core-test/c-dance
           dance.core-test/abc-dance*]
         (map dance-name (subdances abc-dance)))))

(deftest test-dependent-dances
  (is (= [{:pre inc}]              (dependent-dances a-dance)))
  (is (= nil                       (dependent-dances b-dance)))
  (is (= [a-dance b-dance c-dance] (dependent-dances abc-dance))))

(deftest test-impl-dance
  (is (= nil        (dance-impl a-dance)))
  (is (= b-dance*   (dance-impl b-dance)))
  (is (= abc-dance* (dance-impl abc-dance))))

(deftest test-subdances
  (is (= [{:pre inc}]                         (subdances a-dance)))
  (is (= [b-dance*]                           (subdances b-dance)))
  (is (= [a-dance b-dance c-dance abc-dance*] (subdances abc-dance)))
  (testing "with a sequence of dances"
    (is (= [{:pre inc} b-dance* a-dance b-dance c-dance abc-dance*]
           (subdances [a-dance b-dance b-dance abc-dance])))))

(deftest test-atomic-dance?
  (is (false? (atomic-dance? a-dance)))
  (is (true?  (atomic-dance? (-> a-dance subdances first))))
  (is (false? (atomic-dance? b-dance)))
  (is (true?  (atomic-dance? b-dance*)))
  (is (false? (atomic-dance? abc-dance)))
  (is (true?  (atomic-dance? abc-dance*))))

(deftest test-atomic-dances
  (is (= [{:pre inc}] (atomic-dances a-dance)))
  ;; TODO
  #_(is (= [] (atomic-dances (-> a-dance subdances first))))
  (is (= [b-dance*]   (atomic-dances b-dance)))
  (is (= [{:pre inc} b-dance* c-dance* abc-dance*]
         (atomic-dances abc-dance)))
  (testing "with an atomic dance"
    (is (= [{:pre inc}] (atomic-dances {:pre inc})))
    (is (= [b-dance*]   (atomic-dances b-dance*))))
  (testing "with a sequence of dances"
    (is (= [{:pre inc} b-dance* c-dance* abc-dance*]
           (atomic-dances [a-dance b-dance* b-dance abc-dance])))))

(deftest test-simple-dance
  (is (= {:a 2 :b 3 :c 4}
         (dance {:a 1 :b 2 :c 3}
                :pre?  number?
                :post? number?
                :post  inc))))

(deftest test-complex-dance
  #_(is (= '[[+ 2 [- 3 [* 4 (/ 4 5) [dec 3]]]]
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

(def alphabet
  (set (mapcat (comp (juxt str symbol keyword)
                     str char)
               (range 97 (+ 97 26)))))

(defdance scoping-dance
  :before (fn [form ctx]
            [form
             (-> ctx
                 (when-> (<- (is-form? '›› form))
                   (•- (<- (reduce #(update %1 :scoped conj %2)
                                   (-•)
                                   alphabet))))
                 (when-> (<- (is-form? '‹‹ form))
                   (•- (<- (reduce #(update %1 :scoped disj %2)
                                   (-•)
                                   alphabet)))))]))

(defdance scope-testing-dance
  scoping-dance
  :context {:n 0}
  :before (fn [form ctx]
            [form
             (-> ctx
                 (when-> (<- (is-form? '•== form))
                   (let-> [n :n]
                     (tap->
                       (<- (testing form
                             (is (= n (second form))))))))
                 (when-> (<- (is-form? '•= form))
                   (assoc :n (second form))))]))

(deftest test-scoped-context
  (testing "unscoping when scoped"
    (dance '(››
             [(•== 0)
              (‹‹ (•= 1 (•== 1 (‹‹ [(•== 1)]))))
              (•== 1)
              (•= 1)]
             (•== 0))
           scope-testing-dance))
  (testing "scoping when unscoped"
    (dance '(‹‹
             [(•== 0)
              (›› (•= 1 (•== 1 [(•== 1) (‹‹ [(•== 1)])])))
              (•== 0)
              (•= 1)]
             (•== 1))
           scope-testing-dance)))


(defdance precontext-testing-dance
  scoping-dance
  :before (fn [form ctx]
            [form
             (-> ctx
                 (when-> (<- (is-form? '•> form))
                   (as-> $ (add-to-next-context $ (second form))))
                 (when-> (<- (is-form? '•== form))
                   (tap (testing form
                          (is (= (second form)
                                 (select-keys ctx alphabet)))))))]))

(deftest test-precontext-dance
  #_(dance '[(•> {:a 1 :b 2} (•== {}))
           [(•== {:a 1 :b 2})
            (•> {:c 3 :d 4} (•== {:a 1 :b 2}))]
           (•== {:b 2 :c 3 :d 4})]
         precontext-dance
         precontext-testing-dance
         :scoped [:a :c]
         :debug-context-whitelist (concat alphabet [:precontexts
                                                    :precontext-to-absorb])))


(defdance right-context-testing-dance
  :post (fn [form ctx]
          [form
           (-> ctx
               (when-> (<- (is-form? '•> form))
                 (as-> $ (add-to-right-context form $ (second form))))
               (when-> (<- (is-form? '•== form))
                 (tap (testing form
                        (is (= (second form)
                               (select-keys ctx alphabet)))))))]))

;; TODO: remove ?
(deftest test-right-context-dance
  #_(dance '[(•> {:a 1 :b 2} (•== {}))
           [(•== {:a 1 :b 2})
            (•> {:c 3 :d 4} (•== {:a 1 :b 2}))]
           (•== {:b 2})]
         precontext-dance
         right-context-testing-dance
         :scoped [:a :c]
         :debug-context-whitelist alphabet))


(deftest test-with-context-dance
  #_(is (= ["- Walking        : [1 #:dance.core{:with-context {:form [:a], :ctx {:x 123, :y 456}}} 3]"
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
  #_(is (= ["- Walking        : [1 #:dance.core{:with-context {:form [:a], :ctx {:x 123, :y 456, :scoped [:y]}}} 3]"
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
  #_(let [result (dance (DanceTestRecord. 1 2 [3 4 5])
                      :post? number?
                      :post  inc)]
    #_(is (instance? DanceTestRecord result))
    #_(is (= result (DanceTestRecord. 2 3 [4 5 6])))))

(deftest test-break-dance!
  #_(is (= [3 4 :needle [5 6]]
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

;; TODO: move to shuriken.string
(defn strip-ansi-controls [s]
  (str/replace s #"\e\[[0-9;]*m" ""))

(deftest test-debugging-a-dance
  ;; TODO: make a debugging test.
  ;; TODO: need to take into account pre? and post? steps.

  ;; TODO: clojure.core/keyword has [name] or [ns name] for args.
  ;; In other words, when receiving a function with
  ;; conflicting args:
  ;;    - if it is anonymous: favor the longer arity
  ;;    - else, favor the shorter arity
  ;; This requires to hack into the `fn` macro and
  ;; the reader for #() forms, so that they add
  ;; the necessary data to the fn's meta.

  ; (dance [1 2 "abc" '(x y z) [3 4 5]]
  ;        :before (when| number? inc)
  ;        :pre?   (fn [form ctx]
  ;                  [(string? form)
  ;                   (if (= form 2)
  ;                     (assoc ctx :pre? true)
  ;                     ctx)])
  ;        :pre    #(keyword %) ;; TODO: see above.
  ;        :walk?  (fn [form ctx]
  ;                  [(vector? form) (assoc ctx :walkable true)])
  ;        :post?  (fn [form ctx]
  ;                  [(vector? form)
  ;                   (if (= form 3)
  ;                     (assoc ctx :post? true)
  ;                     ctx)])
  ;        :post   (fn [form ctx]
  ;                  [(seq form)
  ;                   (if (> (count form) 3)
  ;                     (assoc ctx :big true)
  ;                     ctx)])
  ;        :after (when| number? inc))

  (doseq [[step space] [["Before"] ["Pre?  "] ["Pre   "]
                        ["Walk? "]
                        ["Post? "] ["Post  "] ["After "]]]
    (testing (str "at step " step)
      (is (= (str "Walking             • 1\n"
                  "│ " step " (new ctx)  · {:n 100}\n")
             (strip-ansi-controls
               (with-out-str
                 (dance 1
                        (-> step str/trim str/lower-case keyword)
                        (fn [form ctx] [form (assoc ctx :n 100)])
                        :debug true
                        :debug-context-whitelist [:n]))))))))

(def gensym-regex #".*__\d+")

(defdance locals-testing-dance
  :pre (fn [form ctx]
         [(-> form
              (when-> (->> (is-form? '•))
                (let-> [ret second
                        then (->> (drop 2))]
                  (<- `(do ~@then ~ret)))))
          (-> ctx
              (when-> (<- (is-form? '•== form))
                (tap->
                  (<- (testing form
                        (is (= (second form)
                               (remove (->| str (|| re-matches gensym-regex))
                                       (:locals ctx)))))))))])
  :walk? (form| (not| (|| is-form? '•==))))

(deftest test-locals-tracking-dance
  #_(dance '(let [k (•== [])]
           (•== [k])
           (•== [k]))
         locals-tracking-dance
         locals-testing-dance
         :debug true
         ; :debug? (fn [form ctx]
         ;           (is-form? '[fn fn*] form))
         :debug-context-whitelist [:locals :scoped]
         :debug-context-style :changed)
  #_(dance '(do
           (let [a                   (• 0 (•== []))
                 [b c]               (• [0 0] (•== [a]))
                 ;; -- redefined var appears last in locals vector
                 b                   (• 0 (•== [a b c]))
                 [d e & {:keys [f]}] (• [0 0 :f 0] (•== [a c b]))
                 ;; -- nested lets
                 g (•= 0 (let [h (• 0 (•== [a c b d e f]))]
                           (•== [a c b d e f h])
                           ;; well scoped
                           (•== [a c b d e f h])))
                 i (•= 0 (•== [a c b d e f g]))]
             ;; -- loop
             #_(loop [j (•= 0 (•== [a c b d e f g i]))
                    ;; -- single body named fns
                    k (•= (fn func [l]
                            (let [m (•= 0 (•== [a c b d e f g i j func l]))]
                              (•== [a c b d e f g i j func l m])
                              ;; well scoped
                              (•== [a c b d e f g i j func l m]))
                            (•== [a c b d e f g i j func l]))
                          (•== [a c b d e f g i j]))]
               ;; -- multiple bodies fns
               (fn
                 ([n]        (•= 0 (•== [a c b d e f g i j k n])))
                 ([o p]      (•= 0 (•== [a c b d e f g i j k o p]))))
               (fn func
                 ([n]        (•= 0 (•== [a c b d e f g i j k func n]))))
               ;; -- flat body fns
               (fn* func [n] (•= 0 (•== [a c b d e f g i j k func n])))
               ;; -- letfn
               #_(letfn [(q [r]    (•= 0 (•== [a c b d e f g i j k q r])))
                         (s ([t]   (•= 0 (•== [a c b d e f g i j k s t])))
                            ([u v] (•= 0 (•== [a c b d e f g i j k s u v]))))]
                   (•== [a c b d e f g i j k q s])
                   ;; well scoped
                   #_(•== [a c b d e f g i j k q s])))
             ;; well scoped
             #_(•== [a c b d e f g i]))
           ;; well scoped
           #_(•== []))
         locals-tracking-dance
         locals-testing-dance
         :debug true
         :debug-depth 4
         :debug-context-whitelist [:locals :precontexts :parent-precontext
                                   :precontext-to-absorb :scoped]))

#_(deftest test-locals-tracking-dance
  (let [form ;; let*
             '(let [a (• 1)
                    [b c] (• [2 3])
                    ; e (• 4)
                    ; ee (• 44)
                    ; [d e & {:keys [f]}] (• [a b :f c])
                    ; ;; nested lets
                    ; g (let [h (• 5)]
                    ;     (• 6))
                    ; i (• 7)
                    ]
               ; (• 8)
               ;; loop*
               #_(loop [
                      ; j (• 9)
                      ; k (• 10)
                      ; ;; fn*
                      ; l (fn f1 [m]
                      ;     (let [n (• 11)]
                      ;       (• (inc n))))
                      ; o (fn [p] (• p))
                      ]
                 ; (• 12)
                 (fn
                   ([q] (• (inc q)))
                   ([r s] (let [t 0]
                            (• (+ r s)))))
                 ;; letfn*
                 (letfn [(u [v] (• (inc v)))
                         #_(w ([x]   (• (inc x)))
                            ([y z] (• (+ y z))))]
                   #_(• (aa (bb 1 2))))
                 ;; reify*
                 ;; TODO: test when method calls itself
                 #_(reify SomeProtocol
                   (method-a [this]    (• 13))
                   (method-b [this cc] (• 14)))

                 ;; shadowing
                 #_(let [let (fn [dd] (inc dd))
                         x (• )])))
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
                  :return [:context :results]
                  :debug true
                  :debug-depth 4
                  ; :debug-context-whitelist [:scoped :locals]
                  :debug-context-blacklist [:next-parent :index :debug-depth
                                            :ancestors :depth :parsed-fn-form
                                            :results]
                  )]
    (pprint results)
    #_(is (= '[[1           ()]
             [[2 3]       (a)]
             [4           (a b c)]
             [44          (a b c e)]
             [[a b :f c]  (a b c e ee)]
             [5           (a b c ee d e f)]
             [6           (a b c ee d e f h)]
             [7           (a b c ee d e f g)]
             [8           (a b c ee d e f g i)]
             [9           (a b c ee d e f g i)]
             [10          (a b c ee d e f g i j)]
             [11          (a b c ee d e f g i j k f1 m)]
             [(inc n)     (a b c ee d e f g i j k f1 m n)]
             [p           (a b c ee d e f g i j k l p)]
             [12          (a b c ee d e f g i j k l o)]
             [(inc q)     (a b c ee d e f g i j k l o q)]
             [(+ r s)     (a b c ee d e f g i j k l o r s t)]
             [(inc v)     (a b c ee d e f g i j k l z t u)]
             #_[(inc x)     (a b c ee d e f g i j k l z t v w)]
             #_[(+ y z)     (a b c ee d e f g i j k l z t v x y)]
             #_[(t (v 1 2)) (a b c ee d e f g i j k l z t v)]
             #_[13          (a b c ee d e f g i j k l z t v)]
             #_[14          (b c ee d e f g i j k l z t v a)]]
           results))))

(deftest test-free-symbols-collecting-dance
  (let [form '(for [a [1 2 3]
                    :let [b (inc (+ a (+ c (+ d 100))))]]
               (+ a d e))]
    #_(is (= '[c d e] (dance form free-symbols-collecting-dance)))))

(deftest test-leafs-collecting-dance
  (let [form [1 2 [3 4] 5]]
    #_(is (= {[] [1 2 [3 4] 5]
            [0] 1
            [1] 2
            [2] [3 4]
            [2 0] 3
            [2 1] 4
            [3] 5}
           (dance form leafs-collecting-dance)))))

(run-tests)
