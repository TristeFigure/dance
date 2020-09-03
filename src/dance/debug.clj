(ns dance.debug
  (:use clojure.pprint)
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]
            [flatland.ordered.map :refer [ordered-map]]
            [backtick :refer [template]]
            [threading.core :refer :all]
            [weaving.core   :refer :all]
            [zprint.core :refer [czprint] :as zprint]
            [zprint.config :refer [default-zprint-options]]
            [shuriken.associative :refer [map-vals]]
            [shuriken.sequential :refer [update-nth]]
            [shuriken.string :refer [lines join-lines tabulate truncate
                                     adjust]]))

;; TODO: backport this multi-sym version to shuriken.
;; We need to copy it over here because it's in shuriken.macro which already
;; depends on dance.core.
;; TODO: change is-form? so that:
;; - the form is the last argument
;; - (is-form? let* [my-func my-var] the-form) is possible
(defn is-form? [sym-or-syms expr]
  (let [syms (-> sym-or-syms
                 (when-not-> coll? list)
                 set)]
    (boolean (and (seq? expr) (-> expr first syms)))))

(defn- safe-resolve [sym]
  (-> sym (or-> resolve
                (->> (format "Unable to resolve symbol: %s in this context")
                     (new RuntimeException)
                     throw))))

;; TODO: dependency on zprint is for parts to move to shuriken
;; TODO: move this to shuriken
(defn edit-bindings [edits binding-expr]
  (let [edits (-> edits
                  (when-> symbol? safe-resolve)
                  (->> flatten (partition 2)
                       (into (ordered-map))))]
    (update-nth
      binding-expr 1
      (fn [bindings]
        (->> (reduce (fn [acc [matcher transformer]]
                       acc
                       (mapcat (fn [sv]
                                 (when-> sv matcher transformer))
                               (partition 2 acc))
                       )
                     bindings
                     edits)
             vec)))))

(defn- match-sym| [target-sym]
  (let [target-sym? (when-not->> target-sym fn? (partial =))]
    (fn [[sym _expr]] (target-sym? sym))))

(defn- match-expr| [target-expr]
  (let [target-expr? (when-not->> target-expr fn? (partial =))]
    (fn [[_sym expr]] (target-expr? expr))))

(defn return-args [& args]
  args)

(defn- insert|
  ([position]                   identity)
  ([position val-expr]          (insert| position '_ val-expr))
  ([position sym-expr val-expr] (fn [sv]
                                  (case position
                                    :before (concat [sym-expr val-expr] sv)
                                    :after  (concat sv [sym-expr val-expr]))))
  ([position s v & more-binds]  (let [compose (case position
                                                :before comp
                                                :after ->|)
                                      binds (->> (concat [s v] more-binds)
                                                 (partition 2))]
                                  (apply compose
                                         (map (|| apply insert| position)
                                              binds)))))

(defn- edit| [fsym fexpr]
  (fn [[s v]]
    [(fsym s) (fexpr v)]))

(defn- edit-sym| [f]
  (edit| f identity))

(defn- edit-expr| [f]
  (edit| identity f))

(def ^:private half-tab " ")
(def ^:private tab      (str half-tab half-tab))

(defmacro should-debug? [ctx condition]
  (template
    (and debug
         ~condition
         (if-let [d (:debug-depth ~ctx)]
           (and (not (neg? d)) ;; TODO: remove this condition ?
                (<= (:depth ~ctx) d))
           true))))

;; TODO: backport to shuriken.debug
(defn term-width []
  (->> (clojure.java.shell/sh "/bin/sh" "-c" "stty -a < /dev/tty")
       :out (re-find #"(\d+) columns") second Integer/parseInt))

;; TODO: ditto
(defn format-code
  ([code] (format-code code 72))
  ([code width] (format-code code width {}))
  ([code width opts]
   (with-out-str
     (czprint code width
              (merge (-> default-zprint-options
                         (update :color-map
                                 (|| map-vals (when| #{:black} (<-| :yellow)))))
                     opts)))))

;; TODO: ditto
(def ^:dynamic *separator* ":")

(defn ^:no-doc debug-print* [tabs label result]
  (let [offset (+ (count label) (count tabs))
        width-left (- (term-width) offset 2)
        result-str (format-code result width-left {:style :community})
        [f & more] (lines result-str)
        left-tabs (apply str "│"
                         (repeat (+ (count label) (count *separator*))
                                 half-tab))]
    (if (empty? more)
      (println (str tabs label *separator* " " f))
      (do (print (str tabs label *separator* " " f))
          (print \newline)
          (println (tabulate (join-lines more) (str tabs left-tabs)))))
    result))

(defmacro debug-print [label expr]
  `(debug-print* ~'tabs ~label ~expr))

(defmacro debug-it [& {:keys [label expr condition else separator form]
                       :or   {condition true separator *separator*}}]
  (template
    (binding [~(do `*separator*) ~separator] ;; TODO deal with ~` properly
      (if (should-debug? ctx (and debug-context ~condition))
        (debug-print ~label ~expr)
        ~@(when else [(template (when (should-debug? ctx debug-context)
                                  ~else))])))))

(defmacro filter-ctx [ctx]
  (template
    (let [ctx# ~ctx]
      (if (seq debug-context-whitelist)
        (select-keys ctx# debug-context-whitelist)
        (into {} (remove #(contains? debug-context-blacklist (key %))
                         ctx#))))))

(defn- prev-sym [sym]
  (symbol (str "prev-" sym)))

(defmacro changed?
  ([sym]      `(changed? ~(prev-sym sym) ~sym))
  ([prev new] `(or (not= ~prev ~new)
                   (not= (class ~prev) (class ~new)))))

(defmacro changed-ctx?
  ([sym]      `(changed-ctx? ~(prev-sym sym) ~sym))
  ([prev new] `(not= (filter-ctx ~prev) (filter-ctx ~new))))

(defmacro debug-changed
  ([label sym] `(debug-changed ~label ~(prev-sym sym) ~sym))
  ([label prev new & {:keys [condition else separator]
                      :or {condition true separator *separator*}}]
   `(debug-it :label ~label :expr ~new
              :condition (and ~condition (changed? ~prev ~new))
              :else ~else
              :separator ~separator)))

(defmacro debug-changed-ctx
  ([label sym] `(debug-changed-ctx ~label ~(prev-sym sym) ~sym))
  ([label prev new & {:keys [condition else separator]
                      :or {condition true
                           separator *separator*}}]
   `(debug-it :label ~label :expr (filter-ctx ~new)
              :condition (and ~condition (case ~'debug-context-style
                                           :changed (changed-ctx? ~prev ~new)
                                           :full    true))
              :else ~else
              :separator ~separator)))

(defmacro safe-step [label & body]
  (template (try ~@body
              (catch Throwable t#
                (println (str tabs ~label))
                (throw t#)))))

(def adjustment
  20)

(defmacro step-lbl [label kind]
  `(adjust :left adjustment
           (case ~'debug-context-style
             :changed (format "%s (new %s) " ~label ~kind)
             :full    ~label)))

(defn empty-lbl [label]
  (apply str "│" (repeat (dec (count label)) " ")))

;; TODO: move to shurikend
(defn fin []
  (throw (Exception. "fin")))

(defn- debug-step|
  ([label] (debug-step| label {}))
  ([label {:keys [condition else separator debug-form debug-ctx]
           :or {condition  true separator *separator*
                debug-form true debug-ctx true}}]
   (->|
     (edit-expr| (fn [expr]
                   `(safe-step ~label ~expr))) ;; TODO: replace ` with '
     (when| (<-| debug-form)
       (insert| :after
                '_ (template (debug-changed (step-lbl ~label "form")
                                            prev-form form
                                            :condition ~condition
                                            :separator ~separator))))
     (when| (<-| debug-ctx)
       (insert| :after
                '_ (template (debug-changed-ctx
                               (step-lbl
                                 (if (and (changed? form) ~debug-form)
                                   (empty-lbl ~label)
                                   ~label)
                                 "ctx")
                               prev-ctx ctx
                               :condition ~condition
                               :separator ~separator
                               :else ~else))))))
  ;; TODO: remove ?
  ([_ifkw condition then-label & [else-label opts]]
   (assert (= _ifkw :if))
   (comp (insert| :before 'prev-ctx 'ctx)
         (debug-step|
           then-label
           (merge {:condition condition
                   :else (when else-label
                           (template
                             (when-not ~condition
                               (debug-changed-ctx
                                 (step-lbl (if (changed? form)
                                             (empty-lbl ~else-label)
                                             ~else-label)
                                   "ctx")
                                 prev-ctx ctx
                                 :else (println
                                         (str tabs (adjust :left ~adjustment
                                                           ~else-label)
                                              "·"))))))}
                  opts)))))

;; TODO: need to take into account pre? and post? steps.
(def ^:private dance-debugs
  [;; Set up some local variables at the top of the let's bindings
   (match-expr| (|| is-form? 'before))
   (insert| :before
            'context      '(update context :depth (fnil inc -1))
            'ctx          'context
            'debug        '(and debug (if debug? (debug? form ctx) true))
            'depth        '(get ctx :depth)
            'tabs         (template (apply str (repeat depth ~tab))))

   ;; Add temp variables to see if things change
   (match-sym| (?| '[form ctx]))
   (insert| :before '[prev-form prev-ctx] '[form ctx])

   (match-sym| (and| vector?
                     (->| first (not| (?| 'form)))
                     (->| second (?| 'ctx))))
   (insert| :before 'prev-ctx 'ctx)

   ;; Header
   (match-expr| (|| is-form? 'before))
   (insert| :before (template (debug-it
                                :expr form
                                :label (adjust :left ~adjustment
                                               "Walking")
                                :separator "•")))

   ;; Before
   (match-expr| (|| is-form? 'before))
   (debug-step|                                "│ Before"  {:separator "·"})

   ;; Pre?
   (match-expr| (|| is-form? 'pre?))
   (debug-step|                                "│ Pre?  "  {:separator "·"
                                                            :debug-form false})

   ;; Pre
   (match-expr| (and| seq? (->| (|| take 2) (?| '(if should-pre)))))
   (debug-step|                                "│ Pre   " {:separator "·"})

   ;; Walk?
   (match-expr| (|| is-form? 'walk?))
   (debug-step|                                "│ Walk? "  {:separator "·"
                                                            :debug-form false})

   (match-expr| (and| seq? (->| (|| take 2) (?| '(if should-walk)))))
   (->| ;; When limited by debug-depth
        (insert|
          :before (template
                    (when (let [d (:debug-depth ctx)]
                            (and d
                                 (coll? form)   should-walk
                                 (not (neg? d)) (= (:depth ctx) d)))
                      (println (str tabs (adjust
                                           :left ~adjustment
                                           (str "... "
                                                (cl-format
                                                  nil "(masking ~d subform~:p)"
                                                  (count form)))))))))
        ;; No walk
        (insert|
          :after (template
                   (when (should-debug? ctx (and debug-context
                                                 (not should-walk)
                                                 (coll? form)))
                     (println (str tabs (adjust :left ~adjustment
                                                "│ No walk") "·"))))))

   ;; Post?
   (match-expr| (|| is-form? 'post?))
   (debug-step|                                "│ Post? "  {:separator "·"
                                                            :debug-form false})

   ;; Post
   (match-expr| (and| seq? (->| (|| take 2) (?| '(if should-post)))))
   (debug-step|                                "│ Post  " {:separator "·"})

   ;; After
   (match-expr| (|| is-form? 'after))
   (debug-step|                                "│ After "  {:separator "·"})])

(defmacro with-debugs [let-statement]
  (edit-bindings dance-debugs let-statement))

