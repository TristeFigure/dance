(ns dance.core
  (:require [shuriken.associative :refer [deep-merge]]
            [shuriken.debug :refer [debug-print]]
            [shuriken.namespace :refer [import-namespace]]
            [arity.core :refer [arities]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [dance.choreography :as choreo]))

(import-namespace dance.floor :except [is-form?])
(import-namespace dance.choreography)

(def ^:private is-form? @#'dance.floor/is-form?)

(def ^:private dance-identity (context| identity))

(def ^:dynamic *default-dance*
  {})

;; TODO: refactor as a dance-enabled ability
(defn break-dance!
  "Used to immediately return a value during a [[dance]]. Can be used
  in any of the dance fns."
  ([form] (break-dance! form nil))
  ([form ctx]
   (throw (ex-info "Returned early"
                   {:type ::break-dance
                    :form form
                    :ctx  ctx}))))

(defn with-context
  ([ctx]
   (with-context ::no-form ctx))
  ([form ctx]
   {::with-context {:form form :ctx ctx}}))

(defn with-context? [form]
  (and (map? form)
       (contains? form ::with-context)))

(defn- handle-with-context [form ctx]
  (if (with-context? form)
    (let [ctx-form (-> form ::with-context :form)
          ctx-ctx  (-> form ::with-context :ctx)]
      [(if (= ctx-form ::no-form) form          ctx-form)
       (if (fn? ctx-ctx)          (ctx-ctx ctx) (deep-merge ctx ctx-ctx))])
    [form ctx]))

(def with-context-dance
  {:walk?      handle-with-context
   :pre?       handle-with-context
   :pre        handle-with-context
   :post?      handle-with-context
   :post       handle-with-context
   :before     handle-with-context
   :after      handle-with-context
   :before-all handle-with-context
   :after-all  handle-with-context})

(defn backtrack
  ([form]
   (with-context form {::backtrack {:form form}}))
  ([form ctx]
   (with-context form {::backtrack {:form form :ctx ctx}})))

(defn backtrack? [ctx]
  (contains? ctx ::backtrack))

(defn- handle-backtrack [form ctx]
  (if (backtrack? ctx)
    [(-> ctx ::backtrack :form)
     (-> ctx ::backtrack (get :ctx (dissoc ctx ::backtrack)))]
    [form ctx]))

(def backtracking-dance
  (merge-dances
    with-context-dance
    {:walk?      handle-backtrack
     :pre?       handle-backtrack
     :pre        handle-backtrack
     :post?      handle-backtrack
     :post       handle-backtrack
     :before     handle-backtrack
     :after      handle-backtrack
     :before-all handle-backtrack
     :after-all  handle-backtrack}))

(defmacro ^:private add-debugs [debugs let-statement]
  (let [debugs (resolve (or (and (symbol? debugs) debugs)
                            debugs))]
    (->> (update (vec let-statement) 1
                 (fn [bindings]
                   (->> bindings
                        (partition 2)
                        (map (fn [[k v]]
                               (if-let [d (debugs k)]
                                 [['prev-ctx 'ctx]
                                  [k v]
                                  ['_ d]]
                                 [[k v]])))
                        (apply concat)
                        (apply concat)
                        vec)))
         (apply list))))

(defn- should-debug? [ctx condition]
  (and condition
       (if-let [d (:debug-depth ctx)]
         (if (neg? d)
           true
           (<= (:depth ctx) (:debug-depth ctx)))
         true)))

(defn- filter-ctx [ctx debug-context-whitelist debug-context-blacklist]
  (if (seq debug-context-whitelist)
    (select-keys ctx debug-context-whitelist)
    (into {} (remove #(contains? debug-context-blacklist (key %))
                     ctx))))

(def ^:private tab "  ")

(def ^:private dance*-debugs
  '{[should-walk ctx]
    (when (should-debug? ctx debug)
      (let [tabs (str (apply str (repeat depth tab))
                      "- ")]
        (cond
          (backtrack? ctx) (debug-print   (str tabs "Backtracked on ") form)
          should-walk      (debug-print   (str tabs "Walking        ") form)
          :else            (debug-print   (str tabs "Not walking    ") form)))
      (when (should-debug? ctx debug-context)
        (debug-print                     (str tabs "Context        ")
                     (filter-ctx ctx
                                 debug-context-whitelist
                                 debug-context-blacklist))))

    [pre-result ctx]
    (when (should-debug? ctx debug)
      (cond
        (backtrack? ctx) (debug-print     (str tabs "Backtracked on ") form)
        should-pre       (when (should-debug? ctx (not= pre-result form))
                           (debug-print   (str tabs "Pre            ") pre-result)))
      (when (should-debug? ctx (and debug-context (not= ctx prev-ctx)))
        (debug-print                      (str tabs "New context    ")
                                          (filter-ctx ctx
                                                      debug-context-whitelist
                                                      debug-context-blacklist)))
      (when (•- ctx (-> :debug-depth (and-> (some-> (>= 0))
                                            (= (-• :depth))
                                            (<- (coll? pre-result)))))
        (println
          (str tabs tab
               "... (not showing " (count pre-result) " children nodes)"))))

    [result ctx]
    (when (should-debug? ctx debug)
      (if should-post
        (when (should-debug? ctx (not= result posted out))
          (debug-print                (str tabs "Post           ") result))
        (debug-print                  (str tabs "No post        ") result))
      (if (should-debug? ctx (and debug-context (not= ctx prev-ctx)))
        (debug-print                  (str tabs "New context    ")
                                      (filter-ctx ctx
                                                  debug-context-whitelist
                                                  debug-context-blacklist))))})

;; Variable names in this function's "let" matter for the debugging from above.
(defn- dance*
  [form {:keys [walk?
                pre?    pre
                post?   post
                before  after
                context return
                debug   debug-context
                step
                debug-context-whitelist debug-context-blacklist]
         :as opts}]
  ;; TODO: ensure debugs are not added at compile-time if unnecessary.
  (add-debugs
    dance*-debugs
    (let [[form ctx]        (before form context)
          depth             (get ctx :depth 0)
          tabs              (apply str (repeat (inc depth) "  "))
          [should-pre ctx]  (pre? form ctx)
          [should-walk ctx] (walk? form ctx)
          [pre-result ctx]  (if should-pre
                              (pre form ctx)
                              [form ctx])
          opts              (assoc opts :context ctx)
          [out ctx]         (if should-walk
                              (context-walk
                                step
                                (fn [form ctx]
                                  (dance* form (assoc opts :context ctx)))
                                dance-identity
                                pre-result
                                ctx)
                              [pre-result ctx])
          [should-post ctx]   (post? out ctx)
          [posted ctx]        (if should-post
                                (post out ctx)
                                [out ctx])
          [result ctx]        (after posted ctx)]
      [result ctx])))

(defn- identities [& args]
  (when-> args (-> count (= 1))
    first))

;; TODO: support deepmerge (the way contexts are merged)
(defn dance
  "A finely tunable, composable, equivalent of clojure.walk with enhancements.

  For a demo, just run
  ```clojure
  (dance
    '(+ 1 (- 2 (* 3 (/ 4 5) (dec 3))))
    :walk?          #(and (seq? %) (-> % first (not= 'dec)))
    :pre?           #(and (seq? %) (-> % first (not= '/)))
    :pre            vec
    :post?          number?
    :post           (fn [x ctx] [(inc x) (update ctx :cnt inc)])
    :context        {:cnt 0}
    :return         :form-and-context
    :debug          true
    )
  ```

  #### Dance fns

  See https://en.wikipedia.org/wiki/Tree_traversal

  Namely `walk?`, `pre?`, `pre`, `post?`, `post`, `before`, `after`,
  `before-all` and `after-all`.

  `pre?` and `post?` respectively condition `pre` and `post` while the
  walk of the substructure itself occurs in between and is conditioned
  by `walk?`. `before`and `after` are respectively called before and
  after any node is processed (before `pre`) while `before-all` and
  `after-all` are called once, at the beginning and the end of the walk.

  Traversal appears to occur in pre-order for `before`, `walk?` `pre?`
  and `pre`, but in post-order for `post?`, `post` and `after`.

  Note that nodes that will not be walked may still be processed by
  `pre` and `post`.

  #### Context

  Dance fns can have 1 argument (the node at hand) or 2, in order to
  receive an optional, latent context. If so, they must return a
  vector like `[result context]` where `result` can be a processed
  substructure (`pre`, `post`, ...) or the return value of a predicate
  (`pre?`, `post?`, ...).

  This context is passed walking down the structure from node to
  subnodes then back to the parent nodes up to the initial root in a
  depth-first manner.

  By default, it is global to the walk and is not scoped by the depth
  at which it being accessed. In other words, the context is passed
  from siblings to siblings in the order of the walk and modifications
  done to it when walking a node can be seen when walking the sister
  nodes, their children and eventually their ancestors.

  However the `:scoped` option can be used to specify which keys in
  the context should be scoped to the current node and its children
  and not thread through the whole traversal. More specifically parents
  pass the context to their children and changes made by the children are
  not visibile by their ancestors or siblings.

  Note that variadic arguments functions count as single-argument to
  accomodate for functions like `constantly`, and thus cannot receive
  a context.

  #### Merging dances

  Multiple dances can be passed to `dance`, including a varargs dance:

  ```clojure
  (dance collection
    first-dance
    second-dance
    :before third-dance-before
    :after  third-dance-after)
  ```

  Options in these dance will be merged in a smart way according to
  this plan:

  ```
  - before, pre : composed from left to right
  - after, post : composed from right to left
  - pre?, walk? : composed like `and`, from left to right
  - post?       : composed like `and`, but from right to left.
  - context     : composed with `merge`
  - scoped      : composed with `#(clojure.set/union (set %1) (set %2))`
  ```

  `:debug`, `:return `, and `:step` are merged normally, i.e.
  via right-most preference.

  #### Early return

  At any moment, from within any of the dance fns, [[break-dance!]]
  can be used to halt the execution, returning the given form (and
  optional context). Consider:

  ```clojure
  (dance [1 2 3 4 5 6]
    :pre? number?
    :pre (fn [x]
            (if (> x 4)
              (break-dance! :abc)
              (println x))))
  => :abc
  ```

  #### Additional options

  - `context`                : The initial context (defaults to `{}`)
  - `return`                 : `:form`, `:form-and-context` or `:context` (:form).
  - `step`                   : low-level option. See [[step]] and
                               [[step-indexed]]. Defaults to [[step]]
  - `depth`                  : The intial depth. Determines tabulation (`0`)
                               when debugging.
  - `debug`                  : To print a detailed trace of the walk (`false`).
  - `debug-depth`            : To limit the debug trace to a certain depth.
  - `debug-context-blacklist`: To mask certain context keys when debugging.
  - `debug-context-whitelist`: To display only certain context keys.

  Any option passed to `dance` is optional, including the dance fns."
  [form & args]
  (let [dances (take-while map? args)
        args-dance (->> args
                        (drop-while map?)
                        (partition 2)
                        (map vec)
                        (into {}))
        dances (concat dances [args-dance])
        opts-dance (apply merge-dances empty-dance *default-dance* dances)
        debug-context (if (contains? opts-dance :debug-context)
                        (:debug-context args-dance)
                        (or (#{:context :form-and-context}
                               (:return args-dance))
                            (contains? args-dance :context)
                            (contains? args-dance :debug-context-whitelist)
                            (contains? args-dance :debug-context-blacklist)
                            (->> (select-keys args-dance
                                              [:walk? :pre? :pre :post? :post
                                               :before :after
                                               :before-all :after-all])
                                 vals
                                 (mapcat arities)
                                 (some #(= % 2)))))
        opts-dance (-> (assoc opts-dance
                         :debug-context debug-context
                         :debug (get opts-dance :debug
                                     (:debug-context opts-dance)))
                       (when->> :debug (merge-dances choreo/depth-dance)))
        {:keys [before-all after-all]} opts-dance
        [form ctx] (before-all form (:context opts-dance))
        [danced ctx] (try (dance* form (-> opts-dance
                                           (dissoc :before-all :after-all)
                                           (assoc :context ctx)
                                           (•- (assoc-in [:context :scoped]
                                                         (-• :scoped)))
                                           (let-> [d :debug-depth]
                                             (when-> (<- d)
                                               (assoc-in [:context :debug-depth]
                                                         d)))))
                       (catch clojure.lang.ExceptionInfo e
                         (let [d (ex-data e)]
                           (if (-> d :type (= ::break-dance))
                             ((juxt :form :ctx) d)
                             (throw e)))))
        [result ctx] (after-all danced ctx)
        [mode returner] (let [r (-> opts-dance :return)]
                          (if (keyword? r) [r identities] r))]
    ;; TODO: document :return properly
    (case mode
      :form             (returner result)
      :context          (returner ctx)
      :form-and-context (returner result ctx))))
