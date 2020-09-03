(ns dance.core
  (:use clojure.pprint)
  (:require [shuriken.associative :refer [map-vals deep-merge]]
            [shuriken.sequential :refer [update-nth insert-at]]
            [shuriken.string :refer [adjust]]
            [shuriken.namespace :refer [import-namespace with-ns]]
            [arity.core :refer [arities]]
            [threading.core :refer :all]
            [weaving.core :refer :all]
            [dance.floor]
            [dance.choreography]))

;; TODO: deprecated in newer versions of shuriken: renamed to
;;  import-namespace-vars
(import-namespace dance.floor)
(import-namespace dance.choreography)

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

(defn- dance*
  [form {:keys [before after
                pre?    pre
                walk?   walk
                step    step-in step-out
                post?   post
                context return
                debug   debug?
                debug-context debug-context-whitelist debug-context-blacklist
                debug-context-style]
         :as opts}]
  ;; Variable names in this "let" matter for the with-debugs macro
  ;; TODO: ensure debugs are not added at compile-time unless necessary.
  (with-debugs
    (let [[form ctx]        (before form context)
          [should-pre ctx]  (pre? form ctx)
          [form ctx]        (if should-pre
                              (pre form ctx)
                              [form ctx])
          [should-walk ctx] (walk? form ctx)
          opts              (assoc opts :context ctx)
          [form ctx]        (if should-walk
                              (walk opts form ctx)
                              [form ctx])
          [should-post ctx] (post? form ctx)
          [form ctx]        (if should-post
                              (post form ctx)
                              [form ctx])
          [form ctx]        (after form ctx)]
      [form ctx])))

(def empty-dance
  (adapt-dance-fns
    {:init (ctx->| prepare-dance-args prepare-scoped prepare-debug)
     :before-all  identity   :after-all  identity
     :before      identity   :after      identity
     :pre?        any?       :pre        identity
     :walk?       any?       :walk       context-walk
     :post?       any?       :post       identity
     :context     nil        :return     :form
     :step        step       :step-in    dance*     :step-out same
     :scoped      #{:scoped}
     :handle-scope standard-handle-scope
     :dance-args   []
     :debug-context-whitelist #{}
     :debug-context-blacklist #{}
     :debug-context-style     :changed}))


(defn- identities [& args]
  (when-> args (-> count (= 1))
    first))

;; TODO: support deepmerge (the way contexts are merged)
;; TODO: modify example to show how to use :init
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
  via right-most preference. TODO: not anymore.

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

  - `context`                : The initial context (defaults to `{}`).
  - `return`                 : `:form`, `:form-and-context` or `:context` (:form).
  - `step`                   : low-level option. See [[step]] and
                               [[step-indexed]]. Defaults to [[step]]
  - `depth`                  : The intial depth. Determines tabulation (`0`)
                               when debugging.
  - `debug`                  : To print a detailed trace of the walk (`false`).
  - `debug-depth`            : To limit the debug trace to a certain depth.
  - `debug-context-blacklist`: To mask certain context keys when debugging.
  - `debug-context-whitelist`: To display only certain context keys.
  - `debug?`                 : a fn form,ctx-->bool to limit debugging output.

  Any option passed to `dance` is optional, including the dance fns."
  [form & args]
  (let [dances (take-while map? args)
        args-dance (->> args
                        (drop-while map?)
                        (partition 2)
                        (map vec)
                        (into {}))
        dances (concat dances [args-dance])
        dance (apply merge-dances empty-dance *default-dance* dances)
        [dance ctx] ((:init dance) (dissoc dance :context) (:context dance))
        {:keys [before-all after-all]} dance
        [form ctx] (before-all form ctx)
        [danced ctx] (try (dance* form (assoc dance :context ctx))
                       (catch clojure.lang.ExceptionInfo e
                         (let [d (ex-data e)]
                           (if (-> d :type (= ::break-dance))
                             ((juxt :form :ctx) d)
                             (throw e)))))
        [result ctx] (after-all danced ctx)
        [mode returner] (let [r (-> dance :return)]
                          (if (keyword? r) [r identities] r))]
    ;; TODO: document :return properly
    (case mode
      :form             (returner result)
      :context          (returner ctx)
      :form-and-context (returner result ctx))))
