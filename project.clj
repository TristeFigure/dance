(defproject dance "0.1.3"
  :description "Advanced tree walking in Clojure"
  :url "https://github.com/TristeFigure/dance"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [arity "0.2.0"]
                 [weaving "0.1.5"]
                 [threading "0.3.4"]
                 [org.clojars.tristefigure/shuriken "0.14.46"]
                 [org.flatland/ordered "1.5.7"]]
  :profiles {:dev {:dependencies [[codox-theme-rdash "0.1.2"]]}}
  :plugins [[lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/TristeFigure/dance/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
