(defproject pseudo-redux "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 ]
  :repl-options {:init-ns pseudo-redux.core}
  :main pseudo-redux.core
  :profiles
  {:uberjar {:omit-source true
             :aot :all
             :uberjar-name "pseudo-redux.jar"}})
