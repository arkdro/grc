(defproject grc "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  ;; :jvm-opts ["-Xmx1000m" "-Xss200m"]
  ;; :global-vars {
  ;;   *warn-on-reflection* true
  ;;   ;;*assert* false
  ;;   }
  :main grc.core
  :dependencies [
    [org.clojure/clojure "1.3.0"]
    [org.clojure/tools.cli "0.2.2"]
    [com.taoensso/timbre "2.1.2"]
    [org.clojure/tools.trace "0.7.5"]
  ])
