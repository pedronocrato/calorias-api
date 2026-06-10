(defproject calorias-api "0.1.0-SNAPSHOT"
  :description "API de controle de calorias"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [clj-http "3.12.3"]
                 [cheshire "5.11.0"]
                 [ring/ring-core "1.9.6"]
                 [ring/ring-jetty-adapter "1.9.6"]
                 [ring/ring-json "0.5.1"]
                 [compojure "1.6.2"]]
  :main ^:skip-aot calorias-api.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})