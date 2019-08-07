(defproject de.active-group/active-codecs "0.1.1"
  :description "Composable codecs, encoding and decoding higher level values to lower level sequences of codes."
  :url "http://github.com/active-group/active-codecs"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.10.0"]]

  :profiles {:test {:source-paths ["src" "test"]}

             :codox {:dependencies [[codox-theme-rdash "0.1.2"]]}}

  :codox {:language :clojure
          :metadata {:doc/format :markdown}
          :themes [:rdash]
          :src-dir-uri "http://github.com/active-group/active-codecs/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :plugins [[lein-codox "0.10.7"]])
