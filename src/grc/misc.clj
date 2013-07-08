(ns grc.misc)

(def ^:dynamic *verbose* 'false)

(defn log-val [tag & val]
  (if-not grc.misc/*verbose* nil
          (println (.toString (java.util.Date.)) tag val)))


