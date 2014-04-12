(defn green [text]
  (.format "\033[32m{}\033[0m" text))


(defn red [text]
  (.format "\033[31m{}\033[0m" text))


(defn bold [text]
  (.format "\033[1m{}\033[0m" text))
