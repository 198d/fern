(import [subprocess [Popen PIPE STDOUT]])


(defn filter-hosts [hosts &optional roles tags]
  (list-comp host
             [host hosts]
             (and (provides? host roles)
                  (tagged? host tags))))


(defn hostname [host]
  (get host 0))


(defn provides? [host match-roles]
  (let [[[name roles tags] host]]
    (or (not match-roles) (any (list-comp true
                                          [role roles]
                                          (in role match-roles))))))


(defn tagged? [host match-tags]
  (let [[[name roles tags] host]]
    (or (not match-tags)
        (and tags
             (all (list-comp (= (first (rest item))
                                (.get tags (first item)))
                             [item (.items match-tags)]))))))


(defn connect [host]
  (try
    (. connect cache)
    (catch [AttributeError]
      (setv connect.cache {})))
  (try
    (get (. connect cache) (hostname host))
    (catch [KeyError]
      (assoc (. connect cache) (hostname host) (Connection host))
      (get (. connect cache) (hostname host)))))


(defclass Connection [Popen]
  [[--init--
    (fn [self host]
      (setv self.host host)
      (apply (. (super Connection self) --init--)
        [["ssh" "-qt" "-o" "StrictHostKeyChecking=no" (hostname host)
          "/bin/bash" "--norc"]]
        {"stdout" PIPE "stderr" STDOUT "stdin" PIPE "bufsize" 0
         "universal_newlines" true})
      None)]

   [--iter--
    (fn [self]
      (iter (. self stdout)))]

   [write
    (fn [self &rest args &kwargs kwargs]
      (apply (. self stdin write) args kwargs))]

   [read
    (fn [self &rest args &kwargs kwargs]
      (apply (. self stdout read) args kwargs))]

   [readline
    (fn [self &rest args &kwargs kwargs]
      (apply (. self stdout readline) args kwargs))]])
