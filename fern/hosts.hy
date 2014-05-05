(import [subprocess [Popen DEVNULL PIPE STDOUT]])


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
      (setv self.control-path "/tmp/%h.sock")
      (apply (. (super Connection self) --init--)
        [(.-ssh-command self ["-M"])]
        {"stdout" DEVNULL "stderr" DEVNULL "stdin" PIPE})
      None)]

   [execute
    (fn [self command]
      (let [[popen (apply Popen
                     [(.-ssh-command self [] [command])]
                     {"stdout" PIPE "stderr" STDOUT "stdin" PIPE
                      "universal_newlines" true})]]
        (, (, (. popen stdin) (. popen stdout) (. popen stderr)) (. popen poll))))]

   [-ssh-command
    (fn [self &optional [extra-options []] [extra-args []]]
      (+ ["ssh"] ["-S" (. self control-path) "-o" "StrictHostKeyChecking=no"]
         extra-options [(hostname (. self host))] extra-args))]])
