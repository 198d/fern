(import [subprocess [Popen DEVNULL PIPE STDOUT]])


(defn hostname [host]
  (get host 0))


(defn roles [host]
  (try
    (get host 1)
    (catch [e IndexError] [])))


(defn tags [host]
  (try
    (get host 2)
    (catch [e IndexError] {})))


(defn provides? [host role]
  (any (list-comp true [-role (roles host)] (= -role role))))


(defn tagged? [host key value]
  (= (.get (tags host) key) value))


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
