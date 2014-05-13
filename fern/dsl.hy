(import [re]
        [hy.models.keyword [HyKeyword]]
        [hy.models.list [HyList]]
        [hy.models.string [HyString]])


(defmacro defhosts [name &rest host-defs]
  `(def ~name (list-comp [(first host-def)
                          (or (nth host-def 1) [])
                          (or (nth host-def 2) {})] [host-def [~@host-defs]])))


(defmacro/g! run [&rest args]
  (let [[(, options body) (parse-args args)]]
    (defn rewrite-body [body]
      (let [[cwd (.get options :in)] [prefix (.get options :with)]]
        (defn parse-arg [option]
          (.sub re "^_+" (fn [match] (print match) (* "-" (len (.group match)))) (str option)))
        (defn parse-command [expr]
          `(.join " " [~@(list-comp (cond [(instance? HySymbol part)
                                          (HyString (parse-arg part))]
                                         [(and (instance? HyList part)
                                               (= 'quote (first part)))
                                          (first (rest part))]
                                         [:else (HyString part)])
                                   [part expr])]))
        (list-comp
          `(.join " && " (list-comp part [part (+ [~(parse-command (if prefix `(~@prefix) '()))]
                                                  [~(parse-command (if cwd `(cd ~cwd) '()))]
                                                  [~(parse-command command)])] part))
          [command body])))
    `(do
      (import [sys [exit :as ~g!exit]]
              [random [sample :as ~g!sample]]
              [fern.hosts [connect :as ~g!connect hostname :as ~g!hostname]]
              [fern.execution [execute :as ~g!execute wait-results :as ~g!wait-results]]
              [fern.output [green :as ~g!green red :as ~g!red bold :as ~g!bold]])
      (let [[~g!commands [~@(rewrite-body body)]]
            [~g!against ~(.get options :against 'nil)]
            [~g!once ~(.get options :once 'false)]
            [~g!hosts (if (and ~g!against ~g!once) (~g!sample ~g!against (int "1")) ~g!against)]
            [~g!print-results (fn [results]
                                (for [result results]
                                  (let [[hostname (~g!hostname (or (. result host) ["local"]))]]
                                    (if (.ready? result)
                                      (if (.success? result)
                                        (print "  " (~g!green "\u2713") hostname)
                                        (print "  " (~g!red "\u2717") hostname))
                                      (print "  " "\u25E6" hostname)))))]
            [~g!tick-fn (fn [results]
                          (import [fern.output [cursor-up]])
                          (apply print [(cursor-up (len results)) "\r"] {"end" ""})
                          (~g!print-results results))]
            [~g!ready-fn (fn [result]
                           (let [[hostname (~g!hostname (or (. result host) ["localhost"]))]]
                             (if (.success? result)
                               (print "  " (~g!green "\u2713") hostname)
                               (do
                                 (print "  " (~g!red "\u2717") hostname)
                                 (for [line (. result stdout)]
                                   (apply print ["  " line] {"end" ""}))
                                 ; hy==0.10.0 defmacro/g! pukes on HyObjects that don't have a
                                 ; `startswith` instance method (e.g. integers), slight hack to
                                 ; avoid that, PR awaiting
                                 (~g!exit (int "1"))))))]]
        (list
          (map
            (fn [command]
              (print (.format "> Executing `{}`" (~g!bold command)))
              (if ~g!hosts
                (let [[results (list-comp (~g!execute command (~g!connect ~g!host)) [~g!host ~g!hosts])]]
                  (~g!print-results results)
                  (~g!wait-results results (fn [&rest args]) ~g!tick-fn))
                (let [[results [(~g!execute command)]]]
                  (~g!print-results results)
                  (~g!wait-results results (fn [&rest args]) ~g!tick-fn))))
            ~g!commands))))))


(defmacro/g! with-hosts [&rest args]
  (let [[(, options body) (parse-args args {:in (fn [arg] `(+ ~@arg []))})]]
    (defn rewrite-body [body]
      (let [[-body '()]]
        (for [part body]
          (.append -body (if (instance? HyList part)
                           (cond [(= 'with-hosts (first part))
                                  `(~@part :in [~g!hosts])]
                                 [(= 'run (first part)) `(~@part :against ~g!hosts)]
                                 [true (rewrite-body part)])
                           part)))
        -body))
    `(do
      (import [fern.hosts [filter-hosts :as ~g!filter-hosts]]
              [pprint [pprint :as ~g!pprint]])
      (let [[~g!hosts (apply ~g!filter-hosts [~(.get options :in)]
                                             {"roles" ~(.get options :providing '[])
                                              "tags" ~(.get options :where '{})})]]
        (if ~(.get options :show 'false)
          (~g!pprint ~g!hosts))
        ~@(rewrite-body body)))))


(defn parse-args [args &optional [map-fns {}]]
  (let [[iter-args (iter args)] [options {}] [body []]]
    (for [arg iter-args]
      (if (instance? HyKeyword arg)
        (assoc options arg ((.get map-fns arg identity) (next iter-args)))
        (.append body arg)))
    (, options body)))
