Fern
====

Fern (a German word meaning 'remote' or 'distant'; pronounced *fɛrn*) is a
remote execution framework written in [Hy](http://hylang.org/). Fern provides
tools for defining clusters of remote hosts, including metadata, and for
executing commands against those hosts, targeting them, arbitrarily, based on
their metadata.


Getting Started
===============

```clojure
(require fern.dsl)


(def applicaton-hosts
  [["web1.application.com" [:web]]
   ["web2.application.com" [:web]]
   ["jobs.application.com" [:jobs]]
   ["db1.application.com" [:db] {:primary true :secondary false}]
   ["db2.application.com" [:db] {:primary false :secondary true}]])


(defn deploy []
  (with-hosts :in application-hosts
              :where (or (provides? host :web)
                         (provides? host :jobs))
    (run :in /opt/application
      (git pull origin master))
    (run :in /opt/application
         :with (source /opt/application/bin/activate)
         :once true
      (python manage.py syncdb))
    (with-hosts :where (provides? host :web)
      (run (sudo service nginx restart)))
    (with-hosts :where (provides? host :jobs)
      (run (circusctl restart celery-worker)))))


(defn snapshot []
  (with-hosts :in application-hosts
              :where (and (provides? host :db)
                          (tagged? host :secondary true))
    (run :once
      (mongodump))))
```

All of Fern's functionality is provided in the form of macros in the `fern.dsl`
module. To make them available, simply `require` the module:

```clojure
(require fern.dsl)
```

For Fern to do anything useful, it needs to know about some hosts that it can
work on. A host, in Fern, is a tuple consisting of a host string, a list of
roles and a dictionary of tags or meta data.  The set of hosts for some
imaginary [Django](https://www.djangoproject.com) might look like this:

```clojure
(def applicaton-hosts
  [["web1.application.com" [:web]]
   ["web2.application.com" [:web]]
   ["jobs.application.com" [:jobs]]
   ["db1.application.com" [:db] {:primary true :secondary false}]
   ["db2.application.com" [:db] {:primary false :secondary true}]])
```

Once there are hosts to work on, Fern's `with-hosts` and `run` macros can be
used. `with-hosts` is used to select and filter hosts that should be operated
on. Using the `application-hosts` defined above, to select the hosts that are
in the `:web` or `:jobs` roles, the call to `with-hosts` would look like this:

```clojure
(with-hosts :in application-hosts
            :where (or (provides? host :web)
                       (provides? host :jobs)))
```
