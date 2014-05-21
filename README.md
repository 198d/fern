Fern
====

Fern (a German word meaning 'remote' or 'distant'; pronounced *fɛrn*) is a
remote execution framework written in [Hy](http://hylang.org/). Fern provides
tools for defining clusters of remote hosts, including metadata, and for
executing commands against those hosts, targeting them, arbitrarily, based on
their metadata.


Getting Started
===============

```hy
(require fern.dsl)


(def applicaton-hosts
  [["web1.application.com" [:web]]
   ["web2.application.com" [:web]]
   ["jobs.application.com" [:jobs]]
   ["db1.application.com" [:db] {:primary true :secondary false}]
   ["db2.application.com" [:db] {:primary false :secondary true}]]


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
