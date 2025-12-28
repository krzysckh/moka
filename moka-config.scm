(define *db-file* "moka.db")
(define *uploads-dir* "static/uploads")
(define *port* 8081) ; port for app served via fastcgi
(define *language* 'PL)

(define *tz-offset*
  (lets ((r w (popen "date +%z"))
         (v (/ (read (car* (force-ll (lines r)))) 100)))
    (map close-port (list r w))
    v))
