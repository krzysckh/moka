;; -*- Owl -*-

(import
 (owl toplevel)
 (prefix (owl sys) sys/)
 (prefix (robusta fastcgi) r/)
 (prefix (robusta log) r/)
 (prefix (robusta server) r/)
 (prefix (robusta http) r/)
 (prefix (robusta dispatcher) r/)
 (prefix (robusta encoding html) html/)
 (prefix (robusta encoding json) json/)
 )

(when (not (has? *features* 'sqlite))
  (error "compile-time sqlite extension is required." #n))

(import
 (prefix (ext sqlite io) s3/))

(define *db-file* "moka.db")
(define *uploads-dir* "static/uploads")

(when (not (sys/directory? *uploads-dir*))
  (sys/mkdir *uploads-dir*))

(define (start-migrate-thread)
  (thread
   'migrate
   (let loop ((fs #n))
     (lets ((who v (next-mail)))
       (tuple-case v
         ((add f) (loop (cons f fs)))
         ((migrate ptr)
          (for-each (λ (tbl) (tbl ptr)) fs)
          (mail who 'ok)
          (loop #n))
         (else
          (loop fs)))))))

(define (add-migration f)
  (mail 'migrate (tuple 'add f)))

(start-migrate-thread)

;; unsafe
(define (table-has-column? ptr name col)
  (let ((v (s3/execute ptr (str "SELECT * FROM pragma_table_info('" name "') WHERE name = ?") (list (str col)))))
    (> (len v) 0)))

(define (list->sql-list lst)
  (cond
   ((null? lst) "")
   ((> (len lst) 1)
    (fold (λ (a b) (str a ", " b)) (car lst) (cdr lst)))
   (else ; = 1
    (str (car lst)))))

;; unsafe
(define-syntax define-table
  (syntax-rules (42 => _getk keys migrate)
    ((_ 42 name)
     (λ _ (print (str "[define-table] ok: " 'name))))
    ((_ 42 name k => (v ...) . rest)
     (λ (ptr)
       (when (not (table-has-column? ptr 'name 'k))
         (print "[define-table] new column for migration: " 'k)
         (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " " (fold (λ (a b) (str a " " b)) "" '(v ...)) ";")))
       ((_ 42 name . rest) ptr)))
    ((_ 42 name k => v . rest)
     (λ (ptr)
       (when (not (table-has-column? ptr 'name 'k))
         (print "[define-table] new column for migration: " 'k)
         (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " " 'v ";")))
       ((_ 42 name . rest) ptr)))
    ((_ _getk k => v . rest)
     (cons 'k (_ _getk . rest)))
    ((_ _getk) #n)
    ((_ (name constructor) . rest)
     (define-values (delivered constructor)
       (values
        (begin
          (add-migration
           (λ (ptr)
             (s3/execute ptr (str "CREATE TABLE IF NOT EXISTS " 'name " (id integer not null primary key)"))
             ((_ 42 name . rest) ptr))))
        (λ (ff*)
          (lets ((keys (filter (λ (x) (not (eq? 'nope (get ff* x 'nope)))) (_ _getk . rest))))
            (λ (ptr)
              (s3/execute
               ptr
               (str "INSERT INTO " 'name "(" (fold (λ (a b) (str a ", " b)) (car keys) (cdr keys)) ") VALUES ("
                    (fold (λ (a b) (string-append a ", ?")) "?" (cdr keys)) ")")
               (map
                (λ (x) (get ff* x 0))
                keys)))))
        )))
    ))

(define (db-refresher)
  (thread
   'db
   (let loop ((ptr (s3/open *db-file*)))
     (lets ((who v (next-mail)))
       (tuple-case v
         ((get)
          (mail who ptr)
          (loop ptr))
         ((refresh)
          ;; (print "refreshing database pointer")
          (s3/close ptr)
          (loop (s3/open *db-file*)))))))
  (thread
   (let loop ()
     (sleep (* 30 1000))
     (mail 'db (tuple 'refresh))
     (loop)))
  )

(define (db)
  (interact 'db (tuple 'get)))

(define-table (roasteries make-roastery)
  name   => text
  image  => int
  url    => text
  notes  => text
  )

(define-table (coffees make-coffee)
  name        => text
  roastery    => int
  roast_level => int
  image       => int    ; -> uploads
  url         => text
  additional  => text
  )

(define-table (grinders make-grinder)
  name        => text
  image       => int
  url         => text
  additional  => text
  )

(define-table (methods make-method)
  name        => text
  image       => int
  additional  => text
  )

(define-table (uploads make-upload)
  timestamp => datetime
  location  => text
  )

(define-table (opinions make-opinion)
  timestamp    => datetime
  coffee       => int  ; -> coffees
  grinder      => int  ; -> grinders
  method       => int  ; -> methods
  local_p      => bool ; made at home?
  grind_level  => int
  rating       => int
  image        => int  ; -> uploads
  notes        => text
  )

(let ((ptr (s3/open *db-file*)))
  (interact 'migrate (tuple 'migrate ptr))
  (s3/close ptr))

(define (db-get table items)
  (lets ((p (db)))
    (s3/execute p (str "SELECT " (list->sql-list items) " FROM " table) #n)))

(define (make-page body)
  (html/encode
   `(html
     (head
      ((meta (name . "viewport") (content . "width=device-width, initial-scale=1")))
      ((meta (charset . "utf-8")))
      ((link (href . "https://cdn.jsdelivr.net/npm/beercss@3.13.1/dist/cdn/beer.min.css") (rel . "stylesheet")))
      ((script (src . "/static/app.js")))
      ((script (type . "module") (src . "https://cdn.jsdelivr.net/npm/beercss@3.13.1/dist/cdn/beer.min.js")))
      ((script (type . "module") (src . "https://cdn.jsdelivr.net/npm/material-dynamic-colors@1.1.2/dist/cdn/material-dynamic-colors.min.js"))))
     ((body (class . "dark"))
      ((main (class . "responsive"))
       ((dialog (id . "choose_upload_dialog"))
        (h5 "wybierz obrazek")
        (article
         ,@(map (λ (it)
                  ;; `((img (src . ,(str (car it))) (loading . "lazy"))))
                  `((article (class . "no-padding border"))
                    ((img (class . "responsive medium") (src . ,(str (car it))) (loading . "lazy")))
                    ((div (class . "absolute bottom left right padding bottom-shadow white-text"))
                     (nav
                      ((button (class . "circle transparent") (type . "button") (onClick . ,(str "image_selected('" (str (cadr it)) "')")))
                       (i "add"))))))
                (db-get 'uploads '(location id)))))
       ,@body)))))

(define (make-input-item type id label . required?)
  (case type
    ('text `((label (class . "field border label"))
             ((input (name . ,id) ,@(if (not (null? required?)) '((required . "true")) '())))
             (label ,label)))
    ('upload `((label (class . "field border label"))
               ((button (class . "chip circle"))
                (i "upload")
                ((input (name . ,id)
                        (type . "file")
                        (accept . "image/png image/jpg image/gif image/heic image/heic-sequence")
                        ,@(if (not (null? required?)) '((required . "true")) '()))))))
    ('image `((div (class . "row"))
              ((label (class . "field border label"))
               ((input (type . "number") (id . ,id) (name . ,id) ,@(if (not (null? required?)) '((required . "true")) '()))
                (label ,label)))
              ((button (type . "button") (onClick . ,(str "load_upload_id('" id "')"))) "wybierz " ,label)
              ))
    (else (error "unknown type: " type))))

(define (make-form action legend lst)
  `((form ("action" . ,action) ("method" . "POST"))
    (fieldset
     (legend ,legend)
     ,@(map (λ (l) (apply make-input-item l)) lst)
     ((label (class . "field"))
      ((button (class . "circle extra"))
       (i "add"))))))

(define (make-list-of table items)
  (let ((its (db-get table items)))
    `((table (class . "stripes"))
      (thead
       (tr
        ,@(map (λ (x) `(th ,(str x))) items)))
      (tbody
       ,@(map
          (λ (it) `(tr ,@(map (λ (x) `(td ,(str x))) it)))
          its)))))

(define (make-page-route add-text add-route input tbl t-its . additional)
  (λ (req)
    (r/response
     code    => 200
     headers => '((Content-type . "text/html"))
     content => (make-page `(,(make-form add-route add-text input)
                             ,(make-list-of tbl t-its)
                             ,@additional)))))

(define route-/roasteries
  (make-page-route "dodaj palarnię"
                   "/new/roastery"
                   `((text  "name"  "nazwa" #t)
                     (text  "url"   "link")
                     (image "image" "obrazek")
                     (text  "notes" "notka"))
                   'roasteries
                   '(name url notes)))

(define route-/uploads
  (make-page-route "dodaj obrazek"
                   "#"
                   `((upload "file" "nazwa" #t))
                   'uploads
                   '(timestamp location)
                   '(script
                     "const form = document.getElementsByTagName('form')[0];"
                     "const input = document.getElementsByTagName('input')[0];"
                     "form.addEventListener('submit', async (e) => {e.preventDefault(); await fetch('/new/upload', {method: 'PUT', headers:{'Content-type':'application/octet-stream'},body: input.files[0]}); window.location = '/uploads'});")
                   ))

(define (make-api-responder table . items)
  (λ (req)
    (r/response
     code    => 200
     headers => '((Content-type . "application/json"))
     content => (json/encode (list->vector (db-get table items))))))

(define app
  (r/make-dispatcher
   "/roasteries"     => route-/roasteries
   "/uploads"        => route-/uploads

   "/new/roastery"   => (λ (req)
                          (if (not (eq? 'POST (get req 'method 'GET)))
                              (r/response
                               code => 405
                               content => "Method not allowed")
                              (lets ((p (get req 'post-data #n)))
                                ((make-roastery (list->ff p)) (db))
                                (r/redirect "/roasteries"))))

   "/new/upload"   => (λ (req)
                        (if (not (eq? 'PUT (get req 'method 'GET)))
                            (r/response
                             code => 405
                             content => "Method not allowed")
                            (lets ((p (get req 'post-data #n))
                                   (filename (str *uploads-dir* "/" (time-ns) ".jpg"))
                                   (filename* (str filename "_orig")))
                              (thread
                               (begin
                                 (r/chunked-post-data->file p filename*)
                                 (system `("convert" ,filename* "-quality" "80" ,filename))
                                 (s3/execute (db) "INSERT INTO uploads (location, timestamp) VALUES (?, current_timestamp)" (list filename))))
                              (r/response code => 200))))

   "/api/roasteries" => (make-api-responder 'roasteries 'name 'image 'url 'notes)
   "/api/coffees"    => (make-api-responder 'coffees 'name 'roastery 'roast_level 'image 'url 'additional)
   "/api/grinders"   => (make-api-responder 'grinders 'name 'image 'url 'additional)
   "/api/methods"    => (make-api-responder 'methods 'name 'image 'additional)
   "/api/uploads"    => (make-api-responder 'uploads 'timestamp 'location)
   "/api/opinions"   => (make-api-responder 'opinions 'timestamp 'coffee 'grinder 'method 'local_p 'grind_level 'rating 'image 'notes)
   "m/^\\/static/"   => (λ (r) (r/static-dispatcher "static" "/static" r))
   ))

(λ (_)
  (db-refresher)
  (r/fastcgi-bind 8081 app (r/make-stdout-logger)))

;; Local Variables:
;; compile-command: "make run"
;; End:
