;; -*- Owl -*-

(import
 (owl toplevel)
 (owl unicode)
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

,load "moka-config.scm"

(define (execute* ptr s arg)
  (map
   (λ (l)
     (map
      (λ (v)
        (if (string? v)
            (bytes->string (utf8-decode (string->bytes v)))
            v))
      l))
   (s3/execute ptr s arg)))

;; (when (not (sys/directory? *uploads-dir*))
;;   (sys/mkdir *uploads-dir*))

(define (start-migrate-thread)
  (thread
   'migrate
   (let loop ((fs #n))
     (lets ((who v (next-mail)))
       (tuple-case v
         ((add f) (loop (cons f fs)))
         ((dump) (mail who fs))
         ;; ((migrate ptr)
         ;;  (for-each (λ (tbl) (tbl ptr)) fs)
         ;;  (mail who 'ok)
         ;;  (loop #n))
         (else
          (loop fs)))))))

(define (add-migration f)
  (mail 'migrate (tuple 'add f)))

(start-migrate-thread)

;; unsafe
(define (table-has-column? ptr name col)
  (let ((v (execute* ptr (str "SELECT * FROM pragma_table_info('" name "') WHERE name = ?") (list (str col)))))
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

(define-table (uploads make-upload)
  timestamp => datetime
  location  => text
  )

(define-table (roasteries make-roastery)
  name   => text
  image  => int
  url    => text
  notes  => text
  )

(define-table (methods make-method)
  name  => text
  image => int                    ; -> uploads
  notes => text
  )

(define-table (coffees make-coffee)
  name        => text
  roastery    => int                    ; -> roasteries
  roast_level => int
  image       => int                    ; -> uploads
  url         => text
  notes       => text
  )

(define-table (grinders make-grinder)
  name  => text
  image => int                    ; -> uploads
  url   => text
  notes => text
  )

(define-table (gear make-gear)          ; espresso machines, moka pots et al
  name  => text
  url   => text
  image => int  ; -> uploads
  notes => text
  )

(define-table (brews make-brew)
  timestamp    => datetime
  coffee       => int  ; -> coffees
  grinder      => int  ; -> grinders
  method       => int  ; -> methods
  gear         => int  ; -> gear
  local_p      => bool ; made at home?
  grind_level  => int
  rating       => int
  image        => int  ; -> uploads
  dose         => int  ; coffee dose (grams)
  yield        => int  ; coffee yield (grams)
  notes        => text
  )

(define (db-get table items)
  (lets ((p (db)))
    (execute* p (str "SELECT " (list->sql-list items) " FROM " table) #n)))

(define known-routes
  `(("opinia"   "coffee"       "/brews")
    ("obrazki"  "image"        "/uploads")
    ("palarnie" "mode_heat"    "/roasteries")
    ("metody"   "procedure"    "/methods")
    ("kawki"    "local_cafe"   "/coffees")
    ("młynki"   "cyclone"      "/grinders")
    ("machiny"  "coffee_maker" "/gear")
    ))

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
      ((nav (class . "m l left max"))
       ;;; TODO: header
       ;; (header
       ;;  (h3 "moka"))
       ,@(map
          (λ (it)
            `((a (href . ,(caddr it)))
              (i ,(cadr it))
              (span ,(car it))))
          known-routes))
      ((main (class . "responsive"))
       ,@body)
      ((nav (class . "s bottom"))
       ,@(map
          (λ (it)
            `((a (href . ,(caddr it)))
              (i ,(cadr it))
              (span ,(car it))))
          known-routes))
      ))))

(define (make-input-item t id label . required?)
  (let ((t (if (list? t)
               (list->tuple t)
               (tuple t))))
    (tuple-case t
      ((text)
       `((label (class . "field border label"))
         ((input (name . ,id) ,@(if (not (null? required?)) '((required . "true")) '())))
         (label ,label)))
      ((upload)
       `((label (class . "field border label"))
         ((button (class . "chip circle"))
          (i "upload")
          ((input (name . ,id)
                  (type . "file")
                  ; (accept . "image/png image/jpg image/gif image/heic image/heic-sequence")
                  ,@(if (not (null? required?)) '((required . "true")) '()))))))
      ((image)
       `((div (class . "row"))
         ((label (class . "field border label"))
          ((input (type . "number") (id . ,id) (name . ,id) ,@(if (not (null? required?)) '((required . "true")) '()))
           (label ,label)))
         ((button (type . "button") (onClick . ,(str "load_upload_id('" id "')"))) "wybierz " ,label)
         ))
      ((relation tbl)
       `((div (class . "row"))
         ((label (class . "field border label"))
          ((input (type . "number") (id . ,id) (name . ,id) ,@(if (not (null? required?)) '((required . "true")) '()))
           (label ,label)))
         ((button (type . "button") (onClick . ,(str "load_relation('" id "', '" tbl "')"))) "wybierz " ,label)
         ))
      ((number from to)
       `((label (class . "field border label"))
         ((input (name . ,id) (type . "number") (min . ,(str from)) (max . ,(str to))
                 ,@(if (not (null? required?)) '((required . "true")) '())))
         (label ,label)))
      ((bool)
       `((label (class . "checkbox label"))
         ((input (name . ,id) (type . "checkbox") ,@(if (not (null? required?)) '((required . "true")) '())))
         (span ,label)))
      (else
       (error "unknown type: " t)))))

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

(define route-/ (λ (req)
                  (r/response
                   code => 200
                   headers => '((Content-type . "text/html"))
                   content => (make-page
                               `((p "pozdro"))))))

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
                     "form.addEventListener('submit', (e) => {e.preventDefault(); put_image(input.files[0], () => {window.location = '/uploads'});})"
                     )
                   ))

(define route-/methods
  (make-page-route "dodaj metodę"
                   "/new/method"
                   `((text  "name"  "nazwa" #t)
                     (image "image" "obrazek")
                     (text  "notes" "notka"))
                   'methods
                   '(name notes)))

(define route-/coffees
  (make-page-route "dodaj kawkę"
                   "/new/coffee"
                   `((text                  "name"        "nazwa"    #t)
                     ((relation roasteries) "roastery"    "palarnia" #t)
                     ((number 0 10)         "roast_level" "poziom wypalenia")
                     (image                 "image"       "zdjęcie")
                     (text                  "url"         "link")
                     (text                  "notes"       "notka"))
                   'coffees
                   '(name roastery roast_level url notes)))

(define route-/grinders
  (make-page-route "dodaj młynek"
                   "/new/grinder"
                   `((text  "name"  "nazwa" #t)
                     (image "image" "zdjęcie")
                     (text  "url"   "link")
                     (text  "notes" "notka"))
                   'grinders
                   '(name url notes)))

(define route-/gear
  (make-page-route "dodaj machinę (ekspres, drip, kawiarka, ...)"
                   "/new/gear"
                   `((text  "name"  "nazwa" #t)
                     (image "image" "obrazek")
                     (text  "url"   "link")
                     (text  "notes" "notka"))
                   'gear
                   '(name url notes)))

(define route-/brews
  (make-page-route "dodaj opinię"
                   "/new/brew"
                   `((text                "timestamp"   "data/godzina (unix timestamp)")
                     ((relation coffees)  "coffee"      "kawka")
                     ((relation grinders) "grinder"     "młynek")
                     ((relation methods)  "method"      "metoda")
                     ((relation gear)     "gear"        "zaparzacz")
                     (bool                "local_p"     "w domu?")
                     ((number 0 100)      "grind_level" "klik na młynku")
                     ((number 0 10)       "rating"      "ocenka")
                     (image               "image"       "obrazek")
                     ((number 0 1000)     "dose"        "ilość ziaren kawy (w gramach)")
                     ((number 0 1000)     "yield"       "ilość wynikowej kawy (w gramach")
                     (text                "notes"       "notka")
                     )
                   'brews
                   '(timestamp coffee grinder method gear local_p grind_level rating image dose yield notes)))

(define (make-api-responder table . items)
  (λ (req)
    (r/response
     code    => 200
     headers => '((Content-type . "application/json"))
     content => (json/encode (list->vector (map (λ (l) (zip cons items l))
                                                (db-get table items)))))))

(define (make-simple-adder constructor redir)
  (λ (req)
    (if (not (eq? 'POST (get req 'method 'GET)))
        (r/response
         code => 405
         content => "Method not allowed")
        (lets ((p (get req 'post-data #n))
               (p (list->ff p))
               (p (if (= 0 (string-length (get p 'timestamp "")))
                      (put p 'timestamp (str (time))) ; lol
                      p)))
          ((constructor p) (db))
          (r/redirect redir)))))

(define (compress-image filename-from filename-to)
  (system `("convert" ,filename-from "-resize" "640" "-quality" "90" ,filename-to)))

(define app
  (r/make-dispatcher
   "/"               => route-/
   "/roasteries"     => route-/roasteries
   "/uploads"        => route-/uploads
   "/methods"        => route-/methods
   "/coffees"        => route-/coffees
   "/grinders"       => route-/grinders
   "/gear"           => route-/gear
   "/brews"          => route-/brews

   "/new/roastery"   => (make-simple-adder make-roastery "/roasteries")
   "/new/method"     => (make-simple-adder make-method   "/methods")
   "/new/coffee"     => (make-simple-adder make-coffee   "/coffees")
   "/new/grinder"    => (make-simple-adder make-grinder  "/grinders")
   "/new/gear"       => (make-simple-adder make-gear     "/gear")
   "/new/brew"       => (make-simple-adder make-brew     "/brews")

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

   "/api/uploads"    => (make-api-responder 'uploads    'id 'timestamp 'location)
   "/api/roasteries" => (make-api-responder 'roasteries 'id 'name 'image 'url 'notes)
   "/api/methods"    => (make-api-responder 'methods    'id 'name 'image 'notes)
   "/api/coffees"    => (make-api-responder 'coffees    'id 'name 'roastery 'roast_level 'image 'url 'notes)
   "/api/grinders"   => (make-api-responder 'grinders   'id 'name 'image 'url 'notes)
   "/api/gear"       => (make-api-responder 'gear       'id 'name 'url 'image 'notes)
   "/api/brews"      => (make-api-responder 'brew       'id 'timestamp 'coffee 'grinder 'method 'gear 'local_p 'grind_level 'rating 'image 'dose 'yield 'notes)

   "m/^\\/uploads\\/[0-9]+$/" => (λ (r)
                                   (let ((id (string->number (last ((string->regex "c/\\//") (get r 'path 'bug)) 0))))
                                     (let ((res (execute* (db) "SELECT location FROM uploads WHERE id = ?" (list id))))
                                       (if (null? res)
                                           (r/response code => 404)
                                           (r/response
                                            code    => 200
                                            headers => '((Content-type . "image/jpg"))
                                            content => (file->list (caar res)))))))
   "m/^\\/static/"   => (λ (r) (r/static-dispatcher "static" "/static" r))
   ))

(define migrate!
  (let ((fs (interact 'migrate (tuple 'dump))))
    (λ (p)
      (for-each (λ (f) (f p)) fs))))

(λ (_)
  (let ((ptr (s3/open *db-file*)))
    (migrate! ptr)
    (s3/close ptr))

  (db-refresher)
  (r/fastcgi-bind *port* app (r/make-stdout-logger)))

;; Local Variables:
;; compile-command: "make run"
;; End:
