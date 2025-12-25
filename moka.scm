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

(define (start-schema-thread schema)
  (thread
   'schema
   (let loop ((schema schema)) ; ff of table -> k -> v
     (lets ((who v (next-mail)))
       (tuple-case v
         ((add-column table column t)
          (loop (put schema table (put (get schema table) column t))))
         ((get-schema table)
          (mail who (get schema table))
          (loop schema))
         ((dump)
          (mail who schema))))))) ; die

(define (add-column table column t)
  (mail 'schema (tuple 'add-column table column t)))

(define (get-schema table)
  (interact 'schema (tuple 'get-schema table)))

(start-schema-thread empty)

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
  (syntax-rules (42 => relation _getk _gets keys migrate id int)
    ((_ 42 name)
     (λ _ (print (str "[define-table] ok: " 'name))))
    ;; ((_ 42 name k => (v ...) . rest)
    ;;  (λ (ptr)
    ;;    (when (not (table-has-column? ptr 'name 'k))
    ;;      (print "[define-table] new column for migration: " 'k)
    ;;      (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " " (fold (λ (a b) (str a " " b)) "" '(v ...)) ";")))
    ;;    ((_ 42 name . rest) ptr)))
    ((_ 42 name k => (relation v) . rest)
     (λ (ptr)
       (when (not (table-has-column? ptr 'name 'k))
         (print "[define-table] new column for migration: " 'k)
         (s3/execute ptr (str "ALTER TABLE " 'name " ADD COLUMN " 'k " int;")))
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
    ((_ _gets name k => v . rest)
     (begin
       (add-column 'name 'k 'v)
       (_ _gets name . rest)))
    ((_ _gets name) (add-column 'name 'id 'int))
    ((_ (name constructor updater) . rest)
     (define-values (delivered constructor updater)
       (values
        (begin
          (_ _gets name . rest)
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
        (λ (ff*)
          (lets ((keys (filter (λ (x) (not (eq? 'nope (get ff* x 'nope)))) (_ _getk . rest))))
            (when (not (get ff* 'id #f))
              (error (ff->list ff*) " does not contain an id"))
            (λ (ptr)
              (s3/execute
               ptr
               (str "UPDATE " 'name " SET " (fold (λ (a b) (str a ", " b " = ?")) (str (car keys) " = ?") (cdr keys)) " WHERE id = ?")
               (append (map (λ (x) (get ff* x 0)) keys) (list (get ff* 'id -1)))))))
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

(define-table (uploads make-upload update-upload)
  timestamp => datetime
  location  => text
  )

(define-table (roasteries make-roastery update-roastery)
  name   => text
  image  => (relation uploads)
  url    => text
  notes  => text
  )

(define-table (methods make-method update-method)
  name  => text
  image => (relation uploads)
  notes => text
  )

(define-table (coffees make-coffee update-coffee)
  name        => text
  roastery    => (relation roasteries)
  roast_level => int
  image       => (relation uploads)
  url         => text
  notes       => text
  )

(define-table (grinders make-grinder update-grinder)
  name  => text
  image => (relation uploads)
  url   => text
  notes => text
  )

(define-table (gear make-gear update-gear)          ; espresso machines, moka pots et al
  name  => text
  url   => text
  image => (relation uploads)
  notes => text
  )

(define-table (brews make-brew update-brew)
  timestamp    => datetime
  coffee       => (relation coffees)
  grinder      => (relation grinders)
  method       => (relation methods)
  gear         => (relation gear)
  image        => (relation uploads)
  local_p      => bool ; made at home?
  grind_level  => int
  rating       => int
  dose         => int  ; coffee dose (grams)
  yield        => int  ; coffee yield (grams)
  notes        => text
  )

(define (db-get table items . id)
  (lets ((p (db)))
    (if (null? id)
        (execute* p (str "SELECT " (list->sql-list items) " FROM " table) #n)
        (car* (execute* p (str "SELECT " (list->sql-list items) " FROM " table " WHERE id = ?") (list (car id)))))))

(define (db-get-where table items where arg)
  (lets ((p (db)))
    (execute* p (str "SELECT " (list->sql-list items) " FROM " table " " where) arg)))

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
      ((link (href . "/static/beercss/beer.min.css") (rel . "stylesheet")))
      ((script (src . "/static/app.js")))
      ((script (type . "module") (src . "/static/beercss/beer.min.js")))
      ((script (type . "module") (src . "/static/beercss/material-dynamic-colors.min.js"))))
     ((body (class . "dark"))
      ((nav (class . "m l left max"))
       (header
        ((button (class . "extend square round")
                 (onClick . "window.location = '/'"))
         (i "kettle")
         (span "moka")))
       ,@(map
          (λ (it)
            `((a (href . ,(caddr it)))
              (i ,(cadr it))
              (span ,(car it))))
          known-routes))
      ((main (class . "responsive"))
       ,@body)
      ((nav (class . "s bottom scroll"))
       (header
        ((button (class . "extend square round")
                 (onClick . "window.location = '/'"))
         (i "kettle")))
       ,@(map
          (λ (it)
            `((a (href . ,(caddr it)))
              (i ,(cadr it))
              (span ,(car it))))
          known-routes))
      ))))

(define (str! it)
  (if (eq? it #f)
      "false"
      (str it)))

;; TODO: implement some sort of nice destructuring bind with . it
(define (make-input-item vs default)
  (lets ((t rest vs)
         (id rest rest)
         (label required? rest))
    (let ((t (if (list? t)
                 (list->tuple t)
                 (tuple t))))
      (tuple-case t
        ((text)
         `((label (class . "field border label"))
           ((input (name . ,id)
                   ,@(if default `((value . ,(str! default))) '())
                   ,@(if (not (null? required?)) '((required . "true")) '())))
           (label ,label)))
        ((upload)
         `((label (class . "field border label"))
           ((button (class . "chip circle"))
            (i "upload")
            ((input (name . ,id)
                    (type . "file")
                    (class . "__up_input")
                                        ; (accept . "image/png image/jpg image/gif image/heic image/heic-sequence")
                    ,@(if (not (null? required?)) '((required . "true")) '()))))))
        ((image)
         `((div (class . "row"))
           ((label (class . "field border label"))
            ((input (type . "number") (id . ,id) (name . ,id)
                    ,@(if default `((value . ,(str! default))) '())
                    ,@(if (not (null? required?)) '((required . "true")) '()))
             (label ,label)))
           ((button (type . "button") (onClick . ,(str "load_upload_id('" id "')"))) "wybierz " ,label)
           ))
        ((relation tbl)
         `((div (class . "row"))
           ((label (class . "field border label"))
            ((input (type . "number") (id . ,id) (name . ,id)
                    ,@(if default `((value . ,(str! default))) '())
                    ,@(if (not (null? required?)) '((required . "true")) '()))
             (label ,label)))
           ((button (type . "button") (onClick . ,(str "load_relation('" id "', '" tbl "')"))) "wybierz " ,label)
           ))
        ((number from to)
         `((label (class . "field border label"))
           ((input (name . ,id) (type . "number") (min . ,(str from)) (max . ,(str to))
                    ,@(if default `((value . ,(str! default))) '())
                   ,@(if (not (null? required?)) '((required . "true")) '())))
           (label ,label)))
        ((bool)
         `((label (class . "checkbox label"))
           ((input (type . "hidden") (value . "0") (name . ,id)))
           ((input (name . ,id) (type . "checkbox")
                   ;;  TODO: default
                   ,@(if (not (null? required?)) '((checked . "true")) '())))
           (span ,label)))
        (else
         (error "unknown type: " t))))))

(define (make-form action legend lst defaults)
  `((form ("action" . ,action) ("method" . "POST"))
    (fieldset
     (legend ,legend)
     ((input (type . "hidden") (name . "id") (value . ,(str (car* defaults)))))
     ,@(zip (λ (a b) (make-input-item a b))
            lst
            (if (and defaults (not (null? (cdr* defaults)))) (cdr* defaults) (make-list (len lst) #f)))
     ((label (class . "field"))
      ((button (class . "circle extra"))
       (i "add"))))))

(define (make-list-of table items)
  (let ((its (db-get table (cons 'id items))))
    `((table (class . "stripes"))
      (thead
       (tr
        ,@(map (λ (x) `(th ,(str x))) items)
        (th "actions")
        ))
      (tbody
       ,@(map
          (λ (it)
            `(tr ,@(map (λ (x) `(td ,(str x))) (cdr it))
                 (td
                  ((form (action . ,(str "/edit/" table)) (method . "POST"))
                   ((input (type . "hidden") (name . "id") (value . ,(str (car it)))))
                   ((button (type . "submit")) (i "edit")))
                  ;; ((form (action . ,(str "/delete/" table)) (method . "POST"))
                  ;;  ((label (type . "hidden") (name . "id") (value . ,(str (car it)))))
                  ;;  ((button (type . "submit")) (i "edit")))
                       )))
          its)))))

;; -> /rt /edit/rt
(define (make-page-routes add-text add-route edit-route input tbl t-its . additional)
  (values
   (λ (req)
     (r/response
      code    => 200
      headers => '((Content-type . "text/html"))
      content => (make-page `(,(make-form add-route add-text input #f)
                              ,(make-list-of tbl t-its)
                              ,@additional))))
   (λ (req)
     (if-lets ((id (cdr* (assoc 'id (get req 'post-data empty))))
               (vs (db-get tbl (cons 'id (map (B string->symbol cadr) input)) id)))
       (r/response
        code    => 200
        headers => '((Content-type . "text/html"))
        content => (make-page `(,(make-form edit-route add-text input vs))))
       (r/response code => 400)))))

(define week (* 60 60 24 7))

(define (db-get-latest-brews)
  (lets ((ks '(brews.timestamp
               brews.local_p
               brews.grind_level
               brews.rating
               brews.image
               brews.dose
               brews.yield
               brews.notes
               brews.local_p
               brews.timestamp
               grinders.name
               coffees.name
               methods.name
               gear.name
               ))
         (ks* (map (λ (k) (string->symbol ((string->regex "s/^brews\\.//") (str k)))) ks)))
    (map (λ (l) (list->ff (zip cons ks* l)))
         (db-get-where
          'brews ks
          "
LEFT JOIN coffees  ON coffee  = coffees.id
LEFT JOIN grinders ON grinder = grinders.id
LEFT JOIN methods  ON method  = methods.id
LEFT JOIN gear     ON gear    = gear.id
WHERE timestamp is not null and cast(timestamp as int) > ?
ORDER BY cast(timestamp as int) desc"
          (list (str (- (time) week)))))))

(define (maybe-string->number s)
  (cond
   ((equal? s "") 0)
   ((string? s) (string->number s))
   (else s)))

(define (maybe-render-key ff key f)
  (if-lets ((g (get ff key #f)))
    `(,(f g))
    ()))

(define (render-coffee c)
  `((article (class . "no-padding border round"))
    ((img (class . "responsive small top-round") (loading . "lazy") (src . ,(str "/uploads/" (get c 'image 0)))))
    ((div (class . "padding"))
     (h5 ,(str (get c 'coffees.name #f)))
     (ul
      (li ,(date-str (maybe-string->number (get c 'timestamp 0)) *tz-offset*))
      ,@(maybe-render-key c 'grinders.name (λ (g) `(li "zmemłana: "   ,(str g))))
      ,@(maybe-render-key c 'methods.name  (λ (g) `(li "metodą: "     ,(str g))))
      ,@(maybe-render-key c 'gear.name     (λ (g) `(li "narzędziem: " ,(str g))))))))

(define route-/ (λ (req)
                  (r/response
                   code => 200
                   headers => '((Content-type . "text/html"))
                   content => (make-page
                               `((p "pozdro")
                                 ((article (class . "border"))
                                  (h3 "kawka wypita (ziarna)")
                                  (h6 ,(str (car* (car* (s3/execute (db) "SELECT CAST(SUM(dose) AS integer) FROM brews" #n))) "g"))
                                  (p "no, gratulacje. oby tak dalej. lecz sie."))
                                 ((nav (class . "row scroll"))
                                  ,@(map render-coffee (db-get-latest-brews)))
                                 )))))

(define-values (route-/roasteries route-/edit/roasteries)
  (make-page-routes "dodaj palarnię"
                    "/new/roastery"
                    "/update/roastery"
                    `((text  "name"  "nazwa" #t)
                      (text  "url"   "link")
                      (image "image" "obrazek")
                      (text  "notes" "notka"))
                    'roasteries
                    '(name url notes)))

(define-values (route-/uploads route-/edit/uploads)
  (make-page-routes "dodaj obrazek"
                    "#"
                    "#"
                    `((upload "file" "nazwa" #t))
                    'uploads
                    '(timestamp location)
                    '(script
                      "const form = document.getElementsByTagName('form')[0];"
                      "const input = document.getElementsByClassName('__up_input')[0];"
                      "form.addEventListener('submit', (e) => {e.preventDefault(); put_image(input.files[0], () => {window.location = '/uploads'});})"
                      )
                    ))

(define-values (route-/methods route-/edit/methods)
  (make-page-routes "dodaj metodę"
                    "/new/method"
                    "/update/method"
                    `((text  "name"  "nazwa" #t)
                      (image "image" "obrazek")
                      (text  "notes" "notka"))
                    'methods
                    '(name notes)))

(define-values (route-/coffees route-/edit/coffees)
  (make-page-routes "dodaj kawkę"
                    "/new/coffee"
                    "/update/coffee"
                    `((text                  "name"        "nazwa"    #t)
                      ((relation roasteries) "roastery"    "palarnia" #t)
                      ((number 0 10)         "roast_level" "poziom wypalenia")
                      (image                 "image"       "zdjęcie")
                      (text                  "url"         "link")
                      (text                  "notes"       "notka"))
                    'coffees
                    '(name roastery roast_level url notes)))

(define-values (route-/grinders route-/edit/grinders)
  (make-page-routes "dodaj młynek"
                    "/new/grinder"
                    "/update/grinder"
                    `((text  "name"  "nazwa" #t)
                      (image "image" "zdjęcie")
                      (text  "url"   "link")
                      (text  "notes" "notka"))
                    'grinders
                    '(name url notes)))

(define-values (route-/gear route-/edit/gear)
  (make-page-routes "dodaj machinę (ekspres, drip, kawiarka, ...)"
                    "/new/gear"
                    "/update/gear"
                    `((text  "name"  "nazwa" #t)
                      (image "image" "obrazek")
                      (text  "url"   "link")
                      (text  "notes" "notka"))
                    'gear
                    '(name url notes)))

(define-values (route-/brews route-/edit/brews)
  (make-page-routes "dodaj opinię"
                    "/new/brew"
                    "/update/brew"
                    `((text                "timestamp"   "data/godzina (unix timestamp)")
                      ((relation coffees)  "coffee"      "kawka")
                      ((relation grinders) "grinder"     "młynek")
                      ((relation methods)  "method"      "metoda")
                      ((relation gear)     "gear"        "zaparzacz")
                      (bool                "local_p"     "w domu?" #t)
                      ((number 0 100)      "grind_level" "klik na młynku")
                      ((number 0 10)       "rating"      "ocenka")
                      (image               "image"       "obrazek")
                      ((number 0 1000)     "dose"        "ilość ziaren kawy (w gramach)")
                      ((number 0 1000)     "yield"       "ilość wynikowej kawy (w gramach")
                      (text                "notes"       "notka")
                      )
                    'brews
                    '(timestamp coffee grinder method gear local_p grind_level rating image dose yield notes)))

(define (make-api-responder table)
  (let ((items (keys (get-schema table))))
    (λ (req)
      (r/response
       code    => 200
       headers => '((Content-type . "application/json"))
       content => (json/encode (list->vector (map (λ (l) (zip cons items l))
                                                  (db-get table items))))))))

(define (make-simple-adder constructor redir)
  (λ (req)
    (case (get req 'method 'GET)
      ('POST
       (lets ((p (get req 'post-data #n))
              (p (list->ff p))
              (p (if (= 0 (string-length (get p 'timestamp "")))
                     (put p 'timestamp (str (time))) ; lol
                     p)))
         ((constructor p) (db))
         (r/redirect redir)))
      ;; ('PATCH
      ;;  (r/response code => 200  content => "not implemented"))
      (else
       (r/response
        code => 405
        content => "Method not allowed")))))

(define (compress-image filename-from filename-to)
  (system `("convert" ,filename-from "-resize" "640" "-quality" "90" ,filename-to)))

(define app
  (r/make-dispatcher
   "/"                => route-/

   "/roasteries"      => route-/roasteries
   "/uploads"         => route-/uploads
   "/methods"         => route-/methods
   "/coffees"         => route-/coffees
   "/grinders"        => route-/grinders
   "/gear"            => route-/gear
   "/brews"           => route-/brews

   "/edit/roasteries" => route-/edit/roasteries
   "/edit/uploads"    => route-/edit/uploads
   "/edit/methods"    => route-/edit/methods
   "/edit/coffees"    => route-/edit/coffees
   "/edit/grinders"   => route-/edit/grinders
   "/edit/gear"       => route-/edit/gear
   "/edit/brews"      => route-/edit/brews

   "/new/roastery"    => (make-simple-adder make-roastery "/roasteries")
   "/new/method"      => (make-simple-adder make-method   "/methods")
   "/new/coffee"      => (make-simple-adder make-coffee   "/coffees")
   "/new/grinder"     => (make-simple-adder make-grinder  "/grinders")
   "/new/gear"        => (make-simple-adder make-gear     "/gear")
   "/new/brew"        => (make-simple-adder make-brew     "/brews")

   "/update/roastery" => (make-simple-adder update-roastery "/roasteries")
   "/update/method"   => (make-simple-adder update-method   "/methods")
   "/update/coffee"   => (make-simple-adder update-coffee   "/coffees")
   "/update/grinder"  => (make-simple-adder update-grinder  "/grinders")
   "/update/gear"     => (make-simple-adder update-gear     "/gear")
   "/update/brew"     => (make-simple-adder update-brew     "/brews")

   "/new/upload"      => (λ (req)
                           (if (not (eq? 'PUT (get req 'method 'GET)))
                               (r/response
                                code => 405
                                content => "Method not allowed")
                               (lets ((p (get req 'post-data #n))
                                      (filename (str *uploads-dir* "/" (time-ns) ".jpg"))
                                      (filename* (str filename "_orig")))
                                 (thread   ; TODO: maybe don't run this in a thread, but keep user waiting for the upload & conversion to finish
                                  (begin
                                    (r/chunked-post-data->file p filename*)
                                    (compress-image filename* filename)
                                    (s3/execute (db) "INSERT INTO uploads (location, timestamp) VALUES (?, current_timestamp)" (list filename))))
                                 (r/response code => 200))))

   "/api/uploads"     => (make-api-responder 'uploads)
   "/api/roasteries"  => (make-api-responder 'roasteries)
   "/api/methods"     => (make-api-responder 'methods)
   "/api/coffees"     => (make-api-responder 'coffees)
   "/api/grinders"    => (make-api-responder 'grinders)
   "/api/gear"        => (make-api-responder 'gear)
   "/api/brews"       => (make-api-responder 'brew)

   "m/^\\/uploads\\/[0-9]+$/" => (λ (r)
                                   (let ((id (string->number (last ((string->regex "c/\\//") (get r 'path 'bug)) 0))))
                                     (let ((res (execute* (db) "SELECT location FROM uploads WHERE id = ?" (list id))))
                                       (if (null? res)
                                           (r/response code => 404)
                                           (r/response
                                            code    => 200
                                            headers => '((Content-type . "image/jpg"))
                                            content => (file->list (caar res)))))))
   "m/^\\/static/"    => (λ (r) (r/static-dispatcher "static" "/static" r))
   ))

;; TODO: hack ish
(define migrate!
  (let ((fs (interact 'migrate (tuple 'dump))))
    (λ (p)
      (for-each (λ (f) (f p)) fs))))

(define schema
  (interact 'schema (tuple 'dump)))

(λ (_)
  (let ((ptr (s3/open *db-file*)))
    (migrate! ptr)
    (s3/close ptr))

  (start-schema-thread schema) ; re-start schema thread with defined schema

  (db-refresher)
  (r/fastcgi-bind *port* app (r/make-stdout-logger)))

;; Local Variables:
;; compile-command: "make run"
;; End:
