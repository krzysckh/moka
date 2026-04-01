;; -*- Owl -*-

(import
 (owl toplevel)
 (owl unicode)
 (owl metric)
 (prefix (owl sys) sys/)
 (prefix (robusta fastcgi) r/)
 (prefix (robusta log) r/)
 (prefix (robusta server) r/)
 (prefix (robusta http) r/)
 (prefix (robusta dispatcher) r/)
 (prefix (robusta mime) r/)
 (prefix (robusta encoding html) html/)
 (prefix (robusta encoding json) json/)
 (robusta l10n)

 (robusta experimental db)
 )

(when (not (has? *features* 'sqlite))
  (error "compile-time sqlite extension is required." #n))

(import
 (prefix (ext sqlite io) s3/))

,load "moka-config.scm"

(define l10n (make-l10n-getter *language*))
(start-l10nizer)

,load "l10n.scm"

;; (when (not (sys/directory? *uploads-dir*))
;;   (sys/mkdir *uploads-dir*))


(start-migrate-thread)
(start-schema-thread empty)

(define-table (uploads make-upload update-upload)
  timestamp => datetime
  location  => text
  )

(define-table (roasteries make-roastery update-roastery)
  name      => text
  image     => (relation uploads)
  url       => text
  notes     => text
  last_used => datetime
  )

(define-table (methods make-method update-method)
  name      => text
  image     => (relation uploads)
  notes     => text
  last_used => datetime
  )

(define-table (coffees make-coffee update-coffee)
  name        => text
  roastery    => (relation roasteries)
  roast_level => int
  image       => (relation uploads)
  url         => text
  notes       => text
  last_used   => datetime
  )

(define-table (grinders make-grinder update-grinder)
  name      => text
  image     => (relation uploads)
  url       => text
  notes     => text
  last_used => datetime
  )

(define-table (gear make-gear update-gear)          ; espresso machines, moka pots et al
  name      => text
  url       => text
  image     => (relation uploads)
  notes     => text
  last_used => datetime
  )

(define-table (brews make-brew update-brew)
  timestamp    => datetime
  coffee       => (relation coffees)
  grinder      => (relation grinders)
  method       => (relation methods)
  gear         => (relation gear)
  image        => (relation uploads)
  local_p      => bool ; made at home?
  private_p    => bool ; should be hidden in export
  grind_level  => int
  rating       => int
  dose         => int  ; coffee dose (grams)
  yield        => int  ; coffee yield (grams)
  notes        => text
  )

(define known-routes
  `((,(l10n 'menu.opinion)    "coffee"       "/brews")
    (,(l10n 'menu.images)     "image"        "/uploads")
    (,(l10n 'menu.roasteries) "mode_heat"    "/roasteries")
    (,(l10n 'menu.methods)    "procedure"    "/methods")
    (,(l10n 'menu.coffees)    "local_cafe"   "/coffees")
    (,(l10n 'menu.grinders)   "cyclone"      "/grinders")
    (,(l10n 'menu.gear)       "coffee_maker" "/gear")
    ))

(define (make-page body)
  (λ (stream)
    (html/encode/printer
     `(html
       (head
        ((meta (name . "viewport") (content . "width=device-width, initial-scale=1")))
        ((meta (charset . "utf-8")))
        ((link (href . "/static/beercss/beer.min.css") (rel . "stylesheet")))
        ((script (src . "/static/app.js")))
        ((script (src . "/static/echarts.min.js")))
        ((script (type . "module") (src . "/static/beercss/beer.min.js")))
        ((script (type . "module") (src . "/static/beercss/material-dynamic-colors.min.js"))))
       ((body (class . "dark"))
        ((nav (class . "m l left max"))
         (header
          ((button (class . "")
                   (onClick . "window.location = '/'"))
           (i "kettle")
           (span ,(l10n 'global.app-name))))
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
          ((button (class . "square round")
                   (onClick . "window.location = '/'"))
           (i "kettle")))
         ,@(map
            (λ (it)
              `((a (href . ,(caddr it)))
                (i ,(cadr it))
                (span ,(car it))))
            known-routes))))
     (html/make-streamer
      (λ (s) (if (eof-object? s) #t (stream s)))
      (<< 1 10)))))

(define (str! it)
  (if (eq? it #f)
      "false"
      (str it)))

(define (days-since t)
  (lets ((∆t (- (time) t)))
    (floor (/ ∆t day))))

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
           ((button (type . "button") (onClick . ,(str "load_upload_id('" id "')"))) ,(l10n 'upload.choose) ,label)
           ))
        ((relation tbl)
         `((div (class . "row"))
           ((label (class . "field border label"))
            ((input (type . "number") (id . ,id) (name . ,id)
                    ,@(if default `((value . ,(str! default))) '())
                    ,@(if (not (null? required?)) '((required . "true")) '()))
             (label ,label)))
           ((button (type . "button") (onClick . ,(str "load_relation('" id "', '" tbl "')"))) ,(l10n 'upload.choose) ,label)
           ))
        ((number from to)
         `((label (class . "field border label"))
           ((input (name . ,id) (type . "number") (min . ,(str from)) (max . ,(str to))
                    ,@(if default `((value . ,(str! default))) '())
                   ,@(if (not (null? required?)) '((required . "true")) '())))
           (label ,label)))
        ((bool)
         `((div (class . "row"))
           ((label (class . "checkbox label"))
            ((input (type . "hidden") (value . "0") (name . ,id)))
            ((input (name . ,id) (type . "checkbox")
                    ;;  TODO: default
                    ,@(if (not (null? required?))
                          '((checked . "true"))
                          '())))
            (span ,label))))
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
    `((nav (class . "scroll max"))
      ((table (class . "stripes"))
       (thead
        (tr
         ,@(map (λ (x) `(th ,(str x))) items)
         (th ,(l10n 'list.actions))
         ))
       (tbody
        ,@(map
           (λ (it)
             `(tr ,@(map (λ (x) `(td ,(str x))) (cdr it))
                  (td
                   ((nav (class . "row"))
                    ((form (action . ,(str "/edit/" table)) (method . "POST"))
                     ((input (type . "hidden") (name . "id") (value . ,(str (car it)))))
                     ((button (type . "submit")) (i "edit")))
                    ((button (onClick . ,(str "window.location = '/" table "/" (car it) "'")))
                     (i "arrow_right")))
                   ;; ((form (action . ,(str "/delete/" table)) (method . "POST"))
                   ;;  ((label (type . "hidden") (name . "id") (value . ,(str (car it)))))
                   ;;  ((button (type . "submit")) (i "edit")))
                   )))
           its))))))

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

(define (db-get-brews where vs)
  (lets ((ks '(brews.timestamp
               brews.local_p
               brews.grind_level
               brews.rating
               brews.image
               brews.dose
               brews.yield
               brews.notes
               brews.local_p
               brews.id
               grinders.name
               coffees.name
               methods.name
               gear.name
               uploads.location
               ))
         (ks* (map (λ (k) (string->symbol ((string->regex "s/^brews\\.//") (str k)))) ks)))
    (map (λ (l) (list->ff (zip cons ks* l)))
         (db-get-where
          'brews ks
          (str "
LEFT JOIN coffees  ON coffee      = coffees.id
LEFT JOIN grinders ON grinder     = grinders.id
LEFT JOIN methods  ON method      = methods.id
LEFT JOIN gear     ON gear        = gear.id
LEFT JOIN uploads  ON brews.image = uploads.id " where) vs))))

(define week (* 60 60 24 7))

(define (db-get-latest-brews)
  (db-get-brews
   "WHERE brews.timestamp is not null and cast(brews.timestamp as int) > ?
ORDER BY cast(brews.timestamp as int) desc"
   (list (str (- (time) week)))))

(define (db-get-best-brews)
  (db-get-brews "WHERE rating IS NOT NULL AND rating <> '' ORDER BY cast(brews.rating as int) desc, brews.timestamp desc LIMIT 10" #n))

(define (db-get-worst-brews)
  (db-get-brews "WHERE rating IS NOT NULL AND rating <> '' ORDER BY cast(brews.rating as int) asc, brews.timestamp desc LIMIT 10" #n))

(define (maybe-string->number s)
  (cond
   ((equal? s "") 0)
   ((string? s) (string->number s))
   ((number? s) s)
   ((null? s) 0)
   (else
    (error "cannot maybe-string->number " s ))))

(define (maybe-render-key ff key f)
  (if-lets ((g (get ff key #f)))
    `(,(f g))
    ()))

(define (render-brew c)
  `((article (class . "no-padding border medium top-round")
             (onClick . ,(str "window.location = '/brews/" (get c 'id 0) "'")))
    ((img (class . "responsive small top-round") (loading . "lazy") (src . ,(str "/uploads/" (get c 'image 0)))))
    ((div (class . "padding"))
     (h5 ,(str (get c 'coffees.name #f)))
     (ul
      (li ,(date-str (maybe-string->number (get c 'timestamp 0)) *tz-offset*))
      ,@(maybe-render-key c 'grinders.name (λ (g) `(li ,(l10n 'render.brew.grinded/w) ,(str g))))
      ,@(maybe-render-key c 'methods.name  (λ (g) `(li ,(l10n 'render.brew.w/method)  ,(str g))))
      ,@(maybe-render-key c 'gear.name     (λ (g) `(li ,(l10n 'render.brew.w/gear)    ,(str g))))))))

(define (article-graph id h3 . x-data)
  `((article (class . "border large"))
    (script ,(str "do_render('" id "')"))
    (h3 ,(l10n h3))
    ((div (x-data . ,(if (null? x-data) "" (car x-data)))
          (class . "max scroll")
          (style . "width: 100%; height: 100%;")
          (id . ,(str "chrt-" id))))))

(define (route-/ req)
  (r/response
   code => 200
   headers => '((Content-type . "text/html"))
   content => (make-page
               `(((article (class . "border"))
                  ,@(lets ((t (maybe-string->number (car* (db-get 'brews '(timestamp) 1))))
                           (sum (or (car* (car* (s3/execute (db) "SELECT CAST(SUM(dose) AS integer) FROM brews" #n))) 0))
                           (days (days-since t)))
                      `((h3 ,(l10n 'render.main.bean-sum))
                        (details        ; TODO: maybe move that somewhere else
                         (summary
                          ((article (class . "round primary no-elevate margin"))
                           (nav
                            ((div (class . "max"))
                             ,(str (l10n 'render.main.bean-sum.total) " " sum "g"))
                            (i "expand_more"))))
                         ((article (class . "round border margin"))
                          (table
                           (thead
                            (tr
                             (th "metoda")
                             (th "suma")))
                           (tbody
                            ,@(map
                               (λ (l) `(tr (td (b ,(car l))) (td ,(cadr l))))
                               (db-get-where
                                'brews
                                '("methods.name" "SUM(CAST(dose as int)) as sum")
                                "left join methods on brews.method=methods.id group by method order by sum desc"
                                #n))))))
                        (p ,(format
                             #f (l10n 'render.main.bean-start+average)
                             (date-str t *tz-offset*)
                             days
                             (floor (/ sum days))))
                        (p ,(l10n 'render.main.congrats)))))
                 ((article (class . "border"))
                  (h3 ,(l10n 'render.main.last-week))
                  ((nav (class . "row scroll"))
                   ,@(map render-brew (db-get-latest-brews))))
                 ((article (class . "border"))
                  (h3 ,(l10n 'render.main.best))
                  ((nav (class . "row scroll"))
                   ,@(map render-brew (db-get-best-brews))))
                 ((article (class . "border"))
                  (h3 ,(l10n 'render.main.worst))
                  ((nav (class . "row scroll"))
                   ,@(map render-brew (db-get-worst-brews))))
                 ,(article-graph
                   'bean-history 'render.main.bean-history
                   (json/encode (list->vector (db-get 'brews '(timestamp dose coffee)))))
                 ,(article-graph
                   'rating-history 'render.main.rating-history
                   (json/encode (list->vector (db-get 'brews '(timestamp rating)))))
                 ))))

(define-values (route-/roasteries route-/edit/roasteries)
  (make-page-routes (l10n 'render.add.roastery)
                    "/new/roastery"
                    "/update/roastery"
                    `((text  "name"  ,(l10n 'global.naming.name) #t)
                      (text  "url"   ,(l10n 'global.naming.url))
                      (image "image" ,(l10n 'global.naming.image))
                      (text  "notes" ,(l10n 'global.naming.notes)))
                    'roasteries
                    '(name url notes)))

(define-values (route-/uploads route-/edit/uploads)
  (make-page-routes (l10n 'render.add.upload)
                    "#"
                    "#"
                    `((upload "file" "file" #t)) ; TODO: what's up with the name here?
                    'uploads
                    '(timestamp location)
                    '(script
                      "const form = document.getElementsByTagName('form')[0];"
                      "const input = document.getElementsByClassName('__up_input')[0];"
                      "form.addEventListener('submit', (e) => {e.preventDefault(); put_image(input.files[0], () => {window.location = '/uploads'});})"
                      )
                    ))

(define-values (route-/methods route-/edit/methods)
  (make-page-routes (l10n 'render.add.method)
                    "/new/method"
                    "/update/method"
                    `((text  "name"  ,(l10n 'global.naming.name) #t)
                      (image "image" ,(l10n 'global.naming.image))
                      (text  "notes" ,(l10n 'global.naming.notes)))
                    'methods
                    '(name notes)))

(define-values (route-/coffees route-/edit/coffees)
  (make-page-routes (l10n 'render.add.coffee)
                    "/new/coffee"
                    "/update/coffee"
                    `((text                  "name"        ,(l10n 'global.naming.name)  #t)
                      ((relation roasteries) "roastery"    ,(l10n 'global.naming.roastery))
                      ((number 0 10)         "roast_level" ,(l10n 'global.naming.roast-level))
                      (image                 "image"       ,(l10n 'global.naming.image))
                      (text                  "url"         ,(l10n 'global.naming.url))
                      (text                  "notes"       ,(l10n 'global.naming.notes)))
                    'coffees
                    '(name roastery roast_level url notes)))

(define-values (route-/grinders route-/edit/grinders)
  (make-page-routes (l10n 'render.add.grinder)
                    "/new/grinder"
                    "/update/grinder"
                    `((text  "name"  ,(l10n 'global.naming.name) #t)
                      (image "image" ,(l10n 'global.naming.image))
                      (text  "url"   ,(l10n 'global.naming.url))
                      (text  "notes" ,(l10n 'global.naming.notes)))
                    'grinders
                    '(name url notes)))

(define-values (route-/gear route-/edit/gear)
  (make-page-routes (l10n 'render.add.gear)
                    "/new/gear"
                    "/update/gear"
                    `((text  "name"  ,(l10n 'global.naming.name) #t)
                      (image "image" ,(l10n 'global.naming.image))
                      (text  "url"   ,(l10n 'global.naming.url))
                      (text  "notes" ,(l10n 'global.naming.notes)))
                    'gear
                    '(name url notes)))

(define-values (route-/brews route-/edit/brews)
  (make-page-routes (l10n 'render.add.brew)
                    "/new/brew"
                    "/update/brew"
                    `((text                "timestamp"  ,(l10n 'global.naming.timestamp))
                      ((relation coffees)  "coffee"     ,(l10n 'global.naming.coffee))
                      ((relation grinders) "grinder"    ,(l10n 'global.naming.grinder))
                      ((relation methods)  "method"     ,(l10n 'global.naming.method))
                      ((relation gear)     "gear"       ,(l10n 'global.naming.gear))
                      (bool                "local_p"    ,(l10n 'global.naming.local?) #t)
                      (bool                "private_p"  ,(l10n 'global.naming.private?))
                      ((number 0 100)      "grind_level",(l10n 'global.naming.grind-level))
                      ((number 0 10)       "rating"     ,(l10n 'global.naming.rating))
                      (image               "image"      ,(l10n 'global.naming.image))
                      ((number 0 1000)     "dose"       ,(l10n 'global.naming.dose))
                      ((number 0 1000)     "yield"      ,(l10n 'global.naming.yield))
                      (text                "notes"      ,(l10n 'global.naming.notes))
                      )
                    'brews
                    '(timestamp coffee grinder method gear local_p grind_level rating image dose yield notes)))

(define (make-api-responder table . limit)
  (let ((items (keys (get-schema table))))
    (λ (req)
      (r/response
       code    => 200
       headers => '((Content-type . "application/json"))
       content => (json/encode (list->vector (map
                                              (λ (l) (zip cons items l))
                                              (let ((timestamped? (or (and (has? items 'timestamp) 'timestamp)
                                                                      (and (has? items 'last_used) 'last_used)
                                                                      'id))
                                                    (limited (if (null? limit) "" "limit ?")))
                                                (db-get-where
                                                 table items
                                                 (format #f "order by ~a desc ~a" timestamped? limited)
                                                 limit)
                                                ))))))))

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

;;;- why tf are these curried
(define complicated-brew-adder
  (λ (it)
    (λ (db)
      ;; ... update last_used per relations
      (let ((coffee  (get it 'coffee #f))
            (grinder (get it 'grinder #f))
            (method  (get it 'method #f))
            (gear    (get it 'gear #f))
            (dt      (str (time))))
        (for-each
         (λ (l)
           (lets ((f x l))
             ((f (ff 'id x 'last_used dt)) db)))
         (zip cons
              (list update-coffee update-grinder update-method update-gear)
              (list coffee grinder method gear)))
        ((make-brew it) db)))))

(define (fold2* op acc l1 l2)
  (if (or (null? l1) (null? l2))
      acc
      (fold2* op (op acc (car l1) (car l2)) (cdr l1) (cdr l2))))

(define (map2 op l1 l2)
  (if (or (null? l1) (null? l2))
      #n
      (cons
       (op (car l1) (car l2))
       (map2 op (cdr l1) (cdr l2)))))

;; /table/[0-9]+
;; this sucks
(define (get* it table thing)
  (get it (string->symbol (str table "." thing)) #f))

(define (make-article-graph id h3 data-fn)
  (λ (id*)
    (article-graph id h3 (data-fn id*))))

(define *graphs*
  (ff
   'coffees `(,(make-article-graph
                'coffee-style-ratios 'render.coffee.style-ratios
                (λ (id)
                  (json/encode
                   (map
                    (λ (l) (cons (str (car l)) (cadr l)))
                    (db-get-where 'brews '("distinct method" "count(method)") "where coffee = ? group by method" (list id))))))
              ,(make-article-graph
                'coffee-rating-distribution 'render.coffee.rating-distribution
                (λ (id)
                  (json/encode
                   (map
                    (λ (l) (cons (str (car l)) (cadr l)))
                    (db-get-where 'brews '("distinct rating" "count(rating)") "where coffee = ? group by rating" (list id))))))
              )
   ))

(define (make-content-renderer table)
  (let ((schema (get-schema table)))
    (λ (r)
      (lets ((id (string->number (last ((string->regex "c/\\//") (get r 'path 'bug)) 0)))
             (rels (ff-fold (λ (a k v) (if (and (list? v) (not (eq? (cadr v) 'uploads))) (cons k a) a)) #n schema)) ; of COURSE uploads are handled differently
             (trels (map (λ (r) (cadr (get schema r 'bug))) rels))
             (rel-names (map (λ (s) (string->symbol (str s ".name"))) trels)) ; WILD assumption that all relations (outside of uploads) have a .name
             (rel-list (list->sql-list rel-names))
             (rel-list (if (null? rels) rel-list (str ", " rel-list)))
             (schema-items (map (λ (s) (string->symbol (str table "." s))) (keys schema)))
             (sql (str
                   "SELECT "
                   (list->sql-list schema-items)
                   rel-list
                   " FROM " table
                   (fold2* (λ (a tbl name) (str a " LEFT JOIN " tbl " ON " tbl ".id = " table "." name)) "" trels rels)
                   " WHERE " table ".id = ?"))
             (it* (car* (execute* (db) sql (list id)))))
        (if (null? it*)
            (r/response code => 404)
            (let ((it (list->ff (zip cons (append (keys schema) rel-names) it*))))
              (r/response
               code    => 200
               headers => '((Content-type . "text/html"))
               content => (make-page
                           `(((article (class . "no-padding top-round border max"))
                              (div
                               ((img (class . "responsive small top-round")
                                     (loading . "lazy")
                                     (src . ,(str "/uploads/" (get it 'image 0)))))
                               ((div (class . "absolute bottom left right padding right-align"))
                                ((button (class . "square round")
                                         (onClick . ,(str "enlarge_image(" (get it 'image 0) ")")))
                                 (i "expand_content"))))
                              ((nav (class . "padding"))
                               (table
                                (tbody
                                 ,@(reverse
                                    (ff-fold
                                     (λ (a k v)
                                       (let ((s (get schema k 'not-there)))
                                         (cond
                                          ((and (list? s) (eq? (cadr s) 'uploads)) a)
                                          ((list? s)
                                           (let ((v* (get* it (cadr s) 'name)))
                                             (if v*
                                                 (cons `(tr (td (b ,(str k)))
                                                            (td ((a (class . "link")
                                                                    (href . ,(str "/" (cadr s) "/" v)))
                                                                 ,(str v*))))
                                                       a)
                                                 a)))
                                          ((eq? s 'not-there) a)
                                          (else
                                           (cons `(tr (td (b ,(str k))) (td ,(str v))) a)))))
                                     #n
                                     it)))))
                              ((div (class . "padding absolute bottom right"))
                               ((form (method . "POST") (action . ,(format #f "/edit/~a" table)))
                                ((label (class . "field border label"))
                                 ((input (type . "hidden") (name . "id") (value . ,(str id))))
                                 ((button (class . "square round") (type . "submit")) (i "edit"))))))
                             ,@(map (λ (f) (f id)) (get *graphs* table #n))
                             )))))))))

(define (create-export)
  (values
   (map ff->list (db-get-brews "where private_p is null or cast(private_p as integer) = 0" '()))
   (car* (car* (db-get-where 'brews '("count(*)") "where private_p is not null and cast(private_p as integer) <> 0" '())))))

(define (compress-image filename-from filename-to)
  ;; (system `("convert" ,filename-from "-resize" "640" "-quality" "90" ,filename-to)))
  (system `("convert" ,filename-from "-resize" "640" "-dither" "FloydSteinberg" "-remap" "netscape:" "-colors" "8" ,filename-to)))

(define (/id tbl)
  (format #f "m/^\\/~a\\/[0-9]+$/" tbl))

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

   (/id 'roasteries)  => (make-content-renderer 'roasteries)
   (/id 'methods)     => (make-content-renderer 'methods)
   (/id 'coffees)     => (make-content-renderer 'coffees)
   (/id 'grinders)    => (make-content-renderer 'grinders)
   (/id 'gear)        => (make-content-renderer 'gear)
   (/id 'brews)       => (make-content-renderer 'brews)

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
   "/new/brew"        => (make-simple-adder complicated-brew-adder "/brews")

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
                                      (filename (str *uploads-dir* "/" (time-ns) ".gif"))
                                      (filename* (str filename "_orig")))
                                 (thread   ; TODO: maybe don't run this in a thread, but keep user waiting for the upload & conversion to finish
                                  (begin
                                    (r/chunked-post-data->file p filename*)
                                    (compress-image filename* filename)
                                    (s3/execute (db) "INSERT INTO uploads (location, timestamp) VALUES (?, current_timestamp)" (list filename))))
                                 (r/response code => 200))))

   "/api/uploads"      => (make-api-responder 'uploads)
   "/api/last-uploads" => (make-api-responder 'uploads 20)
   "/api/roasteries"   => (make-api-responder 'roasteries)
   "/api/methods"      => (make-api-responder 'methods)
   "/api/coffees"      => (make-api-responder 'coffees)
   "/api/grinders"     => (make-api-responder 'grinders)
   "/api/gear"         => (make-api-responder 'gear)
   "/api/brews"        => (make-api-responder 'brews)
   "/api/export"       => (λ (req)
                           (lets ((vs nskip (create-export)))
                             (r/response
                              code    => 200
                              headers => '((Content-type . "application/json"))
                              content => (json/encode
                                          `(("skipped" . ,nskip)
                                            ("items"   . ,vs))))))

   "m/^\\/uploads\\/[0-9]+$/" => (λ (r)
                                   (let ((id (string->number (last ((string->regex "c/\\//") (get r 'path 'bug)) 0))))
                                     (let ((res (execute* (db) "SELECT location FROM uploads WHERE id = ?" (list id))))
                                       (if (null? res)
                                           (r/response code => 404)
                                           (r/response
                                            code    => 200
                                            headers => `((Content-type . ,(r/path->mime (caar res))))
                                            content => (file->list (caar res)))))))
   "m/^\\/static/"    => (λ (r) (r/static-dispatcher "static" "/static" r))
   ))


;; i don't like that
(define migrate! (get-migrator))
(define schema (interact 'schema (tuple 'dump)))
(define l10n!  (interact 'l10n (tuple 'dump)))

(λ (_)
  (let ((ptr (s3/open *db-file*)))
    (migrate! ptr)
    (s3/close ptr))

  (l10n!)
  (start-schema-thread schema) ; re-start schema thread with defined schema

  (db-refresher *db-file*)
  (r/fastcgi-bind *port* app (r/make-stdout-logger)))

;; Local Variables:
;; compile-command: "make run"
;; End:
