(define-module (hackdepot core db)
  #:use-module (oop goops)
  #:use-module (sqlite3)
  #:use-module (hackdepot core types item)
  #:export (<db>
            db-handle
            db-open!
            db-close!
            db-add-item!
            db-get-item
            db-get-item/all))


(define-class <db> ()
  (handle
   #:init-value   #f
   #:init-keyword #:handle
   #:getter       db-handle
   #:setter       db-handle-set!))



(define-method (%db-init! db)
  (let ((sql (string-append
              "CREATE TABLE items(id INTEGER PRIMARY KEY, owner INTEGER, name TEXT, description TEXT);"
              "CREATE TABLE users(id INTEGER PRIMARY KEY, user_id TEXT, name TEXT);"
              "CREATE TABLE components(id INTEGER PRIMARY KEY, item_id INTEGER, component_id INTEGER)")))
    (sqlite-exec (db-handle db) sql)))


(define-method (db-open! (db <db>) (filename <string>))
  (if (file-exists? filename)
      (db-handle-set! db (sqlite-open filename))
      (begin
        (db-handle-set! db (sqlite-open filename))
        (%db-init! db))))


(define-method (db-close! (db <db>))
  (sqlite-close (db-handle db)))

;; Execute a QUERY on the database DB.  Return the execution status pair (return
;; code and a message), or #f when an error occurred.
(define-method (db-exec (db <db>) (query <string>))
  (sqlite-exec (db-handle db) query))

;; Get a row from the previous query.
(define-method (db-prepare (db <db>) (stmt <string>))
  (sqlite-prepare (db-handle db) stmt))

(define-method (db-bind (db <db>) key value)
  (sqlite-bind (db-handle db) key value))

(define-method (db-step (db <db>))
  (sqlite-step (db-handle db)))



(define-method (db-get-item (db <db>) (item-id <number>))
  (let* ((handle (db-handle db))
         (stmt   (sqlite-prepare handle
                                 "SELECT * FROM items WHERE id = ?")))
    (sqlite-bind stmt 1 item-id)
    (let ((bv (sqlite-step stmt)))
      (and bv
           (make <item>
             #:id          (vector-ref bv 0)
             #:owner       (vector-ref bv 1)
             #:name        (vector-ref bv 2)
             #:description (vector-ref bv 3))))))

(define-method (db-get-item/all (db <db>))
  (let* ((handle (db-handle db))
         (stmt   (sqlite-prepare handle "SELECT * FROM items")))
    (let loop ((bv     (sqlite-step stmt))
               (result '()))
      (if bv
          (loop (sqlite-step stmt)
                (cons (make <item>
                        #:id          (vector-ref bv 0)
                        #:owner       (vector-ref bv 1)
                        #:name        (vector-ref bv 2)
                        #:description (vector-ref bv 3))
                      result))
          (reverse result)))))



(define-method (db-add-item! (db <db>) (item <item>))
  (let* ((handle (db-handle db))
         (stmt   (sqlite-prepare handle
                                 (string-append
                                  "INSERT INTO items(owner, name, description)"
                                  "VALUES (?, ?, ?)"))))
    (sqlite-bind stmt 1 (item-owner       item))
    (sqlite-bind stmt 2 (item-name        item))
    (sqlite-bind stmt 3 (item-description item))
    (sqlite-step stmt)))

;;; db.scm ends here.
