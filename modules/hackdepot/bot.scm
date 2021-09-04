(define-module (hackdepot bot)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (deck core types matrix-id)
  #:use-module (deck core types state)
  #:use-module (deck matrix)
  #:use-module (deck matrix-client)
  #:use-module (deck core room)
  #:use-module (deck core session)
  #:use-module (hackdepot core types item)
  #:use-module (hackdepot core db)
  #:export (<bot>
            bot-matrix
            bot-matrix-session
            bot-db
            bot-rooms
            bot-user
            bot-login!
            bot-start!))

(define-class <bot> ()
  ;; <matrix>
  (matrix
   #:getter       bot-matrix
   #:setter       bot-matrix-set!)

  ;; <session>
  (matrix-session
   #:getter       bot-matrix-session
   #:setter       bot-matrix-session-set!)

  ;; <matrix-client>
  (matrix-client
   #:getter       bot-matrix-client
   #:setter       bot-matrix-client-set!)

  ;; <db>
  (db
   #:init-keyword #:db
   #:init-value   #f
   #:getter       bot-db
   #:setter       bot-db-set!)

  ;; <list>
  (rooms
   #:init-value   '()
   #:init-keyword #:rooms
   #:getter       bot-rooms)

  (user
   #:init-value   #f
   #:getter       bot-user))



(define (bot-handle-event bot client event-room-id event)
  (format #t "[debug] event: ~a, room: ~a~%" event event-room-id)
  (when (member (matrix-id->string event-room-id) (bot-rooms bot))
    (let* ((message-content (assoc-ref event "content"))
           (message-body    (assoc-ref message-content "body")))
      (when (string=? message-body "!list")
        (let ((room  (matrix-client-room client event-room-id))
              (items (db-get-item/all (bot-db bot))))
          (format #t "[debug] room:  ~a~%" room)
          (format #t "[debug] items: ~a~%" items)
          (room-send room "m.room.message"
                     `(("body"    . ,(string-join
                                      (map (lambda (item)
                                             (format #f
                                                     "- ~a: ~a"
                                                     (item-id item)
                                                     (item-name item)))
                                           items)
                                      "\n"))
                       ("msgtype" . "m.text"))))))))


(define-method (bot-login! (bot      <bot>)
                           (user     <string>)
                           (password <string>))
  (let ((user-id (string->matrix-id user)))
    (bot-matrix-set! bot (make <matrix>
                           #:use-https? #f
                           #:home-server (build-uri 'https
                                                     #:host (matrix-id-server user-id)))))
  (bot-matrix-session-set! bot
                           (matrix-login (bot-matrix bot)
                                         "m.login.password"
                                         user
                                         password)))



(define-method (bot-start! (bot <bot>))
  (let ((client (make <matrix-client>
                  #:session   (bot-matrix-session bot)
                  #:on-update (lambda (client update)
                                #t)
                                ;; (format #t "room update; ~a~%" (room-update-content update)))
                  #:on-timeline-event `(("m.room.message"
                                         . ,(lambda (client room-id event)
                                              (bot-handle-event bot
                                                                client
                                                                room-id
                                                                event)))))))
    (bot-matrix-client-set! bot client)
    (for-each (lambda (room-id)
                (session-join-room (bot-matrix-session bot) room-id))
              (bot-rooms bot))
    (matrix-client-start! client)))

;;; bot.scm ends here.
