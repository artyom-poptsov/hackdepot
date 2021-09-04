(define-module (hackdepot core types item)
  #:use-module (oop goops)
  #:export (<item>
            item?
            item-id
            item-id-set!
            item-owner
            item-name
            item-name-set!
            item-description
            item-description-set!
            item-components
            item-components-set!
            item-compound?))

(define-class <item> ()
  ;; <number> | #f
  (id
   #:init-keyword #:id
   #:init-value   #f
   #:getter       item-id
   #:setter       item-id-set!)

  ;; <string> | #f
  (owner
   #:init-keyword #:owner
   #:init-value   #f
   #:getter       item-owner
   #:setter       item-owner-set!)

  ;; <string> | #f
  (name
   #:init-keyword #:name
   #:init-value   #f
   #:getter       item-name
   #:setter       item-name-set!)

  ;; <string> | #f
  (description
   #:init-keyword #:description
   #:init-value   #f
   #:getter       item-description
   #:setter       item-description-set!)

  ;; Components from which the item consists of.
  ;;
  ;; <list>
  (components
   #:init-value   '()
   #:init-keyword #:items
   #:getter       item-components
   #:setter       item-components-set!))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))

(define-method (%display (item <item>) (port <port>))
  (format port "#<item ~a: ~a ~a>"
          (item-id item)
          (item-name item)
          (object-address/hex-string item)))

(define-method (display (item <item>) (port <port>))
  (%display item port))

(define-method (write (item <item>) (port <port>))
  (%display item port))



(define (item? x)
  (is-a? x <item>))

(define-method (item-compound? (item <item>))
  (not (null? (item-components item))))

