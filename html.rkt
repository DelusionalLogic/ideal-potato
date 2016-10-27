#lang scheme

(provide html body head title h1 div ul li)

(define (list-between sep lst)
  (match lst
      ['() (list sep)]
      [(list x xs ...) (append (list sep x) (list-between sep xs))]
    )
  )

(define (list->sep-string sep lst)
  (apply string-append (list-between sep lst))
  )

(define (tag name params children)
  (string-append
   (format "<~A~A>" name (list->sep-string "" params))
   (list->sep-string "" (flatten children))
   (format "</~A>" name)
   )
  )

(define (html . children)
  (tag "html" '() children)
  )

(define (body . children)
  (tag "body" '() children)
  )

(define (head . children)
  (tag "head" '() children)
  )

(define (title . children)
  (tag "title" '() children)
  )

(define (h1 . children)
  (tag "h1" '() children)
  )

(define (div . children)
  (tag "div" '() children)
  )

(define (ul . children)
  (tag "ul" '() children)
  )

(define (li . children)
  (tag "li" '() children)
  )
