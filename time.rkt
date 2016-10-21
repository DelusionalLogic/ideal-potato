#lang scheme

;Unix timestamp parsing, adapted from the source of GMTIME in minix
(provide second minute hour day-of-week day-of-month day-of-year month year)

(define SEC_PER_DAY 86400)

(define (sec-of-day ts)
  (modulo ts SEC_PER_DAY)
  )

(define (day-since-1-1970 ts)
  (floor (/ ts SEC_PER_DAY))
  )

(define (second ts)
  (modulo (sec-of-day ts) 60)
  )

(define (minute ts)
  (modulo (floor (/ (sec-of-day ts) 60)) 60)
  )

(define (hour ts)
  (modulo (floor (/ (sec-of-day ts) 3600)) 24)
  )

(define (day-of-week ts)
  (modulo (+ (day-since-1-1970 ts) 3) 7) ;The 0th day was a thursday, so align to sunday with +4
  )

; Calculate year {{{
(define (is-leap? year)
  (cond
    [(eq? (modulo year 400) 0) #t]
    [(eq? (modulo year 100) 0) #f]
    [(eq? (modulo year 4) 0) #t]
    [else #f]
    )
  )

(define (year-size year)
  (if (is-leap? year) 366 365)
  )

(define (modulo-year_help day year)
  (cond
    [(>= day (year-size year)) (modulo-year_help (- day (year-size year)) (+ year 1))]
    [else (list year day)]
    )
  )

(define (unix-modulo-year ts)
  (modulo-year_help (day-since-1-1970 ts) 1970)
  )
; }}}
(define (day-of-year ts) ;{{{
  (list-ref (unix-modulo-year ts) 1)
  ) ;}}}

(define (year ts) ;{{{
  (list-ref (unix-modulo-year ts) 0)
  ) ;}}}

(define (year-descriptor year) ;{{{
  (if (is-leap? year)
    '(31 29 31 30 31 30 31 31 30 31 30 31)
    '(31 28 31 30 31 30 31 31 30 31 30 31)
    )
  ) ;}}}
(define (days-in-month year month) ;{{{
  (list-ref (year-descriptor year) month)
  ) ;}}}

(define (modulo-month_help year day month)
  (cond
    [(>= day (days-in-month year month)) (modulo-month_help year (- day (days-in-month year month)) (+ month 1))]
    [else (list month day)]
    )
  )

(define (unix-modulo-month ts)
  (modulo-month_help (year ts) (day-of-year ts) 0)
  )

(define (month ts)
  (list-ref (unix-modulo-month ts) 0)
  )

(define (day-of-month ts)
  (list-ref (unix-modulo-month ts) 1)
  )
