#lang scheme

;Unix timestamp parsing, adapted from the source of GMTIME in minix
(provide second minute hour day-of-week day-of-month day-of-year month year
         is-timestamp?
         timestamp-new timestamp-add-second timestamp-add-minute timestamp-add-hour timestamp-add-day timestamp-add-month timestamp-add-year
         string->time)

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

(define (timestamp-new)
  0
  )

(define (timestamp? ts)
  (number? ts)
  )


(define (timestamp-add-second timestamp [seconds 1])
  (+ timestamp seconds)
  )

(define (timestamp-add-minute timestamp [minutes 1])
  (timestamp-add-second timestamp (* minutes 60))
  )

(define (timestamp-add-hour timestamp [hours 1])
  (timestamp-add-minute timestamp (* hours 60))
  )

(define (timestamp-add-day timestamp [days 1])
  (timestamp-add-hour timestamp (* days 24))
  )

(define (timestamp-add-month_help monthsleft timestamp)
  (cond
    [(eq? monthsleft 0) timestamp]
    [else (timestamp-add-month_help (- monthsleft 1) (timestamp-add-day timestamp (days-in-month (year timestamp) (month timestamp))))]
    )
  )

(define (timestamp-add-month timestamp [months 1])
  (timestamp-add-month_help months timestamp)
  )

(define (timestamp-add-year timestamp [years 1])
  (timestamp-add-month timestamp (* 12 years))
  )

(define (parse-string str)
  (match (regexp-match #px"(\\d{2})\\/(\\d{2}) \\- (\\d{4}) (\\d{2})\\:(\\d{2})\\.(\\d{2})" str)
    [(list _ other ...) (map string->number other)]
    )
  )

(define (string->time str)
  (match (parse-string str)
    [(list day month year hour minute second) (timestamp-add-year (timestamp-add-month (timestamp-add-day (timestamp-add-hour (timestamp-add-minute (timestamp-add-second (timestamp-new) second) minute) hour) (- day 1)) (- month 1)) (- year 1970))]
    )
  )
