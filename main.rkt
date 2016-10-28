#lang scheme
(require "time.rkt")
(require "html.rkt")

; Calendar layout
;    CALENDAR:
;    '(
;        <NAME>
;        <DESCRIPTION>
;        '(
;            <EVENT>*
;        )
;        '(
;            <CALENDAR>*
;        )
;    )
;
;    EVENT:
;    '(
;        <NAME>
;        <DESCRIPTION>
;        <TIMES>
;    )
;
;    TIMES:
;        <TIME> (FROM TIME)
;        <TIME> (TO TIME)
;
;    TIME:
;        <UNIXTIME>

(define (event? event)
  (match event
         [(list name desc start end) (and (string? name) (string? desc) (timestamp? start) (timestamp? end))]
         [_ #f]
         )
  )

(define (calendar? cal)
  (match cal
         [(list name desc events cals) (and (string? name) (string? desc) (andmap event? events) (andmap calendar? cals))]
         [_ #f]
         )
  )

(define (with-calendar cal func)
  (if (calendar? cal)
    (func cal)
    (error "Not a calendar")
    )
  )

(define (with-event event func)
  (if (event? event)
    (func event)
    (error "Not an Event")
    )
  )

(define (calendar-name cal)
    (with-calendar cal
      (λ (cal)
        (list-ref cal 0)
        )
      )
  )

(define (calendar-description cal)
    (with-calendar cal
      (λ (cal)
        (list-ref cal 1)
        )
      )
  )

(define (calendar-events cal)
    (with-calendar cal
      (λ (cal)
        (list-ref cal 2)
        )
      )
  )

(define (calendar-calendars cal)
    (with-calendar cal
      (λ (cal)
        (list-ref cal 3)
        )
      )
  )

(define (event-name event)
  (with-event event
    (λ (event)
      (list-ref event 0)
      )
    )
  )

(define (event-desc event)
  (with-event event
    (λ (event)
      (list-ref event 1)
      )
    )
  )

(define (event-start event)
  (with-event event
    (λ (event)
      (list-ref event 2)
      )
    )
  )

(define (event-end event)
  (with-event event
    (λ (event)
      (list-ref event 3)
      )
    )
  )

(define (list-mod_help lst curindex index function res)
  (match lst
    [(list x xs ...) (list-mod_help xs (+ curindex 1) index function (cons (if (eq? index curindex) (function x) x) res))]
    [_ res]
    )
  )

(define (list-mod lst index function)
  (reverse (list-mod_help lst 0 index function '()))
  )

(define (list-rem_help lst curindex index res)
  (match lst
    [(list x xs ...) (list-rem_help xs (+ curindex 1) index (if (eq? index curindex) res (cons x res)))]
    [_ res]
    )
  )

(define (list-rem lst index)
  (reverse (list-rem_help lst 0 index '()))
  )

(define (calendar-new name desc [events '()] [subcal '()])
  (cond
    [(and (string? name) (string? desc) (andmap is-event? events) (andmap is-calendar? subcal)) (list name desc events subcal)]
    [else (error "Invalid parmeters")]
    )
  )

(define (event-new name desc start end)
  (cond
    [(and (string? name) (string? desc) (timestamp? start) (timestamp? end)) (list name desc start end)]
    )
  )

(define (calendar-add-event cal event)
  (with-calendar cal
    (λ (cal)
      (list-mod cal 2 (λ (old) (cons event old)))
      )
    )
  )

(define (calendar-add-subcal cal subcal)
  (with-calendar cal
    (λ (cal)
      (list-mod cal 3 (λ (old) (cons subcal old)))
      )
    )
  )

(define (calendar-remove-event cal eventnum)
  (with-calendar cal
    (λ (cal)
      (list-mod cal 2 (λ (old) (list-rem old eventnum)))
      )
    )
  )

(define (calendar-remove-subcal cal subcalnum)
  (with-calendar cal
    (λ (cal)
      (list-mod cal 3 (λ (old) (list-rem old subcalnum)))
      )
    )
  )

(define (range_help current end step lst)
  (cond
    [(<= end current) lst]
    [else (range_help (+ current step) end step (cons current lst))]
    )
  )

(define range (case-lambda
    [(end) (reverse (range_help 0 end 1 '()))]
    [(start end) (reverse (range_help start end 1 '()))]
    [(start end step) (reverse (range_help start end step '()))]
  )
  )

(define (calendar-flatten cal)
  (with-calendar cal (λ (cal)
    (calendar-new 
      (calendar-name cal)
      (calendar-description cal)
      (apply append
        (calendar-events cal) 
        (map (λ (x)
          (calendar-events (calendar-flatten x))) (calendar-calendars cal)
          )
        )
      '())
    ))
  )

(define (event-starts-before? e1 e2)
  (< (event-start e1) (event-start e2))
  )

(define (event-ends-before? e1 e2)
  (with-event e1 (λ (e1)
    (with-event e2 (λ (e2)
      (< (event-start e1) (event-start e2))
      ))
    ))
  )

(define (find-appointments cal pred)
  (filter pred (calendar-events (calendar-flatten cal)))
  )

(define (find-first-appointment cal pred)
  (car (sort (find-appointments cal pred) event-starts-before?))
  )

(define (find-last-appointment cal pred)
  (car (reverse (sort (find-appointments cal pred) event-starts-before?)))
  )

(define (events-overlap? e1 e2)
  (and (< (event-start e1) (event-end e2)) (< (event-start e2) (event-end e1)))
  )

(define (calendars-overlap? cal1 cal2)
  (ormap 
    (λ (x) 
      (apply events-overlap? x)
      )
    (cartesian-product
      (calendar-events
        (calendar-flatten cal1)
        )
      (calendar-events
        (calendar-flatten cal2)
        )
      )
    )
  )

(define (day-name day)
  (list-ref '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day)
  )

(define (replicate num char)
  (cond
    [(<= num 0) ""]
    [else (string-append (replicate (- num 1) char) char)]
    )
  )

(define (left-pad char len str)
  (string-append (replicate (- len (string-length str)) char) str)
  )

(define (format-time ts)
  (format "~a ~a/~a-~a - ~a:~a.~a"
    (day-name (day-of-week ts))
    (+ 1 (day-of-month ts))
    (+ 1 (month ts))
    (left-pad "0" 4 (number->string (year ts)))
    (left-pad "0" 2 (number->string (hour ts)))
    (left-pad "0" 2 (number->string (minute ts)))
    (left-pad "0" 2 (number->string (second ts)))
    )
  )


(define (display-time ts)
  (format-time ts)
  )

(define (display-event event)
  (li (event-name event) " @ " (display-time (event-start event)) " - " (display-time (event-end event)))
  )

(define (display-event-list event-list)
  (ul (map display-event (sort event-list event-starts-before?)))
  )


(define (display-calendar cal)
  (div
    (h1 (calendar-name cal))
    (display-event-list (calendar-events (calendar-flatten cal)))
    )
  )

(define cal
(calendar-new
  "Space"
  "My trip to space"
  (list
    (event-new
      "Walk on the moon"
      "I wanna me lance legstrong"
      (string->time "12/04 - 2016 12:15.02")
      (string->time "12/04 - 2016 12:16.03")
    )
    (event-new
      "Walk on mars"
      "But i ended up being lance armstronger"
      (string->time "20/04 - 2016 14:42.32")
      (string->time "22/05 - 2017 18:15.02")
    )
  )
  (list
    (calendar-new
      "Cal2"
      "Cal2 desc"
      (list
        (event-new
          "Event2"
          "Event2 desc"
          (string->time "12/04 - 2016 12:15.02")
          (string->time "14/04 - 2016 12:15.02")
          )
        )
      )
    )
  )
)

(define cal2
(calendar-new
  "Dinner plans"
  "My dinner plans"
  (list
    (event-new
      "Dinner at mosebys"
      "I'm hungry"
      (string->time "11/04 - 2016 12:15.02") ;Wont overlap
      (string->time "11/04 - 2016 12:16.03")
    )
    (event-new
      "Eat at McD"
      "Because i hate myself"
      (string->time "17/04 - 2016 14:42.32")
      (string->time "20/04 - 2016 18:15.02") ;Should overlap
    )
  )
  (list
    (calendar-new
      "Jeff's dinner plans"
      "My names jeff"
      (list
        (event-new
          "I'm out"
          "What am i doing with my life"
          (string->time "20/06 - 2016 12:15.02")
          (string->time "21/06 - 2016 12:15.02")
          )
        )
      )
    )
  )
)

(define cal3
(calendar-new
  "Non overlapping plans"
  "This is where i keep all my logical plans"
  (list
    (event-new
      "AAA"
      ""
      (string->time "11/04 - 2016 12:15.02") ;Wont overlap
      (string->time "11/04 - 2016 12:16.03")
    )
    (event-new
      "BBB"
      ""
      (string->time "17/04 - 2016 14:42.32")
      (string->time "19/04 - 2016 18:15.02") ;Wont overlap
    )
  )
  )
)

(display 
  (html
    (body
      (display-calendar cal)
      (display-calendar cal2)
      (display-calendar cal3)
      (h1 "Tests")
      (ul
        (li "Overlap" (format " ~a" (calendars-overlap? cal cal2)))
        (li "Overlap" (format " ~a" (calendars-overlap? cal cal3)))
        )
      )
    )
  )
