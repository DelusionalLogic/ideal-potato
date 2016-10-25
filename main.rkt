#lang scheme
(require "time.rkt")

(define (day-name day)
  (list-ref '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day)
  )

(define (format-time ts)
  (format "~a ~a/~a-~a - ~a:~a.~a" (day-name (day-of-week ts)) (+ 1 (day-of-month ts)) (+ 1 (month ts)) (year ts) (hour ts) (minute ts) (second ts))
  )

; Calendar layout
;	CALENDAR:
;	'(
;		<NAME>
;		<DESCRIPTION>
;		'(
;			<EVENT>*
;		)
;		'(
;			<CALENDAR>*
;		)
;	)
;
;	EVENT:
;	'(
;		<TIMES>
;		<DESCRIPTION>
;	)
;
;	TIMES:
;	'(
;		<TIME> (FROM TIME)
;		<TIME> (TO TIME)
;	)
;
;	TIME:
;	<UNIXTIME>
(define cal
(list
  "PEWPEW"
  "WOO"
  (list
	(list
	  "TIME"
	  "WOP"
	  123
	  123
	)
	(list
	  "TIME"
	  "WOP"
	  234
	  123
	)
  )
  (list
	(list
	  "Cal2"
	  "Cal2 desc"
	  (list
		(list
		  "Event2"
		  "Event2 desc"
		  1222
		  1337
		  )
		)
	  (list
		)
	  )
	 )
)
)
(define (is-time? ts)
  (number? ts)
  )


(define (is-event? event)
  (match event
		 [(list name desc start end) (and (string? name) (string? desc) (is-time? start) (is-time? end))]
		 [_ #f]
		 )
  )

(define (is-calendar? cal)
  (match cal
		 [(list name desc events cals) (and (string? name) (string? desc) (andmap is-event? events) (andmap is-calendar? cals))]
		 [_ #f]
		 )
  )

(define (with-calendar cal func)
  (if (is-calendar? cal)
	(func cal)
	(error "Not a calendar")
	)
  )

(define (with-event event func)
  (if (is-event? event)
	(func event)
	(error "Not an Event")
	)
  )

(define (calendar-name cal)
	(with-calendar cal
	  (lambda (cal)
		(list-ref cal 0)
		)
	  )
  )

(define (calendar-description cal)
	(with-calendar cal
	  (lambda (cal)
		(list-ref cal 1)
		)
	  )
  )

(define (calendar-events cal)
	(with-calendar cal
	  (lambda (cal)
		(list-ref cal 2)
		)
	  )
  )

(define (calendar-calendars cal)
	(with-calendar cal
	  (lambda (cal)
		(list-ref cal 3)
		)
	  )
  )

(define (event-name event)
  (with-event event
	(lambda (event)
	  (list-ref event 0)
	  )
	)
  )

(define (event-desc event)
  (with-event event
	(lambda (event)
	  (list-ref event 1)
	  )
	)
  )

(define (event-start event)
  (with-event event
	(lambda (event)
	  (list-ref event 2)
	  )
	)
  )

(define (event-end event)
  (with-event event
	(lambda (event)
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

(define (calendar-new name desc)
  (list name desc '() '())
  )

(define (calendar-add-event cal event)
  (with-calendar cal
	(lambda (cal)
	  (list-mod cal 2 (lambda (old) (cons event old)))
	  )
	)
  )

(define (calendar-add-subcal cal subcal)
  (with-calendar cal
	(lambda (cal)
	  (list-mod cal 3 (lambda (old) (cons subcal old)))
	  )
	)
  )

(define (calendar-remove-event cal eventnum)
  (with-calendar cal
	(lambda (cal)
	  (list-mod cal 2 (lambda (old) (list-rem old eventnum)))
	  )
	)
  )

(define (calendar-remove-subcal cal subcalnum)
  (with-calendar cal
	(lambda (cal)
	  (list-mod cal 3 (lambda (old) (list-rem old subcalnum)))
	  )
	)
  )

(print (calendar-add-event cal (list-ref (calendar-events cal) 1)))
(display "\n")
(display "\n")
(print (calendar-add-subcal cal cal))
(display "\n")
(display "\n")
(print (calendar-remove-event cal 0))
(display "\n")
(display "\n")

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

(calendar-calendars cal)
(list-ref (calendar-events cal) 0)
(format-time (event-end (list-ref (calendar-events cal) 0)))
