(in-package :dat-time)
;;;
;;; Misc. functions for
;;;
;;;   1. manipulating date/time values
;;;   2. quantifying execution time for code bodies (not scheduling)
;;;   3. fixes/workarounds for broken code in other time/date systems
;;;
(defparameter *day-in-seconds* 86400)

(defparameter *hour-in-seconds* 3600)

(defparameter *week-in-seconds* 604800)

(defparameter *year-in-seconds* (* 365 *day-in-seconds*))

(defparameter *posix-universal-difference*
  ;(- (encode-universal-time 0 0 00 01 01 1970 0) (encode-universal-time 0 0 00 01 01 1900 0))
  2208988800
  "Difference between POSIX time and Common Lisp universal time in seconds")

;; posix/epoch/unix time
(defun posix-time-now ()
  "Return an integer."
  (local-time:timestamp-to-unix (local-time:now)))

;; FIXME: CURRENT-YEAR better?
(defun year-now ()
  "Return a number."
  (local-time:timestamp-year (local-time:now))
  ;; or... (multiple-value-bind (a b c d e year) (get-decoded-time) (declare (ignore a b c d e)) year)
  )

(defun hours-to-seconds (hours)
  (* hours 3600))

(defgeneric seconds-delta (time1 time2))

(defmethod seconds-delta ((time1 string) (time2 string))
  "Return an integer representing the number of seconds which have
  elapsed between the 'first' time, time1, and the succeeding time,
  time2. TIME1 and TIME2 are strings in ISO8601 format. If time2 is
  not later than time1, the resulting value is a negative number.

notes:
  - TIME-DIFFERENCE is a symbol exported by CLSQL.
  - rfc3339 allows 'partial-time' format:
      partial-time = time-hour : time-minute : time-second"
  (- (local-time:timestamp-to-universal
      (local-time:parse-timestring time2))
     (local-time:timestamp-to-universal
      (local-time:parse-timestring time1))))

(defun seconds-to-hours (seconds)
  (/ seconds *hour-in-seconds*))

(defun pprint-datetime (&key (time-p t))
  "Return 'nicely formatted' string representing date and time."
  (multiple-value-bind
	(second minute hour date month year)
      (get-decoded-time)
    (with-output-to-string (s)
      (format s "~4,'0D-~2,'0,D-~2,'0,D" year month date)
      (if time-p
	  (format s "  ~2,'0,D:~2,'0,D:~2,'0,D"
		  hour minute second)))))

;; HOUR is an integer between 0 and 23
(defun us-hour (hour)
  (cond ((> hour 12)
	 (- hour 12))
	((= hour 0)
	 12)
	(t hour)))
