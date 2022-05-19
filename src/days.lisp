(in-package :dat-time)

(defparameter *day-abbr2* '("Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))

(defparameter *day-abbr3* '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter *day-keywords*
  '(:mon :tue :wed :thu :fri :sat :sun))

;(defparameter *day-names* '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter *day-names*
  (mapcar #'(lambda (n day)
	      (cons n day))
	  '(0 1 2 3 4 5 6)
	  *day-keywords*))

(deftype day-name ()
  '(satisfies day-name-p))

(defun day-name-p (x)
  (member x *day-keywords*))

(deftype day-of-week ()
  '(integer 0 6))

(defparameter *seconds-per-day*
  (* 60 60 24))

(defun day-of-week->day-name (day-of-week)
  (declare (day-of-week day-of-week))
  (the day-name
    (car (rassoc day-of-week *day-names*))))

(defun day-name->day-of-week (day-name)
  (declare (day-name day-name))
  (the day-of-week
    (cdr (assoc day-name *day-names*))))

;; FIXME: use :today for today?
(defun next-day-with-name (day-name)
  "Return a universal time."
  (declare (day-name day-name))
  ;; current date/time
  (let ((universal-time (get-universal-time)))
    (declare (integer universal-time))
    ;; CURRENT-DAY-OF-WEEK is an integer corresponding to day of week (0=Mon,1=Tues, ...)
    (multiple-value-bind (sec min h d m y current-day-of-week)
	(decode-universal-time universal-time)
      (declare (ignore sec min h d m y))
      (let ((next-day-of-week (day-name->day-of-week day-name)))
	;; handle month and year boundaries by using universal time
	(let ((raw-day-delta
	       (- current-day-of-week next-day-of-week)))
	  (let ((actual-day-delta
		 (if (> raw-day-delta 0)
		     raw-day-delta
		     (+ 7 raw-day-delta))))
	    ;; now return universal time
	    (+ universal-time
	       (* *seconds-per-day* actual-day-delta))))))))

(defun weekend-day-p (universal)
  (> (universal-time-day-of-week universal)
     4))
