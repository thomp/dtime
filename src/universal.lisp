(in-package :dat-time)
;;
;; universal.lisp: utilities for working with universal time
;;
(defun a-week-ago (time)
  "Return universal time corresponding to one week before the time TIME."
  (- time *week-in-seconds*))

(defgeneric adjust-to (time time-component x))

(defmethod adjust-to (time (time-component (eql :hr)) x)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (declare (ignore hour))
    (encode-universal-time second minute x date month year)))

;; TIME is a universal time; TIME-COMPONENT is a keyword; X is the
;; corresponding minimum value for that slot in the returned universal
;; time equivalent
(defgeneric adjust-to-at-least (time time-component x))

(defmethod adjust-to-at-least (time (time-component (eql :hr)) x)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (if (>= hour x)
	(encode-universal-time second minute hour date month year)
	(encode-universal-time 0 0 x date month year))))

(defgeneric adjust-to-no-more-than (time time-component x))

(defmethod adjust-to-no-more-than (time (time-component (eql :hr)) x)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (if (<= hour x)
	(encode-universal-time second minute hour date month year)
	(encode-universal-time 0 0 x date month year))))

(defun days-spanned-by (time1 time2)
  "Return an integer. TIME1 is a universal time preceding universal
time TIME2."
  (assert (> time2 time1))
  ;; identify start (first second) of first complete day succeeding time1
  ;; identify end (last second) of first complete day preceding time2
  (let ((first-whole-day-start (start-of-nth-day time1 1))
	(last-whole-day-end (+ (1- *day-in-seconds*)
			       (start-of-nth-day time2 -1))))
    (cond ((= (- first-whole-day-start last-whole-day-end)
	      1)
	   ;; time1 is exact start of first day and time 2 is exact start of succeeding day
	   2)
	  ((> first-whole-day-start last-whole-day-end)
	   ;; a complete day isn't bracketed by time1 and time2 (e.g., time1 is on 8/22/2013 and time2 is on 8/23/2013)
	   0)
	  (t
	   (let ((number-of-days
		  (/ (- (1+ last-whole-day-end)
			first-whole-day-start)
		     *day-in-seconds*
		     )))
	     (if (< time1 first-whole-day-start)
		 (incf number-of-days))
	     (if (> time2 last-whole-day-end)
		 (incf number-of-days))
	     number-of-days)))))

(defun end-of-day (time)
  "Return the universal time representing the end of the day
containing INITIAL-TIME."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (declare (ignore second minute hour))
    (encode-universal-time 59 59 23 date month year)))

;; test: > (hours-in-range 3569871654 3569903999) -> (15 16 17 18 19 20 21 22 23)

;; return ascending list of universal times corresponding to hours within range START and END
(defun hours-in-range (start end)
  (let ((accum nil))
    (do ((time end (- time 3600)))
	((< time start))
      (push (universal-time-hr time) accum))
    accum))

(defun next-month (month year)
  "Given month MONTH in year YEAR, return as multiple values the month
and year value corresponding to the next month."
  (declare (fixnum month year))
  (assert (and (<= month 12) (> month 0)))
  (cond ((= 12 month)
	 (values 1 (1+ year)))
	(t ; (< month 12)
	 (values (1+ month) year))))

;; > (start-of-day 3586143600) --> 3586143600

(defun start-of-day (time)
  "Return the universal time representing the start of the day
containing INITIAL-TIME."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 date month year)))

(defun start-of-next-month (time)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (declare (ignore second minute hour date))
    (multiple-value-bind (next-month next-year)
	(next-month month year)
      (encode-universal-time 0 0 0 1 next-month next-year))))

(defun start-of-nth-day (initial-time day-index)
  "Return the universal time representing the start of the nth day
relative to universal time INITIAL-TIME where DAY-INDEX is an integer
representing the nth day where 0 corresponds to the day containing
INITIAL-TIME."
  ;; DAY-LENGTH = (* 24 60 60) = 86400 s
  (start-of-day (+ (* *day-in-seconds* day-index) initial-time)))

;; return a true value if universal time TIME is in the day associated with universal time DAYTIME
(defun time-in-day-p (time daytime)
  (time-in-range-p time (start-of-day daytime) (end-of-day daytime)))

(defun time-in-range-p (time start end)
  (and (>= time start)
       (<= time end)))

(defun universal-time-day-abbr1 (universal-time)
  (declare (integer universal-time))
  (nth (universal-time-day-of-week universal-time) *day-abbr1*))

(defun universal-time-day-abbr3 (universal-time)
  (declare (integer universal-time))
  (nth (universal-time-day-of-week universal-time) *day-abbr3*))

(defun universal-time-date (universal-time)
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour month day-of-week year))
    date))

(defun universal-time-month (universal-time)
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour date day-of-week year))
    month))

(defun universal-time-day-name (universal-time)
  (declare (integer universal-time))
  (nth (universal-time-day-of-week universal-time) *day-names*))

(defun universal-time-day-of-week (universal-time)
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour date month year))
    day-of-week))

(defun universal-time-hr (universal-time)
  "Return an integer"
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute date month year))
    hour))

(defun universal-time-hr-exact (universal-time)
  "Return a value between 0 and 24."
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore date month year))
    (+ hour
       (/ minute 60)
       (/ second *hour-in-seconds*))))

(defun universal-time-min (universal-time)
  "Return an integer"
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal-time)
    (declare (ignore second hour date month year))
    minute))

(defun universal-time-year (universal-time)
  "Return an integer."
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time universal-time)
    (declare (ignore second minute month hour date day-of-week))
    year))

(defun universal-time-p (x)
  "Test whether X can be interpreted as a Common Lisp universal time."
  (and (integerp x) (>= x 0)))

(deftype universal-time ()
  '(satisfies universal-time-p))

(defun universal-to-posix (universal-time)
  ;; offset is 2208988800
  ;;(local-time:timestamp-to-unix (local-time:universal-to-timestamp universal-time))
  (- universal-time 2208988800))
