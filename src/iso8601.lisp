(in-package :dat-time)
;;
;; Generate ISO 8601 strings
;;
(defun iso8601-date-string (universal-time &key day mn yr)
  "Return a string, representing a date, with the form 2010-06-07.
UNIVERSAL-TIME, an integer, supersedes current time if UNIVERSAL-TIME
is non-negative. DAY, MN, and YR supersede UNIVERSAL-TIME or current time."
  (declare (integer universal-time))
  (multiple-value-bind (second minute hour date month year)
      (if (>= universal-time 0)
	  (decode-universal-time universal-time)
	  (get-decoded-time))
    (declare (ignore second minute hour))
    (let ((d (or day date))
	  (mo (or mn month))
	  (y (or yr year)))
      (format nil "~4,'0D-~2,'0,D-~2,'0,D" y mo d))))

(defun iso8601-time-string (&key sec min hr day mn yr offset stream web-p)
  "Generate a string of format 2006-10-20T15:56:35.xxxx (an ISO8601
  representation of the current date and time, using the system
  date/time values. If SEC, MIN, HR, DAY, MN (month), or YR are
  specified, use in place of system date/time values. Write to stream
  STREAM if STREAM is non-NIL.

Note that (1) the digits representing a fraction of a second may have
no significance (2) no time zone information is included.

If WEB-P is true, the colons (which are not transparent in URLs) are
not included in the string (note that this is also a legitimate
ISO8601 time format).

If OFFSET is an integer, add that number of seconds to the time."
  (let ((universal-time (get-universal-time)))
    (if (integerp offset)
	(setq universal-time (+ universal-time offset)))
    (multiple-value-bind (second minute hour date month year)
	(decode-universal-time universal-time)
      (let ((s (if sec sec second))
	    (m (if min min minute))

	    (h (if hr hr hour))
	    (d (if day day date))
	    (mo (if mn mn month))
	    (y (if yr yr year)))
	;; FIXME: get desired :FORMAT arg working, then use
	;; (universal-to-iso8601 universal-time :stream s)
	(format (or stream nil)
       	(if web-p
       	    "~4,'0D-~2,'0,D-~2,'0,DT~2,'0,D~2,'0,D~2,'0,D.~A"
       	    "~4,'0D-~2,'0,D-~2,'0,DT~2,'0,D:~2,'0,D:~2,'0,D.~A")
       	y mo d h m s
       	(get-internal-real-time))
	))))

;; Returns coordinated UTC
;; example: 2012-04-16T16:19:24.000000Z
(defun iso8601-time-string-Z (&optional local-time-timestamp)
  "Return a string."
  (local-time:format-timestring nil	;If destination is T, the string is written to *standard-output*. If destination is a stream, the string is written to the stream.
				(or local-time-timestamp (local-time:now))
				:format local-time:+iso-8601-format+
				:timezone local-time:+UTC-ZONE+))

;; (dtime::universal-to-iso8601 (get-universal-time))
;; "2014-11-30T14:03:06.000000-08:00"
(defun universal-to-iso8601 (universal-time &key stream)
  "Return a string representing an ISO 8601 date/time string."
  (local-time:format-timestring
   stream
   (local-time:universal-to-timestamp
    universal-time)
   :format local-time:+iso-8601-format+))
