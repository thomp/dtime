(in-package :dat-time)
;;
;; parse a string representation of date/time
;;
(defun rfc822-to-universal (some-string)
  (net.telent.date:parse-time some-string))

(defun rfc3339-date-time-to-universal (timestring)
  (rfc3339-date-time-to-universal-KLUDGE timestring))

;; kludge to deal with LOCAL-TIME bug (reported 2010-07 on local-time-devel) limiting number of digits following decimal point (%SPLIT-TIMESTRING doesn't handle extra digits...)
(defun rfc3339-date-time-to-universal-KLUDGE (timestring)
  "TIMESTRING must be a RFC3339 'date-time' string (ISO8601) of the
form YYYY-MM-DDTHH:MM:SS.FFFFFFFF... Sub-second precision is ignored."
  (local-time:timestamp-to-universal
   (local-time:parse-timestring (string-right-trim "0123456789" timestring))))

(defun rfc3339-date-time-to-universal-BROKEN (date-time)
    "See RFC 3339 section 5.6."
    (local-time:timestamp-to-universal
     (local-time:parse-timestring date-time
				  ;; :start start
				  ;; :end end
				  ;; :fail-on-error fail-on-error
				  ;; :time-separator time-separator
				  ;; :date-separator date-separator
				  ;; :date-time-separator date-time-separator
				  ;; :allow-missing-elements allow-missing-elements
				  ;; :allow-missing-date-part allow-missing-date-part
				  ;; :allow-missing-time-part allow-missing-time-part
				  ;; :allow-missing-timezone-part allow-missing-timezone-part
				  ;; :offset offset
				  )))
