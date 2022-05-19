(in-package :cl-user)

(defpackage :dat-time
  (:use :cl)
  (:documentation
   "A front-end to various date/time-related utilities. The program is
released under the terms of the Lisp Lesser GNU Public License
http://opensource.franz.com/preamble.html, also known as the LLGPL.
Copyright: David A. Thompson, 2009-2022"
   )
  (:nicknames :dtime)
  (:export 
   *day-in-seconds*
   *hour-in-seconds*
   *week-in-seconds*
   *year-in-seconds*
   iso8601-string-p
   iso8601-time-string
   iso8601-time-string-Z
   posix-time-now
   pprint-datetime
   rfc822-to-universal
   seconds-delta
   universal-time-p
   universal-to-iso8601
   year-now))
