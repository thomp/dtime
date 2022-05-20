# DAT-TIME

DAT-TIME serves as a relatively stable[^1] set of functions for parsing and generating date and time representations.

[^1]: The intent is to reduce maintenance overhead. For example, LOCAL-TIME has changed its API significantly between versions.

Functionality provided:

- generation of select ISO 8601 date/time strings
- parsing select ISO 8601 date/time strings


The current implementation largely relies on the LOCAL-TIME library (http://common-lisp.net/project/local-time/). LOCAL-TIME is advertised as ISO8601 but warnings suggest RFC3339 is authority.


## ISO 8601 date/time string generation

    ;; coordinated universal time
	> (dtime:iso8601-time-string-Z)
	"2022-05-19T05:38:32.513618Z"


	> (dtime:iso8601-time-string :hr 11 :day 20 :mn 31 :yr 2001)
	"2001-31-20T11:35:30.308833"


	> (dtime:iso8601-time-string)
	"2012-02-28T11:37:06.404897"


## Parsing

    > (dtime::rfc3339-date-time-to-universal "2022-05-20T15:33:39.023424Z")
    3862049619
