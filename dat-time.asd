(defsystem dat-time
  :description "Interface to underlying flavor(s) of the day for date/time manipulations"
  :serial t
  :version "0.0.1"
  :components ((:module "src"
			:components ((:file "packages")
				     (:file "days")
				     (:file "iso8601")
				     (:file "parse")
				     (:file "shared-datetime")
				     (:file "universal")))) 
  :depends-on (:local-time
	       :net-telent-date))
