(use srfi-78 ports extras test yaml)

; test using ini-file in workspace
(include "../ini-file.scm")

(import ini-file)

(let ((data (read-ini "example2.ini")))
  (print data))

