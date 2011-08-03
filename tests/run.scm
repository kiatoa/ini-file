(use srfi-78 ini-file)

(define default (default-section))

(define (input str) (with-input-from-string str read-ini))
(define (output alist) (with-output-to-string (lambda () (write-ini alist))))

(define-syntax fails?
  (syntax-rules ()
    ((_ <e>) (condition-case <e> ((exn ini) #t)))))

;; valid input
(check (input "")                   => '())
(check (input "; comment")          => '())
(check (input "[section]")          => '((section)))
(check (input "[sec tion]")         => '((|sec tion|)))
(check (input "one")                => `((,default (one . #t))))
(check (input "one=2")              => `((,default (one . 2))))
(check (input "one=two")            => `((,default (one . "two"))))
(check (input "one=\"two\"")        => `((,default (one . "\"two\""))))
(check (input "one=  two  ")        => `((,default (one . "two"))))
(check (input "1  =  two  3  ")     => `((,default (|1| . "two  3"))))
(check (input "one = two
               [section]
               three = four")       => `((section  (three . "four"))
                                         (,default (one . "two"))))

;; malformed input
(check (fails? (input "[section"))  => #t)
(check (fails? (input "one="))      => #t)
(check (fails? (input "one=#"))     => #t)
(check (fails? (input "=two"))      => #t)
(check (fails? (input "one=\ntwo")) => #t)

;; valid output
(check (output '())                              => "")
(check (output '(one . 2))                       => "one=2\n")
(check (output '(section))                       => "[section]\n\n")
(check (output '((one . 2) (three . "four")))    => "three=four\none=2\n")
(check (output '(section (one . 2) (three . 4))) => "[section]\nthree=4\none=2\n\n")
(check (output '((section (one . "two"))))       => "[section]\none=two\n\n")
(check (output '(((section (one . "two")))))     => "[section]\none=two\n\n")

;; test file from wikipedia
(check (read-ini "example.ini")
  => `((database (file . "\"payroll.dat\"")
                 (port . 143)
                 (server . "192.0.2.62"))
       (owner    (organization . "Acme Widgets Inc.")
                 (name . "John Doe"))))

;; roundtrip
(check (input (output (read-ini "example.ini"))) => (read-ini "example.ini"))

(if (not (check-passed? 25))
  (begin (check-report)
         (error 'ini-file "Failed to pass test suite")))
