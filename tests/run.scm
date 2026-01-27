(use srfi-78 ports extras)

; test using ini-file in workspace
(include "../ini-file.scm")

(import ini-file)

(define default (default-section))

(define (input str) (with-input-from-string str read-ini))
(define (output alist) (with-output-to-string (lambda () (write-ini alist))))

(define-syntax fails?
  (syntax-rules ()
    ((_ <e>) (condition-case <e> ((exn ini) #t)))))

;; permissive defaults
(allow-empty-values? #t)
(allow-bare-properties? #t)

;; valid input
(check (input "")                   => '())
(check (input "; comment")          => '())
(check (input "[section]")          => '((section)))
(check (input "[sec tion]")         => '((|sec tion|)))
(check (input "one")                => `((,default (one . #t))))
(check (input "one=     ")          => `((,default (one . ""))))
(check (input "one=# two")          => `((,default (one . ""))))
(check (input "one=2")              => `((,default (one . 2))))
(check (input "one=-2")             => `((,default (one . -2))))
(check (input "one=0.2")            => `((,default (one . 0.2))))
(check (input "one=two")            => `((,default (one . "two"))))
(check (input "one=  two  ")        => `((,default (one . "two"))))
(check (input "one two = three ")   => `((,default (|one two| . "three"))))
(check (input "one=\" two \"")      => `((,default (one . " two "))))
(check (input "one=\"\"two\"\"")    => `((,default (one . "\"two\""))))
(check (input "1  =  two  3  ")     => `((,default (|1| . "two  3"))))
(check (input "one = two
               [section]
               three = four")       => `((section  (three . "four"))
                                         (,default (one . "two"))))

;; malformed input
(allow-empty-values? #f)
(allow-bare-properties? #f)
(check (fails? (input "one"))       => #t)
(check (fails? (input "one="))      => #t)
(check (fails? (input "one=#"))     => #t)
(check (fails? (input "=two"))      => #t)
(check (fails? (input "one=\ntwo")) => #t)
(check (fails? (input "one=\"two\nthree\"")) => #t)

;; value mapping
(default-section 'ini)
(check (input "one = true")         => '((ini (one . #t))))
(check (input "one = false")        => '((ini (one . #f))))
(check (output '(one . #t))         => "one=true\n")
(check (output '(one . #f))         => "one=false\n")

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
  => `((database (file . "payroll.dat")
                 (port . 143)
                 (server . "192.0.2.62"))
       (owner    (organization . "Acme Widgets Inc.")
                 (name . "John Doe"))))

;; roundtrip
(check (input (output (read-ini "example.ini"))) => (read-ini "example.ini"))

(if (not (check-passed? 36))
  (begin (check-report)
         (error 'ini-file "Failed to pass test suite")))
