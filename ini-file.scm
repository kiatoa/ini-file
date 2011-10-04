;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ini-file.scm - Read & write INI configuration files.
;;
;; Copyright (c) 2011, Evan Hanson
;; See LICENSE for details
;;
;; This is a simple module for reading & writing INI files. INI
;; is a stupid, fuzzy and almost entirely unspecified file format
;; that exists in a zillion different forms, with about as many
;; features. This module handles a very small subset of those.
;; See http://wikipedia.org/wiki/INI_file for more information.
;;
;; (read-ini [file-or-port])
;;
;;   Reads configuration directives from `file-or-port` until #!eof,
;;   returning an alist of alists corresponding hierarchically to
;;   the source INI's SECTION -> PROPERTY -> VALUE structure.
;;
;;   Numeric values are read as such; everything else is treated
;;   as a string literal. Properties appearing before any section
;;   heading are placed in an alist under the key given by the
;;   `default-section` parameter.
;;
;;   If `file-or-port` is a port, it is not closed.
;;
;; (write-ini alist [file-or-port])
;;
;;   Writes `alist` as INI directives to `file-or-port`.
;;
;;   A symbol at the head of `alist` signifies a section of that name.
;;   The write order of sections and properties is reverse that of `alist`.
;;
;;   The `property-separator` parameter specifies the character or
;;   string with which to separate property names & values.
;;
;;   If `file-or-port` is a port, it is not closed.

(module ini-file
  (read-ini write-ini
   default-section property-separator
   allow-empty-values? allow-bare-properties?)
  (import scheme chicken extras ports regex)
  (require-library regex)

;; Default section name, under which to put unlabeled properties when reading.
(define default-section (make-parameter 'default))

;; Property name/value separator to use when writing.
(define property-separator (make-parameter #\=))

;; Is the empty string is a valid value?
(define allow-empty-values? (make-parameter #f))

;; Are single-term properties allowed?
(define allow-bare-properties? (make-parameter #t))

;; Signal a parsing error.
(define (ini-error loc msg . args)
  (signal (make-composite-condition
            (make-property-condition 'ini)
            (make-property-condition 'exn
                                     'location  loc
                                     'message   msg
                                     'arguments args))))

;; cond-like syntax for
;; regular-expression matching.
(define-syntax match-string
  (syntax-rules (else)
    ((_ str ((pat lst ...) body ...) tail ...)
     (let ((match (string-match (regexp pat) str)))
       (if (not match)
         (match-string str tail ...)
         (apply (lambda (lst ...) body ...)
                (cdr match)))))
    ((_ str (else body ...))
     (begin body ...))
    ((_ str) (void))))

(define (read-directive port)
  (let ((line (read-line port)))
    (match-string line
      ;; Section header.
      ((" *\\[(.*?)\\] *([;#].*)?" section comment)
       (string->symbol section))
      ;; Name/value pair.
      (("([^:;=#]+?) *[:=] *(.*?) *" name value)
       (let ((name (string->symbol name)))
         (let lp ((value value))
           (match-string value
             ;; Quoted string.
             (("\"(.*?)\"" value)
              (cons name value))
             ;; Number.
             (("[-+]?[0-9]+\\.?[0-9]*")
              (cons name (with-input-from-string value read)))
             ;; Trailing comment.
             (("(.*?) *[;#].*" match)
              (lp match))
             (else
              (cond
                ((allow-empty-values?)
                 (cons name value))
                ((zero? (string-length value))
                 (ini-error
                   'read-ini
                   "Empty value"
                   line))
                (else
                 (cons name value))))))))
      ;; Unrecognized.
      (else
       (if (allow-bare-properties?)
         (cons (string->symbol line) #t)
         (ini-error
           'read-ini
           "Malformed INI directive"
           line))))))

;; cons a new section or property onto the configuration alist.
(define (cons-directive dir alist)
  (cond ((symbol? dir)
         (cons (list dir) alist))
        ((pair? dir)
         (if (null? alist)
           (cons-directive dir `((,(default-section))))
           (cons (cons (caar alist)
                       (cons dir (cdar alist)))
                 (cdr alist))))))

;; Discard comments and
;; whitespace from the port.
(define (chomp port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch))
          ((char-whitespace? ch)
           (read-char port)
           (chomp port))
          ((memq ch '(#\# #\;))
           (read-line port)
           (chomp port)))))

;; Read an INI configuration file as an alist of alists.
;; If input is a port, it is not closed.
(define read-ini
  (case-lambda
    (() (read-ini (current-input-port)))
    ((in)
     (cond ((string? in)
            (call-with-input-file in read-ini))
           ((input-port? in)
            (let lp ((alist `()))
              (chomp in)
              (if (eof-object? (peek-char in))
                alist
                (lp (cons-directive
                      (read-directive in)
                      alist)))))
           (else (error 'read-ini
                        "Argument is neither a file nor input port"
                        in))))))

;; Write an alist of alists as an INI configuration file.
;; If output is a port, it is not closed.
(define write-ini
  (case-lambda
    ((alist) (write-ini alist (current-output-port)))
    ((alist out)
     (cond ((string? out)
            (call-with-output-file out
              (lambda (file) (write-ini alist file))))
           ((output-port? out)
            (parameterize ((current-output-port out))
              (let loop ((lst alist))
                (cond ((null? lst) (void))
                      ((list? lst)
                       (if (not (symbol? (car lst)))
                         (for-each loop (reverse lst))
                         (begin (for-each
                                  display
                                  (list #\[ (car lst) #\]
                                        #\newline))
                                (loop (cdr lst))
                                (display #\newline))))
                      ((pair? lst)
                       (for-each display
                                 (list (car lst)
                                       (property-separator)
                                       (cdr lst)
                                       #\newline)))
                      (else (ini-error 'write-ini
                                       "Malformed INI property list"
                                       lst))))))
           (else (error 'write-ini
                        "Argument is neither a file nor output port"
                        out)))))))
