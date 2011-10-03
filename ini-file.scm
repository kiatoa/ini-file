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
  (import (except scheme newline)
          (only ports with-input-from-string with-output-to-string)
          (only chicken void case-lambda make-parameter parameterize
                        make-composite-condition make-property-condition
                        error signal handle-exceptions))

  ;; Default section name, under which to put unlabeled properties when reading.
  (define default-section (make-parameter 'default))

  ;; Property name/value separator to use when writing.
  (define property-separator (make-parameter #\=))

  ;; Is the empty string is a valid value?
  (define allow-empty-values? (make-parameter #f))

  ;; Are single-term properties allowed?
  (define allow-bare-properties? (make-parameter #t))

  ;; Simple string trim, as in srfi-13.
  (define (string-trim-right str)
    (if (zero? (string-length str))
      str
      (let loop ((lst (reverse (string->list str))))
        (if (char-whitespace? (car lst))
          (loop (cdr lst))
          (list->string (reverse lst))))))

  ;; Signal a parser error.
  (define (ini-error loc msg . args)
    (signal (make-composite-condition
              (make-property-condition 'ini)
              (make-property-condition 'exn
                                       'location  loc
                                       'message   msg
                                       'arguments args))))

  ;; Read a string characterwise until the given predicate is satisfied.
  ;; Optional port argument defaults to current-input-port.
  ;; Optional failure procedure forces an error if satisfied.
  ;; It is an error if #!eof is reached.
  (define read-until
    (case-lambda
      ((pred?) (read-until pred? (current-input-port)))
      ((pred? port) (read-until pred? port (lambda args #f)))
      ((pred? port fail?)
       (with-output-to-string
         (lambda ()
           (let loop ()
             (let ((ch (peek-char port)))
               (cond ((pred? ch))
                     ((fail? ch)       (error 'read-until "Failure condition met"))
                     ((eof-object? ch) (error 'read-until "Premature end of file"))
                     (else             (write-char (read-char port))
                                       (loop))))))))))

  ;; Character matching.
  (define-syntax match
    (syntax-rules (not)
      ((_ (not <p> ...))      (lambda (a) (and (not (<p> a)) ...)))
      ((_ <p>)                (lambda (a) (<p> a)))
      ((_ <p1> <p2> <pn> ...) (lambda (a) (or ((match <p1>) a)
                                              ((match <p2>) a)
                                              ((match <pn>) a) ...)))))

  (define (char . chars)
    (lambda (c) (memq c chars)))

  (define eof         eof-object?)
  (define newline     (char #\newline))
  (define comment     (char #\# #\;))
  (define separator   (char #\= #\:))
  (define whitespace  (match eof char-whitespace?))
  (define terminal    (match eof newline comment))

  ;; Discard comments and whitespace from the port.
  (define (move-to-next-token port)
    (let ((ch (peek-char port)))
      (cond ((eof ch)        (void))
            ((whitespace ch) (read-char port)
                             (move-to-next-token port))
            ((comment ch)    (read-until (match eof newline) port)
                             (move-to-next-token port)))))

  ;; Read a single INI directive from the port.
  ;; Returns either a symbol (if a section header) or a pair (if a property).
  (define (read-directive port)
    (if (equal? #\[ (peek-char port))
      (read-section-header port)
      (read-property port)))

  ;; Read a property name as a symbol.
  (define (read-property-name port)
    (let* ((str (read-until (match separator terminal) port))
           (key (string-trim-right str)))
      (if (> (string-length key) 0)
        (string->symbol key)
        (ini-error 'read-ini
                   "Property name expected"
                   (read-until terminal port)))))

  ;; Read a bracket-enclosed section header as a symbol.
  (define (read-section-header port)
    (string->symbol
      (with-output-to-string
        (lambda ()
          (handle-exceptions exn
            (ini-error 'read-ini "Malformed section header")
            (read-char port)
            (display (read-until (char #\]) port terminal))
            (read-char port))))))

  ;; Read a single property from the port.
  ;; Returns a name/value pair.
  (define (read-property port)
    (let ((key (read-property-name port))
          (sep (read-char port)))
      (cond ((terminal sep)
             ;; If allowed, single-term
             ;; properties are taken to be #t.
             (if (allow-bare-properties?)
               (cons key #t)
               (ini-error 'read-ini "Malformed property" key)))
            ((separator sep)
             (handle-exceptions exn
               ;; If not allowed, an empty
               ;; property value will signal an error.
               (ini-error 'read-ini "No value given for property" key)
               (if (allow-empty-values?)
                 (read-until (match terminal (not whitespace)) port)
                 (read-until (match (not terminal whitespace)) port terminal)))
             (let ((val (string-trim-right (read-until terminal port))))
               (cons key (or (string->number val) val)))))))

  ;; cons a new section or property onto the configuration alist.
  (define (cons-directive dir alist)
    (cond ((symbol? dir) (cons (list dir) alist))
          ((pair? dir) (if (null? alist)
                         (cons-directive dir `((,(default-section))))
                         (cons (cons (caar alist)
                                     (cons dir (cdar alist)))
                               (cdr alist))))))

  ;; Read an INI configuration file as an alist of alists.
  ;; If input is a port, it is not closed.
  (define read-ini
    (case-lambda
      (() (read-ini (current-input-port)))
      ((in)
       (cond ((string? in)
              (call-with-input-file in read-ini))
             ((input-port? in)
              (let loop ((alist `()))
                (move-to-next-token in)
                (if (eof-object? (peek-char in))
                  alist
                  (loop (cons-directive (read-directive in) alist)))))
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
                         (if (symbol? (car lst))
                           (begin (for-each display
                                            (list #\[ (car lst) #\]
                                                  #\newline))
                                  (loop (cdr lst))
                                  (display #\newline))
                           (for-each loop (reverse lst))))
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
