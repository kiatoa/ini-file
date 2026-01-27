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
;; See http://wiki.call-cc.org/egg/ini-file for documentation.

(module ini-file
  (read-ini write-ini read-property
   default-section property-separator property-separator-patt
   property-value-map allow-empty-values? allow-bare-properties?)
  (import scheme chicken extras ports regex)
  (require-library regex)

;; Default section name, under which to put unlabeled properties when reading.
(define default-section (make-parameter 'default))

;; Property name/value separator to use when writing.
(define property-separator (make-parameter #\=))

;; Property name/value separator pattern to use when reading
(define property-separator-patt (make-parameter " *[:=] *"))

;; Is the empty string is a valid value?
(define allow-empty-values? (make-parameter #f))

;; Are single-term properties allowed?
(define allow-bare-properties? (make-parameter #f))

;; Special-case value mappings (for booleans, etc.).
(define property-value-map
  (make-parameter
    '(("true"  . #t)
      ("false" . #f))))

;; Swap the value map for reading/writing.
(define (invert alist)
  (map cons (map cdr alist) (map car alist)))

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


;; system and other macros  produce lines to be used.  These do not come
;; from the input port.  the stuffed-lines parameter keeps a list
;; of lines that have been stuffed in to preempt the input port when
;; getting the next line of input
(define stuffed-lines (make-parameter (list)))


;; Discard comments and
;; whitespace from the string.
(define (chomp-str line)
  (string-substitute*
   line
   '( ("[;#].*" . "") ("\\s+$" . "") ) ))

;; process line, search for macros, then hand off line
;; if macro generates new lines, add them to stuffed-lines parameter
(define (preprocess-line rawline)
  (let ((line (chomp-str rawline)))
    (match-string
     line
     ;; include)
     (("\\s*\\[\\s*include\\s+([^\\]]+?)\\s*\\]" include-file)
      (let* ((all-lines (read-lines include-file))
             (first-line (if (null? all-lines) "" (car all-lines)))
             (rest-lines (if (null? all-lines) '() (cdr all-lines)))
             (prev-stuffed-lines (stuffed-lines))
             )
        (stuffed-lines (append rest-lines prev-stuffed-lines))
        (preprocess-line first-line)))

     ;; no macros; pass it along unmolested
     (else line))))
  
  
  ;; get next line and preprocess it;
  ;;  1) check stuffed-lines parameter to preempt input port
  ;;  2) run preprocessor to handle new macros
(define (read-and-preprocess-line port)
  (if (null? (stuffed-lines))
      (preprocess-line (read-line port))
      (let* ((temp-lines (stuffed-lines))
             (next-stuffed-line (car temp-lines))
             (rest-stuffed-lines (cdr temp-lines)))
        (stuffed-lines rest-stuffed-lines)
        (preprocess-line next-stuffed-line))))



;; Read a single property from the port.
;; If it's a section header, returns a symbol.
;; If it's a name/value pair, returns a pair.
;; If it's a blank line, returns #f
(define read-property
  (case-lambda
    (() (read-property (current-input-port)))
    ((port)
     (let ((line
            (read-and-preprocess-line port))
	   (name-value-patt
            (string-append
             "([^:;=#]+?)"
             (property-separator-patt)
             "(.*?) *")))
       (match-string
        line

        ;; Empty string. 
        (("") #f)
        ;; Section header.
        ((" *\\[(.*?)\\] *([;#].*)?" section comment)
         (string->symbol section))
        ;; Name/value pair.
        ((name-value-patt name value)
         (let ((name (string->symbol name)))
           (let lp ((value value))
             (match-string
              value
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
                 (let ((mapped (assoc value (property-value-map))))
                   (if mapped
                       (cons name (cdr mapped))
                       (cons name value))))))))))
        ;; Unrecognized.
        (else
         (if (allow-bare-properties?)
             (cons (string->symbol line) #t)
             (ini-error
              'read-ini
              "Malformed INI directive"
              line))))))))

;; cons a new section or property onto the configuration alist.
(define (cons-property p alist)
  (cond ((not p) alist)
        ((symbol? p)
         (cons (list p) alist))
        ((pair? p)
         (if (null? alist)
           (cons-property p `((,(default-section))))
           (cons (cons (caar alist)
                       (cons p (cdar alist)))
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
              (if
               (and
                (null? (stuffed-lines))
                (eof-object? (peek-char in)))
               alist
               (lp (cons-property
                    (read-property in)
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
              (let ((vmap (invert (property-value-map))))
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
                                         (let ((mapped (assoc (cdr lst) vmap)))
                                           (if mapped
                                             (cdr mapped)
                                             (cdr lst)))
                                         #\newline)))
                        (else (ini-error 'write-ini
                                         "Malformed INI property list"
                                         lst)))))))
             (else (error 'write-ini
                          "Argument is neither a file nor output port"
                          out)))))))
