(declare (unit struct-indexer))

;;(use trace)
(use yaml)

;; fix-alist: alist -> alist
;;   * handle malformed alists where data is a list of one item instead of that item
(define (fix-alist alist)
  (map
   (lambda (x)
     (if (and
          (list? (cdr x))
          (= 1 (length (cdr x))))
         (cons (car x) (cadr x))
         x))
   alist))

;; wrapper around alist-ref to choose correct
;;   equation founction and fix-alist
(define (my-alist-ref index alist)
  (let ((eqfunc
          (cond
           ((string? index) equal?)
           ((symbol? index) eq?)
           (else equal?))))

    (try-string-and-symbol
        (lambda (item)
          (alist-ref
           item
           (fix-alist alist)
           eqfunc))
        index)))

(define (my-hash-table-ref ht index)
  (try-string-and-symbol
   (lambda (item)
     (hash-table-ref/default
      ht
      item
      #f))
   index))

;; try a function with one argument using the item argument
;; both string and symbol.
;;   returns whichever result is not #f
;;   original type is checked first.
(define (try-string-and-symbol func item )
  (if (symbol? item)
      (let ((symres (func item)))
        (if symres
            symres
            (func (symbol->string item))))
      
      (let ((strres (func item)))
        (if strres
              strres
              (func (string->symbol item))))))

;; lookup
(define (lu struct key)
  (define (descender key-list-left struct-left)
    (cond
     ((null? key-list-left)
      struct-left)

     ((null? struct-left)
      #f)

     ((not struct-left)
      #f)

     ((type-checks#alist? struct-left)

      (descender
       (cdr key-list-left)
       (my-alist-ref
        (car key-list-left)
        struct-left)))

     ((hash-table? struct-left)
      (descender
       (cdr key-list-left)
       (my-hash-table-ref
        struct-left
        (car key-list-left))))

     (else
      (print "descender: case else")
      #f)))

  (descender (string-split key ".") struct))
