#lang racket
(require 2htdp/batch-io)
(provide (contract-out
          [split ((compose not null?) list? . -> . list?)]
          [exec-repl (-> null?)]
          [exec ((compose not null?) . -> . (compose not null?))]
          [exec-file (string? . -> . list?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; ;;;;;;;;;; ;;;;;;;;;; ;;;;;;;;;; ;;      ;; ;;;;;;;;;;
                                        ; ;;         ;;             ;;     ;;      ;; ;;      ;;
                                        ; ;;;;;;;;;; ;;;;;;;;       ;;     ;;      ;; ;;;;;;;;;;
                                        ;         ;; ;;             ;;     ;;      ;; ;;
                                        ; ;;;;;;;;;; ;;;;;;;;;;     ;;     ;;;;;;;;;; ;;         "Get ready for THIS!"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                                        ; Set up namespaces and global variables for language. TODO: Make *area* non-mutable.
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define *area* '())
(define separator '..)

                                        ; Custom assoc-type function, with all unbound identifiers being symbols, and allowing for scoping.
(define (lookup key list block-level)
  (define new-list (filter (compose (curry >= block-level) third) list))
  (define result (assoc key
                        new-list))
  (cond
   [(false? result) #f]
   [(and (list? (second result)) (equal? (first (second result)) 'ref))
    (lookup (second (second result)) (rest list) block-level)]
   [else result]))

                                        ; This allows me to interface boolean operations from racket into alonzo
(define (convert-bool bool)
  (if bool
      (exec '(true))
      (exec '(false))))

                                        ; Allows for constructing recursive calls from flat calls. Saves parenthisis.
(define (parenify list (level 0))
  (cond
   [(null? list) '()]
   [(= level 0)
    (cons (first list) (parenify (rest list) (+ level 1)))]
   [else `(,(first list) ,(parenify (rest list)))]))

                                        ; Turns a list of symbols into a string
(define (slist->string slst)
  (string-join (map symbol->string slst) " "))

(define (index-of l x)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

                                        ; Split a list by a certain recurring element.
(define (split a loa)
  (cond
   [(null? loa) '()]
   [else (cons (take loa (or (index-of loa a) (length loa)))
               (split a (drop loa (if (index-of loa a)
                                      (add1 (index-of loa a))
                                      (length loa)))))]))
                                        ; A untility funciton to copy lists.
(define (full-copy list)
  (if (null? list)
      '()
      (if (list? list)
          (cons (full-copy (car list)) (full-copy (cdr list)))
          list)))

                                        ; Repeat an element n times.
(define (repeat el n)
  (cond
   [(zero? n) '()]
   [else (cons el (repeat el (sub1 n)))]))

(define (zip . lists) (apply map list lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; ;;;;;;;;;;     ;;        ;;  ;;;;;;;;;;     ;;;;;;;;;;
                                        ; ;;               ;;    ;;    ;;             ;;
                                        ; ;;;;;;;;           ;;;;      ;;;;;;;;       ;;
                                        ; ;;               ;;    ;;    ;;             ;;
                                        ; ;;;;;;;;;;     ;;        ;;  ;;;;;;;;;;     ;;;;;;;;;;  "The heart of the alonzo language"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (exec code [block-level 0] [area *area*])
  (if (and (list? code) (not (equal? (memq separator code) #f)))
      (last (map (λ (e) (exec e block-level)) (split separator code)))
      (match code
        [`(using ,file)        (exec-file (cond
                                           [(symbol? file) (symbol->string file)]
                                           [else file]))]
        [`(λ ,arg ,body)       (list `(,arg) `,(split separator body) (full-copy area))]
        [`(\\ ,arg ,body)      (list `(,arg) `,(split separator body) (full-copy area))]
        [`(fn ,args ,body)     (list `,args `,(split separator  body) (full-copy area))]
        [`(l ,lst)             (exec (parenify (add-between (flatten (cons lst 'empty-list)) ':)) block-level)]

        [`(,thing ,args)       (define fn (exec thing block-level area))
         (define a (third fn))
         (last (map (lambda (e)
                      (exec e (+ block-level 1) (append (zip (first fn)
                                                             (map (lambda (arg)
                                                                    (if (lookup arg a (add1 block-level))
                                                                        `(ref ,arg)
                                                                        arg)) args)
                                                             (repeat (add1 block-level) (length args))) a))) (second fn)))]
        [`(,a + ,b)            (+ (exec a block-level area) (exec b block-level area))]
        [`(,a - ,b)            (- (exec a block-level area) (exec b block-level area))]
        [`(,a * ,b)            (* (exec a block-level area) (exec b block-level area))]
        [`(,a / ,b)            (/ (exec a block-level area) (exec b block-level area))]
        [`(,a == ,b)           (convert-bool (equal? (exec a block-level area) (exec b block-level area)))]
        [`(,a <= ,b)           (convert-bool (<= (exec a block-level area) (exec b block-level area)))]
        [`(,a >= ,b)           (convert-bool (>= (exec a block-level area) (exec b block-level area)))]
        [`(,a > ,b)            (convert-bool (> (exec a block-level area) (exec b block-level area)))]
        [`(,a < ,b)            (convert-bool (< (exec a block-level area) (exec b block-level area)))]
        [`(,a : ,b)            (exec `(cons (,a ,b)) block-level area)]

        [`(,a := ,b)           (set! *area* (cons `(,a ,(if (lookup b area block-level)
                                                            `(ref ,b)
                                                            b) ,block-level) area))
         (exec `,a block-level area)]
        [`(,a ↦ ,b)            (set! *area* (cons `(,a ,(if (lookup b area block-level)
                                                            `(ref ,b)
                                                            b) ,block-level) area))
         (exec `,a block-level area)]
        ['undefined            'undefined]
        ['(undefined)          'undefined]
        [(? integer?)          code]
        [(? string?)           code]
        [(? symbol?)           (let ([look (lookup code area block-level)])
                                 (if look
                                     (exec (second look) block-level area)
                                     code))]
        [`(,thing)             (exec thing block-level area)]
        ['()                   (exec '(undefined))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; ;;;;;;;;;; ;;;;      ;; ;;;;;;;;;; ;;;;;;;;;; ;;;;;;;;;; ;;;;;;;;;; ;;;;;;;;;; ;;;;;;;;;; ;;;;;;;;;;
                                        ;     ;;     ;;  ;;    ;;     ;;     ;;         ;;      ;; ;;         ;;      ;; ;;         ;;
                                        ;     ;;     ;;    ;;  ;;     ;;     ;;;;;;;;   ;;;;;;;;;; ;;;;;;;;   ;;;;;;;;;; ;;         ;;;;;;;;
                                        ;     ;;     ;;      ;;;;     ;;     ;;         ;;;;;      ;;         ;;      ;; ;;         ;;
                                        ; ;;;;;;;;;; ;;        ;;     ;;     ;;;;;;;;;; ;;  ;;;;;; ;;         ;;      ;; ;;;;;;;;;; ;;;;;;;;;;           "How you know this stuff is working."
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exec-repl)
  (display ">>>(Alonzo)>>> ")
  (define res (exec (read (open-input-string (string-append "(" (read-line) ")")))))
  (pretty-print res)
  (and (not (eq? res 'exit)) (exec-repl)))

(define (exec-file file-name)
  (map exec (split separator (file->list file-name))))
