#lang racket
(require 2htdp/batch-io)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define area '())
(define (lookup key list block-level (rep #f))
  (define result (assoc key
                        (filter (lambda (e)
                                  (or (<= (third e) block-level) rep)) list)))
  (cond
    [(not result) (displayln (string-append "Cannot locate variable for " (symbol->string key) ", from block level: " (number->string block-level)))]
    [(eq? (second result) key) (lookup key (cdr list) block-level #t)]
    [else result]))

(define (convert-bool bool)
  (if bool (exec '(true)) (exec '(false))))

(define (parenify list (level 0))
  (cond
    [(null? list) '()]
    [(= level 0)
     (cons (car list) (parenify (cdr list) (+ level 1)))]
    [else `(,(car list) ,(parenify (cdr list)))]))

(define (slist->string slst)
  (string-join (map symbol->string slst) " "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (exec code [block-level 0])
  (match code
                [(list 'begin thing ... 'end)   (car (cdr (map exec thing)))]
                [`(λ ,arg ,body)    (list `(,arg) `,body 'func)]
                [`(fn ,args ,body)  (list `,args `,body 'func)]
                [`(l ,lst) (exec (parenify (add-between (flatten (cons lst 'empty-list)) ':)) block-level)]
                
                [`(let ,assocs ,body) (for ([i assocs])
                                        (set! area (cons (list i (+ block-level 1)) area)))
                                      (exec body (+ block-level 1))]
                
                [`(,thing ,args) (define fn (exec thing block-level))
                                 (for ([v args]
                                       [k (car fn)])
                                   (set! area (cons (list k v (+ block-level 1)) area)))
                                 (last (map (lambda (e)
                                              (exec e (+ block-level 1))) (second fn)))]
                
                [`(,a + ,b)            (+ (exec a) (exec b))]
                [`(,a - ,b)            (- (exec a) (exec b))]
                [`(,a * ,b)            (* (exec a) (exec b))]
                [`(,a / ,b)            (/ (exec a) (exec b))]
                [`(,a == ,b)           (convert-bool (equal? (exec a) (exec b)))]
                [`(,a : ,b)            (exec `(cons (,a ,b)))]
                
                [`(,a := ,b)            (set! area (cons `(,a ,b ,block-level) area))
                                        (exec `,a block-level)]
                [`(,a ↦ ,b)            (set! area (cons `(,a ,b ,block-level) area))
                                       (exec `,a block-level)]
                
                [`undefined               'undefined]
                [(? integer?)          code]
                [(? string?)           code]
                [(? symbol?)           (exec (second (lookup code area block-level)) block-level)]
                [`(,thing)             (exec thing block-level)]
                ['()                   (exec '(undefined))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (exec-repl)
  (displayln (exec (read (open-input-string (string-append "(" (read-line) ")")))))
  (exec-repl))

(define (exec-file file-name)
  (map exec (file->list file-name)))

(exec-file "lib.alz")
(exec-repl)