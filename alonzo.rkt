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

(define (balanced? lst)
  (letrec
      ([count-open (lambda (l n)
                     (cond
                       [(empty? l) n]
                       [(equal? (car l) #\() (count-open (cdr l) (add1 n))]
                       [(equal? (car l) #\{) (count-open (cdr l) (add1 n))]
                       [(equal? (car l) #\[) (count-open (cdr l) (add1 n))]
                       [else (count-open (cdr l) n)]))]
       [count-close (lambda (l n)
                      (cond
                        [(empty? l) n]
                        [(equal? (car l) #\)) (count-close (cdr l) (add1 n))]
                        [(equal? (car l) #\}) (count-close (cdr l) (add1 n))]
                        [(equal? (car l) #\]) (count-close (cdr l) (add1 n))]
                        [else (count-close (cdr l) n)]))])
    (= (count-open (string->list lst) 0) (count-close (string->list lst) 0))))

(define (read-full-lines file-name)
  (define lines-lst '())
  (define build-up "")
  (define building? #f)
  (for ([line (read-lines file-name)])
    (cond
      [(balanced? line) (if building?
                            (set! build-up (string-append build-up line))
                            ; else
                            (set! lines-lst (append `(,line) lines-lst)))]
      [(not (balanced? line)) (set! build-up (string-append build-up line))
                              (set! building? #t)])
    (if (balanced? build-up)
        (begin
          (set! lines-lst (append `(,build-up) lines-lst))
          (set! build-up "")
          (set! building? #f))
        '()))
  (cond
    [(balanced? build-up) (set! lines-lst (append `(,build-up) lines-lst))
                          (set! build-up "")]) ; one last time, for that last multiline statement.
  lines-lst)

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
  (define res (match code
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
                                              (display e)
                                              (displayln (string-append " at " (number->string (+ block-level 1))))
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
  res)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (exec-repl)
  (displayln (exec (read (open-input-string (string-append "(" (read-line) ")")))))
  (exec-repl))

(define (exec-file file-name)
  (define file (read-full-lines file-name))
  (for ([line file])
    (exec (read (open-input-string (string-append "(" line ")"))))))

(exec-file "lib.alz")
(exec-repl)