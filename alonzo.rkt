#lang racket
(require 2htdp/batch-io)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define area '())
(define (lookup key list)
  (define result (assoc key list))
  (cond
    [(not result) (displayln (string-append "Cannot locate variable for " (symbol->string key)))]
    [(eq? (car (cdr result)) key) (lookup key (cdr list))]
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (exec code [in-block #f])
  (define res (match code
                [(list 'begin thing ... 'end)   (car (cdr (map exec thing)))]
                [`(λ ,arg ,body)    (list `(,arg) `,body 'func)]
                [`(fn ,args ,body)  (list `,args `,body 'func)]
                
                [`(let ,assocs ,body) (for ([i assocs])
                                        (set! area (cons (list i #t) area)))
                                      (exec body #t)]
                
                [`(,thing ,args) (define fn (exec thing))
                                 (for ([v args]
                                       [k (car fn)])
                                   (set! area (cons (list k v #t) area)))
                                 (map (lambda (e) (exec e #t)) (car (cdr fn)))]
                
                [`(,a + ,b)            (+ (exec a) (exec b))]
                [`(,a - ,b)            (- (exec a) (exec b))]
                [`(,a * ,b)            (* (exec a) (exec b))]
                [`(,a / ,b)            (/ (exec a) (exec b))]
                [`(,a == ,b)           (convert-bool (equal? (exec a) (exec b)))]
                [`(out ,name ,stuff)   (eval `(apply ,name '(,(exec stuff))) ns) (exec '(undefined))]
                
                [`(,a := ,b)            (set! area (cons `(,a ,b ,in-block) area))
                                        (exec `,a)]
                [`(,a ↦ ,b)            (set! area (cons `(,a ,b ,in-block) area))
                                        (exec `,a)]
                
                [`undefined               'e-undef]
                [(? integer?)          code]
                [(? string?)           code]
                [(? symbol?)           (exec (car (cdr (lookup code area))))]
                [`(,thing)             (exec thing)]
                ['()                   (exec '(undefined))]))
  
  (define no-clojure (match res
                       [`(,args ,body func) #f]
                       [_ #t]))
  
  (if (and in-block no-clojure)
      (set! area (filter (lambda (e)
                           (displayln e)
                           (eq? (caddr e) #f)) area))
      #f)
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