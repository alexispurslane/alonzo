#lang racket
(require 2htdp/batch-io)

(define area '())
(define (exec code [in-block #f])
  (define res (match code
                [`(λ ,arg ,body)    (list `(,arg) `,body 'func)]
                [`(fn ,args ,body)  (list `,args `,body 'func)]
                
                [`(let ,assocs ,body) (for ([i assocs])
                                        (set! area (cons (list i #t) area)))
                                      (exec body #t)]
                
                [`(,thing ,args) (define fn (exec thing))
                                 (for ([v args]
                                       [k (car fn)])
                                   (set! area (cons (list k v #t) area)))
                                 (exec (car (cdr fn)) #t)]
                
                [`(,a + ,b)            (+ (exec a) (exec b))]
                [`(,a - ,b)            (- (exec a) (exec b))]
                [`(,a * ,b)            (* (exec a) (exec b))]
                [`(,a / ,b)            (/ (exec a) (exec b))]
                
                [`(,a := ,b)            (set! area (cons `(,a ,b ,in-block) area)) ]
                
                [(? integer?)          code]
                ['true                 #t]
                ['false                #f]
                [(? string?)           code]
                [(? symbol?)           (exec (car (cdr (assoc code area))))]
                [`(,thing)             (exec thing)]
                ['()                   #f]))
  
  (define no-clojure (match res
                       [`(,args ,body func) #f]
                       [_ #t]))
  
  (if (and in-block no-clojure)
      (set! area (filter (lambda (e)
                           (eq? (caddr e) #f)) area))
      #f)
  res)

(define (exec-repl)
  (displayln (exec (read (open-input-string (string-append "(" (read-line) ")")))))
  (exec-repl))

(define (exec-file file-name)
  (define file (read-lines file-name))
  (for ([line file])
    (exec (read (open-input-string (string-append "(" line ")"))))))