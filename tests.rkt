#lang racket/base

(require rackunit
         "alonzo.rkt")
(exec-file "lib.alz")

(check-equal? (exec '(1 + 1)) 2 "Simple addition")
(check-equal? (exec '(1 - 1)) 0 "Simple subtraction")
(check-equal? (exec '(1 * 1)) 1 "Simple multiplication")
(check-equal? (exec '(1 / 1)) 1 "Simple division")

(check-equal? (exec '(1 == 1)) '((x y) ((x)) func) "Simple equality")
(check-equal? (exec '(1 >= 1)) '((x y) ((x)) func) "Simple meq comparison")
(check-equal? (exec '(1 <= 1)) '((x y) ((x)) func) "Simple leq comparison")
(check-equal? (exec '(1 < 1)) '((x y) ((y)) func) "Simple l comparison")
(check-equal? (exec '(1 > 1)) '((x y) ((y)) func) "Simple m comparison")


(check-equal? (exec '( if(true "yay" "no") )) "yay" "Simple t if")
(check-equal? (exec '( if(false "no" "yay") )) "yay" "Simple f if")
(check-equal? (exec '( if((1 == 1) "yay" "no") )) "yay" "Simple comparison if")
(check-equal? (exec '( if((1 == 1) (begin (1 + 1) (2 + 2) end) "no") )) 4 "Simple comparison-multiline if")
(check-equal? (exec '( var := (1 == 1) )) '((x y) ((x)) func) "Simple var assignment")
(check-equal? (exec '( if(var 1 2) )) 1 "Simple var based if")

(check-equal? (exec '( (fn (x y) { x }) )) '((x y) (x) func) "Simple function literal")
(check-equal? (exec '( (λ x { x }) )) '((x) (x) func) "Simple lambda literal")