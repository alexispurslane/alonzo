#lang racket/gui
(require 2htdp/batch-io)
(require "alonzo.rkt")
(define (alz-val->string res)
  (match res
    [(? number?) (number->string res)]
    [(? symbol?) (symbol->string res)]
    [else res]))
(define frame (new frame% [label "Alonzo IDE"]
                   [width 800]
                   [height 800]))
(define repl-frame (new frame% [label "Alonzo IDE REPL"]
                        [width 300]
                        [height 90]))
(send repl-frame create-status-line)
(define repl-editor%
  (class editor-canvas%
    (define/override (on-char ke)
      (if (not (eq? #\return (send ke get-key-code)))
          (super on-char ke)
          (send repl-frame set-status-text (alz-val->string (exec (read (open-input-string (string-append "(" (send (send this get-editor) get-flattened-text) ")"))))))))
    (super-new)))

(define editor-canvas (new repl-editor%
                           (parent repl-frame)))
(define text (new text%))
(send editor-canvas set-editor text)
(send text set-max-height 1)

(define loc 1)
(define document-name (symbol->string (gensym "Document_")))
(define modified-message "")

(define custom-editor%
  (class editor-canvas%
    (define/override (on-char ke)
      (super on-char ke)
      (set! modified-message "[modified]")
      (send frame set-status-text (format "~s [~s LOC] ~s" document-name
                                          (length (regexp-split #px"\n" (send (send this get-editor) get-flattened-text))) modified-message)))
    (super-new)))

(define editor (new custom-editor% [parent frame]))
(define edit-text (new text%))
(send edit-text set-file-format 'text)
(send editor set-editor edit-text)
(send frame show #t)

(define mb (new menu-bar% [parent frame]))
(define file-menu (new menu%
                       (label "File")
                       (parent mb)))
(new menu-item%	 
     [label "New"]	 
     [parent file-menu]	 
     [callback (lambda (a b)
                 (set! modified-message "")
                 (set! document-name (symbol->string (gensym "Document ")))
                 (send edit-text erase)
                 (send frame set-status-text (format "~s [~s LOC] ~s" document-name
                                                     (length (regexp-split #px"\n" (send (send editor get-editor) get-flattened-text))) modified-message)))])
(new separator-menu-item% [parent file-menu])
(new menu-item%	 
     [label "Open"]	 
     [parent file-menu]
     [callback (lambda (a b)
                 (set! modified-message "")
                 (set! document-name (path->string (get-file "Please choose file name..."	 	 	 	 
                                                             frame)))
                 (send edit-text erase)
                 (send edit-text insert-file document-name)
                 (send frame set-status-text (format "~s [~s LOC] ~s" document-name
                                                     (length (regexp-split #px"\n" (send (send editor get-editor) get-flattened-text))) modified-message)))])

(new separator-menu-item% [parent file-menu])
(new menu-item%	 
     [label "Save"]	 
     [parent file-menu]	 
     [callback (lambda (a b)
                 (set! modified-message "")
                 (write-file document-name (send (send editor get-editor) get-flattened-text))
                 (send frame set-status-text (format "~s [~s LOC] ~s" document-name
                                                     (length (regexp-split #px"\n" (send (send editor get-editor) get-flattened-text))) modified-message)))])
(new menu-item%	 
     [label "Save As"]	 
     [parent file-menu]	 
     [callback (lambda (a b)
                 (set! modified-message "")
                 (set! document-name (path->string (put-file "Please choose where you want to save this file..."	 	 	 	 
                                                             frame)))
                 (write-file document-name (send (send editor get-editor) get-flattened-text))
                 (send frame set-status-text (format "~s [~s LOC] ~s" document-name
                                                     (length (regexp-split #px"\n" (send (send editor get-editor) get-flattened-text))) modified-message)))])

(new separator-menu-item% [parent file-menu])
(new menu-item%	 
     [label "Run"]	 
     [parent file-menu]
     [callback (lambda (a b)
                 (define res (last (exec-file document-name)))
                 (send frame set-status-text (string-append "Result: " (alz-val->string res)))
                 (sleep 5)
                 (send frame set-status-text (format "~s [~s LOC] ~s" document-name
                                                     (length (regexp-split #px"\n" (send (send editor get-editor) get-flattened-text))) modified-message)))])
(new menu-item%	 
     [label "Open REPL"]	 
     [parent file-menu]	 
     [callback (lambda (a b)
                 (send repl-frame show #t))])
(define edit-menu (new menu%
                       (label "Edit")
                       (parent mb)))
(append-editor-operation-menu-items edit-menu #t)

(send frame create-status-line)
(send frame set-status-text (format "~s [~s LOC]" document-name loc))
