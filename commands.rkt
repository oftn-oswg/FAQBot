#lang racket
;; This module is the commands used for the FAQBot itself
;; These are intended to be used in callback functions
;; for the irclib module (i.e. in response to private messages
;; or whatever)
(require "database.rkt")
(require "config.rkt")
;; Command Parsing

(define (string-head str)
  (match (string-length str)
    [(? ((curry <) 0)) (substring str 0 1)]
    [_ #f]))

(define (string-tail str)
  (match (string-length str)
      [(? ((curry eq?) 0)) 'nil]
      [len (substring str 1 len)]))

(define (cmd? cmdchar cmd)
  (match (string-head cmd)
    [#f #f]
    [content (string=? cmdchar content)]))

(define split-whitespace
  ((curry regexp-split) #rx" "))

(define (cmdparse command-char)
  (compose
   (lambda (command)
    (match (cmd? command-char (car command))
    [#f 'nil]
    [#t (list (string-tail (car command))
                            (cdr command))]))
   split-whitespace))

(define parse-at
  (cmdparse "?"))

;; Command Dispatcher
(define (dispatch nick command)
  (match (allowed?)
    [#f 'nil]
    [_ (match (parse-at command) ; if we are allowed, proceed
    ['nil 'nil]
    [(list-rest "set" args) (match (car args)
                              ; this part checks if the right number of arguments are there
                              [(list-rest name text) 
                               (submit-question name (string-join text " "))]
                              ; if for some reason this did not work, return nil
                              [_ 'nil])]
    [(list-rest "alias" (list (list new old))) (make-alias new old)]
    [(list-rest name _) (get-question name)])]))

(provide (all-defined-out))