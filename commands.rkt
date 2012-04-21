#lang racket
;; This module is the commands used for the FAQBot itself
;; These are intended to be used in callback functions
;; for the irclib module (i.e. in response to private messages
;; or whatever)
(require "database.rkt")
(require "config.rkt")
;; Command Parsing

(define (string-head str)
  (substring str 0 1))

(define (string-tail str)
  (substring str 1 (string-length str)))

(define (cmd? cmdchar cmd)
  (string=? cmdchar (string-head cmd)))

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

(define parse-exclamation
  (cmdparse "!"))

(define parse-at
  (cmdparse "@"))

;; Command Dispatcher
(define (dispatch nick command)
  (match (allowed?)
    [#f "Please try again in a few minutes"]
    [_ (match (parse-exclamation command)
    [(list-rest "set" args) (match args
                              ; this part checks if the right number of arguments are there
                              [(list (list name) (list-rest _ text)) 
                               (submit-question name (string-join text " "))
                               "Done!"]
                              [_ 'nil])]
    [(list-rest "alias" (list args)) (make-alias (first args) (second args))]
    [(list-rest name _) (get-question name)]
         [_ 'nil])]))

(provide (all-defined-out))