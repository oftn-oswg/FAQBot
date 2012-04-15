#lang racket
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
  (match (parse-exclamation command)
    [(list-rest "set" args) (submit-question (caar args) (string-join (cdar args) " "))]
    [(list-rest "alias" args) (make-alias (cadar args) (caar args))]
    [(list-rest name _) (get-question name)]))

