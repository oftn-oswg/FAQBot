#lang racket
(require "commands.rkt")
(require "config.rkt")

;; CALLBACK FUNCTIONS
(define (privmsg-handler send userinfo content join part quit)
  (match (parse-at content)
   ['nil 'nil]
    
   ; joining a channel
   [(list-rest "join" args) 
    (match (admin-info (first userinfo)) ; check to see if we have privileges, 0 = highest privs
    [0 (join (car (first args)))]
    [_ 'nil])]
    
   [(list-rest "quit" (list message))
    (match (admin-info (first userinfo))
    [0 (match message
           [(list-rest quit-msg _)
            (quit quit-msg)]
         [_ 'nil])]
    [_ 'nil])]
    
   [result 
    (let ([result (dispatch userinfo content)])
      (display result)
      (match result
        ['nil 'nil]
        [(? hash?) (send "OK")]
        [_ (send (format "~a" result))]))]))

;; Callback for join messages
(define (join-handler privmsg userinfo)
  userinfo)
;; Callback for quit messages
(define (quit-handler userinfo)
  userinfo)

(provide (all-defined-out))