#!  /usr/bin/racket
#lang racket
(require "irclib.rkt")
(require "config.rkt")
(require "callbacks.rkt")
(require "services.rkt")
(require "output.rkt")

;; Base input stuff
;; raw input getter
(define (get-input port)
  (let ([result (read-line port 'return)])
       (match result
         [(? eof-object?) "done"]
         [_ result])))

;; Setting the callbacks
(define parse-input (register-callbacks               
                     privmsg-handler
                     quit-handler
                     join-handler))

;; The actual setup of the connection
;; Initial startup commands
(define (initial-work in out)
  (let* ([nickname irc-username]
         [username irc-username]
         [realname irc-username])
  (display (format "NICK ~a\r\n" nickname) out)
  (display (format "USER ~a 8 * ~a\r\n" username realname) out))
  (flush-output out))

(make-handler handle-input
              (out qs input-string)
              (match (parse-input qs input-string)
                     ['nil 'nil]
                     [result ((put-raw-output result out))]))

;; The main loop that the bot runs in
(define (ircloop in out)
  (letrec ([inner-loop 
  (λ (query-service)
    (match in
    ['nil "Exited"]
      [_ (match (get-input in)
           ["done" 
            (query-service 'kill)
            "Exited"]
           [result
            ;(displayln result)
            (handle-input out query-service result)
            (inner-loop query-service)])]))])
    (initial-work in out)
    (displayln "done initial work\n")
    (inner-loop (make-querier out services))))

;; The connector, connects to a network and then passes control to the main loop
(define (connect hostname port)
  (let-values ([(input output) (tcp-connect hostname port)])
    (file-stream-buffer-mode output 'line)
    (values input output)))

(call-with-values (λ () (connect "irc.freenode.org" 6667))
                  ircloop)
