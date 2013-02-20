#!  /usr/bin/racket
#lang racket
(require "irclib.rkt")
(require "commands.rkt")
(require "database.rkt")
(require "config.rkt")
(require "callbacks.rkt")

;; frequency of pings
(define *PING* 40)
;; amount of time to wait on a command before failing
(define *TIMEOUT* 20)

;; Timeout combinator, tries to get output and times out if there is none
(define ((timeout-check max-wait) input?)
  (letrec ([matcher 
         (lambda (acc)
           (sleep acc)
           (match (input?)
             ['nil 
              (cond
                     [(> acc max-wait) "done"]
                     [else (matcher (* acc 1.5))])]
             [result
              (display (format "~a\n" result)) ;; display each line being received
              result]))])
    (matcher 0.000015)))
;; create a timeout function
(define check (timeout-check 3))

;; Base input stuff
;; raw input getter
(define ((get-raw-input port))
  (check
   (λ () 
     (let ([result (read-line port 'return)])
       (match result
         [(? eof-object?) "done"]
         [_ result])))))
;; Gets input and parses it
(define (get-input inport)
   (let ([result ((get-raw-input inport))])
     (match result
       ["done" "done"]
       [_ result])))

;; Sends raw text to the irc server
(define ((put-raw-output text port))
  (display (format "trying to send ~a\n" text))
  (display (format "~a\r\n" text) port)
  (flush-output port))

;; Setting the callbacks
(define parse-input (register-callbacks privmsg-handler
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

;; maybe ping back?
(define (ping-server out)
  (sleep *PING*)
  ((put-raw-output 
    (pingpong "ping" (format "~a" (current-seconds)))
    out))
  (ping-server out))

(define (handle-input out input-string)
  (thread (λ () 
         (let ([thd 
                (thread (λ() 
                          (match (parse-input input-string)
                            ['nil 'nil]
                            [result ((put-raw-output result out))])))])
           (sync/timeout *TIMEOUT* (thread-dead-evt thd))
           (kill-thread thd)))))


;; The main loop that the bot runs in
(define (ircloop in out)
  (letrec ([inner-loop 
  (λ ()
    (match in
    ['nil 'nil]
      [_ (match (get-input in)
           ["done" 'nil]
           [result
            (handle-input out result)
            (inner-loop)])]))])
    (initial-work in out)
    (display "done initial work\n")
    (thread (λ () (ping-server out)))
    (inner-loop)))

;; The connector, connects to a network and then passes control to the main loop
(define (connect hostname port)
  (let-values ([(input output) (tcp-connect hostname port)])
    (file-stream-buffer-mode output 'line)
    (values input output)))

(call-with-values (λ () (connect "irc.freenode.org" 6667))
                  ircloop)
