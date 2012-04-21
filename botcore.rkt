#!  /usr/bin/racket
#lang racket
(require "irclib.rkt")
(require "commands.rkt")
(require "database.rkt")
(require "config.rkt")

;Here is how the connection flow has to go
;First send the NICK and USER commands to the server
;Wait a few seconds for a PING, if no PING increase
;the wait time by the last time * 1.5 to a maximum of n seconds

;; CALLBACK FUNCTIONS
(define (privmsg-handler send userinfo content join part)
  (match (parse-exclamation content)
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
    
   [result (send (format "~a is ~a" (first result) (dispatch userinfo content)))]))

;; Callback for join messages
(define (join-handler privmsg userinfo)
  userinfo)
;; Callback for quit messages
(define (quit-handler userinfo)
  userinfo)

;; Timeout combinator, tries to get output and times out if there is none
(define ((timeout-check max-wait) input?)
  (letrec ([matcher 
         (lambda (acc)
           (sleep acc)
           (match (input?)
             ['nil 
              (cond
                     [(> acc max-wait) 'nil]
                     [else (matcher (* acc 1.5))])]
             [result
              (display result) ;; display each line being received
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
(define (get-input parse inport)
   (let ([result ((get-raw-input inport))])
     (match result
       ["done" "done"]
       [_ (parse-input result)])))

(define (input? port)
   (get-input parse-input port))

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

;; The main loop that the bot runs in
(define (ircloop in out)
  (letrec ([inner-loop 
  (λ ()
    (match in
    ['nil 'nil]
      [_ (match (input? in)
           ["done" 'nil]
           ['nil (inner-loop)]
           [result
            ((put-raw-output result out))
            (inner-loop)])]))])
    (initial-work in out)
    (display "done initial work\n")
    (inner-loop)))

;; The connector, connects to a network and then passes control to the main loop
(define (connect hostname port)
  (let-values ([(input output) (tcp-connect hostname port)])
    (file-stream-buffer-mode output 'line)
    (values input output)))

(call-with-values (λ () (connect "irc.freenode.org" 6667))
                  ircloop)
