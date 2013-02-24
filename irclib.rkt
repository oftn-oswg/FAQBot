#lang racket

;; These functions build strings for requisite IRC commands
(require "commands.rkt")
(require "database.rkt")
(require "config.rkt")

; PRIVMSG command
(define (privmsg target msg)
  (format "PRIVMSG ~a :~a"
          target
          msg))

; USER command
(define (usermsg username realname)
  (format 
   "USER ~a 0 * :~a" username realname))

; JOIN command
(define (join channel)
  (format
   "JOIN :~a" channel))

; PART command
(define (part channel reason)
  (format
   "PART ~a :~a"
   channel
   reason))
  
; QUIT command
(define (quit reason)
  (format "QUIT :~a"
          reason))

; NICK command
(define (nick nickname)
  (format "NICK ~a" nickname))

; PING/PONG command
(define (pingpong pingorpong message)
  (match pingorpong
    ["ping" (format "PING :~a" message)]
    ["pong"
     (format "PONG ~a" message)]))

;; Parses IRC responses
;; turns a list of words into a string
;; and skips the first character
(define content->string
  (compose
   string-tail
   (lambda (words)
     (string-join words " "))))

(define (parse-hostmask hostmask)
  (match (regexp-split #rx"!" hostmask)
    [(list nick host)
     (match (regexp-split #rx"@" host)
       [(list username hostname) (list (string-tail nick)
                                       username
                                       hostname)])]))
;; This function parses the raw IRC messages
;; the output is intended to be used with the callback handler
(define (parse-message message)
  (match message
    ['nil 'nil]
    [_ (match (regexp-split #rx" " message)
    ;; Matches a ping
    [(list "PING" content) (list "PING" content)]
    
    ;; Matches a pong
    [(list "PONG" _ content) (list "PONG" content)]
    
    ;; Matches a private message command
    [(list-rest hostmask "PRIVMSG" channel content) 
     (list "PRIVMSG" (parse-hostmask hostmask) channel 
           (content->string content))]
    
    ;; Matches a Quit command
    [(list-rest hostmask "QUIT" _) (list "QUIT" (parse-hostmask hostmask))]
    
    ;; Matches a Join command
    [(list-rest hostmask "JOIN" (list channel)) 
     (list "JOIN" (parse-hostmask hostmask) (string-tail channel))]
    [_ 'nil])]))

;; Callback handler
;;This is how callback functions are chosen
;;First we parse the IRC messages with parse-message
;;then we use pattern matching to decide which callback
;;function will be called
;;It should be easy to add support for new IRC codes this way
;; query-service allows you to interact with daemons running
;; it is the callback and daemon's responsibility to make sure everything is thread
; safe/atomic/etc...
(define ((register-callbacks
         privmsg-response
         quit-response
         join-response)
         query-service
         message)
 
  (match message
    ['nil 'nil]
    [_ (match (parse-message (strip-newlines message)) ;MUST strip newlines and carriage returns
    ; private message handler (called when we receive a private message)
    ; First match to see if they are messaging us and not a channel! Or else we end up in an infinite loop
    [(list "PRIVMSG" userinfo channel content)
     (privmsg-response
      query-service
      ((curry privmsg) (me? channel (first userinfo)))
      userinfo
      content
      join
      ((curry part) (first userinfo))
      quit)]
    ; JOIN handler (called when a user joins the channel)
    [(list "JOIN" userinfo channel) (join-response
                                     ((curry privmsg) 
                                      channel)
                                     userinfo)]
    ; QUIT handler (called when a user quits)
    [(list "QUIT" userinfo) (quit-response userinfo)]
    ; PING handler (called when we receive a PING)
    [(list "PING" message)
     (display (format "pinging with ~a\n" message))
     (pingpong "pong" message)]
    
    ; PONG handler
    [(list "PONG" _)
     (display "received a pong\n")]
    
    [_ 'nil])]))

;; Check if a nick is the bot or not
(define (me? channel recipient)
  (cond
  [(string=? irc-username channel) recipient]
  (else channel)))

(provide (all-defined-out))