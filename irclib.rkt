#lang racket

; This module builds strings for requisite IRC commands

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
  