#lang racket
(require (planet mordae/couchdb:1:11))
(require (planet dherman/json:4:0))
(require net/base64)
(require "redis.rkt")
(require "config.rkt")

;; This module is all of the database stuff
;; as well as the ratelimiting function

; Couch Database operations
; object used to connect to the database

(define reserved-words (set "counter" "alias" "set"))

(define (database-connection database-name)
  (couchdb-db
   (couchdb-connect
   #:host (database-info "hostname")
   #:port (database-info "port")
   #:user (database-info "user")
   #:password (database-info "pass"))
   database-name))

; gets a single uuid
(define (get-uuid)
  (couchdb-get (database-connection "_uuids") ""))

; submits a question
(define couchdb-submit
  (compose ((curry couchdb-put)
            (database-connection "faqbot"))
           ; build the jsexpr (hasheq)
           (lambda (question)
             (hasheq '_id
                     (first question)
                     'text 
                     (second question)))
           ; strip all newlines
           (lambda (question)
             (list (first question)
                   (strip-newlines
                    (second question))))))

; trys to get a question from couchdb

(define (try-couchdb-get database id)
  (with-handlers ([exn:couchdb:not-found?
                   (lambda (_) 
                     'nil)])
    (couchdb-get database id)))
  

; gets a question
(define couchdb-retrieve
  (compose
   (lambda (result)
     (match result
       ['nil 'nil]
       [_ (hash-ref result 'text)]))
   ((curry try-couchdb-get)
   (database-connection "faqbot"))))

; strips all newlines
(define (strip-newlines text)
  (match text
    ['nil 'nil]
    [_ (regexp-replace* #rx"[\n\r]" text "")]))

; Redis Database Operations

; make a command
(define make-cmd redis-set)

; gets a specified command ID from redis
(define (get-id cmd)
  (match (redis-get cmd)
    ['nil 'nil]
    [result (match (bytes->string/utf-8 result)
    ['nil 'nil]
    [result (match (regexp-match #rx"^[0-9a-z]*$" result)
              [#f 'nil]
              [_ result])])]))

; make a command alias
(define (make-alias new-cmd old-cmd)
  (match (get-id old-cmd)
    ['nil 'nil]
    [_ (make-cmd
   new-cmd
   (get-id old-cmd))]))

; associates a name with an id in Redis, then puts the question in couchdb
(define (submit-question name content)
  (match name
    [(? ((curry set-member?) reserved-words)) "reserved word"]
    [_ (let* ([id (caar (hash-values [get-uuid]))])
    (make-cmd name id)
    (couchdb-submit (list id content)))]))

; gets an id associated with a name from Redis, then gets the question from couchdb

(define (get-question name)
  (let* ([id (get-id name)])
    (match id
      ['nil 'nil]
      [_ (match (couchdb-retrieve id)
           ['nil (redis-del! name) ; if there is nothing in the database
                 'nil]       ; then remove it
           [result result])])))

;; Rate Limiting
; if |hC Current| > k, then set hC to Current, and hN to 0, and allow
; if |hC Current| < k and hN == n, then deny
; if |hC Current| < k, and hN < n, then allow, and increment hN by 1

(define (bytes->number x)
  (string->number (bytes->string/utf-8 x)))
; Is there a built-in Racket function to convert byte strings to floats?

(define (time-magnitude a b)
  (abs (- b (bytes->number a))))

(define (rate-check-helper mag hN n k)
  (let ([hN (bytes->number hN)])
    (cond
    [(and (> k mag) (eq? hN n)) #f]
    [(and (< mag k) (< hN n)) (redis-hincrby "counter" "n" "1") #t]
    [else (redis-hset "counter" "n" "0")
          (redis-hset "counter" "current" (number->string (current-inexact-milliseconds)))
          #t])))

(define ((rate-check n k))
  (let* ([counter (redis-hvals "counter")]
         [current-time (current-inexact-milliseconds)])
    (let* ([hC-Current (time-magnitude (second counter) current-time)])
      (rate-check-helper hC-Current (first counter) n k))))

; Allows no more than 6 commands every 45 seconds
; resets after 5 minutes have passed from the last time it was reset
(define allowed? (rate-check 10 (* 2 60000)))
;; Admin Stuff

(provide (all-defined-out))