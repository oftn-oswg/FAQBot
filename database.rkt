#lang racket
(require (planet mordae/couchdb:1:11))
(require (planet dherman/json:4:0))
(require net/base64)
(require "redis.rkt")
(require "config.rkt")

; Couch Database operations
; object used to connect to the database
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

; gets a question
(define couchdb-retrieve
  (compose
   (lambda (result)
     (hash-ref result 'text))
   ((curry couchdb-get)
   (database-connection "faqbot"))))

; strips all newlines
(define (strip-newlines text)
  (regexp-replace* #rx"\n" text ""))

; Redis Database Operations

; make a command
(define make-cmd set)

; gets a specified command ID from redis
(define (get-id cmd)
  (match (get cmd)
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


;; Command Dispatch Functions
;; These functions take a string, parse it, and then execute the command

; associates a name with an id in Redis, then puts the question in couchdb
(define (submit-question name content)
  (let* ([id (caar (hash-values [get-uuid]))])
    (make-cmd name id)
    (couchdb-submit (list id content))))

; gets an id associated with a name from Redis, then gets the question from couchdb

(define (get-question name)
  (let* ([id (get-id name)])
    (match id
      ['nil "No question found"]
      [_ (couchdb-retrieve id)])))

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
    [(and (< mag k) (< hN n)) (hincrby "counter" "n" "1") #t]
    [else (hset "counter" "n" "0")
          (hset "counter" "current" (number->string (current-inexact-milliseconds)))
          #t])))

(define ((rate-check n k))
  (let* ([counter (hvals "counter")]
         [current-time (current-inexact-milliseconds)])
    (let* ([hC-Current (time-magnitude (second counter) current-time)])
      (rate-check-helper hC-Current (first counter) n k))))

; Allows no more than 20 commands every 5 minutes
; resets after 5 minutes have passed from the last time it was reset
(define allowed? (rate-check 20 (* 5 60000)))
;; Admin Stuff

(provide (all-defined-out))