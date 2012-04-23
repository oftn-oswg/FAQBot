;;; redis.rkt
;;;
;;; Implements an interface to the redis persistent key-value
;;; database. Communicates with the database through its TCP
;;; interface.
#lang racket

(require racket/date)

(define-struct connection (in out cust))

(define (connect)
  (let ([cust (make-custodian)])
    (parameterize ([current-custodian cust])
      (let-values ([(in out) (tcp-connect "localhost" 6379)])
        (make-connection in out cust)))))

(define current-connection (make-parameter (connect)))

(define (disconnect!)
  (define conn (current-connection))
  (with-handlers ([exn:fail:network? void])
    (close-output-port (connection-out conn)))
  (with-handlers ([exn:fail:network? void])
    (close-input-port (connection-in conn))))

(define send-command
  (lambda commands
    (define out (connection-out (current-connection)))
    (fprintf out "*~a\r\n" (length commands))
    (for-each (lambda (command)
                (fprintf out "$~a\r\n~a\r\n"
                         (bytes-length
                          (string->bytes/utf-8
                           command))
                         command))
              commands)
    (flush-output out)))

(define-struct exn:redis (message))

(define (read-reply)
  (define in (connection-in (current-connection)))
  (match (read-bytes 1 in)
    [#"-" (read-line in 'return-linefeed)]
    [#"+" (read-line in 'return-linefeed)]
    [#"$" (read-bulk-reply in)]
    [#"*" (read-multi-bulk-reply in)]
    [#":" (string->number (read-line in 'return-linefeed))]
    [_ (raise (make-exn:redis (format "invalid control character: ~a"
                                      (read-byte in))))]))

(define (read-bulk-reply in)
  (flush-output)
  (let ([length (string->number (read-line in 'return-linefeed))])
    (match length
      [-1 'nil]
      [_ (begin0 (read-bytes length in)
      (read-line in 'return-linefeed))])))

(define (read-multi-bulk-reply in)
  (let ([length (string->number (read-line in 'return-linefeed))])
    (flush-output)
    (build-list length
                (lambda (_) (read-reply)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS

(define (redis-ping)
  (send-command "PING")
  (read-reply))

;; Connection handling

(define (redis-quit)
  (send-command "QUIT")
  (disconnect!))

(define (redis-auth)
  (send-command "AUTH")
  (read-reply))

;; Commands operating on all value types

(define (redis-exists? key)
  (send-command "EXISTS" key) 
  (match (read-reply)
         [1 #t]
         [0 #f]))

(define redis-del!
  (lambda keys
    (apply send-command `("DEL" ,@keys))
    (read-reply)))

(define (redis-type key)
  (send-command "TYPE" key)
  (string->symbol (read-reply)))

(define (redis-keys pattern)
  (send-command "KEYS" pattern)
  (read-reply))

(define (redis-randomkey)
  (send-command "RANDOMKEY")
  (read-reply))

(define (redis-rename! oldkey newkey)
  (send-command "RENAME" oldkey newkey)
  (read-reply))

(define (redis-renamenx! oldkey newkey)
  (send-command "RENAME" oldkey newkey)
  (match (read-reply)
         [1 #t]
         [0 #f]))

(define (redis-dbsize)
  (send-command "DBSIZE")
  (read-reply))

(define (redis-expire! key seconds)
  (send-command "EXPIRE" key seconds)
  (read-reply))

(define (redis-expireat! key date)
  (send-command "EXPIREAT" key (date->seconds date))
  (read-reply))

(define (redis-ttl key)
  (send-command "TTL" key)
  (read-reply))

(define (redis-select key)
  (send-command "SELECT" key)
  (read-reply))

(define (redis-move key dbindex)
  (send-command "MOVE" key dbindex)
  (read-reply))

(define (redis-flushdb)
  (send-command "FLUSHDB")
  (read-reply))

(define (redis-flushall)
  (send-command "FLUSHALL")
  (read-reply))

(define (redis-set key value)
  (send-command "SET" key value)
  (read-reply))

(define (redis-get key)
  (send-command "GET" key)
  (read-reply))

(define (redis-getset key value)
  (send-command "GETSET" key value)
  (read-reply))

(define redis-mget
  (lambda keys
    (apply send-command `("MGET" ,@keys)
    (read-reply))))

(define (redis-incrby key n)
  (send-command "INCRBY" key n)
  (read-reply))

;; Hash commands

(define (redis-hset key field value)
  (send-command "HSET" key field value)
  (read-reply))

(define (redis-hget key field)
  (send-command "HGET" key field)
  (read-reply))

(define (redis-hgetall key)
  (send-command "HGETALL" key)
  (read-reply))

(define (redis-hincrby key field num)
  (send-command "HINCRBY" key field num)
  (read-reply))

(define (redis-hincrbyfloat key field num)
  (send-command "HINCRBYFLOAT" key field num)
  (read-reply))

(define (redis-hkeys key)
  (send-command "HKEYS" key)
  (read-reply))

(define (redis-hvals key)
  (send-command "HVALS" key)
  (read-reply))

(provide (all-defined-out))