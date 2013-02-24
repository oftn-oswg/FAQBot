#lang racket

(require "output.rkt")
(require "irclib.rkt")

;; makes the querier procedure
;; required to interact with services
(define (make-querier out procs)
  (let ([thds (make-hash)]) 
         (map ((curry apply) 
               (λ (nm f) 
                 (hash-set! thds
                            nm
                            (thread (λ () (f out)))))) 
              procs) 
    (λ (name [message #f])
      (match message
        ;; kill all of the services, should
        ;; only be used when the bot is quitting
        [#f (map (compose kill-thread cdr) (hash->list thds))]
        [_ (match (hash-ref thds name #f)
             [#f 
              (error 
               (format "The service ~a does not exist" name))]
             [service (thread-send service message)])]))))

;; things that run in the background

;; frequency of pings
(define *PING* 40)

;; maybe ping back?
(define (ping-server out)
  (sleep *PING*)
  ((put-raw-output 
    (pingpong "ping" (format "~a" (current-seconds)))
    out))
  (ping-server out))

(define services
  (list (list "ping" ping-server)))

(provide services make-querier)