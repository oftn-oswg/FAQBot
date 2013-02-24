#lang racket

;; amount of time to wait on a command before failing
(define *TIMEOUT* 20)

;; macro for making "handlers" that spawn new threads
(define-syntax-rule (make-handler
                     name
                     (param ...)
                     body)
  (define (name param ...)
    (thread (λ ()
              (let ([thd
                     (thread
                      (λ ()
                        body))])
               (sync/timeout *TIMEOUT* (thread-dead-evt thd))
               (kill-thread thd))))))

;; Sends raw text to the irc server
(define ((put-raw-output text port))
  ;(display (format "sending: ~a\n" text))
  (display (format "~a\r\n" text) port)
  (flush-output port))

(provide (all-defined-out))