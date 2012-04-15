#lang racket
; Config stuff
(define inp-port
  (open-input-file "./config.sexp"))

; Decides what to do with a config item
(define (config-parse item)
  (match item
    [(list-rest x (list-rest xs)) 
     (update (make-hash) item)] ; make this into a new hash-table
    [(list-rest xs) item] ; a flat list
    [_ item])) ; any other type

; Creates a config hash table
(define (update htable settings)
    (cond ((null? settings) htable)
    (else (map (lambda (setting)
        (hash-set! htable (first setting) (config-parse
                                           (second setting)))) settings)
        htable)))

; the actual config hash-table
(define config
  (update (make-hash)
          (read inp-port)))

; get database info
(define database-info
  (let* [(database
    (hash-ref
     config
     "database"))]
  (lambda (key)
    (hash-ref
     database
     key))))

; get admin info, each admin returns an int which is access level
(define admin-info
  (let* [(admins
          (hash-ref
           config
           "admins")
          )]
    (lambda (key)
      (hash-ref
       admins
       key 1 ; default is 1 which is non-admin))))

(provide (all-defined-out))