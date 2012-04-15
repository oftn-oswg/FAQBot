#lang racket
 
;;TCP
;;print-from-port : port -> void
;;prints out lines from the port until there are no more lines to print.
(define (print-from-port port)
  (if (char-ready? port)
      (let ([nextchar (read-char port)])
        (printf (if (char=? #\~ nextchar)
                    "~~"
                    (string nextchar)))
        (print-from-port port))
      (printf "~n")))
 
;;send-to-port : port -> void
;;sends direct input from the user to the port.
(define (send-to-port port)
  (let ([input (read-line)])
    (if (not (string=? input "n"))
        (begin (display (string-append input "\r\n") port)
               (flush-output port)
               (printf "Sending ~a~n" input))
        (printf "Not sending anything.~n"))))
 
;;irc-connect : string number (port port -> void) -> void
;;connects to an IRC network, and begins running the provided irc handler.
(define (irc-connect server port handler)
         (call-with-values (lambda () (tcp-connect server port))
                  (lambda (arguments) (handler (first arguments) (second arguments)))))
 
(define (irc-handle in out)
  (print-from-port in)
  (send-to-port out)
  (irc-handle in out))
 
;(irc-connect "localhost" 6667 irc-handle)