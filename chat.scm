#!/usr/local/bin/guile \
--debug -e main -s
!#
;;; $Id: chat.scm,v 1.2 2003/04/09 13:35:10 friedel Exp friedel $

(define default-nick "Friedel")

;;; Configuration (global constants... sort of :)
(define admin-nick "Administrator")

(define default-from-arg "")
(define default-to-arg "All")
(define default-rand-arg "0")

(define server-port 80)

(define server-url "www.roskilde-festival.dk")
(define base-url "/community/chat/php/")
(define send-url "RFChatSendMessages.php")
(define read-url "RFChatMessages.php")
(define online-url "RFChatOnline.php")
(define login-url "RFChatLogin.php")
(define logoff-url "RFChatRemoveOnline.php")

(define rand-arg "random")
(define nick-arg "name")
(define message-arg "message")
(define from-arg "messageFrom")
(define to-arg "messageTo")
(define login-arg "login")
(define password-arg "password")

(use-modules (ice-9 readline)
             (srfi srfi-1)
             (ice-9 regex))

;;; Functions for building strings
(define (url-encode text)
  "Very cheap url-encoding :)"
  ;;; Speculation for fun: The reason why the roskilde chat eats "+"
  ;;; signs is probably that *their* url-encoding function is probably
  ;;; as cheap as this one! :)
  (let ((len (string-length text))
        (newtext (string-copy text)))
    (let loop ((offs (string-index newtext #\space)))
         (if offs
             (begin
              (string-set! newtext offs #\+)
              (loop (string-index newtext #\space (1+ offs))))
           newtext))))

(define (make-script-args script args)
  "Takes a script url and an arg-parameter alist, returns the string
  for the GET-Request"
  (let ((str (string-append script "?"
                            (apply string-append
                                   (map (lambda (pair)
                                          (string-append (car pair)
                                                         "="
                                                         (cdr pair)
                                                         "&"))
                                        args)))))
    (substring str 0 (1- (string-length str)))))

(define (make-base-script-args base script args)
  "Calls make-script-args with base prepended to script"
  (make-script-args (string-append base script) args))


(define (list->alist lst)
  (if (null? lst)
      '()
    (let ((rest (cdr lst)))
      (cons (cons (car lst)
                  (if (null? rest)
                      (error "List must have even number of
                      elements!")
                    (car rest)))
            (list->alist (cddr lst))))))


(define (make-args-apply base script . args)
  "Constructs an alist alternating over the rest argument and calls
  make-base-script-args"
  (make-base-script-args base script (list->alist args)))

(define (chat-random)
  "Return a random number as a string"
  (number->string (random 1000000)))

(define (make-msg-url nick msgstring from to)
  "Return a mesage url for a messagestring"
  (make-args-apply base-url send-url
                   rand-arg  (chat-random)
                   nick-arg (url-encode nick)
                   message-arg (url-encode msgstring)
                   from-arg (url-encode from)
                   to-arg (url-encode to)))

;;; FIXME: New parameter: timestamp for optional Only-If-Newer Header
(define (http-get-request host url)
  (string-append "GET "
                 url
                 " HTTP/1.0\r\n"
                 "\r\n"
                 "Host: " host "\r\n"
                 "\r\n"))

(define (http-get-args host base script . args)
  (http-get-request host
                    (make-base-script-args base script
                                    (list->alist args))))

(define (make-retrieve-url nick)
  "Return an url to retrieve messages for <nick>"
  (make-args-apply base-url read-url
                 to-arg (url-encode nick)))

(define (drop-before-match lines regexp remove)
  "drop all lines in <lines> before the first one matching <regexp>.
  Remove the matched substring from the first matching line if
  <remove> is #t"
  ;; FIXME: A (do) loop would be cleaner here
  (let* ((matcher #f) ;; matcher is modified in drop-while predicate
         (filtered (drop-while (lambda (line)
                                 (set! matcher (string-match regexp
                                                             line))
                                 (not matcher))
                               lines)))
    (if (not (null? filtered))
        (let ((firstmatch (car filtered))
              (m-start (match:start matcher))
              (m-end (match:end matcher)))
          (if remove
              (cons (string-append (substring firstmatch
                                              0
                                              m-start)
                                   (substring firstmatch
                                              m-end
                                              (string-length firstmatch)))
                    (cdr filtered))
            filtered))
      '())))

(define (current-date-and-time)
  (strftime "%T, %Y-%m-%d" (gmtime (current-time))))

;;; Network and other IO

(define (connect-chat server-url port)
  "Return a port for talking with the server"
  (let* ((entry (gethostbyname server-url))
         (addrs (array-ref entry 4))
         (addr (car addrs))
         (sock (socket AF_INET SOCK_STREAM 0)))
    (if (connect sock AF_INET addr port)
        sock
      (error "Could not connect!"))))


(define (send-to-server request sock)
  (let ((sock (connect-chat server-url server-port)))
    (display request sock)
    (flush-response sock)))


(define (send-msg sock nick msgstring from to)
  (let ((get-url (make-msg-url nick msgstring from to)))
    (send-to-server (http-get-request server-url get-url)
             sock)))

(define (send-public sock nick msgstring)
  (send-msg sock nick msgstring nick default-to-arg))

(define (get-response-lines sock)
  "Return the text response from the webserver as a list of strings
  (lines)"
  (let ((answer #f)
        (response '()))
    (while (not (eof-object? answer))
      (set! answer (read-line sock))
      (if (eof-object? answer)
          #t ;; while will always return #t anyways!
          (set! response (cons answer response))))
    (close-port sock)
    (reverse response)))


(define (get-response sock regex)
  (reverse (drop-before-match (get-response-lines sock)
                              regex
                              #t)))

(define (get-answer sock regex)
  "get the response from the server, filter the lines with
  (drop-before-match) and print the result"
  (let ((response (get-response sock regex)))
    (if (not (null? response))
        (for-each (lambda (line)
                    (display line)
                    (newline))
                  response))))

(define (get-text sock nick)
  (let ((get-url (make-retrieve-url nick))
        (sock (connect-chat server-url server-port)))
    (display (http-get-request server-url get-url)
             sock)
    (get-answer sock "^&message=")))

(define flush-response close-port)

(define (send-admin-msg sock message)
      (send-msg sock
              admin-nick
              message
              admin-nick
              default-to-arg))

(define (login sock nick pw)
  "Send login message to server, return #t if 'ok', #f otherwise"
  (let ((sock (connect-chat server-url server-port)))
    (display (http-get-args server-url
                            base-url login-url
                            rand-arg (chat-random)
                            login-arg nick
                            password-arg pw)
             sock)
    (string=? "ok&" (car (get-response sock "^&entry=")))))

(define (logoff sock nick)
  (send-to-server (http-get-args server-url
                                 base-url logoff-url
                                 rand-arg (chat-random)
                                 login-arg nick)
                  sock))


;;; Main

(define (main argl)
  (let ((in (current-input-port))
        (out (current-output-port)))
    (activate-readline)
  ;;; FIXME: Parse command line arguments
    (let* ((sock '())
           (nick default-nick)
           (line "")
           (sighandler (lambda (x)
                         (restore-signals)
                         (logoff sock nick)
                         (primitive-exit))))
      (while (not (login sock
                         nick
                         (getpass (format #f
                                          "Password for ~a: "
                                          nick))))
        (display "Sorry, Try again!")
        (newline))
      (send-admin-msg sock
                      (string-append nick
                                     " has just logged in"))
      (sigaction SIGINT sighandler)
      (sigaction SIGQUIT sighandler)
      (let loop ()
         ;;; FIXME: This needs to use select for the local input, and
         ;;; we should poll regularly if there's new input available
           (get-text sock nick)
           (set! line (read-line))
           (if (not (string-null? line))
               (send-public sock nick line))
           (loop)))))

;;; $Log: chat.scm,v $
;;; Revision 1.2  2003/04/09 13:35:10  friedel
;;; Refactoring
;;;

;;; EOF
