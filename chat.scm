#!/usr/local/bin/guile \
--debug -e main -s
!#
;;; $Id: chat.scm,v 1.5 2003/04/11 00:53:22 friedel Exp friedel $

;;; A little configuration:
(define default-nick "Friedel")
(define DEBUGGING #t)

;;; Global constants (should not be modified)
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

(define message-re "^&message=")

(define command-c #\/) ;; Use / as start of commands
;;; End of constants

(use-modules (srfi srfi-1)
             (ice-9 regex)
             (ice-9 threads))

;; GLOBAL VARIABLES! EVIL! :)
(define sync-mutex (make-mutex)) ;; Global mutex, 2 threads. Should be
                                 ;; enough
(define FINISHED #f)
;;; End of variables

(dynamic-link "libncurses.so")
(load-extension "./guile-ncurses.so" "init_curses")

;;; Functions for building strings

(define (make-command-string text)
  "Prepends command-character to the string"
  (string-append (make-string 1 command-c)
                 text))

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
    (get-answer sock message-re)))

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

(define (get-new-lines sock nick lines)
  ;; No check for now
  (let ((get-url (make-retrieve-url nick))
        (sock (connect-chat server-url server-port)))
    (display (http-get-request server-url get-url)
             sock)
    (get-response sock message-re)))

(define (get-nick)
  (if (not (null? (cdr (command-line))))
      (cadr (command-line))
    (begin
     (display "Nick: ")
     (read-line))))

(define (enter-string window)
  "Read a string from an ncurses window"
  (usleep 100000) ; 1/10 s
  (let ((startpos (getyx window))
        (width (getmaxx window))
        (length (getmaxy window))
        (-1>0 (lambda (x)
                (let ((newx (1- x)))
                  (if (< newx 0)
                      x
                    newx))))
        (+1< (lambda (x max)
               (let ((newx (1+ x)))
                 (if (< newx max)
                     newx
                   x)))))
    (let rec-edit ((strpos 0)
                   (line ""))
;         (display (format #f "l: ~s~%" line)
;                  (current-error-port))
         (let* ((liney (quotient strpos width))
                (y  (+ liney
                       (car startpos)))
                (x (if (zero? liney)
                       (+ strpos (cdr startpos))
                     (- strpos (* liney width))))
                (len (string-length line)))
           (lock-mutex sync-mutex)
           (wmove window y (-1>0 x))
           (waddstr window
                    (substring line
                               (-1>0 strpos)
                               len))
           (wclrtoeol window)
           (wmove window y x)
           (wrefresh window)
           (unlock-mutex sync-mutex)
           (let ((c (wgetch window)))
;             (display (format #f "c: ~s~%" c)
;                      (current-error-port))
             (if (or FINISHED
                     (equal? c #\cr))
                 line
               (case c
                 ((key-backspace #\del #\bs)
                  (rec-edit (-1>0 strpos)
                            (string-append
                             (substring line
                                        0
                                        (-1>0 strpos))
                             (substring line
                                        (+1< strpos len)
                                        len))))
                 ((#\etx) (make-command-string "quit"))
                 ((#\sub) (make-command-string "stop"))
                 (else (if (char? c)
                           (rec-edit (+1< strpos width)
                                     (string-append;; add char
                                      (substring line
                                                 0
                                                 strpos)
                                      (make-string 1 c)
                                      (substring line
                                                 (+1< strpos len)
                                                 len)))
                         (rec-edit strpos
                                   line))))))))))

;;; Parser for entered line, checks for command-character at the start
;;; of the line
(define (parse-user-input line sock nick)
  ;;very basic commands (operating on the whole line)
  (let ((sendpub (lambda ()
                   (send-public sock nick line)))
        (donothing (lambda () #t)))
    (if (string-null? line)
        donothing ;; do nothing
      (if (char=? (string-ref line 0)
                  command-c)
          (let* ((command-end (string-index line #\space))
                 (command-args (string-split line #\space))
                 (command (substring (car command-args)
                                     1
                                     (string-length (car command-args))))
                 (args (cdr command-args))
                 (rest-from (lambda (n)
                              (let ((joined (fold (lambda (x y)
                                                    (string-append x
                                                                   " "
                                                                   y))
                                                  ""
                                                  (list-cdr-ref args
                                                                n))))
                                (substring joined
                                           0
                                           (1- (string-length
                                                joined))))))
                 ;; more sophisticated commands:
                 (sendmsg (lambda ()
                            (send-msg sock
                                      nick
                                      (substring (rest-from 2))))))
            (cond
             ((string=? command "") sendpub);; Line started with <command-c>#\space,
             ((string=? command "quit") (lambda () (set! FINISHED #t)))
             ((or (string=? command "suspend")
                  (string=? command "stop")) (lambda () (kill (getpid)
                                                              SIGSTOP)))
             ((string=? command "msg") sendmsg)
             (else donothing)))
        ;; no command, send the line
        sendpub))))

;;; Main

(define (main argl)
  ;;; FIXME: Parse command line arguments
  (if (not (defined? 'call-with-new-thread))
      (error "Please reconfigure guile with --with-threads and
      recompile and install!"))
  (let* ((sock '())
         (nick (get-nick))
         (lines '())
         (line ""))
    (if (not DEBUGGING)
        (begin
         (while (not (login sock
                            nick
                            (getpass (format #f
                                             "Password for ~a: "
                                             nick))))
           (display "Sorry, Try again!")
           (newline))
         (send-admin-msg sock
                         (string-append nick
                                        " has just logged in"))))
    (let ((stdscr (initscr))
          (typesize 1)
          (COLS 80)
          (LINES 24)
          (scrollwin #f)
          (typewin #f))
      (letrec ((winchhandler (lambda (x)
                               (lock-mutex sync-mutex)
                               (let ((columns (getenv "COLUMNS"))
                                     (lines (getenv "LINES")))
                                 (set! COLS
                                       (if columns
                                           (string->number columns)
                                         (getmaxx stdscr)))
                                 (set! LINES (if lines
                                                 (string->number lines)
                                               (getmaxy stdscr))))
                               (set! scrollwin (newwin (- LINES typesize)
                                                       COLS
                                                       0
                                                       0))
                               (set! typewin (newwin typesize
                                                     COLS
                                                     (- LINES typesize)
                                                     0))
                               (unlock-mutex sync-mutex)))
               (ignore-all-signals (lambda ()
                                     (sigaction SIGCONT SIG_IGN)
                                     (sigaction SIGINT SIG_IGN)
                                     (sigaction SIGQUIT SIG_IGN)
                                     (sigaction SIGWINCH SIG_IGN)))
               (aborthandler (lambda (x) (set! FINISHED #t)))
               (conthandler (lambda (x) (set-handlers)))
               (set-handlers (lambda ()
                               (sigaction SIGCONT conthandler)
                               (sigaction SIGINT aborthandler)
                               (sigaction SIGQUIT aborthandler)
                               (sigaction SIGWINCH winchhandler))))
              (ignore-all-signals)
      ;;; some ncurses setup
              ;; (cbreak)
              (raw);; We parse the control characters!
              (noecho)
              (nonl)
              (winchhandler #f);; Call the handler once to get the screen
              ;; size and setup the windows.
              (scrollok stdscr #f)
              (scrollok scrollwin #t)
              (scrollok typewin #f)
              (leaveok stdscr #t)
              (leaveok scrollwin #t)
              (leaveok typewin #f)
              (keypad typewin #t)
              (nodelay typewin #t)
              (clear)
              (set-handlers)
              ;; 2 Threads now:
              (let* ((scroller
                      (begin-thread
                       (let loop ()
                            (set! lines (get-new-lines sock nick lines))
                            (lock-mutex sync-mutex)
                            (wmove scrollwin 0 0)
                            (for-each (lambda (line)
                                        (waddstr scrollwin line)
                                        (waddstr scrollwin
                                                 (make-string 1
                                                              #\newline)))
                                      lines)
                            (wrefresh scrollwin)
                            (unlock-mutex sync-mutex)
                            (sleep 1)
                            (if (not FINISHED)
                                (loop))))))
                (let loop ()
                     (lock-mutex sync-mutex)
                     (wclear typewin)
                     (wmove typewin 0 0)
                     (wstandout typewin)
                     (waddstr typewin nick)
                     (wstandend typewin)
                     (waddstr typewin ">> ")
                     (wrefresh typewin)
                     (unlock-mutex sync-mutex)
                     (set! line (enter-string typewin))
                     ((parse-user-input line sock nick)) ; returns a thunk
                                        ; that is executed immediately
                     (if FINISHED
                         (begin
                          (join-thread scroller)
                          (restore-signals)
                          (if (not DEBUGGING)
                              (logoff sock nick))
                          (endwin)
                          (primitive-exit))
                       (loop))))))))

;;; $Log: chat.scm,v $
;;; Revision 1.5  2003/04/11 00:53:22  friedel
;;; Threaded version with own low-level recursive edit (*Phew!*)
;;;
;;; Revision 1.4  2003/04/09 15:46:16  friedel
;;; Forked version (with totally garbled screen output!)
;;;
;;; Revision 1.3  2003/04/09 15:09:50  friedel
;;; logon with hidden input! :)
;;;
;;; Revision 1.2  2003/04/09 13:35:10  friedel
;;; Refactoring
;;;

;;; EOF
