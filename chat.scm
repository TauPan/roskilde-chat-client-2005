#!/bin/sh
exec guile --debug -e main -s $0 $@
!#
;;; $Id: chat.scm,v 1.14 2003/04/15 12:13:09 friedel Exp friedel $

;;; A little configuration:
(define default-nick "Friedel")
(define DEBUGGING #f)

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
(define userlist-url "RFChatPrivate.php")

(define rand-arg "random")
(define nick-arg "name")
(define message-arg "message")
(define from-arg "messageFrom")
(define to-arg "messageTo")
(define login-arg "login")
(define password-arg "password")

(define message-re "^&message=")

(define command-c #\/) ;; Use / as start of commands

(define BUFSIZE 1024)

;;; End of constants

(use-modules (srfi srfi-1)
             (ice-9 regex)
             (ice-9 threads)
             (ice-9 format))

;;; GLOBAL VARIABLES! EVIL! :)
(define sync-mutex (make-mutex)) ; Global mutex, 2 threads. Should be
                                 ; enough
(define FINISHED #f)   ; it #t, client will terminate
(define REDRAWEDIT #f) ; if #t, editing area is fully redrawn
(define REDRAWLINES #f); if #t, text area is fully redrawn
(define PASSWORD "")   ; we have to memorize the password because we
                                        ; need to re-authenticate

;;; End of variables

(dynamic-link "libncurses.so")
(load-extension "./guile-ncurses.so" "init_curses")

;;; Functions for building strings

(define (make-command-string text)
  "Prepends command-character to the string"
  (string-append (make-string 1 command-c)
                 text))

(define (url-encode text)
  "Cheap url encoding (makes hex out of everything :)"
  (let ((hexlist (map (lambda (c)
                        (format #f "%~x" (char->integer c)))
                      (string->list text))))
    (apply string-append hexlist)))

(define (url-encode-old text)
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
  "Convert lst with even number of elements to an alist, using the
  elements at uneven positions as car and the others as cdr for each
  pair"
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
  "Build a http-request for url on host"
  (string-append "GET "
                 url
                 " HTTP/1.0\r\n"
                 "\r\n"
                 "Host: " host "\r\n"
                 "\r\n"))

(define (http-get-args host base script . args)
  "call http-get-request to host with url built by
  make-base-script-args"
  (http-get-request host
                    (make-base-script-args base script
                                    (list->alist args))))

(define (make-retrieve-url nick)
  "Return an url to retrieve messages for <nick>"
  (make-args-apply base-url read-url
                 nick-arg (url-encode nick)))

(define (drop-before-match lines regexp remove)
  "drop all lines in <lines> before the first one matching <regexp>.
  Remove the matched substring from the first matching line if
  <remove> is #t"
  (let remover ((filtered lines)
                (matcher (string-match regexp (car lines))))
       (if (null? filtered)
           '()
         (if matcher
             (if remove
                 (let ((firstmatch (car filtered))
                       (m-start (match:start matcher))
                       (m-end (match:end matcher)))
                   (cons (string-append (substring firstmatch
                                                   0
                                                   m-start)
                                        (substring firstmatch
                                                   m-end
                                                   (string-length
                                                    firstmatch)))
                         (cdr filtered)))
               filtered)
           (remover (cdr filtered)
                    (string-match regexp (cadr filtered)))))))


(define (current-date-and-time)
  "Return the date+time string in the format the roskilde
  webchat-server uses"
  (strftime "%T, %Y-%m-%d" (gmtime (current-time))))

;;; Network and other IO

(define (get-connector server-name port)
  "Return a thunk that will return a connected socket to
  <port> on <server-name>"
  (let* ((entry (gethostbyname server-url))
         (addrs (array-ref entry 4))
         (addr (car addrs))
         (sock (socket AF_INET SOCK_STREAM 0)))
    (letrec ((connectme
              (lambda ()
                (if (port-closed? sock)
                    (set! sock (socket AF_INET SOCK_STREAM 0)))
                (if (connect sock AF_INET addr port)
                    (setvbuf sock _IOLBF)
                  (error "Could not connect!"))))
             (handler
              (lambda (key . args)
                (format (current-error-port)  "Warning: ~s failed.~%" (car args))
                (format (current-error-port) (cadr args) (cddr args))
                (newline (current-error-port))))
             (connector
              (lambda () (catch 'system-error
                           connectme
                           handler)
                sock)))
            connector)))

(define connect-chat (get-connector server-url server-port))

(define (send-to-chatserver request)
  "Send request to chatserver, ignore the response"
  (lock-mutex sync-mutex)
  (let ((sock (connect-chat)))
    (display request sock)
    (flush-response sock)
    (unlock-mutex sync-mutex)))

(define (get-from-chatserver request regex)
  "Send get-request to chatserver, return the list of response lines,
  filtered by drop-before-match"
  (lock-mutex sync-mutex)
  (let ((sock (connect-chat))
        (response #f))
    (display request sock)
    (set! response (get-response sock regex))
    (unlock-mutex sync-mutex)
    response))

(define (send-msg nick msgstring from to)
  "Send the message with text <msgstring> as name <nick> from <from>
  to <to>"
  (let ((get-url (make-msg-url nick msgstring from to)))
    (send-to-chatserver (http-get-request server-url get-url))))

(define (send-public nick msgstring)
  "Send a public message as <nick>"
  (send-msg nick msgstring nick default-to-arg))

(define (get-response-lines sock)
  "Return the text response from the webserver as a list of strings
  (lines) [the order of the webserver response is preserved, the chat
  lines will be ordered from newest to oldest]"
  (let ((answer #f)
        (response '()))
    (while (not (eof-object? answer))
      (set! answer (read-line sock))
      (if (eof-object? answer)
          #t;; while will always return #t anyways!
        (set! response (cons answer response))))
    (close-port sock)
    (reverse response)))

(define (get-response sock regex)
  "return text response from webserver, filtered by drop-before-match"
  (drop-before-match (get-response-lines sock)
                     regex
                     #t))

(define flush-response close-port)

(define (send-admin-msg message)
  "Send a message as Administrator"
  (send-msg admin-nick
            message
            admin-nick
            default-to-arg))

(define (login nick pw)
  "Send login message to server, return #t if 'ok', #f otherwise"
  (string=? "ok&"
            (car (get-from-chatserver (http-get-args server-url
                                                     base-url login-url
                                                     rand-arg (chat-random)
                                                     login-arg nick
                                                     password-arg pw)
                                      "^&entry="))))

(define (logoff nick)
  "Remove <nick> from Chat"
  (send-to-chatserver (http-get-args server-url
                                     base-url logoff-url
                                     rand-arg (chat-random)
                                     login-arg nick)))

(define (users)
  "Get list of users from chatserver"
  (let* ((rawlist (car (get-from-chatserver (http-get-args server-url
                                                           base-url
                                                           userlist-url
                                                           rand-arg
                                                           (chat-random))
                                            "^&users=")))
         (cutraw (substring rawlist 0 (1- (string-length rawlist)))))
    (delete1! "-1"
            (delete1! default-to-arg
                    (string-split cutraw #\,)))))

(define (get-first-line sock message-re)
  "Get first line containing text matching <message-re> (with the
  match removed)"
  (let loop ((answer #f)
             (matcher #f))
       (cond ((eof-object? answer) #f);; Failure to match
             (matcher (let ((m-start (match:start matcher))
                            (m-end (match:end matcher)))
                        (string-append (substring answer
                                                  0
                                                  m-start)
                                       (substring answer
                                                  m-end
                                                  (string-length
                                                   answer)))))
             (else (let* ((newans (read-line sock))
                          (newmatch (if (not (eof-object? newans))
                                        (string-match message-re
                                                      newans)
                                      #f)))
                     (loop newans newmatch))))))

(define (get-next-line sock)
  "get next line from sock"
  (let ((ans (read-line sock)))
    (if (eof-object? ans)
        #f
      ans)))

(define (get-new-lines sock nick lines)
  "Get new lines in chat"
  (lock-mutex sync-mutex)
  (let ((get-url (make-retrieve-url nick))
        (sock (connect-chat))
        (response #f))
    (display (http-get-request server-url get-url)
             sock)
    (if (null? lines)
        (set! response (get-response sock message-re))
      (let loop ((line (get-first-line sock message-re))
                 (newlines '()))
           (if (or (not line)
                   (string=? line (car lines)))
               (begin
                (flush-response sock)
                (set! response (reverse newlines)))
             (loop (get-next-line sock)
               (cons line newlines)))))
    (unlock-mutex sync-mutex)
    response))

(define (get-nick)
  "Get the nick to connect as"
  (if (not (null? (cdr (command-line))))
      (cadr (command-line))
    (begin
     (display "Nick: ")
     (read-line))))

(define (enter-string window)
  "Read a string from an ncurses window"
  (letrec ((startpos (getyx window))
           (width (getmaxx window))
           (length (getmaxy window))
           (-1>0
            (lambda (x)
              (let ((newx (1- x)))
                (if (< newx 0)
                    x
                  newx))))
           (+1<
            (lambda (x max)
              (let ((newx (1+ x)))
                (if (< newx max)
                    newx
                  x))))
           (rec-edit ;; the infamous recursive editor function
            (lambda (strpos line)
              (usleep 10000)            ; 1/100 s
              (let* ((liney (quotient (+ strpos (cdr startpos))
                                      width))
                     (y  (+ liney
                            (car startpos)))
                     (x (if (zero? liney)
                            (+ strpos (cdr startpos))
                          (- strpos (* liney width))))
                     (len (string-length line)))
                (lock-mutex sync-mutex)
                ;; Draw the (changed part of the) line
                (if REDRAWEDIT ;; draw everything
                    (begin
                     (wmove window (car startpos) (cdr startpos))
                     (waddstr window line)
                     (set! REDRAWEDIT #f))
                  (begin ;; draw the tail
                   (wmove window y (-1>0 x))
                   (waddstr window
                            (substring line
                                       (-1>0 strpos)
                                       len))))
                (wclrtoeol window)
                (wmove window y x)
                (wrefresh window)
                (unlock-mutex sync-mutex)
                (let ((c (wgetch window))) ;; get a character
                  (if (or FINISHED
                          (equal? c #\cr)) ;; line complete
                      line
                    (case c ;; examine character
                      ((key-backspace #\del #\bs);; delete backwards
                       (rec-edit (-1>0 strpos)
                                 (string-append
                                  (substring line
                                             0
                                             (-1>0 strpos))
                                  (substring line
                                             (+1< strpos len)
                                             len))))
                      ((#\np) (begin (redraw) ;; ctrl-l
                                     (rec-edit strpos
                                               line)))
                      ((#\etx) (make-command-string "quit"));; ctrl-c
                      ((#\sub) (make-command-string "stop"));; ctrl-z
                      (else (if (char? c);; entered char
                                (rec-edit (+1< strpos width)
                                          (string-append
                                           (substring line
                                                      0
                                                      strpos)
                                           (make-string 1 c)
                                           (substring line
                                                      (+1< strpos len)
                                                      len)))
                              (rec-edit strpos
                                        line))))))))))
          (rec-edit 0 "")))

;;; Some list functions
(define (appendmax max . args)
  "Append the arguments and then truncate the resulting list to <max>
  elements"
  (let ((lst (apply append args)))
    (if (> (length lst) max)
        (list-head lst max)
      lst)))

;;; A few commands that are needed or useful outside of parse-user-input

;;; Counterparts to the unix commands true and false:
(define (true)
  "true - do nothing, successfully"
  #t)

(define (false)
  "false - do nothing, unsuccessfully"
  #f)

;;; Redraw all:
(define redraw
  (lambda ()
    (set! REDRAWLINES #t)
    (set! REDRAWEDIT #t)))

;;; Parser for entered line, checks for command-character at the start
;;; of the line
(define (parse-user-input line sock nick)
  "Return a thunk, based on the users command"
  ;;very basic commands (operating on the whole line or no args):
  (if (string-null? line)
      true;; do nothing
    (let* ((command-end (string-index line #\space))
           (command-args (string-split line #\space))
           (command (substring (car command-args)
                               1
                               (string-length (car command-args))))
           (args (cdr command-args))
           (rest-from
            (lambda (n)
              (let ((joined (fold-right (lambda (x y)
                                          (string-append x
                                                         " "
                                                         y))
                                        ""
                                        (list-cdr-ref args
                                                      n))))
                (substring joined
                           0
                           (1- (string-length joined))))))
                  ;;; COMMANDS:
           (help
            (lambda ()
              (send-msg nick
                        (string-append
                         "*** Known commands: help, quit, stop, msg "
                         "<nick> <text>, login, logoff, fakemsg "
                         "<from> <to> <text>, fakepub <from>, names")
                        nick
                        nick)))
           (quitchat
            (lambda () (set! FINISHED #t)))
           (stopchat
            (lambda () (kill (getpid) SIGSTOP)))
           (sendpub
            (lambda ()
              (send-public nick line)))
           (sendmsg
            (lambda ()
              (send-msg nick
                        (rest-from 1)
                        nick
                        (car args))))
           (logmein
            (lambda ()
              (login nick PASSWORD)))
           (logmeoff
            (lambda ()
              (logoff nick)))
           (fakemsg
            (lambda ()
              (send-msg (car args)
                        (rest-from 2)
                        (car args)
                        (cadr args))))
           (fakepub
            (lambda ()
              (send-msg (car args)
                        (rest-from 1)
                        (car args)
                        default-to-arg)))
           (names
            (lambda ()
              (send-msg nick
                        (format #f "*** Logged in Users: ~a"
                                (users))
                        nick
                        nick))))
      (if (char=? (string-ref line 0)
                  command-c)
          (cond
           ((string=? command "")
            sendpub)                    ; Line started with <command-c>#\space
           ((string=? command "quit")
            quitchat)
           ((or (string=? command "suspend")
                (string=? command "stop"))
            stopchat)
           ((string=? command "msg")
            sendmsg)
           ((string=? command "logoff")
            logmeoff)
           ((string=? command "login")
            logmein)
           ((string=? command "fakemsg")
            fakemsg)
           ((string=? command "fakepub")
            fakepub)
           ((string=? command "help")
            help)
           ((or (string=? command "names")
                (string=? command "users"))
            names)
           (else false))
        ;; no command, send the line
        sendpub))))

;;; Main

(define (main argl)
  ;;; FIXME: Parse command line arguments
  (if (not (defined? 'call-with-new-thread))
      (error "Please reconfigure guile with --with-threads and
      recompile and install!"))
  (letrec ((sock '())
           (nick (get-nick))
           (line "")
           (stdscr #f)
           (typesize 1)
           (COLS 80)
           (LINES 24)
           (scrollwin #f)
           (typewin #f)
           (winchhandler
            (lambda (x)
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
              (redraw)
              (unlock-mutex sync-mutex)))
           (ignore-all-signals
            (lambda ()
              (sigaction SIGCONT SIG_IGN)
              (sigaction SIGINT SIG_IGN)
              (sigaction SIGQUIT SIG_IGN)
              (sigaction SIGWINCH SIG_IGN)))
           (aborthandler
            (lambda (x) (set! FINISHED #t)))
           (conthandler
            (lambda (x) (set-handlers)))
           (set-handlers
            (lambda ()
              (sigaction SIGCONT conthandler)
              (sigaction SIGINT aborthandler)
              (sigaction SIGQUIT aborthandler)
              (sigaction SIGWINCH winchhandler)))
           (setup
            (lambda ()
              (set! stdscr (initscr))
              (ignore-all-signals)
      ;;; some ncurses setup
              ;; (cbreak)
              (raw);; We parse the control characters!
              (noecho)
              (nonl)
              (winchhandler #f);; Call the handler once to get
              ;; the screen size and setup the windows.
              (scrollok stdscr #f)
              (scrollok scrollwin #t)
              (scrollok typewin #f)
              (leaveok stdscr #t)
              (leaveok scrollwin #t)
              (leaveok typewin #f)
              (keypad typewin #t)
              (nodelay typewin #t)
              (clear)
              (set-handlers)))
           (trylogin
            (lambda ()
              (if (not DEBUGGING)
                  (begin
                   (while
                       (not (login
                             nick
                             (begin (set! PASSWORD (getpass (format #f
                                                                    "Password for ~a: "
                                                                    nick)))
                                    PASSWORD)))
                     (display "Sorry, Try again!")
                     (newline))
                   (send-admin-msg
                    (string-append nick
                                   " has just logged in"))))))
           (makescroller
            (lambda ()
              (begin-thread
               (let loop ((lines '())
                          (newlines (get-new-lines sock
                                                   nick
                                                   '())))
                    (let ((drawlines (if REDRAWLINES
                                         (begin
                                          (werase scrollwin)
                                          (wmove scrollwin 0 0)
                                          (set! REDRAWLINES #f)
                                          (appendmax LINES
                                                     newlines
                                                     lines))
                                       newlines)))
                      (lock-mutex sync-mutex)
                      (for-each (lambda (line)
                                  (waddstr scrollwin
                                           (make-string 1
                                                        #\newline))
                                  (waddstr scrollwin line))
                                (reverse drawlines))
                      (wrefresh scrollwin)
                      (unlock-mutex sync-mutex)
                      (sleep 2)
                      (if (not FINISHED)
                          (let ((oldlines (appendmax (* 2 LINES)
                                                     newlines
                                                     lines)))
                            (loop oldlines
                              (get-new-lines sock
                                             nick
                                             oldlines))))))))))
          (trylogin)
          (setup)
          (let ((scroller (makescroller)))
            ;; Screen update as a new thread:
            (let loop ()
                 (lock-mutex sync-mutex)
                 (wclear typewin)
                 (wmove typewin 0 0)
                 (wstandout typewin)
                 (waddstr typewin nick)
                 (wstandend typewin)
                 (waddstr typewin " >> ")
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
                          (logoff nick))
                      (endwin)
                      (primitive-exit))
                   (loop))))))

;;; $Log: chat.scm,v $
;;; Revision 1.14  2003/04/15 12:13:09  friedel
;;; Another bug in parse-user-input
;;;
;;; Revision 1.13  2003/04/14 22:38:00  friedel
;;; Uhm, a bug in parse-user-input ((letrec) instead of (let*)) sneaked in
;;; during "cleanups" :-}
;;;
;;; Revision 1.12  2003/04/14 22:36:08  friedel
;;; Cleaned up, added docstrings to most functions
;;;
;;; Revision 1.11  2003/04/13 22:11:48  friedel
;;; cleaned up drop-before-match and connect-chat
;;;
;;; Revision 1.10  2003/04/12 01:12:26  friedel
;;; Edit area is refreshed, too. Small cleanups.
;;;
;;; Revision 1.9  2003/04/11 22:25:51  friedel
;;; Make resize do something useful: redraw the text area
;;;
;;; Revision 1.8  2003/04/11 19:43:57  friedel
;;; Reduced CPU time now... usleep was in the wrong place *Wheee!* :)
;;;
;;; Revision 1.7  2003/04/11 13:25:45  friedel
;;; Hm, fixed some bugs. Now it's possible to have a normal chat.
;;;
;;; Revision 1.6  2003/04/11 13:03:47  friedel
;;; First version with all the basic features. Next step: cleaning up.
;;;
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
