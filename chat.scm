#!/bin/sh
exec guile --debug -e main -s $0 $@
!#
;;; $Id: chat.scm,v 1.29 2003/04/27 10:44:46 friedel Exp friedel $
;;; There's no documentation. But the changelog at the bottom of the
;;; file should give useful hints.

;;; A little configuration:
(define REFRESH-LINES-TIMEOUT 5) ; Lines will be refreshed every n
                                        ; seconds
(define GET-NEW-LINE-WAIT 100000) ; 1/10 s ; usecs to wait before the
                                        ; sent line is expected to be
                                        ; on the  server
(define NEXT-CHARACTER-SLEEP 5000) ; 5/1000 s ; usecs to wait before next
                                        ; character is accepted

;;; Global constants (should not be modified)
(define admin-nick "Administrator")

(define default-from-arg "")
(define default-to-arg "All")
(define default-rand-arg "0")

(define info-nick "***")

(define server-port 80)

(define server-url "www.roskilde-festival.dk")
(define base-url "/community/chat/php/")
(define send-url "RFChatSendMessages.php")
(define read-url "RFChatMessages.php")
(define online-url "RFChatOnline.php")
(define login-url "RFChatLogin.php")
(define logoff-url "RFChatRemoveOnline.php")
(define userlist-url "RFChatPrivate.php")

(define random-arg "random")
(define name-arg "name")
(define message-arg "message")
(define from-arg "messageFrom")
(define to-arg "messageTo")
(define login-arg "login")
(define password-arg "password")

(define message-re "^&message=")

(define command-c #\/) ; Use / as start of commands

;;; End of constants

(use-modules (srfi srfi-1)
             (ice-9 regex)
             (ice-9 threads)
             (ice-9 format)
             (ice-9 getopt-long))

;;; Get options, they won't change:
(define OPTIONS (getopt-long (command-line)
                             '((nologin (single-char #\n))
                               (nologfile (single-char #\L))
                               (logfile (single-char #\l)
                                        (value #t)))))

;;; GLOBAL VARIABLES! EVIL! :)
;; parsed options:
(define LOGFILE (string-append  "." ;(getenv "HOME")
                                "/roskilde-chat.log"))
(define SHOULD-LOGIN #t)

(define mutex-sync (make-mutex)) ; Global mutex for non-threadsafe
                                        ; operations
(define FINISHED #f)   ; it #t, client will terminate
(define REDRAWEDIT #f) ; if #t, editing area is fully redrawn
(define REDRAWLINES #f); if #t, text area is fully redrawn
(define PASSWORD "")   ; we have to memorize the password because we
                                        ; need to re-authenticate
(define cutbuffer "")  ; cutbuffer for the editor

(define cond-ready (make-condition-variable)) ; condition to update
                                        ; the scroller window
(define mutex-ready (make-mutex)) ; mutex for the above condition

;;; End of variables

(dynamic-link "libncurses.so")
(load-extension "./guile-ncurses.so" "init_curses")

;;; Functions for building strings

(define (char->string c)
  (make-string 1 c))

(define (make-command-string text)
  "Prepends command-character to the string"
  (string-append (char->string command-c)
                 text))

(define (url-encode text)
  "Cheap url encoding (makes hex out of everything :)"
  ;; lock, because of format
  (let ((hexlist (with-mutex mutex-sync
                             (map (lambda (c)
                                    (format #f "%~x" (char->integer c)))
                                             (string->list text)))))
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
                      (error
                       "List must have even number of elements!")
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
                   random-arg  (chat-random)
                   name-arg (url-encode nick)
                   message-arg (url-encode msgstring)
                   to-arg (url-encode to)
                   from-arg (url-encode from)))

;;; FIXME: New parameter: timestamp for optional Only-If-Newer Header
(define (http-get-request host url)
  "Build a http-request for url on host"
  (string-append "GET "
                 url
                 " HTTP/1.0\r\n"
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
                 name-arg (url-encode nick)))

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

;;; format is not thread safe, grmbl!
;;; got the following from
;;; http://mail.gnu.org/archive/html/bug-guile/2002-11/msg00001.html
(define safe-format
  (let ((m (make-mutex)))
    (lambda args
      (with-mutex m
                  (apply format args)))))
;;; This definition is not used right now, i decided to use
;;; simple-format where possible and thereby reduced the use of
;;; (format) to one thread.

(define (get-connector server-name port)
  "Return a thunk that will return a connected socket to
  <port> on <server-name>"
  (let* ((entry (gethostbyname server-url))
         (addrs (array-ref entry 4))
         (addr (car addrs)))
    (letrec ((connectme
              (lambda ()
                (let ((sock (socket AF_INET SOCK_STREAM 0)))
                  (if (connect sock AF_INET addr port)
                      (begin
                       (setvbuf sock _IOLBF)
                       sock)
                    (error "Could not connect!")))))
             (handler
              (lambda (key . args)
                (simple-format (current-error-port)
                        "Warning: ~s failed.~%"
                        (car args))
                (simple-format (current-error-port)
                        (cadr args)
                        (cddr args))
                (newline (current-error-port))))
             (connector
              (lambda () (catch 'system-error
                           connectme
                           handler))))
            connector)))

(define connect-chat (get-connector server-url server-port))

(define (send-to-chatserver request)
  "Send request to chatserver, ignore the response"
  (let ((sock (connect-chat)))
    (display request sock)
    (flush-response sock)))

(define (get-from-chatserver request regex)
  "Send get-request to chatserver, return the list of response lines,
  filtered by drop-before-match"
  (let ((sock (connect-chat))
        (response #f))
    (display request sock)
    (set! response (get-response sock regex))
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
          #t ; while will always return #t anyways!
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
                                                     random-arg (chat-random)
                                                     login-arg nick
                                                     password-arg pw)
                                      "^&entry="))))

(define (logoff nick)
  "Remove <nick> from Chat"
  (send-to-chatserver (http-get-args server-url
                                     base-url logoff-url
                                     random-arg (chat-random)
                                     login-arg nick)))

(define (users)
  "Get list of users from chatserver"
  (let* ((rawlist (car (get-from-chatserver (http-get-args server-url
                                                           base-url
                                                           userlist-url
                                                           random-arg
                                                           (chat-random))
                                            "^&users=")))
         (cutraw (substring rawlist 0 (1- (string-length rawlist)))))
    (delete1! "-1"
            (delete1! default-to-arg
                    (string-split cutraw #\,)))))

(define (logged-in? nick)
  "Return #t if nick is logged in, #f otherwise."
  (if (member nick (users))
      #t
    #f))

(define (numusers)
  "Return number of logged in users"
  (string->number (car (get-from-chatserver (http-get-args server-url
                                                           base-url
                                                           online-url
                                                           random-arg
                                                           (chat-random))
                                            "^&amount="))))

(define (get-first-line sock message-re)
  "Get first line containing text matching <message-re> (with the
  match removed)"
  (let loop ((answer #f)
             (matcher #f))
       (cond ((eof-object? answer) #f); Failure to match
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
    response))

(define (get-nick)
  "Get the nick to connect as"
  (let ((cmdargs (option-ref OPTIONS '() #f)))
    (if cmdargs
        (car cmdargs)
      (begin
       (display "Nick: ")
       (read-line)))))

(define ->
  (lambda (x dec min)
    (let ((newx (- x dec)))
      (if (> newx min)
          newx
        min))))

(define -1>0
  (lambda (x)
    (-> x 1 0)))

(define +<
  (lambda (x inc max)
    (let ((newx (+ x inc)))
      (if (< newx max)
          newx
        max))))

(define +1<
  (lambda (x max)
    (+< x 1 max)))

(define history-access
  (let ((history '())                   ; history for the editor
        (pos 0))                        ; position
    (lambda (direction line)
      (let* ((newhist (if (not (eq? direction 'reset))
                          (if (zero? pos)
                              (cons line (delete line history))
                            (newinsert line history pos))
                        history))
             (newlen (length newhist))
             (added (not (= (length history)
                            newlen))))
        (set! history newhist)
        (if (not (null? history))
            (case direction
              ((reset)
               (set! pos 0))
              ((add)
               (set! pos 0)
               line)
              ((up)
               (if (not added)
                   (set! pos (+1< pos (-1>0 newlen)))
                 (set! pos (+< pos 2 (-1>0 newlen))))
               (list-ref history pos))
              ((down)
               (if (not added)
                   (set! pos (-1>0 pos)))
               (list-ref history pos))
              (else (list-ref history pos)))
          line)))))

(define (enter-string window)
  "Read a string from a line in an ncurses window"
  (letrec ((startpos (getyx window))
           (width (getmaxx window))
           (textlen (* 5 (getmaxx window)))
           (rec-edit; the infamous recursive editor function
            (lambda (strpos line)
              (usleep NEXT-CHARACTER-SLEEP)
              (let* ((len (string-length line))
                     (textwidth (- width (cdr startpos)))
                     (partnums 7); line is partitioned for scrolling
                     (leaveoff (- partnums 1))
                     (linepart (/ 1 partnums))
                     (partwidth (inexact->exact (* textwidth linepart)))
                     (parts (inexact->exact (/ strpos
                                               partwidth)))
                     (linepos (if (>= parts
                                      leaveoff)
                                  (* (- parts leaveoff)
                                     partwidth)
                                0))
                     (x (- (+ strpos
                              (cdr startpos))
                           linepos))
                     (prevwordstart (lambda ()
                                      (+1< (or (string-rindex line
                                                              #\space
                                                              0
                                                              (-1>0 strpos))
                                               -1)
                                           len)))
                     (afterword (lambda ()
                                  (or (string-index line
                                                    #\space
                                                    (+1< strpos len))
                                      len))))
                (with-mutex mutex-sync
                            ;; Draw the line
                            (wmove window (car startpos) (cdr startpos))
                            (waddstr window (substring line
                                                       linepos
                                                       (+< (1- width)
                                                           linepos
                                                           len)))
                            (wclrtoeol window)
                            (wmove window 0 x)
                            (wrefresh window))
                (let ((c (wgetch window))); get a character
                  (if (or FINISHED
                          (equal? c #\cr)); line complete
                      (history-access 'add line)
                    (case c; examine character
                      ((#\eot key-dc); delete forwards
                       (rec-edit strpos
                                 (string-append
                                  (substring line
                                             0
                                             strpos)
                                  (substring line
                                             (+1< strpos len)
                                             len))))
                      ((key-backspace #\del #\bs); delete backwards
                       (rec-edit (-1>0 strpos)
                                 (string-append
                                  (substring line
                                             0
                                             (-1>0 strpos))
                                  (substring line
                                             strpos
                                             len))))
                      ((#\np) (begin (redraw); ctrl-l
                                     (rec-edit strpos
                                               line)))
                      ((key-up #\dle) (let ((newline (history-access
                                                      'up
                                                      line)))
                                        (rec-edit (string-length
                                                   newline)
                                                  newline)))
                      ((key-down #\so) (let ((newline (history-access
                                                       'down
                                                       line)))
                                         (rec-edit (string-length
                                                    newline)
                                                   newline)))
                      ((key-left #\stx) (rec-edit (-1>0 strpos); left
                                                  line))
                      ((key-right #\ack) (rec-edit (+1< strpos len)
                                                   line));right
                      ((#\nak) (begin (set! cutbuffer line)
                                      (rec-edit 0 ""))); ctrl-u
                      ((#\vt) (begin (set! cutbuffer; ctrl-k
                                           (substring line
                                                      strpos
                                                      len))
                                     (rec-edit strpos
                                               (substring line
                                                          0
                                                          strpos))))
                      ((#\em) (rec-edit (+ strpos; ctrl-y
                                           (string-length cutbuffer))
                                        (string-append (substring line
                                                                  0
                                                                  strpos)
                                                       cutbuffer
                                                       (substring line
                                                                  strpos
                                                                  len))))
                      ((#\etx) (make-command-string "quit")); ctrl-c
                      ((#\sub) (make-command-string "stop")); ctrl-z
                      ((#\soh) (rec-edit 0; ctrl-a
                                         line))
                      ((#\enq) (rec-edit len; ctrl-e
                                         line))
                      ((#\esc); esc prefix keymap:
                                        ; (or Meta)
                       (case (wgetch window)
                         ((#\f key-right); next word
                          (rec-edit (afterword)
                                    line))
                         ((#\b key-left); previous word
                          (rec-edit (prevwordstart)
                                    line))
                         ((#\d key-dc); delete next word
                          (begin
                           (set! cutbuffer
                                 (substring line
                                            strpos
                                            (afterword)))
                           (rec-edit
                            strpos
                            (string-append (substring line
                                                      0
                                                      strpos)
                                           (substring line
                                                      (afterword)
                                                      len)))))
                         ((key-backspace #\del #\bs);del prevword
                          (begin
                           (set! cutbuffer
                                 (substring line
                                            (prevwordstart)
                                            strpos))
                           (rec-edit (prevwordstart)
                                     (string-append (substring
                                                     line
                                                     0
                                                     (prevwordstart))
                                                    (substring line
                                                               strpos
                                                               len)))))
                         (else (rec-edit strpos line))))
                      (else (if (char? c); entered char
                                (rec-edit (+1< strpos textlen)
                                          (string-append
                                           (substring line
                                                      0
                                                      strpos)
                                           (char->string c)
                                           (substring line
                                                      strpos
                                                      len)))
                              (rec-edit strpos
                                        line))))))))))
          (rec-edit 0 "")))


(define (append-log-maybe lines)
  "Append the list of lines to the logfile, if a log should be written"
  (if LOGFILE
      (let ((logport (open LOGFILE
                           (logior O_WRONLY O_APPEND O_CREAT)
                           #o600)))

        (for-each (lambda (line)
                    (simple-format logport
                                   "~a~%"
                                   line))
                  lines)
        (close logport))
    #t))

;;; Some list functions
(define (appendmax max . args)
  "Append the arguments and then truncate the resulting list to <max>
  elements"
  (let ((lst (apply append args)))
    (if (> (length lst) max)
        (list-head lst max)
      lst)))

(define (newinsert elt lst pos)
  "Insert new (!) element into list at pos (used by history)"
  (if (member elt lst)
      lst
    (append (list-head lst pos)
            (cons elt (list-tail lst pos)))))

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
  (if (string-null? line)
      true; do nothing
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
              (send-msg info-nick
                        (string-append
                         "Known commands: help, quit, stop, msg "
                         "<nick> <text>, login [password], logoff, fakemsg "
                         "<from> <to> <text>, fakepub <from>, names")
                        info-nick
                        nick)))
           (quitchat
            (lambda () (set! FINISHED #t)))
           (stopchat
            (lambda () (raise SIGSTOP)))
           (sendpub
            (lambda ()
              (send-public nick line)))
           (sendrest
            (lambda ()
              (send-public nick (rest-from 0))))
           (sendmsg
            (lambda ()
              (send-msg nick
                        (rest-from 1)
                        nick
                        (car args))))
           (logmein
            (lambda ()
              (if (login nick (if (not (null? args))
                                  (rest-from 0)
                                PASSWORD))
                  (set! SHOULD-LOGIN #t))))
           (logmeoff
            (lambda ()
              (set! SHOULD-LOGIN #f)
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
              (send-msg info-nick
                        (simple-format #f "Logged in Users: ~a"
                                       (users))
                        info-nick
                        nick))))
      (if (char=? (string-ref line 0)
                  command-c)
          (cond
           ((string=? command "")
            sendrest) ; Line started with <command-c>#\space
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
  ;;; Parse options (other than nick)
  (set! SHOULD-LOGIN (not (option-ref OPTIONS 'nologin #f)))
  (set! LOGFILE (and (not (option-ref OPTIONS 'nologfile #f))
                     (option-ref OPTIONS
                                 'logfile
                                 LOGFILE)))
  (if (and LOGFILE
           (not (or (access? LOGFILE
                             (logior W_OK
                                     F_OK))
                    (access? (dirname LOGFILE)
                             W_OK))))
      (error (format #f "No write permission on logfile ~a.~% Bailing out!~%"
                     LOGFILE)))
  (letrec ((sock '())
           (nick (get-nick))
           (line "")
           (stdscr #f)
           (typesize 1)
           (COLS 80)
           (LINES 24)
           (scrollwin #f)
           (typewin #f)
           (alarmhandler
            (lambda (x)
              #t))
           (winchhandler
            (lambda (x)
              (ignore-all-signals)
              (with-mutex mutex-sync
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
                          (redraw))
              (set-handlers)))
           (aborthandler
            (lambda (x)
              (ignore-all-signals)
              (set! FINISHED #t)))
           (conthandler
            (lambda (x)
              (setup)))
           (ignore-all-signals
            (lambda ()
              (sigaction SIGCONT SIG_IGN)
              (sigaction SIGINT SIG_IGN)
              (sigaction SIGQUIT SIG_IGN)
              (sigaction SIGALRM SIG_IGN)
              (sigaction SIGWINCH SIG_IGN)))
           (set-handlers
            (lambda ()
              (sigaction SIGCONT conthandler)
              (sigaction SIGINT aborthandler)
              (sigaction SIGQUIT aborthandler)
              (sigaction SIGWINCH winchhandler)
              (sigaction SIGALRM alarmhandler)))
           (setup
            (lambda ()
              (set! stdscr (initscr))
              (ignore-all-signals)
      ;;; some ncurses setup
              ;; (cbreak)
              (raw)                     ; We parse the control characters!
              (noecho)
              (nonl)
              (winchhandler #f)         ; Call the handler once to get
                                        ; the screen size and setup the windows.
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
              (if SHOULD-LOGIN
                  (begin
                   (while
                       (not (login
                             nick
                             (begin
                              (set! PASSWORD
                                    (getpass
                                     (simple-format #f
                                                    "Password for ~a: "
                                                    nick)))
                              PASSWORD)))
                     (display "Sorry, Try again!")
                     (newline))
                   (send-admin-msg
                    (string-append nick
                                   " has just logged in"))))))
           ;; Screen update as a new thread:
           (makescroller
            (lambda ()
              (begin-thread
               (let loop ((lines '())
                          (newlines (get-new-lines sock
                                                   nick
                                                   '())))
                    (append-log-maybe (reverse newlines))
                    (let ((drawlines (if REDRAWLINES
                                         (begin
                                          (werase scrollwin)
                                          (wmove scrollwin 0 0)
                                          (set! REDRAWLINES #f)
                                          (appendmax LINES
                                                     newlines
                                                     lines))
                                       newlines)))
                      (with-mutex mutex-sync
                                  (for-each
                                   (lambda (line)
                                     (waddstr scrollwin
                                              (char->string #\newline))
                                     (waddstr scrollwin line))
                                   (reverse drawlines))
                                  (wrefresh scrollwin))
                      (if (and SHOULD-LOGIN
                               (not (logged-in? nick)))
                          (login nick PASSWORD))
                      (wait-condition-variable cond-ready
                                               mutex-ready)
                      (if (not FINISHED)
                          (let ((oldlines (appendmax (* 2 LINES)
                                                     newlines
                                                     lines)))
                            (loop oldlines
                              (get-new-lines sock
                                             nick
                                             oldlines)))
                        (begin
                         (with-mutex
                          mutex-sync
                          (waddstr scrollwin
                                   (char->string #\newline))
                          (waddstr scrollwin
                                   "Exiting chat. Please wait a moment...")
                          (wrefresh scrollwin))))))))))
          (simple-format #t "Currently logged in: ~a users.~%" (numusers))
          (trylogin)
          (append-log-maybe (list
                             (simple-format #f
                                            "Log started at ~a~%"
                                            (current-date-and-time))))
          (setup)
          (let ((scroller (makescroller))
                (sleeper (begin-thread
                          (let loop ()
                               (signal-condition-variable cond-ready)
                               (yield)
                               (sleep REFRESH-LINES-TIMEOUT)
                               (if (not FINISHED)
                                   (loop))))))
            (let loop ()
                 (with-mutex mutex-sync
                             (wclear typewin)
                             (wmove typewin 0 0)
                             (wstandout typewin)
                             (waddstr typewin nick)
                             (wstandend typewin)
                             (waddstr typewin " >> ")
                             (wrefresh typewin))
                 (set! line (enter-string typewin))
                 ((parse-user-input line sock nick)) ; returns a thunk
                                        ; that is executed immediately
                 ;;  Wake up the sleeping scroller (we may have just
                 ;;  created a new line)
                 (usleep GET-NEW-LINE-WAIT)
                 (signal-condition-variable cond-ready)
                 (yield)
                 (if FINISHED
                     (begin
                      (with-mutex mutex-sync
                                  (wclear typewin)
                                  (waddstr
                                   typewin
                                   "Exiting chat. Please wait a moment...")
                                  (wrefresh typewin))
                      (join-thread scroller)
                      (join-thread sleeper)
                      (restore-signals)
                      (if SHOULD-LOGIN
                          (logoff nick))
                      (endwin)
                      (append-log-maybe
                       (list
                        (simple-format #f
                                       "Log ended at ~a~%"
                                       (current-date-and-time))))
                      (primitive-exit))
                   (loop))))))

;;; $Log: chat.scm,v $
;;; Revision 1.29  2003/04/27 10:44:46  friedel
;;; Entering a line causes immediate screen update,
;;; corrected comment conventions,
;;; exchanged (lock-mutex) (unlock-mutex) pairs with (with-mutex) macro,
;;; misc small changes.
;;;
;;; Revision 1.28  2003/04/26 11:25:37  friedel
;;; made lines refresh time a constant, moved auto-relogin from input to
;;; scroller (Doh!)
;;;
;;; Revision 1.27  2003/04/25 23:20:30  friedel
;;; beautified info display
;;;
;;; Revision 1.26  2003/04/24 20:13:01  friedel
;;; A few more editor commands (with esc-prefix or meta)
;;;
;;; Revision 1.25  2003/04/22 15:12:15  friedel
;;; Fixed editor behaviour when going back (with key-left or ctrl-b),
;;; history jumps to end of line, sanitized history behaviour (a little)
;;;
;;; Revision 1.24  2003/04/22 14:46:34  friedel
;;; Working history (more or less the behaviour i'd expect)
;;;
;;; Revision 1.23  2003/04/21 19:23:39  friedel
;;; Hopefully fixed the crash in format
;;;
;;; Revision 1.22  2003/04/21 18:47:33  friedel
;;; Make /login take optional password arguments :)
;;;
;;; Revision 1.21  2003/04/21 18:39:10  friedel
;;; Improved line Editor (more function keys work)
;;;
;;; Revision 1.20  2003/04/21 13:04:19  friedel
;;; Make editor scroll on one line
;;;
;;; Revision 1.19  2003/04/21 12:07:42  friedel
;;; User is automatically logged in when the timeout kicks in
;;;
;;; Revision 1.18  2003/04/21 11:42:40  friedel
;;; Logfile support. New command-line switches: --nologfile (or -L)
;;;                                     and --logfile <logfile> (or
;;;                                     -l)
;;;
;;; Revision 1.17  2003/04/15 22:22:25  friedel
;;; command-line switch --nologin (or -n) instead of DEBUGGING Parameter
;;;
;;; Revision 1.16  2003/04/15 17:47:36  friedel
;;; Make get-connector be a little less smart and just give out new
;;; sockets every time (which will be garbage-collected eventually). This
;;; removes the need for mutex locks around network io (because multiple
;;; sockets can be used)
;;;
;;; Revision 1.15  2003/04/15 14:46:09  friedel
;;; Added commands: help, login, logoff, fakemsg, fakepub
;;;
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
