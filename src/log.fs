\ --------------------------- Change Block -------------------------------
\
\
\ ------------------------- End Change Block -----------------------------
\
\ Experimental: a fully optimising, STC based, ANS Forth compliant kernel
\
\
\    Copyright [c] 2005, 2017 by Alex McDonald (alex at rivadpm dot com)
\
\    Parts copyright others; borrowed from public domain Win32Forth 
\    and heavily modified.
\
\ ------------------------------------------------------------------------
\ $Id: $
\ History.f G.Hubert Sunday, November 19 2006
\ A short file to store the console ouput for the STC version to make it
\ easier to save error messages, timings code, etc.

internal

0 value log-file

defer std-type
defer std-accept

: log-write ( addr len -- )  log-file write-file abort" writing log" ;
: log-type  ( addr len -- )  2dup std-type log-write ;

: log-accept     ( addr1 n1 -- n2 )
               action-of std-type is type
               over swap std-accept
               tuck log-write
               ['] log-type is type ;
               
' type   is std-type
' accept is std-accept

defer save-type

: log-enter    ( -- )
               action-of std-type is save-type
               ['] 2drop is std-type ;
: log-exit     ( -- )
               action-of save-type is std-type ;
external

: log-dtstamp  ( -- )
               ." | " .date space .time ."  | "  ;

: log          ( addr len -- )
               log-enter
               cr log-dtstamp type
               log-exit ;

: end-log  ( -- )
               log-file if
                 s" --- End of log ---" log
                 log-file close-file abort" cannot close log"
                 action-of std-type   is type
                 action-of std-accept is accept
               then
               0 to log-file ;

create log-name maxbuffer allot
               s" log.txt" log-name place
               
: start-log ( -- )
               end-log
               log-name count r/w open-file if
                 drop log-name count r/w create-file abort" cannot create log"
               else
                 dup file-append drop
               then to log-file
               action-of type is std-type
               ['] log-type     is type
               action-of accept is std-accept
               ['] log-accept   is accept
               log-enter
               cr log-dtstamp ." --- Start of log ---"
               cr log-dtstamp ." program name: " program-name type
               cr log-dtstamp ." compiled:     " .cversion
               cr log-dtstamp ." version:      " .version
               cr log-dtstamp ." ---"
               log-exit
               ;

: clear-log ( -- )
               end-log
               log-name count delete-file abort" failed to clear log"
               start-log ;

' start-log  init-chain   chain+
start-log

module

