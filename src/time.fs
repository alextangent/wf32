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

16 constant TIME-LEN

next-user dup @ aligned swap !

time-len +user TIME-BUF
        \ +0  year
        \ +2  month
        \ +4  day of week
        \ +6  day of month
        \ +8  hour
        \ +10 minute
        \ +12 second
        \ +14 milliseconds

32 +user date$
32 +user time$

1 import: GetLocalTime

: get-local-time ( -- )                 \ get the local computer date and time
        time-buf GetLocalTime drop ;

create compile-version time-len allot   \ a place to save the compile time (global)

get-local-time                          \ save as part of compiled image

time-buf compile-version time-len move  \ move time into buffer

: time&date     ( -- sec min hour day month year )
                get-local-time
                time-buf 12 + w@        \ seconds
                time-buf 10 + w@        \ minutes
                time-buf  8 + w@        \ hours
                time-buf  6 + w@        \ day of month
                time-buf  2 + w@        \ month of year
                time-buf      w@ ;      \ year

: .#"           ( n1 n2 -- a1 n3 )
                >r 0 <# r> 0 ?do # loop #> ;

6 import: GetDateFormat

: gdf ( -- )
    null LOCALE_USER_DEFAULT
    GetDateFormat date$ swap 1- ;

: >date"        ( time_structure -- )
                >r 31 date$ null \ z" ddddd',' MMMM dd yyyy"
                r> gdf ;

: .date         ( -- )
                get-local-time time-buf >date" type ;

: >month,day,year" ( time_structure -- )
                >r 31 date$  z" ddddd',' MMMM dd yyyy"
                r> gdf ;


: .month,day,year ( -- )
                get-local-time time-buf >month,day,year" type ;

6 import: GetTimeFormat

: gtf ( -- )
    null LOCALE_USER_DEFAULT
    GetTimeFormat time$ swap 1- ;

: >time"        ( time_structure -- )
                >r 31 time$ null
                r> gtf ;

: .time         ( -- )
                get-local-time time-buf >time" type ;

: >am/pm"       ( time_structure -- )
                >r 31 time$ z" h':'mmtt"
                r> gtf ;

: .am/pm        ( -- )
                get-local-time time-buf >am/pm" type ;

: .cversion     ( -- )
                compile-version dup >date" type
                              space >time" type ;

: ms@           ( -- ms )
                get-local-time
                time-buf
                dup   8 + w@     60 *           \ hours
                over 10 + w@ +   60 *           \ minutes
                over 12 + w@ + 1000 *           \ seconds
                swap 14 + w@ + ;                \ milli-seconds

0 value start-time

: time-reset    ( -- )
                ms@ to start-time ;

' time-reset alias timer-reset

: .elapsed      ( -- )
                ." elapsed time: "
                ms@ start-time -
                1000 /mod
                  60 /mod
                  60 /mod 2 .#" type ." :"
                          2 .#" type ." :"
                          2 .#" type ." ."
                          3 .#" type ;

: elapse        ( -<commandline>- )
                time-reset interpreter cr .elapsed ;
