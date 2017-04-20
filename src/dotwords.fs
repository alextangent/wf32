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
\    Redistribution and use in source and binary forms, with or without
\    modification, are permitted provided that the following conditions are
\    met:
\
\    1. Redistributions of source code must retain the above copyright
\    notice, this list of conditions and the following disclaimer.
\
\    2. Redistributions in binary form must reproduce the above copyright
\    notice, this list of conditions and the following disclaimer in the
\    documentation and/or other materials provided with the distribution.
\
\    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
\    IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
\    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
\    PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
\    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
\    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
\    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
\    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
\    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (
\    INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
\    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. ))
\
\
\
\ ------------------------------------------------------------------------

cr .( Loading dot words... ) sourcefilename type
  
internal

\ Filtering is available on some .<name> words.

\ This filter is case insensitive and simply tests for the existence of a substring in the
\ passed string. More sophisticated filters might be possible (grep, anyone?)

create filter-str 24 allot                       \ there's a limit to this...

: set-filter    ( -- <filter> )
                parse-name 22 min filter-str place
                filter-str lowercase drop ;
                
: filter-match? ( addr len -- flag )            \ case insensitive substring filter
                filter-str count dup if
                  2swap buf-allot dup>r place
                  r> lowercase count
                  2swap search nip nip
                else 4drop true then
                ;

: (displayed)   ( total matched addr len -- )
                2swap ." displayed " #. ." of " #. type ;

: count-voc-words   ( wid -- n )
    0 [: drop 1+ true ;] rot >body traverse-wordlist ;

: count-words   ( -- n1 )
    0 voc-link @
    [: voc.xt @ count-voc-words + ;]
    list-apply ;
                                                                  
: isvoc?        ( cfa -- f )
                >name head.tfa c@ tvoc = ;

external

: .vocs  ( -- ) \ display vocabularies
    cr ." Vocabulary     Threads    Words   Average" underline
    0 voc-link @
    [: dup voc.xt @ isvoc? if       \ don't look through classes
         cr
         dup voc.xt @ .name  18 #tab
         dup voc#threads dup>r 4 .r
         voc.xt @ count-voc-words tuck
         dup 9 .r 10 * r> / 10 .r.1
         +
       else drop then
    ;] list-apply underline
    cr ." Total" 26 .r ;

: .libs         ( -- )
    cr ." Location  Handle    Name" underline
    lib-link @
    [:
      cr dup H.8                         \ address
      10 #col dup lib.handle @ dup 0<>
      if h.8 else drop ." -noload-" then
      20 #col lib.name @ count type
    ;] list-apply ;

: .paths ( -- ) \ display search paths
    pathlist
    begin dup @ ?dup while
      zcount type ';' emit
      cell+
    repeat drop ;

: .imports      ( -- <filter> )
    set-filter
    cr ." Location  Import"   40 #col ." Prm  ImportEP  LibName" underline
    0 0
    [: ( count countmatch nfa -- count' countmatch' true )
      dup head.tfa c@ timp = if
        dup count filter-match? if
          cr
          dup>r name>interpret >body
          dup H.8 10 #col                       \ address
          r@ count type 40 #col             \ import name
          r> (in/out@) drop 2 .r 45 #col      \ in count
          dup @ dup ['] imp-resolve = if    \ real ep, or helper?
            2drop
          else
            h.8
            imp.lib @ ?dup if
              lib.name @ count 55 #col type
            then
          then
          1+
        else drop
        then swap 1+ swap true
      then
    ;] [ ' imports >body ] literal traverse-wordlist
    cr s" imported functions" (displayed) ;

: .files       ( -- )
    set-filter cr
    0 0
    [: ( count countmatch nfa -- count' countmatch' true )
      count 2dup filter-match? if
        type 1+ cr
      else
        2drop
      then swap 1+ swap true
    ;] [ ' files >body ] literal traverse-wordlist
    s" included files" (displayed) ;

\ ------------------------ .chains ---------------------------------------

internal

: .chain        ( chain -- )
                dup @ 0=
                if drop ." [empty]"
                else
                  begin @ ?dup
                  while   dup cell+ @
                    >name
                    dup count nip 3 + ?cr
                    get-xy drop 24 <
                    cols 48 > and
                    if 24 #col then .id space
                  repeat
                then    ;

: (.chains)     ( link -- )          \ display the contents of chains
                begin   @ ?dup
                while   dup
                        cr dup cell+ @ .id 23 #col space
                        2 cells- .chain
                repeat  cr ;
                
external

: .chains       ( -- ) chain-link (.chains) ;

\ ------------------------------- Memory Management functions ------------------------

: .mem          ( -- )  \ Display the amount of used and available program memory
    base @ decimal
    cr ." Area     Address      Total       Used       Free" underline
    0 dp-link @
    [: ( n link )
      cr [ 0 data.link ] literal -
      dup  data.name             count type tab
      dup  data.origin @ dup     h.8      ( origin )
      over data.top    @ over -  11 u,.r  ( total )
      over data.here   @ swap -  dup 11 u,.r  ( used )
                         rot + swap      ( add in to used )
      dup  data.top    @ swap
           data.here   @      -  11 u,.r  ( free )
    ;] list-apply underline
    cr ." Total" tab tab tab 14 u,.r cr ( total )
    base ! ;

: used          ( -- )  \ Display the mount of memory used by the following command line
    sys-free >r code-free >r app-free >r
    interpreter
    cr
    r> app-free - ." APP mem: " dup 1 u,.r
    r> code-free - ." , CODE mem: "     dup 1 u,.r
    r> sys-free - ." , SYS mem: "     dup 1 u,.r
    + + ."  Total: " 1 u,.r ;

\ --------------------------- End Memory Management functions ------------------------
 
module


