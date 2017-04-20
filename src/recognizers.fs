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

cr .( Loading recognizers... ) sourcefilename type

\ ----------------- Windows Constant Server -----------------------

library wincon.dll
3 import: wcFindWin32Constant

: wincon-call   ( a1 -- n f )                \ call to find constant
    >r 0 sp@ r> count swap wcFindWin32Constant ;

: rec:wincon ( addr u -- d r:number | dt:null )             \ find constant
    buf-allot dup>r place r@ uppercase \ uppercase a copy
    wincon-call dup 0= if        \ find constant
      2drop                      \ drop returned values
      [char] A r@ c+place        \ append an 'A'
      r@ wincon-call             \ find it again
    then rdrop
    if 0 dt:number else drop dt:null then ;

' rec:wincon get-recognizers 1+ set-recognizers  \ to end

\ ------------------ ]] macros [[ ---------------------------------

  :noname -600 ( throw_macroerr ) throw ;
  dup                              \ :noname throw_macroerr throw ;
  :noname ]                        \ return to compilation state
    get-recognizers 1- set-recognizers drop ;
dt-token: dt:macro ( --  )

: rec:macro ( addr u -- dt:macro | dt:null )
    s" [[" str= if dt:macro else dt:null then ;  \ switch into compile state

: ]] ( -- )  \ switch into postpone state
  ['] rec:macro get-recognizers 1+ set-recognizers
  -2 state ! ; immediate

\ -------------------- 'xxxx type  --------------------------------

: rec:tick ( addr u -- n 0 dt:number | dt:null )
    over c@ ''' = if 
      1 /string find-name ?dup if
        name>interpret 0 dt:number exit
      then
    else
      2drop
    then dt:null ;

' rec:tick get-recognizers 1+ set-recognizers

