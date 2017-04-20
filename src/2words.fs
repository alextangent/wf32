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

cr .( Loading 2word support... ) sourcefilename type

\ --------------------------- Defining Words 2 -------------------------------

: 2variable ( -<name>- )   variable 0 , t2vr tfa! 0 2 in/out ;
: 2constant ( n m "name" ) create , ,   t2cn tfa! 0 2 in/out does> 2@ ;
: 2value    ( n m name )   create , ,   t2vl tfa! 0 2 in/out does> 2@ ;

: (to2!)  ( body -- ) plit postpone 2! ;
: (+to2!) ( body -- ) plit postpone d+! ;

' 2!      (to-interp)   t2vl 1- cells + !
' d+!     (+to-interp)  t2vl 1- cells + !
' (to2!)  (to-compile)  t2vl 1- cells + !
' (+to2!) (+to-compile) t2vl 1- cells + !

also assembler also asm-hidden  \ how to disassemble

:noname ( xt nfa -- ) swap execute swap . . ." 2value " .id             ; t2vl desc-how \ for 2value
:noname ( xt nfa -- ) ." 2variable " .id ."  ( is " execute 2@ swap #. #. ." ) " ; t2vr desc-how \ for variable
:noname ( xt nfa -- ) swap >body 2@ swap . . ." 2constant " .id         ; t2cn desc-how \ for 2constant

previous previous
