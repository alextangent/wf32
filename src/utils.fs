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

cr .( Loading Utility Words...)

: >string-execute ( ... xt -- ... addr len )
    [: pad +place ;]
    ['] type dup defer@ >r defer!
    pad off execute
    r> ['] type defer!
    pad count ;

\ -------------------- Get/Set current directory ----------------------------

2 import: GetCurrentDirectory
1 import: SetCurrentDirectory

: current-dir$  ( -- a l )      \ get the full path to the current directory
                buf-allot dup maxcounted call GetCurrentDirectory ;

: $current-dir! ( a l -- f )    \ directory string
                buf-allot dup place 1+
                call SetCurrentDirectory 0= ;

code cpuid      ( 16byte-buff eax -- )      \ cpuid into 16byte area
     2 0 in/out
                push    ebx
                mov     edi { ebp }
                cpuid
                mov     { edi } eax
                mov     { $4 edi } ebx
                mov     { $8 edi } ecx
                mov     { $c edi } edx
                mov     eax { cell ebp }
                pop     ebx
                next;

code rdtsc      ( -- timer-lo timer-hi )
     0 2 in/out
                mov     { -cell ebp } eax
                xor     eax eax
                push    ebx
                cpuid
                rdtsc
                mov     { -2 cells ebp } eax
                mov     eax edx
                pop     ebx
                next;

: horiz-line ( -- ) cols get-xy drop - dashes ;
: underline ( -- ) get-xy drop cr dashes ;

: buffer: ( u "<name>" -- ; -- addr ) create allot ;


