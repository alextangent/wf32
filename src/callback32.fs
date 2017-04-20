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

cr .( Loading Windows Callback...) sourcefilename type

\ --------------------------- Windows callback support ----------------------

\ Windows API externals interface words

\ When Windows calls back into user code, the address passed as one of the
\ parameters to the call to windows is the address of a routine to handle
\ the callback. We must save the caller's registers, and set up the new data
\ stack.

\ n callback: <name> ... ;
\           identifies a word as callable by Windows (and, by implication,
\           not callable by Forth directly!).

\ Because of the way the callback restores registers on exit, the stack does
\ not need to be clean on exit; the value on the top of the stack is taken as
\ the return value. There is a limit of around 4K or 1024 parameters maximum
\ on the stack (referenced by ebp), so be careful as very deep nesting may
\ well overrun the stack.

\ This will NOT work if the callback is executed in the context of another
\ thread that is not a W32F thread, since we use FS:14 to store a pointer 
\ that allows us to set up thread local storage -- which is based off segreg FS.

\ Callback entry and exit macros

also assembler definitions
also forth

: up asm[ up0 - ebx ]asm ; \ calculate offset for { ebx }

: cb-exit ( n -- ) \ exit code
    >r
      pop     { rp0 up }              \ restore rp0
      pop     { sp0 up }              \ and sp0
      pop     edi                     \ restore regs
      pop     esi
      pop     ebx
      pop     ebp                     \ original ebp
      ret     r@ cells
    r>drop ;

: cb-entry ( n -- ) \ variable part of entry code
    >r
      push    ebp                     \ caller's regs;
      push    ebx                     \ save std regs
      push    esi
      push    edi
      mov     ebx { fs: $14 }         \ get UP from TIB
      push    { sp0 up }              \ save sp0
      push    { rp0 up }              \ save rp0
      lea     ebp { -4096 esp }       \ room for call
      mov     { sp0 up } ebp          \ new sp0
      mov     { rp0 up } esp a;       \ new rp0
    r@ dup 0 ?do                      \ generate n fetches
            mov     eax { 7 i + cells esp } a; \ recover parm n
      i over <> if                    \ if not TOS in eax
            mov     { i 1+ cells negate ebp } eax a; \ save on stack
      then                            \ loop is not asm loop; it's forth
    loop 1- if                        \ make any adjustments to ebp
            lea     ebp { r@ 1- cells negate ebp } a; \ adjust stack
    then r>drop ;

only forth also definitions

: callback: ( n -- ) \ generate callback head
    >r : tcbk tfa! ( set the type )
    macro[ r@ cb-entry
      call    @@1 a;            \ call the code
      r@ cb-exit
    @@1: r>drop ]macro ;

