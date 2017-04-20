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


cr .( Loading Windows error handler...) sourcefilename type

((

Exception Handling
------------------

    Exception handling now uses a type 2 thread based exception handler to trap all 
    OS generated errors in wf32.

Catching Errors
---------------

    The use of THROW makes it easy to catch system errors in code. For instance:

    : x -4 @ ; \ will cause an access exception
    : y ['] x catch if ." Error" else ." OK" then ;

    If you don't do a CATCH, then the last CATCH executed will be run.

    Here's some sample output:

        9 8 7 6 5 @
                  ^
        Error -9 in (console): @ invalid memory address
        .err
        STC Experimental 32bit: 0.06.08 Build: 54 4/19/2017 17:20:56
        exception $C0000005 access violation (read) at $00000005
        ( $4012AD ) mov   eax dword { eax }                 \ 8B00
        Registers:
         eax $00000005
         ebx $00020000
         ecx $004012AD @+$0  in src/kernel/gkernel32.fs at line 288
         edx $00000040
         edi $01000E12
         esi $00C016DC
         ebp $0019DF60
         esp $0019FF54
         efl $00010216 IPVAVR.NIODETSZ.A.P.C
                       000010000001000010110
        Backtrack: depth=8
         eip $004012AD @+$0  in src/kernel/gkernel32.fs at line 288
             $00406600 recognize+$E4  in src/kernel/gkernel32.fs at line 3541
             $004069DE interpreter+$2D  in src/kernel/gkernel32.fs at line 3634
             $00408436 quit+$1A  in src/kernel/gkernel32.fs at line 4291
             $004022F6 catch+$F  in src/kernel/gkernel32.fs at line 1950
             $00000000
             $0019DF70
             $00408447 quit+$2B  in src/kernel/gkernel32.fs at line 4291
             $00000000
        Data stack: depth=5
         eax $00000005
             $00000006
             $00000007
             $00000008
             $00000009
             $00000000
             $00000000
             $00000000
             $00000000  ok

User Words
----------

    .err     Show the results of the last exception executed
    show-exception on | off
             Turn the output from an exception
             on or off (off is default)
    unset-except ( routine -- )
             Debugging can be problematic if there's an exception handler.
             This allows other than the default exception handler; for
             instance, a pointer to BREAK to drop back to the debugger.

))

\ -------------------- Windows Error functions ------------------------------

7 import: FormatMessage

variable winerrmsg 0 winerrmsg !            \ win error flag

: getlastwinerrmsg ( n -- addr )            \ build string for error message
    0 buf-allot dup>r rot >r    \ null buffer r: n
    maxcounted 1 /string swap   \ null buf-len buffer
    0 r> 0                      \ null buf-len buffer langid n null
    [ FORMAT_MESSAGE_FROM_SYSTEM FORMAT_MESSAGE_MAX_WIDTH_MASK or ] literal
    FormatMessage r@ c! r> ; \ save length in buffer

: getlastwinerr ( -- n )              \ get windows error code
    GetLastError          \ get error number
    dup getlastwinerrmsg  \ create error message
    winerrmsg @
    if ( throw_winerr ) -311 nabort!
    else drop             \ remove error message
    then ;                \ return error code

only forth also hidden definitions

throw_msgs link, 9998                   , ," Windows exception trapped"

: .exhex ( n -- ) space $.8 space ;

: .exc-code ( n -- )                      \ get the last exception
    dup cr ." exception" .exhex
    case
      0                                   of ." (no exception recorded)"  endof
      EXCEPTION_ACCESS_VIOLATION          of ." access violation"         endof
      EXCEPTION_INT_DIVIDE_BY_ZERO        of ." int divide by zero"       endof
      EXCEPTION_FLT_DIVIDE_BY_ZERO        of ." flt divide by zero"       endof
      EXCEPTION_FLT_STACK_CHECK           of ." flt stack check"          endof
      EXCEPTION_FLT_INEXACT_RESULT        of ." flt inexact result"       endof
      EXCEPTION_FLT_UNDERFLOW             of ." flt underflow"            endof
      EXCEPTION_FLT_DENORMAL_OPERAND      of ." flt denormal operand"     endof
      EXCEPTION_FLT_INVALID_OPERATION     of ." flt invalid operation"    endof
      EXCEPTION_FLT_OVERFLOW              of ." flt overflow"             endof
      EXCEPTION_ILLEGAL_INSTRUCTION       of ." illegal instruction"      endof
      EXCEPTION_DATATYPE_MISALIGNMENT     of ." datatype misalignment"    endof
\     EXCEPTION_GUARD_PAGE                of ." guard page"               endof
      EXCEPTION_BREAKPOINT                of ." breakpoint (int 3)"       endof
\     EXCEPTION_SINGLE_STEP               of ." single step"              endof
\     EXCEPTION_NONCONTINUABLE            of ." noncontinuable"           endof
      EXCEPTION_PRIV_INSTRUCTION          of ." priv instruction"         endof
\     EXCEPTION_IN_PAGE_ERROR             of ." in-page error"            endof
\     EXCEPTION_STACK_OVERFLOW            of ." stack overflow"           endof
\     EXCEPTION_INVALID_DISPOSITION       of ." invalid disposition"      endof
\     EXCEPTION_ARRAY_BOUNDS_EXCEEDED     of ." array bounds exceeded"    endof
\     EXCEPTION_NONCONTINUABLE_EXCEPTION  of ." noncontinuable exception" endof
      EXCEPTION_INT_OVERFLOW              of ." int overflow"             endof
      EXCEPTION_INVALID_HANDLE            of ." invalid handle"           endof
      STATUS_NO_MEMORY                    of ." out of memory"            endof
    endcase
    ;

: .addr-info    ( xt -- )              \ print off code info
    dup .exhex                         \ print the address
    dup code>name                      \ see if we can find the nfa
    dup if                             \ yes
      dup>r dup count type             \ type the name
      name>interpret - '+' emit $.            \ and the offset
      r> .viewinfo
    else 2drop
    then ;

: .exc-stack    ( a1 -- )                            \ print off the stack
    dup 8 cells+ swap
    ?do i @                              \ possibly addr of something in code
      cr 4 spaces .addr-info
    cell +loop ;

: .exreg ( n -- )  
    cr cells dup
    z"  edi esi ebx edx ecx eax ebp eip ??? flg esp"
    + 4 type
    ctx-rec ctx.edi + @ .addr-info ;

: .eflags ( n -- )
    cr ."  efl" dup .exhex
    ." IPVAVR.NIODETSZ.A.P.C"
    cr 15 spaces 21 b.n ;

only forth definitions also hidden

: .err          ( -- )                               \ print exception info
    cr .version space .cversion
    exc-rec dup
    exc.code @
    dup .exc-code
    dup if
      EXCEPTION_ACCESS_VIOLATION = if
        dup exc.parms @ if ."  (write)" else ."  (read)" then
        ."  at" exc.access @ .exhex
      else drop then
      ctx-rec
      dup ctx.eip @
      dup cdp cell+ @ code-here between if
        cr [ also assembler also asm-hidden ] dis-op [ previous previous ]
      then drop
      cr ." Registers:"
      10 6 1 0 3 4 2 5  ( don't change! register index )
      8 cells 0 do
        .exreg
      cell +loop
      ctx.eflags @ .eflags ( eflags )
      cr ." Backtrack: depth="
        rp0 @ ctx-rec ctx.esp @ - cell+ 2 rshift #.
        7 .exreg ( eip )
        exc-stack  .exc-stack
      cr ." Data stack: depth="
        sp0 @ ctx-rec ctx.ebp @ cell- - 2 rshift #.
        5 .exreg ( eax )
        exc-rstack .exc-stack
    else 2drop
    then
    ;

variable show-exception
         show-exception on                    \ show exception?
         
only forth also hidden definitions

: exc-recover   ( except context -- 0|1 code ) \ attempt recovery
    over exc.flag @ >r          \ don't do if not recoverable or unwinding
    over exc.code @ EXCEPTION_BREAKPOINT = r> or if \ or a breakpoint
      2drop 1 0                   \ zero ignored
    else
      ['] throw over ctx.eip ! \ get address of throw
      up0       over ctx.ebx ! \ recover saved up, best we can do!
      swap exc.code @
      case                     \ turn these into Forth errors
        EXCEPTION_ACCESS_VIOLATION     of  -9  endof
        EXCEPTION_INT_DIVIDE_BY_ZERO   of -10  endof
        EXCEPTION_FLT_DIVIDE_BY_ZERO   of -42  endof
        STATUS_NO_MEMORY               of -270 endof
        9998 swap                  \ default
      endcase dup>r
      swap ctx.eax !           \ exception to top of stack for throw
      0 r>                     \ r> ignored, do recovery
    then ;
    
\ At the time of the call to the per-thread handler, ESP points to three
\ structures as follows:-
\    ESP+4 Pointer to structure EXCEPTION_RECORD
\    ESP+8 Pointer to own ERR structure
\    ESP+C Pointer to structure CONTEXT record
\ This is handled by exc-route in the kernel.

: exc-main ( except err context -- 0|1)  \ save records & stacks
    dup>r ctx-rec ctx% move
    r@ ctx.esp @ exc-stack  8 cells move
    r@ ctx.ebp @ exc-rstack 8 cells move
    drop \ err
    dup>r exc-rec exc% move
    r> r> exc-recover
    9998 = show-exception @ and if
      .err    \ only auto-show for 9998 (Windows) exceptions
    then ;

only forth definitions also hidden

: set-except ( -- ) \ set default exception routine handler
    ['] exc-main win-handler ! ; 
: unset-except ( prev-except-addr -- ) \ unset exception routine
    ['] exc-default win-handler ! ; 


set-except
' set-except init-chain chain+

only forth definitions

