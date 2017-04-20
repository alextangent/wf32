\ --------------------------- Change Block -------------------------------
\
\ arm 3/13/2017 13:45:39 V6.08
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


\ -------------------------- Start of code -----------------------------

DECIMAL

\ ---------------------------  Primitives  -----------------------------

\ Required first as meta compiler requires these up front due to
\ their use in several words. Most of these have no interpretation semantics

code unnest ( -- )
      ret
      next; inline

code dovar  ( -- n )                \ variable
    0 1 in/out
      mov     { -4 ebp } eax
      mov     eax ecx   
      next; inline

code bra ( -- ) ( non-executable )
      jmp     0
      next; inline

code -?bra ( n -- n ) ( non-executable ) \ non destructive test & branch
      test    eax eax
      jz      0
      next; inline

code ?bra ( n --  ) ( non-executable ) \ standard destructive test & branch
      add     ebp cell
      test    eax eax
      mov     eax { -cell ebp }
      jz      0
      next; inline
      
\ -------------------- Execute  -------------------------------------

code execute    ( xt -- [n]* )        \ execute xt
    1 -1 in/out
      mov     ecx eax        \ save xt in ecx
      mov     eax { ebp }              \ new tos
      add     ebp cell
      call    ecx           \ go execute
      next; inline

code perform    ( xt@ -- [n]* )        \ perform xt  @ execute
    1 -1 in/out
      mov     ecx { eax }              \ save xt in ecx
      mov     eax { ebp }              \ new tos
      add     ebp cell
      call    ecx           \ go execute
      next; inline

: noop ; immediate 

\ ----------------------------- Libraries  ---------------------------

library kernel32.dll
library user32.dll
library shlwapi.dll

\ -------------------- System Wide Constants --------------------------

\ WF32, in common with many Forths, extends the meaning of the
\ constant true to be any non-zero bit pattern, not just -1 (all bits set).
\ The value null is used primarily for pointers; 0 is not a valid address
\ in Windows.

 0  constant false
-1  constant true
 0  constant null
 4  constant cell
-4  constant -cell
 32 constant bl

12  constant #vocs       \ maximum number of vocabularies in search order

512 constant maxbuffer   \ maximum length of a buffer
255 constant maxcounted  \ maximum length of contents of a counted string
255 constant maxstring   \ maximum length of string
' maxbuffer alias max-path

\ -------------------- Stack Operators --------------------------------

code dup        ( n -- n n )    \ duplicate top entry on data stack
    1 2 in/out
      mov     { -cell ebp } eax 
      next; inline

code swap       ( n1 n2 -- n2 n1 ) \ exchange first and second items on data stack
    2 2 in/out
      mov     ecx { ebp }
      mov     { ebp } eax
      mov     eax ecx
      next; inline

code drop       ( n -- )        \ discard top entry on data stack
    1 0 in/out
      mov     eax { ebp }
      next; inline

code rot        ( n1 n2 n3 -- n2 n3 n1 ) \ rotate third item to top of data stack
    3 3 in/out
      mov     ecx { ebp }
      mov     edx { cell ebp }
      mov     { ebp } eax
      mov     { cell ebp } ecx
      mov     eax edx
      next; inline

code over       ( n1 n2 -- n1 n2 n1 ) \ copy second item to top of data stack
    2 3 in/out
      mov     { -cell ebp } eax
      mov     eax { ebp }
      next; inline

code -rot       ( n1 n2 n3 -- n3 n1 n2 ) \ rotate top of data stack to third item
    3 3 in/out
      mov     ecx { cell ebp }
      mov     edx { ebp }
      mov     { cell ebp } eax
      mov     { ebp } ecx
      mov     eax edx
      next; inline

code ?dup       ( n -- n [n] )  \ duplicate top of data stack if non-zero
    1 -1 in/out
      test    eax eax
      je      short @@1
      mov     { -cell ebp } eax
      sub     ebp cell
@@1:  next; inline

code nip        ( n1 n2 -- n2 ) \ discard second item on data stack
    2 1 in/out
      next; inline

code tuck       ( n1 n2 -- n2 n1 n2 ) \ copy top data stack to under second item
    2 3 in/out
      mov     ecx { ebp }
      mov     { ebp } eax
      mov     { -cell ebp } ecx
      next; inline

code pick       ( ...  k -- ... n[k] )
      mov     eax { ebp eax *cell }    \ just like that!
      next; inline

code depth      ( -- n )        \ return the current data stack depth
    0 1 in/out
      mov     { -cell ebp } eax
      mov     eax { sp0 ebx } \ fetch
      sub     eax ebp
      sar     eax cell msbit \ 2 is div by 4, 3 is div by 8
      add     eax 1
      next;

code dup>r ( n -- n r: n )
      push    eax
      next; inline

code >r ( n -- r: n )
    1 0 in/out
      push    eax
      mov     eax { ebp }
      next; inline

code r> ( r: n -- n )
    0 1 in/out
      mov     { -cell ebp } eax
      pop     eax
      next; inline

code r@ ( r: n -- n r: n )
    0 1 in/out
      mov     { -cell ebp } eax
      mov     eax { esp }
      next; inline

code rdrop ( r: n -- )
      add     esp cell
      next; inline

' rdrop alias r>drop inline   \ don't use in the kernel

code sp@ ( -- r: )
      sub     ebp cell
      mov     { ebp } eax
      mov     eax ebp
      next;   inline 0 1 in/out

code sp! ( n -- r: )
    1 0 in/out
      mov     ebp eax
      mov     eax { ebp }
      next; inline

code rp@ ( -- r: )
    0 1 in/out
      mov     { -cell ebp } eax
      mov     eax esp
      next; inline

code rp! ( n -- r: )
    1 0 in/out
      mov     esp eax
      mov     eax { ebp }
      next; inline

code 2>r ( n n -- r: n n )
    2 0 in/out
      push    { ebp }
      push    eax
      mov     eax { cell ebp }
      next; inline

code 2r> ( -- n n )
      sub     ebp 2 cells
      mov     { cell ebp } eax
      pop     eax
      pop     { ebp }
      next; inline  0 2 in/out

code 2r@ ( -- n n )
      sub     ebp 2 cells
      mov     { cell ebp } eax
      mov     edx { cell esp }
      mov     eax { esp }
      mov     { ebp } edx
      next; inline  0 2 in/out

code 2rdrop ( r: n n -- )
      add     esp 2 cells
      next; inline

\ -------------------- Common Operators ---------------------------

code c@         ( a1 -- c1 )    \ fetch the character c1 from address a1
      movzx   eax byte { eax }
      next; inline

code c!         ( c1 a1 -- )    \ store character c1 into address a1
    2 0 in/out
      mov     ecx { ebp }
      mov     { eax } cl
      mov     eax { cell ebp }
      next; inline

code @          ( addr -- n )
      mov     eax { eax }
      next; inline

code !          ( n addr -- )
    2 0 in/out
      mov     ecx { ebp }
      mov     { eax } ecx
      mov     eax { cell ebp }
      next; inline

code +          ( n1 n2 -- n3 ) \ add n1 to n2, return sum n3
    2 1 in/out
      add     eax { ebp }
      next; inline

code -          ( n1 n2 -- n3 ) \ subtract n2 from n1, return difference n3
    2 1 in/out
      neg     eax
      add     eax { ebp }
      next; inline

\ -------------------- Non-std memory operators -----------------------

code b@         ( a1 -- b1 )    \ fetch the byte (8 bits)
      movzx   eax byte { eax }
      next;

code sb@        ( a1 -- b1 )    \ fetch the signed byte (8 bits)
      movsx   eax byte { eax }
      next;

code b!         ( b1 a1 -- )    \ store byte (8bit)
    2 0 in/out
      mov     edx { ebp }
      mov     { eax } dl
      mov     eax { cell ebp }
      next;

code w@         ( a1 -- w1 )    \ fetch the word (16bit) w1 from address a1
      movzx   eax word { eax }
      next;

code sw@        ( a1 -- w1 )    \ sign fetch the word w1
      movsx   eax word { eax }
      next;

code w!         ( w1 a1 -- )    \ store word (16bit) w1 into address a1
    2 0 in/out
      mov     ecx { ebp }
      mov     { eax } cx
      mov     eax { cell ebp }
      next;

code L@         ( addr -- n ) \ fetch the long (32bit) L1 from address a1
      mov     eax { eax }
      next;

code L!         ( n addr -- )
    2 0 in/out
      mov     ecx { ebp }
      mov     { eax } ecx
      mov     eax { cell ebp }
      next;

code q@         ( a1 -- d1 ) \ fetch the 64bit number; NOT the same as 2@
    1 2 in/out
      mov     ecx eax
      mov     edx { ecx }
      mov     eax { cell ecx }
      mov     { -cell ebp } edx
      next;

code q!         ( d1 a1 -- ) \ store the 64bit number; NOT the same as 2!
    3 0 in/out
      mov     ecx { ebp }
      mov     edx { cell ebp }
      mov     { eax } edx
      mov     { cell eax } ecx
      mov     eax { 2 cells ebp }
      next;

\ -------------------- Arithmetic Operators ----------------------------

code negate     ( n1 -- n2 ) \ negate n1, returning 2's complement n2
    1 1 in/out
      neg     eax
      next; inline

code abs        ( n -- |n| ) \ return the absolute value of n1 as n2
    1 1 in/out
      cdq
      xor     eax edx
      sub     eax edx
      next; inline

code 2*         ( n1 -- n2 ) \ multiply n1 by two
    1 1 in/out
      add     eax eax
      next; inline

code 2/         ( n1 -- n2 ) \ signed divide n1 by two
    1 1 in/out
      sar     eax 1
      next; inline

code u2/        ( n1 -- n2 ) \ unsigned divide n1 by two
    1 1 in/out
      shr     eax 1
      next; inline

code 1+         ( n1 -- n2 ) \ add one to n1
    1 1 in/out
      add     eax 1
      next; inline

code 1-         ( n1 -- n2 ) \ subtract one from n1
    1 1 in/out
      sub     eax 1
      next; inline

code 2+         ( n1 -- n2 ) \ add 2 to n1
    1 1 in/out
      add     eax 2
      next; inline

code 2-         ( n1 -- n2 ) \ subtract 2 from n1
    1 1 in/out
      sub     eax 2
      next; inline

code +!         ( n1 a1 -- )    \ add n1 to cell a1
    2 0 in/out
      mov     ecx { ebp }
      add     { eax } ecx
      mov     eax { cell ebp }
      next; inline

code @+! ( n addr -- n' ) ( fetch and add )
    2 1 in/out
      mov     ecx eax
      mov     eax { ebp }
      xadd    { ecx } eax
      next; inline

code 1+!        ( addr -- )     \ increment the contents of addr
    1 0 in/out
      add     dword { eax } 1
      mov     eax { ebp }
      next; inline

code 1-!        ( addr -- )     \ decrement the contents of addr
    1 0 in/out
      sub     dword { eax } 1
      mov     eax { ebp }
      next; inline

code 1+c!       ( addr -- )     \ increment the byte contents of addr
    1 0 in/out
      add     byte { eax } 1
      mov     eax { ebp }
      next; inline

code 1-c!       ( addr -- )     \ decrement the byte contents of addr
    1 0 in/out
      sub     byte { eax } 1
      mov     eax { ebp }
      next; inline

code c+!        ( c1 a1 -- )    \ add c1 to char a1
    2 0 in/out
      mov     ecx { ebp }
      add     { eax } cl
      mov     eax { cell ebp }
      next; inline

\ -------------------- Logical Operators ------------------------------

code and        ( n1 n2 -- n3 ) \ perform bitwise and of n1,n2, return result n3
    2 1 in/out
      and     eax { ebp }
      next; inline

code or         ( n1 n2 -- n3 ) \ perform bitwise or of n1,n2, return result n3
    2 1 in/out
      or      eax { ebp }
      next; inline

code xor        ( n1 n2 -- n3 ) \ perform bitwise xor of n1,n2, return result n3
    2 1 in/out
      xor     eax { ebp }
      next; inline

code lshift     ( u1 n -- u2 )  \ shift u1 left by n bits (multiply)
    2 1 in/out
      mov     ecx eax
      mov     eax { ebp }
      shl     eax cl
      next; inline

code rshift     ( u1 n -- u2 )  \ shift u1 right by n bits (divide)
    2 1 in/out
      mov     ecx eax
      mov     eax { ebp }
      shr     eax cl
      next; inline

code arshift    ( u1 n -- u2 )  \ arithmetic shift u1 right by n bits
    2 1 in/out
      mov     ecx eax
      mov     eax { ebp }
      sar     eax cl
      next; inline

code on         ( addr -- )     \ set the contents of addr to on (-1)
    1 0 in/out
      or      dword { eax } -1
      mov     eax { ebp }
      next; inline

code off        ( addr -- )     \ set the contents of addr of off (0)
    1 0 in/out
      and     dword { eax } 0
      mov     eax { ebp }
      next; inline

code invert     ( n1 -- n2 )    \ invert bits
    1 1 in/out
      not     eax
      next; inline

\ -------------------- Bit Operators ----------------------------------

code count-bits ( n -- bits-in-n ) \ population count of bits in eax
    1 1 in/out
      popcnt  eax eax
      next; inline

code msbit ( n -- msb )          \ most significant bit in n
    1 1 in/out
      or      ecx -1             \ if not found
      bsr     eax eax            \ bit scan
      cmovz   eax ecx            \ -1 if not found
      next; inline

code lsbit      ( n -- lsb )               \ least significant bit in n
    1 1 in/out
      or      ecx -1             \ if not found
      bsf     eax eax            \ bit scan
      cmovz   eax ecx            \ -1 if not found
      next; inline

\ -------------------- Comparison Operators ---------------------------

code 0=         ( n1 -- f1 )    \ return true if n1 equals zero
    1 1 in/out
      sub     eax 1
      sbb     eax eax
      next; inline

' 0= alias not

code 0<>        ( n1 -- f1 )
    1 1 in/out
      add     eax -1
      sbb     eax eax
      next; inline

code 0<         ( n1 -- f1 )
    1 1 in/out
      sar     eax cell 8 * 1- \ 31 for 32bit, 63 for 64bit
      next; inline

code 0>         ( n1 -- f1 )
    1 1 in/out
      sub     eax 1
      cmp     eax -1 1 rshift \ $7fffffff \ ??? needs 64bit equiv
      sbb     eax eax
      next; inline

code =          ( n1 n2 -- f1 )
    2 1 in/out
      sub     eax { ebp }
      sub     eax 1
      sbb     eax eax
      next; inline

code <>         ( n1 n2 -- f1 )
    2 1 in/out
      sub     eax { ebp }
      add     eax -1
      sbb     eax eax
      next; inline

code <          ( n1 n2 -- f1 )
    2 1 in/out
      mov     edx eax
      or      ecx -1
      xor     eax eax
      cmp     { ebp } edx
      cmovl   eax ecx
      next; inline

code >          ( n1 n2 -- f1 )
    2 1 in/out
      mov     edx eax
      or      ecx -1
      xor     eax eax
      cmp     { ebp } edx
      cmovg   eax ecx
      next; inline

code <=         ( n1 n2 -- f1 )
    2 1 in/out
      mov     edx eax
      or      ecx -1
      xor     eax eax
      cmp     { ebp } edx
      cmovle  eax ecx
      next; inline

code >=         ( n1 n2 -- f1 )
    2 1 in/out
      mov     edx eax
      or      ecx -1
      xor     eax eax
      cmp     { ebp } edx
      cmovge  eax ecx
      next; inline

code u<         ( u1 u2 -- f1 )
    2 1 in/out
      cmp     { ebp } eax
      sbb     eax eax
      next; inline

code u>         ( u1 u2 -- f1 )
    2 1 in/out
      cmp     eax { ebp }
      sbb     eax eax
      next; inline

code u>=        ( u1 u2 -- f1 )
    2 1 in/out
      cmp     { ebp } eax
      sbb     eax eax
      not     eax
      next; inline

code u<=        ( u1 u2 -- f1 )
    2 1 in/out
      cmp     eax { ebp }
      sbb     eax eax
      not     eax
      next; inline

code min        ( n1 n2 -- n3 ) \ return the lesser of n1 and n2
    2 1 in/out
      mov     ecx { ebp }
      cmp     ecx eax
      cmovl   eax ecx
      next; inline

code max        ( n1 n2 -- n3 ) \ return the greater of n1 and n2
    2 1 in/out
      mov     ecx { ebp }
      cmp     ecx eax
      cmovg   eax ecx
      next; inline

code 0max       ( n1 -- n2 ) \ return n2 the greater of n1 and zero
    1 1 in/out
      xor     ecx ecx
      cmp     ecx eax
      cmovg   eax ecx
      next; inline

code umin       ( u1 u2 -- n3 ) \ return the lesser of unsigned u1 and
            \ unsigned u2
    2 1 in/out
      mov     ecx { ebp }
      cmp     ecx eax
      cmovb   eax ecx
      next; inline

code umax       ( u1 u2 -- n3 ) \ return the greater of unsigned u1 and
            \ unsigned u2
    2 1 in/out
      mov     ecx { ebp }
      cmp     ecx eax
      cmova   eax ecx
      next; inline

code within     ( n1 low high -- f1 ) \ f1=true if ((n1 >= low) & (n1 < high))
    3 1 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      sub     eax edx
      sub     ecx edx
      sub     ecx eax
      sbb     eax eax
      next;

: between       ( n1 low high -- f1 ) \ f1=true if ((n1 >= low) & (n1 <= high))
    1+ within ;

\ -------------------- Char Operators ---------------------------------

: chars ( n1 -- n1*char ) ; immediate 1 1 in/out
' 1+ alias char+

\ -------------------- Double Stack Operators -------------------------

code 2drop      ( n1 n2 -- ) \ discard two single items from the data stack
    2 0 in/out
      mov     eax { cell ebp }
      next; inline

code 2dup       ( n1 n2 -- n1 n2 n1 n2 )
    2 4 in/out
      mov     ecx { ebp }
      mov     { -cell ebp } eax
      mov     { -2 cells ebp } ecx
      next; inline

code 2nip       ( n1 n2 n3 n4 -- n3 n4 ) \ discard third and fourth items from data stack
    4 2 in/out
      mov     ecx { ebp }
      mov     { 2 cells ebp } ecx
      next; inline

code 2swap      ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
    4 4 in/out
      xchg    ebp esp
      pop     ecx               \ n3
      pop     edx               \ n2
      pop     edi               \ n1
      push    ecx               \ n3
      push    eax               \ n4
      push    edi               \ n1
      xchg    ebp esp
      mov     eax edx          \ n2
      next;

code 2rot      ( n1 n2 n3 n4 n5 n6 -- n3 n4 n5 n6 n1 n2 )
    6 6 in/out
      push    ebx
      push    esi
      xchg    ebp esp
      pop     ebx              \ n5
      pop     ecx              \ n4
      pop     edx              \ n3
      pop     edi              \ n2
      pop     esi              \ n1
      push    edx              \ n3
      push    ecx              \ n4
      push    ebx              \ n5
      push    eax              \ n6
      push    esi              \ n1
      xchg    ebp esp
      mov     eax edi
      pop     esi
      pop     ebx
      next;

code 2over      ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
    4 6 in/out
      mov     { -cell ebp } eax
      mov     ecx { 2 cells ebp }
      mov     eax { cell ebp }
      mov     { -2 cells ebp } ecx
      next;

\ These should really be Forth words; they're infrequently used

code 3drop      ( n1 n2 n3 -- ) \ discard three items from the data stack
    3 0 in/out
      mov     eax { 2 cells ebp }
      next;

code 4drop      ( n1 n2 n3 n4 -- ) \ discard four items from the data stack
    4 0 in/out
      mov     eax { 3 cells ebp }
      next;

code 3dup       ( n1 n2 n3 -- n1 n2 n3 n1 n2 n3 )
    3 6 in/out
      mov     edx { ebp }        \ n2
      mov     ecx { cell ebp }      \ n1
      mov     { -cell    ebp } eax     \ n3
      mov     { -2 cells ebp } ecx     \ n1
      mov     { -3 cells ebp } edx    \ n2
      next;

code 4dup       ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 n3 n4 )
    4 8 in/out
      mov     edx { ebp }        \ n3
      mov     ecx { cell ebp }      \ n2
      mov     edi { 2 cells ebp }      \ n1
      mov     { -cell    ebp } eax     \ n4
      mov     { -2 cells ebp } edi     \ n1
      mov     { -3 cells ebp } ecx    \ n2
      mov     { -4 cells ebp } edx    \ n3
      next;

code s-reverse ( n[k]..2 1 0 k -- 0 1 2..n[k] ) \ wf32
\ *g reverse n items on stack   \n
\ ** usage: 1 2 3 4 5 5 s-reverse ==> 5 4 3 2 1
    -1 -1 in/out
      lea     ecx { -cell ebp }     \ ecx points 4 under top of stack
      lea     eax { cell ecx eax *cell } \ eax points 4 over stack
               \ bump pointers, if they overlap, stop
@@1:  sub     eax cell       \ adjust top
      add     ecx cell       \ adjust bottom
      cmp     ecx eax          \ compare
      jae     short @@2         \ ecx passing eax , so exit
                      \ rotate a pair
      mov     edx { eax }      \ bottom to edx
      xor     { ecx } edx      \ exchange top and edx
      xor     edx { ecx }
      xor     { ecx } edx
      mov     { eax } edx      \ eax to bottom
      jmp     short @@1         \ next pair

@@2:  mov     eax { ebp }
      add     ebp cell
      next;

\ -------------------- Double memory Operators ------------------------

code 2@         ( a1 -- d1 ) \ fetch the double number n1 from address a1
    1 2 in/out
      mov     ecx eax
      mov     eax { ecx }
      mov     edx { cell ecx }
      mov     { -cell ebp } edx
      next; inline

code 2!         ( d1 a1 -- ) \ store the double number d1 into address a1
    3 0 in/out
      mov     ecx { ebp }
      mov     edx { cell ebp }
      mov     { eax } ecx
      mov     { cell eax } edx
      mov     eax { 2 cells ebp }
      next; inline

\ -------------------- Cell Operators ---------------------------------

code cells      ( n1 -- n1*cell )       \ multiply n1 by the cell size
    1 1 in/out
      sal     eax cell msbit  \ 32bit=2, 64bit=3
      next; inline

code cells+     ( a1 n1 -- a1+n1*cell ) \ multiply n1 by the cell size and add
                    \ the result to address a1
    2 1 in/out
      mov     ecx { ebp }
      lea     eax { ecx eax *cell }
      next; inline

code +cells     ( n1 a1 -- a1+n1*cell ) \ multiply n1 by the cell size and add
                    \ the result to address a1
    2 1 in/out
      mov     ecx { ebp }
      lea     eax { eax ecx *cell }
      next; inline

code cells-     ( a1 n1 -- a1-n1*cell ) \ multiply n1 by the cell size and subtract
                    \ the result from address a1
    2 1 in/out
      lea     ecx { eax *cell }
      mov     eax { ebp }
      sub     eax ecx
      next; inline

: -cells        ( n1 a1 -- n1*cell+a1 ) \ multiply n1 by the cell size and sub
    swap cells- ;           \ the result from address a1

code cell+      ( a1 -- a1+cell )       \ add a cell to a1
    1 1 in/out
      add     eax cell
      next; inline

code cell-      ( a1 -- a1-cell )       \ subtract a cell from a1
    1 1 in/out
      sub     eax cell
      next; inline

code aligned    ( addr1 -- addr2 )
    1 1 in/out
      add     eax cell 1-
      and     eax -cell
      next;

code -aligned   ( addr1 -- addr2 )
    1 1 in/out
      and     eax -cell
      next;

code naligned   ( addr n -- addr2 )
    2 1 in/out
      lea     ecx { -1 eax }    \ n-1
      neg     eax              \ -n
      add     ecx { ebp }       \ addr+n-1
      and     eax ecx         \ addr+n-1 and -n
      next;

\ -------------------- Block Memory Operators -------------------------

code cmove      ( source dest len -- ) \ move bytes "from" -> "to" of "count"
    3 0 in/out
      mov     edi { ebp }
      mov     esi { cell ebp }
      mov     ecx eax        \ start at first byte of "from"
      rep     movs byte
      mov     eax { 2 cells ebp }
      next;

code cmove>     ( source dest len -- )  \ move bytes "from" -> "to" of "count"
    3 0 in/out
      mov     edi { ebp }
      mov     esi { cell ebp }
      lea     edi { -1 eax edi }
      lea     esi { -1 eax esi }
      mov     ecx eax        \ start at last byte of "from"
      std
      rep     movs byte
      cld
      mov     eax { 2 cells ebp }
      next;

code move       ( source dest len -- )  \ move len bytes from source address to dest address
    3 0 in/out
      mov     edi { ebp }
      mov     esi { cell ebp }
      mov     ecx edi        \ check for overlap
      sub     ecx esi
      cmp     ecx eax
      mov     ecx eax        \ start at first byte of "from"
      jbe     short @@1      \ overlap

      shr     ecx 2
      rep     movs dword
      mov     ecx eax
      and     ecx 3
      rep     movs byte
      mov     eax { 2 cells ebp }
      add     ebp 3 cells
      ret

@@1:  lea     edi { -1 eax edi }
      lea     esi { -1 eax esi }
      std
      rep     movs byte
      cld
      mov     eax { 2 cells ebp }
      next;

code fill       ( addr len char -- )    \ fill addr with char for len bytes
    3 0 in/out
      mov     ecx { ebp }
      mov     edi { cell ebp }    \ destination
      rep     stos byte
      mov     eax { 2 cells ebp }
      next;

: erase ( addr len -- ) 0 fill ;  \ fill addr for len bytes with zero
: blank ( addr len -- ) bl fill ; \ fill addr for len bytes with blank
      

\ -------------------- Unsigned Multiply & Divide ---------------------------

code um*        ( u1 u2 -- ud1 ) \ multiply unsigned u1 by unsigned u2
    2 2 in/out
      mul     dword { ebp }
      mov     { ebp } eax
      mov     eax edx
      next;

code um/mod     ( ud1 u1 -- rem quot ) \ divide unsigned double ud1 by the
    3 2 in/out              \ unsigned number u1
      mov     ecx eax
      mov     edx { ebp }
      mov     eax { cell ebp }
      div     ecx
      mov     { cell ebp } edx
      next;

\ -------------------- Signed Multiply & Divide -----------------------------

\ This code uses SYMMETRIC DIVISION OPERATORS
\
\ The Forth-79 Standard specifies that the signed division operators (/,
\ /MOD, MOD, */MOD, and */) round non-integer quotients towards zero
\ (symmetric division). Forth-83 changed the semantics of these operators
\ to round towards negative infinity (floored division). Some in the
\ Forth community have declined to convert systems and applications from
\ the Forth-79 to the Forth-83 divide. To resolve this issue, an ANS
\ Forth system is permitted to supply either floored or symmetric
\ operators. In addition, ANS Forth systems must provide a floored
\ division primitive (FM/MOD), a symmetric division primitive (SM/REM),
\ and a mixed precision multiplication operator (M*)

code m*         ( n1 n2 -- d1 ) \ multiply n1 by n2, return double result d1
    2 2 in/out
      imul    dword { ebp }
      mov     { ebp } eax
      mov     eax edx
      next;

code sm/rem     ( d n -- rem quot )     \ symmetric division
    3 2 in/out
      mov     ecx eax
      mov     edx { ebp }
      mov     eax { cell ebp }
      idiv    ecx
      mov     { cell ebp } edx
      next;

code fm/mod     ( d n -- mod quot )     \ floored division
    3 2 in/out
      mov     edx { ebp }      \ high numerator
      mov     ecx edx        \ copy for testing
      xor     ecx eax        \ test against divisor
      mov     ecx eax        \ get divisor
      mov     eax { cell ebp }    \ get low numerator
      jns     short @@1       \ signs differ, so jump

      idiv    ecx
      test    edx edx        \ set zero flag
      je      short @@2
      add     edx ecx        \ add divisor to remainder
      sub     eax 1             \ decrement quotient
      jmp     short @@2

@@1:  idiv    ecx
@@2:  mov     { cell ebp } edx
      next;

code *          ( n1 n2 -- n3 ) \ multiply n1 by n2, return single result n3
    2 1 in/out
      imul    dword { ebp }
      next;

code /mod       ( n1 n2 -- rem quot )   \ equiv of >r s>d r> sm/rem
    2 2 in/out
      mov     ecx eax        \ >r
      mov     eax { ebp }
      cdq           \ s>d
      idiv    ecx             \ r> sm/rem
      mov     { ebp } edx
      next;

code /          ( n1 n2 -- quot )       \ equiv of >r s>d r> sm/rem nip
    2 1 in/out
      mov     ecx eax        \ >r
      mov     eax { ebp }
      cdq           \ s>d
      idiv    ecx             \ r> sm/rem
      next;

code mod        ( n1 n2 -- rem )        \ equiv of >r s>d r> sm/rem drop
    2 1 in/out
      mov     ecx eax        \ >r
      mov     eax { ebp }
      cdq           \ s>d
      idiv    ecx             \ r> sm/rem
      mov     eax edx
      next;

code */         ( n1 n2 n3 -- n1*n2/n3 ) \ equiv of >r m* r> sm/rem swap drop
    3 1 in/out
      mov     ecx eax
      mov     eax { ebp }      \ n2
      imul    dword { cell ebp }   \ n1*n2
      idiv    ecx             \ n1*n2/n3
      next;

code */mod      ( n1 n2 n3 -- rem n1*n2/n3 ) \ equiv of >r m* r> sm/rem
    3 2 in/out
      mov     ecx eax
      mov     eax { ebp }      \ n2
      imul    dword { cell ebp }   \ n1*n2
      idiv    ecx             \ n1*n2/n3
      mov     { cell ebp } edx    \ rem
      next;

\ ----------------- Double comparison Operators ----------------------------

code d=         ( d1 d2 -- f1 ) \ f1=true if double d1 is equal to double d2
    4 1 in/out
      mov     ecx { 2 cells ebp }
      sub     ecx { ebp }
      sbb     eax { cell ebp }
      or      eax ecx
      sub     eax 1
      sbb     eax eax
      next;

code d0<        ( d1 -- f1 )   \ signed compare d1 double number with zero.
    2 1 in/out
      sar     eax cell 8 * 1- \ 31 for 32bit, 63 for 64bit
      next;

code d0=        ( d -- f) \ double compare to 0
    2 1 in/out
      or      eax { ebp }
      cmp     eax 1
      sbb     eax eax
      next;

code d<         ( d1 d2 -- f ) \ signed compare two double numbers.
    4 1 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      cmp     { 2 cells ebp } edx
      sbb     ecx eax
      mov     eax 0
      jge     short @@1
      sub     eax 1
@@1:  next;

code du<        ( d1 d2 -- f ) \ signed compare two double numbers.
    4 1 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      cmp     { 2 cells ebp } edx
      sbb     ecx eax
      mov     eax 0
      jae     short @@1
      sub     eax 1
@@1:  next;

code du>        ( d1 d2 -- f ) \ signed compare two double numbers.
    4 1 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      cmp     { 2 cells ebp } edx
      sbb     ecx eax
      mov     eax 0
      jb      short @@1
      sub     eax 1
@@1:  next;

code d>         ( d1 d2 -- f ) \ signed compare two double numbers.
    4 1 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      cmp     { 2 cells ebp } edx
      sbb     ecx eax
      mov     eax 0
      jl      short @@1
      sub     eax 1
@@1:  next;

code d<>        ( d1 d2 -- d ) \ signed compare two double numbers.
    4 1 in/out
      mov     ecx { 2 cells ebp }
      sub     ecx { ebp }
      sbb     eax { cell ebp }
      or      eax ecx
      sub     eax 1
      sbb     eax eax
      not     eax
      next;

\ -------------------- Double Arithmetic Operators --------------------------

code d+         ( d1 d2 -- d3 ) \ perform a double add (64bit)
    4 2 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      add     { 2 cells ebp } edx
      adc     eax ecx
      next;

code d+!        ( d1 a1 -- )            \ double accumulate
    3 0 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      add     { eax } edx
      adc     { cell eax } ecx
      mov     eax { 2 cells ebp }
      next;

code d-         ( d1 d2 -- d3 ) \ perform a double subtract (64bit)
    4 2 in/out
      mov     edx { ebp }
      mov     ecx { cell ebp }
      sub     { 2 cells ebp } edx
      sbb     ecx eax
      mov     eax ecx
      next;

code dnegate    ( d1 -- d2 )    \ negate d1, returning 2's complement d2
    2 2 in/out
      neg     eax
      neg     dword { ebp }
      sbb     eax 0
      next;

code dabs       ( d1 -- d2 )    \ return the absolute value of d1 as d2
    2 2 in/out
      test    eax eax
      js      short ' dnegate
      next; \ must not inline!

code d2*        ( d1 -- d2 ) \ multiply the double number d1 by two
    2 2 in/out
      shl     { ebp } 1
      rcl     eax 1
      next;

code d2/        ( d1 -- d2 ) \ divide the double number d1 by two
    2 2 in/out
      sar     eax 1
      rcr     { ebp } 1
      next;

code s>d        ( n1 -- d1 ) \ convert single signed n1 to a signed
                   \ double d1
    1 2 in/out
      cdq
      mov     { -cell ebp } eax
      mov     eax edx
      next;

: d>s ( d -- s ) drop ;         \ convert double to single

: dmin ( d1 d2 -- d3 )          \  replace with the smaller of the two (signed).
    4dup d> if 2swap then 2drop ;
: dmax          ( d1 d2 -- d3 ) \  replace with the larger of the two (signed).
    4dup d< if 2swap then 2drop ;

: m+ ( d n -- d ) ( add cell to double )
    s>d d+ ;

: tnegate ( t1lo t1mid t1hi -- t2lo t2mid t2hi )
    invert >r
    invert >r
    invert 0 -1 -1 d+ s>d r> 0 d+
    r> + ;

: ut*  ( ulo uhi u -- utlo utmid uthi )
    swap >r dup>r um* 0 r> r> um* d+ ;

: mt*  ( lo hi n -- tlo tmid thi )
    dup 0< if
      abs over 0< if
        >r dabs r> ut*
      else    
        ut* tnegate
      then
    else    
      over 0< if
        >r dabs r> ut* tnegate
      else    
        ut*
      then
    then ;

: ut/  ( utlo utmid uthi n -- d1 )
    dup>r um/mod -rot r> um/mod nip swap ;

: m*/  ( d1 n1 +n2 -- d2 )
    >r mt* dup 0< if
      tnegate r> ut/ dnegate
    else
      r> ut/
    then ;

\ ------------------------ String counting -----------------------

code count      ( str -- addr len ) \ byte counted strings
    1 2 in/out
      add     eax 1
      mov     { -cell ebp } eax
      movzx   eax byte { -1 eax }
      next; inline

code zcount     ( str -- addr len )  \ null terminated ascii strings
    1 2 in/out
      mov     { -cell ebp } eax           \ addr on stack
      mov     edi eax      \ edi = absolute address of string
      or      ecx -1               \ scan way on up there... it had better stop!
      xor     eax eax      \ look for null
      repnz   scas byte
      lea     eax { 2 ecx }
      neg     eax
      next;

code lastchar   ( str -- char )       \ returns last character from c-string
    1 1 in/out
      movzx   ecx byte { eax }
      movzx   eax byte { eax ecx }
      next;

code slastchar   ( addr len -- char )           \ returns last character from s-string
    2 1 in/out
      mov     ecx { ebp }              \ addr
      movzx   eax byte { -1 eax ecx }
      next;

code exchange   ( addr1 addr2 -- )            \ exchange cells pointed to
    2 0 in/out                     \ : exchange 2dup @ swap @ rot ! swap ! ;
      mov     ecx { ebp }            \ fetch addr1
      mov     edx { eax }            \ fetch ptr(addr2)
      mov     edi { ecx }            \ ptr(addr1)
      mov     { eax } edi
      mov     { ecx } edx
      mov     eax { cell ebp }
      next;

\ ------------------------ Character translation tables -----------------------

create ucasetab               \ upperCASE a thru z to A thru Z
    $00 C, $01 C, $02 C, $03 C, $04 C, $05 C, $06 C, $07 C,
    $08 C, $09 C, $0A C, $0B C, $0C C, $0D C, $0E C, $0F C,
    $10 C, $11 C, $12 C, $13 C, $14 C, $15 C, $16 C, $17 C,
    $18 C, $19 C, $1A C, $1B C, $1C C, $1D C, $1E C, $1F C,
    $20 C, $21 C, $22 C, $23 C, $24 C, $25 C, $26 C, $27 C, \ | !"#$%&'|
    $28 C, $29 C, $2A C, $2B C, $2C C, $2D C, $2E C, $2F C, \ |()*+,-./|
    $30 C, $31 C, $32 C, $33 C, $34 C, $35 C, $36 C, $37 C, \ |01234567|
    $38 C, $39 C, $3A C, $3B C, $3C C, $3D C, $3E C, $3F C, \ |89:;<=>?|
    $40 C, $41 C, $42 C, $43 C, $44 C, $45 C, $46 C, $47 C, \ |@ABCDEFG|
    $48 C, $49 C, $4A C, $4B C, $4C C, $4D C, $4E C, $4F C, \ |HIJKLMNO|
    $50 C, $51 C, $52 C, $53 C, $54 C, $55 C, $56 C, $57 C, \ |PQRSTUVW|
    $58 C, $59 C, $5A C, $5B C, $5C C, $5D C, $5E C, $5F C, \ |XYZ[\]^_|
    $60 C, $41 C, $42 C, $43 C, $44 C, $45 C, $46 C, $47 C, \ |`ABCDEFG|
    $48 C, $49 C, $4A C, $4B C, $4C C, $4D C, $4E C, $4F C, \ |HIJKLMNO|
    $50 C, $51 C, $52 C, $53 C, $54 C, $55 C, $56 C, $57 C, \ |PQRSTUVW|
    $58 C, $59 C, $5A C, $7B C, $7C C, $7D C, $7E C, $7F C, \ |XYZ{|}~|
    $80 C, $81 C, $82 C, $83 C, $84 C, $85 C, $86 C, $87 C,
    $88 C, $89 C, $8A C, $8B C, $8C C, $8D C, $8E C, $8F C,
    $90 C, $91 C, $92 C, $93 C, $94 C, $95 C, $96 C, $97 C,
    $98 C, $99 C, $9A C, $9B C, $9C C, $9D C, $9E C, $9F C,
    $A0 C, $A1 C, $A2 C, $A3 C, $A4 C, $A5 C, $A6 C, $A7 C,
    $A8 C, $A9 C, $AA C, $AB C, $AC C, $AD C, $AE C, $AF C,
    $B0 C, $B1 C, $B2 C, $B3 C, $B4 C, $B5 C, $B6 C, $B7 C,
    $B8 C, $B9 C, $BA C, $BB C, $BC C, $BD C, $BE C, $BF C,
    $C0 C, $C1 C, $C2 C, $C3 C, $C4 C, $C5 C, $C6 C, $C7 C,
    $C8 C, $C9 C, $CA C, $CB C, $CC C, $CD C, $CE C, $CF C,
    $D0 C, $D1 C, $D2 C, $D3 C, $D4 C, $D5 C, $D6 C, $D7 C,
    $D8 C, $D9 C, $DA C, $DB C, $DC C, $DD C, $DE C, $DF C,
    $E0 C, $E1 C, $E2 C, $E3 C, $E4 C, $E5 C, $E6 C, $E7 C,
    $E8 C, $E9 C, $EA C, $EB C, $EC C, $ED C, $EE C, $EF C,
    $F0 C, $F1 C, $F2 C, $F3 C, $F4 C, $F5 C, $F6 C, $F7 C,
    $F8 C, $F9 C, $FA C, $FB C, $FC C, $FD C, $FE C, $FF C,

create lcasetab               \ LOWERcase A thru Z to a thru z
    $00 C, $01 C, $02 C, $03 C, $04 C, $05 C, $06 C, $07 C,
    $08 C, $09 C, $0A C, $0B C, $0C C, $0D C, $0E C, $0F C,
    $10 C, $11 C, $12 C, $13 C, $14 C, $15 C, $16 C, $17 C,
    $18 C, $19 C, $1A C, $1B C, $1C C, $1D C, $1E C, $1F C,
    $20 C, $21 C, $22 C, $23 C, $24 C, $25 C, $26 C, $27 C, \ | !"#$%&'|
    $28 C, $29 C, $2A C, $2B C, $2C C, $2D C, $2E C, $2F C, \ |()*+,-./|
    $30 C, $31 C, $32 C, $33 C, $34 C, $35 C, $36 C, $37 C, \ |01234567|
    $38 C, $39 C, $3A C, $3B C, $3C C, $3D C, $3E C, $3F C, \ |89:;<=>?|
    $40 C, $61 C, $62 C, $63 C, $64 C, $65 C, $66 C, $67 C, \ |@abcdefg|
    $68 C, $69 C, $6A C, $6B C, $6C C, $6D C, $6E C, $6F C, \ |hijklmno|
    $70 C, $71 C, $72 C, $73 C, $74 C, $75 C, $76 C, $77 C, \ |pqrstuvw|
    $78 C, $79 C, $7A C, $5B C, $5C C, $5D C, $5E C, $5F C, \ |xyz[\]^_|
    $60 C, $61 C, $62 C, $63 C, $64 C, $65 C, $66 C, $67 C, \ |`abcdefg|
    $68 C, $69 C, $6A C, $6B C, $6C C, $6D C, $6E C, $6F C, \ |hijklmno|
    $70 C, $71 C, $72 C, $73 C, $74 C, $75 C, $76 C, $77 C, \ |pqrstuvw|
    $78 C, $79 C, $7A C, $7B C, $7C C, $7D C, $7E C, $7F C, \ |xyz{|}~|
    $80 C, $81 C, $82 C, $83 C, $84 C, $85 C, $86 C, $87 C,
    $88 C, $89 C, $8A C, $8B C, $8C C, $8D C, $8E C, $8F C,
    $90 C, $91 C, $92 C, $93 C, $94 C, $95 C, $96 C, $97 C,
    $98 C, $99 C, $9A C, $9B C, $9C C, $9D C, $9E C, $9F C,
    $A0 C, $A1 C, $A2 C, $A3 C, $A4 C, $A5 C, $A6 C, $A7 C,
    $A8 C, $A9 C, $AA C, $AB C, $AC C, $AD C, $AE C, $AF C,
    $B0 C, $B1 C, $B2 C, $B3 C, $B4 C, $B5 C, $B6 C, $B7 C,
    $B8 C, $B9 C, $BA C, $BB C, $BC C, $BD C, $BE C, $BF C,
    $C0 C, $C1 C, $C2 C, $C3 C, $C4 C, $C5 C, $C6 C, $C7 C,
    $C8 C, $C9 C, $CA C, $CB C, $CC C, $CD C, $CE C, $CF C,
    $D0 C, $D1 C, $D2 C, $D3 C, $D4 C, $D5 C, $D6 C, $D7 C,
    $D8 C, $D9 C, $DA C, $DB C, $DC C, $DD C, $DE C, $DF C,
    $E0 C, $E1 C, $E2 C, $E3 C, $E4 C, $E5 C, $E6 C, $E7 C,
    $E8 C, $E9 C, $EA C, $EB C, $EC C, $ED C, $EE C, $EF C,
    $F0 C, $F1 C, $F2 C, $F3 C, $F4 C, $F5 C, $F6 C, $F7 C,
    $F8 C, $F9 C, $FA C, $FB C, $FC C, $FD C, $FE C, $FF C,

\ -------------------- String Primitives ------------------------------------

\ COMPARE compares two strings. The return value is:
\
\      0 = string1 = string2
\      -1 = string1 < string2
\      1 = string1 > string2

code compare    ( adr1 len1 adr2 len2 -- n )    \ 0 is equal , -1 < and 1 >
    4 1 in/out
      mov     edi { ebp }              \ edi=adr2
      mov     ecx { cell ebp }            \ ecx=len1
      mov     esi { 2 cells ebp }            \ esi=adr1
      cmp     ecx eax      \ compare lengths
      cmova   ecx eax      \ compare shorter of the strings
      mov     edx 1
      cmova   eax edx      \ set string1 longer flag
      mov     edx 0
      cmovz   eax edx      \ strings equal lengths
      mov     edx -1
      cmovb   eax edx      \ set string2 longer flag
      repz    cmps byte         \ compare the strings
      mov     esi 1
      cmovb   eax edx      \ string1 > string2
      cmova   eax esi      \ string1 < string2
      next;

code str=       ( adr1 len1 adr2 len2 -- flag ) \ compares two strings, case sensitive
    4 1 in/out             \ same as compare 0=
      mov     ecx { cell ebp }            \ ecx=len1
      cmp     eax ecx      \ if not equal lengths
      jnz     short @@7               \ false, strings not equal
      mov     edi { ebp }              \ edi=adr2
      mov     esi { 2 cells ebp }            \ esi=adr1
      or      eax -1               \ set true
      repz    cmps byte         \ equal?
      je      short @@9               \ true, strings equal
@@7:  xor     eax eax      \ false, strings not equal (eax=0)
@@9:  next;

code istr=      ( adr1 len1 adr2 len2 -- flag ) \ compares two strings, case insensitive
    4 1 in/out
      cmp     eax { cell ebp }            \ len1=len2?
      jnz     short @@7               \ no, leave now=false
      mov     edi { ebp }              \ edi=adr2
      mov     esi { 2 cells ebp }            \ esi=adr1
@@1:  sub     eax 1      \ decrement eax
      jb      short @@9               \ finished, and so eax=-1=true
      movzx   edx byte { eax edi }   \ get last byte of str2
      movzx   ecx byte { eax esi }   \ and of str1
      cmp     ecx edx      \ equal?
      je      short @@1               \ yes, back round
      movzx   ecx byte { ucasetab ecx } \ uppercase
      movzx   edx byte { ucasetab edx } \ uppercase
      cmp     ecx edx      \ equal?
      je      short @@1               \ yes, back round
@@7:  xor     eax eax      \ no, set false
@@9:  next;

code skip       ( adr len char -- adr' len' ) \ skip leading char
    3 2 in/out
      mov     ecx { ebp }
      test    ecx ecx
      jz      short @@8
      cmp     eax $20          \ skipping spaces?
      jne     short @@2        \ no: use repz scas

      mov     edi { cell ebp }    \ pointer
@@1:  cmp     byte { edi } $20
      ja      short @@7        \ skip if <= space
      add     edi 1
      sub     ecx 1
      jnz     short @@1
      jmp     short @@7

@@2:  mov     edi { cell ebp }    \ for exact match
      repz    scas byte
      je      short @@7
      sub     edi 1
      add     ecx 1
@@7:  mov     { cell ebp } edi
@@8:  mov     eax ecx
@@9:  next;

: blskip ( addr len -- addr' len' ) bl skip ;

code -skip     ( addr n1 chr -- addr n2 ) \ remove trailing char from addr,n1
    3 2 in/out
      mov     ecx { ebp }
      test    ecx ecx
      jz      short @@9
      mov     edx { cell ebp }
@@1:  cmp     { -1 edx ecx } al
      jne     short @@9
      sub     ecx 1
      jnz     short @@1
@@9:  mov     eax ecx
      next;

: -trailing  ( addr n1 -- addr n2 ) bl -skip ; \ remove trailing blanks from addr,n1
: -trailing-nulls ( addr n1 -- addr n2 )  0 -skip ; \ remove trailing nulls from addr,n1

\ Search str1 for substring str2.
\ If found, return the address of the start of the
\ string, the characters remaining in str1 and a true flag.
\ Otherwise return the original str1 and a false flag.

code search     ( addr1 len1 addr2 len2 -- addr3 len3 flag )
    4 3 in/out
      test    eax eax           \ if len2 is zero...
      jz      short @@9         \ then found flag=-1
      mov     ecx { cell ebp }  \ len1
      lea     edx { 1 ecx }     \ edx is len2+1
      sub     edx  eax          \ # of compares is len2-len1+1
      jbe     short @@6         \ not long enough, so exit flag=0
      push    ebx               \ save loop count
      mov     ebx { 2 cells ebp }    \ addr1

@@1:  mov     ecx eax           \ get len2 in ecx
      mov     esi { ebp }       \ addr2
      mov     edi ebx           \ addr1 in edi
      repz    cmps byte         \ equal?
      jz      short @@7         \ found, so exit
      add     ebx 1             \ next addr1
      sub     edx 1             \ reduce
      jnz     short @@1         \ not found, so next

      pop     ebx               \ restore ebx
@@6:  xor     eax eax
      add     ebp cell
      ret             \ return no success

@@7:  mov     ecx ebx           \ copy to ecx
      sub     ecx { 2 cells ebp }    \ found - original address
      sub     { cell ebp } ecx  \ adjust length
      mov     { 2 cells ebp } ebx    \ address where found
      pop     ebx               \ restore EBX
@@9:  or      eax -1            \ true flag
      next;

code scan       ( adr len char -- adr' len' )
    3 2 in/out
      mov     ecx { ebp }
      test    ecx ecx
      jz      short @@2
      mov     edi { cell ebp }
      repnz   scas byte
      jne     short @@1
      add     ecx 1
      sub     edi 1
@@1:  mov     { cell ebp } edi
@@2:  mov     eax ecx
      next;

code -scan     ( adr len char -- adr' len' )   \ scan string backwards
    3 2 in/out
      mov     ecx { ebp }
      mov     edx  ecx
      test    ecx ecx
      jz      short @@2
      mov     edi { cell ebp }
      add     edi  ecx
      std
      repnz   scas byte
      cld
      jne     short @@1
      sub     edx  ecx
      sub     edx 1
      add     edi 1
@@1:  mov     { cell ebp } edi
@@2:  mov     eax edx
      next;

\ -------------------- Dictionary hashes ------------------------------------

code thread-hash ( a1 n1 #threads -- n2 )        \ ignore case hash
    3 1 in/out
      push    eax
      mov     edi #16777619            \ fnv32 prime
      mov     eax #2166136261          \ fnv32 basis
      mov     esi { cell ebp }         \ get string address into esi
      mov     ecx { ebp }              \ count into ecx
@@1:  movzx   edx  byte { -1 esi ecx } \ get the char
      xor     al { lcasetab edx }      \ xor with lower case
      mul     edi
      sub     ecx 1
      ja      short @@1      \ +ve, keep going
      pop     edi
      xor     edx  edx
      div     edi            \ perform modulus by #threads
      lea     eax { edx *cell }        \ multiply by cell size
      next;

\ -------------------- Strings ----------------------------------------------

code /string    ( addr1 len1 n1 -- addr2 len2 )
    3 2 in/out
      mov     ecx { ebp }
      test    eax eax
      jle     short @@9
      cmp     eax ecx
      cmova   eax ecx
@@9:  add     { cell ebp } eax
      sub     ecx eax
      mov     eax ecx
      next;

code tr         ( addr len table -- )       \ translate a buffer
    3 0 in/out
      mov     esi { ebp }          \ buffer length
      mov     edi { cell ebp }        \ buff address
@@1:  sub     esi 1            \ past end of string?
      js      short @@9           \ yes, -ve so exit
      movzx   edx byte { edi esi }         \ get the char
      movzx   ecx byte { eax edx }         \ translate it
      mov     { edi esi } cl               \ and store it back
      jnz     short @@1
@@9:  mov     eax { 2 cells ebp }
      next;

code +place     ( addr len dest -- )  \ append string addr,len to counted dest
    3 0 in/out
      mov     ecx { ebp }             \ get len
      movzx   edx byte { eax }        \ get current length
      mov     edi maxcounted 1-
      sub     edi edx
      cmp     ecx edi      \ get min of ecx edi in ecx
      cmovg   ecx edi      \
      add     byte { eax } cl         \ save the new length
      lea     edi { 1 eax edx }       \ point at dest+1 + len
      mov     esi { cell ebp }        \ addr
      mov     byte { edi ecx } 0      \ trail with a null
      rep     movs byte
      mov     eax { 2 cells ebp }
      next;
' +place alias append

code place      ( addr len dest -- )  \ place string at counted str dest
    3 0 in/out
      mov     byte { eax } 0          \ zero the length
      jmp     short ' +place          \ uses +place
      next;

code c+place     ( char dest -- )     \ add a character to end of c-string
    2 0 in/out
      movzx   edx byte { eax }        \ length of cstring
      mov     ecx { ebp }             \ char
      add     byte { eax } 1          \ increment length
      mov     word { 1 eax edx } cx   \ store char & null
      mov     eax { cell ebp }
      next;

\ -------------------- UPPER/lower functions ------------------------

: upc           ( char -- char )       \ convert char to uppercase
      $ff and ucasetab + c@ ;

: upper         ( addr len -- )        \ translate to uppercase
      ucasetab tr ;          \ use tr to do the work

: lower         ( addr len -- )        \ translate to lowercase
      lcasetab tr ;          \ use tr to do the work

: uppercase     ( str -- str )         \ uppercase a c-string
      dup count upper ;

: lowercase     ( str -- str )         \ lowercase a c-string
      dup count lower ;

\ ---------------------------- Type words --------------------------------

\ For TO and +TO these will be used to generate a jump table to correctly
\ generate code for each type of name. Not yet implemented.

\ 0 constant tunk     \ constants for type system
\ bits 0-2            nonzero for TO type of word
\         0         TO doesn't work with this word
\         1         defer (works with IS)
\         2         value
\          3         2value
\          4 to 7    others (float, locals, object etc)
\ bit 3    1         bits 0-2 refer to locals
\ bit 4-7
\         0         TO works with this word (0-3 for type)
\         1 to F    unstructured

hex 

$01 constant tdef     \ defer
$02 constant tval     \ value
$03 constant t2vl     \ 2value

$81 constant tpri    \ primitive definition
$82 constant tcol    \ colon definition
$83 constant tnon    \ :noname definition
$84 constant tquo    \ quotation
$85 constant tdos    \ does>

$90 constant tcon    \ constant
$91 constant tcre    \ create
$92 constant tvar    \ variable
$93 constant toff    \ offset
$94 constant tusr    \ user
$95 constant t2vr    \ 2var
$96 constant t2cn    \ 2constant

$97 constant tvoc    \ vocabulary
$98 constant tlib    \ library
$99 constant timp    \ import
$9A constant texp    \ export
$9B constant tcbk    \ callback
$9C constant tfil    \ file

decimal

\ -------------------- System Wide Variables --------------------------

variable warning true warning !
variable msg 0 msg !        \ message pointer
variable throw_msgs 0 throw_msgs !     \ list header

\ -------------------- System Messages --------------------------------

\ These should really be in a resource or external file so that messages
\ can be translated to other locales.
((
    -1 ABORT
    -2 ABORT"
    -3 stack overflow
    -4 stack underflow
    -5 return stack overflow
    -6 return stack underflow
    -7 do-loops nested too deeply during execution
    -8 dictionary overflow
    -9 invalid memory address
    -10 division by zero
    -11 result out of range
    -12 argument type mismatch
    -13 undefined word
    -14 interpreting a compile-only word
    -15 invalid FORGET
    -16 attempt to use zero-length string as a name
    -17 pictured numeric output string overflow
    -18 parsed string overflow
    -19 definition name too long
    -20 write to a read-only location
    -21 unsupported operation (e.g., AT-XY on a too-dumb terminal)
    -22 control structure mismatch
    -23 address alignment exception
    -24 invalid numeric argument
    -25 return stack imbalance
    -26 loop parameters unavailable
    -27 invalid recursion
    -28 user interrupt
    -29 compiler nesting
    -30 obsolescent feature
    -31 >BODY used on non-CREATEd definition
    -32 invalid name argument (e.g., TO name)
    -33 block read exception
    -34 block write exception
    -35 invalid block number
    -36 invalid file position
    -37 file I/O exception
    -38 non-existent file
    -39 unexpected end of file
    -40 invalid BASE for floating point conversion
    -41 loss of precision
    -42 floating-point divide by zero
    -43 floating-point result out of range
    -44 floating-point stack overflow
    -45 floating-point stack underflow
    -46 floating-point invalid argument
    -47 compilation word list deleted
    -48 invalid POSTPONE
    -49 search-order overflow
    -50 search-order underflow
    -51 compilation word list changed
    -52 control-flow stack overflow
    -53 exception stack overflow
    -54 floating-point underflow
    -55 floating-point unidentified fault
    -56 QUIT
    -57 exception in sending or receiving a character
    -58 [IF], [ELSE], or [THEN] exception
    -59 ALLOCATE
    -60 FREE
    -61 RESIZE
    -62 CLOSE-FILE
    -63 CREATE-FILE
    -64 DELETE-FILE
    -65 FILE-POSITION
    -66 FILE-SIZE
    -67 FILE-STATUS
    -68 FLUSH-FILE
    -69 OPEN-FILE
    -70 READ-FILE
    -71 READ-LINE
    -72 RENAME-FILE
    -73 REPOSITION-FILE
    -74 RESIZE-FILE
    -75 WRITE-FILE
    -76 WRITE-LINE
    -77 Malformed xchar
    -78 SUBSTITUTE
    -79 REPLACES
))

\ ANS defined
  -1    equ throw_abort      \ no message
  -2    equ throw_abortq               \ message from abort"
  -4    equ throw_stackunder           \ " stack underflow"
  -9    equ throw_invmemaddr           \ " invalid memory address"
  -10   equ throw_div0       \ " division by zero"
  -13   equ throw_undefined            \ " is undefined"
  -14   equ throw_componly             \ " is compile only"
  -16   equ throw_namereqd             \ " requires a name"
  -22   equ throw_mismatch             \ " control structure mismatch"
  -38   equ throw_filenotfound         \ " file not found"
  -39   equ throw_earlyeof             \ " unexpected end of file"
  -42   equ throw_floatdiv0            \ " FP divide by zero"

(( to be addressed; http://www.forth200x.org/throw-iors.html
  -59	equ throw_allocate
  -60	equ throw_free
  -61	equ throw_resize
  -62	equ throw_close-file
  -63	equ throw_create-file
  -64	equ throw_delete-file
  -65	equ throw_file-position
  -66	equ throw_file-size
  -67	equ throw_file-status
  -68	equ throw_flush-file
  -69	equ throw_open-file
  -70	equ throw_read-file
  -71	equ throw_read-line
  -72	equ throw_rename-file
  -73	equ throw_reposition-file
  -74	equ throw_resize-file
  -75	equ throw_write-file
  -76	equ throw_write-line
))

  -77   equ throw_malxchar             \ " malformed xchar"

\ System extended
  -260  equ throw_notdefer             \ " is not a defer"
  -261  equ throw_defernoval           \ " defer is uninitialised"
  -262  equ throw_notvalue             \ " is not a value, local or defer"
  -270  equ throw_outofmem             \ " out of memory"
  -271  equ throw_memallocfail         \ " memory allocation failed"
  -272  equ throw_memrelfail           \ " memory release failed"
  -280  equ throw_filecreatefail       \ " create-file failed"
  -281  equ throw_filereadfail         \ " read-file failed"
  -282  equ throw_filewritefail        \ " write-file failed"
  -290  equ throw_interponly           \ " is interpretation only"
  -310  equ throw_impnotfound          \ " import not found"
  -311  equ throw_winerr               \ " windows error"
  -320  equ throw_stackchg             \ " control stack mismatch"
  -330  equ throw_methexit             \ " can't use exit in a method"
  -331  equ throw_methdoes>            \ " can't use does> in a method"
  -332  equ throw_meth;m               \ " method must end with ;m"
  -340  equ throw_auxstacku            \ " aux stack underflow"
  -341  equ throw_auxstacko            \ " aux stack overflow"
  -350  equ throw_ctexecute            \ " ct is immediate"
  -360  equ throw_fatal      \ " fatal error"
  -400  equ throw_malestr              \ " malformed escape string"
  -500  equ throw_plistfull            \ " path list full"
  -600  equ throw_macroerr             \ " internal macro error"

\ Warnings
  -4100 equ warn_notunique             \ " is already defined"
  -4101 equ warn_sysword               \ " is a system word in an application definition"
  -4102 equ warn_sysword2              \ " is an application word set to a system word"
  -4103 equ warn_stack       \ " stack depth increased"

\ ANS defined
  throw_msgs link, throw_stackunder     , ," stack underflow"
  throw_msgs link, throw_invmemaddr     , ," invalid memory address"
  throw_msgs link, throw_div0           , ," division by zero
  throw_msgs link, throw_undefined      , ," is undefined"
  throw_msgs link, throw_componly       , ," is compilation only"
  throw_msgs link, throw_namereqd       , ," requires a name"
  throw_msgs link, throw_mismatch       , ," control structure mismatch"
  throw_msgs link, throw_filenotfound   , ," file not found"      
  throw_msgs link, throw_earlyeof       , ," unexpected end of file"
  throw_msgs link, throw_floatdiv0      , ," FP divide by zero"
  throw_msgs link, throw_malxchar       , ," malformed xchar"

\ System extended
  throw_msgs link, throw_notdefer       , ," is not a DEFER"
  throw_msgs link, throw_defernoval     , ," DEFER is uninitialised"
  throw_msgs link, throw_notvalue       , ," can't be used with TO or +TO"
  throw_msgs link, throw_outofmem       , ," out of memory"
  throw_msgs link, throw_memallocfail   , ," memory allocation failed"
  throw_msgs link, throw_memrelfail     , ," memory release failed"
  throw_msgs link, throw_filecreatefail , ," create-file failed"
  throw_msgs link, throw_filereadfail   , ," read-file failed"
  throw_msgs link, throw_filewritefail  , ," write-file failed"
  throw_msgs link, throw_interponly     , ," is interpretation only"
  throw_msgs link, throw_impnotfound    , ," import not found"
  throw_msgs link, throw_winerr         , ," windows call error"
  throw_msgs link, throw_stackchg       , ," control stack mismatch"
\  throw_msgs link, throw_methexit       , ," can't be used in a method"
\  throw_msgs link, throw_methdoes>      , ," can't use does> in a method"
\  throw_msgs link, throw_meth;m         , ," method must end with ;m"
  throw_msgs link, throw_auxstacku      , ," aux stack underflow"
  throw_msgs link, throw_auxstacko      , ," aux stack overflow"
\  throw_msgs link, throw_ctexecute      , ," ct is immediate"
  throw_msgs link, throw_fatal          , ," fatal error"
  throw_msgs link, throw_malestr        , ," malformed escape string"
  throw_msgs link, throw_plistfull      , ," path list full"
  throw_msgs link, throw_macroerr       , ," internal macro error"

\ Warnings
  throw_msgs link, warn_notunique      , ," is redefined"

\ -------------------- User Variables ---------------------------------------

\ Task based variables are here. Each task has its own local copy of an uninitialised
\ variable. All other VARIABLEs and VALUEs are global the the entire process, and must
\ not be changed in a task unless locked or you know what you're doing.

0                 \ init offset
  dup user up0              \ base for addressing
  dup user handler    cell+ \ throw frame ptr
  dup user win-handler cell+ \ windows exception handler
  dup user sp0        cell+ \ initial data stack pointer
  dup user rp0        cell+ \ initial return stack pointer
  dup user base       cell+ \ numeric radix
  dup user buf-off    cell+ \ buffer offset
  dup user buf-base   cell+ \ buffer base
  dup user rllen      cell+ \ read line length, used in file i/o see read-line
  dup user rlneof     cell+ \ read line not eof, used in file i/o see read-line
  dup user ior        cell+ \ ior for extended windows errors
  dup user tcb        cell+ \ task control block ptr

  dup user hld              \ numeric output pointer
  maxbuffer chars aligned + \ numeric output formatting buffer
  dup user pad              \ extra
  maxbuffer chars aligned + \ user string buffer

  dup user exc-rec    exc% +     \ save area for exception record
  dup user ctx-rec    ctx% +     \ context record
  dup user exc-stack  8 cells+   \ stack saved
  dup user exc-rstack 8 cells+   \ rstack saved

variable next-user next-user !

\ -------------------- Circular buffer support -----------------------

\ Supports 64 buffers of 512 bytes in a circular buffer. No de-allocation
\ is supported, so it's possible to over-run the buffers if you stack
\ more than 64 deep. Thread safe.

maxbuffer 64 * equ buf-size

((
: buf-allot  ( -- addr )
             buf-off 2@
             dup maxbuffer + dup
             [ buf-size maxbuffer - ] literal <= and
             buf-off ! + ;
))

code buf-allot ( -- addr )
    0 1 in/out
      mov     { -cell ebp } eax          ( save eax )
      mov     eax { buf-off ebx }        ( offset )
      add     eax maxbuffer
      cmp     eax buf-size maxbuffer -
      jl      short @@1
      xor     eax eax
@@1:  mov     { buf-off ebx } eax
      add     eax { buf-base ebx }
      next;

\ -------------------- throw/catch handler ------------------------

code catch ( xt -- flag )
      mov     ecx eax              ( save xt )
      push    ebp                  \ sp@ >r
      push    { handler ebx }      \ handler @ >r
      mov     { handler ebx } esp  \ rp @ handler !
      mov     eax { ebp }          ( drop xt )
      add     ebp cell
      call    ecx                  \ execute
      mov     { -cell ebp } eax
      pop     { handler ebx }      \ r> handler !
      sub     ebp cell
      add     esp cell             \ junk old ebp
      xor     eax eax              \ 0
      next;

code throw ( n -- )
      test    eax eax              \ ?dup if
      jz      ' drop               ( drop if zero )
      mov     esp { handler ebx }  \ handler @ rp!
      pop     { handler ebx }      \ r> handler !
      pop     ebp                  \ r> swap >r sp! drop r>
      next;   ( don't inline )

: abort    ( -- )        throw_abort throw ;
: nabort!  ( addr n -- ) swap msg ! throw ; \ set message, n throw
: abort!   ( addr -- )   throw_abortq nabort! ; \ abort, print counted string
: ?throw   ( f n -- )    swap if throw else drop then ;

\ -------------------- Kernel Structures ----------------------------
\ See gstructs.f for structure

include src/kernel/gstructs.fs        \ kernel structures

\ -------------------- Compiling words ----------------------------

\  DP is the current data pointer, DP @ is the equivalent of HERE
\
\  Each set of pointers to a data ("dictionary") space is a structure.
\  3 defined by default;
\    APPDATA -- std data area
\    SYSDATA -- system area, used by system for headers etc.
\    CODE    -- executable code
\  Actual values for these 3 are filled in by the meta compiler.

variable dp-link       \ list of dp structures
    0 dp-link !

(     name addr len top  link           label )
create sdp  0 , 0 , 0 , dp-link link, ," sys"     \ system
create adp  0 , 0 , 0 , dp-link link, ," app"     \ application
create cdp  0 , 0 , 0 , dp-link link, ," code"    \ code

adp value dp           \ data pointer defaults to app space
cdp value xdp          \ xdp is the default code pointer
dp  value odp
xdp value oxdp

\ ----------------- Switching section areas --------------------

\ To switch between data areas
\
\ IN-xxxx is used to switch HERE ALLOT , W, etc to point
\ to the specific data area; the current DP is saved in ODP, so
\ it can be reseted using IN-PREV.
\

: get-section   ( -- n m ) dp xdp ;
: set-section   ( n m -- ) xdp to oxdp
                 dp  to odp
                 to xdp to dp ;

: in-app        ( -- ) adp cdp  set-section ;
: in-sys        ( -- ) sdp cdp  set-section ;
: in-prev       ( -- ) odp to dp oxdp to xdp ;

: (free)        ( sect -- ) dup data.top @ swap @ - ;
: app-free      ( -- n1 ) adp (free) ;
: sys-free      ( -- n1 ) sdp (free) ;
: code-free     ( -- n1 ) cdp (free) ;
: unused        ( -- n1 ) dp  (free) ;

: here  ( -- a ) dp @ ;                    \ next free byte
: allot ( n -- ) dp +! ;                   \ allot n bytes
: c,    ( n -- ) here b! 1         allot ; \ char store
: ,     ( n -- ) here  ! cell      allot ; \ cell store, incr
: 2,    ( n -- ) here 2! [ 2 cells ] literal allot ; \ double cell store, incr
: b,    ( n -- ) here c! 1         allot ; \ byte store
: w,    ( n -- ) here w! 2         allot ; \ 2byte store, incr
: L,    ( n -- ) here L! 4         allot ; \ 4byte store, incr
: q,    ( d -- ) here q! 8         allot ; \ 8byte store, incr
: allot&zero ( n -- ) 0max here over allot swap erase ; \ allot and clear n bytes

: align         ( -- )                \ align data space & pad
      here dup aligned over -         \ addr len
      dup allot erase ;               \ allot

: code-here     ( -- a )  xdp @ ;
: code-allot    ( n1 -- ) xdp +! ;
: code-b,       ( n -- )  code-here b! 1 code-allot ;
: code-w,       ( n -- )  code-here w! 2 code-allot ;
: code-L,       ( n -- )  code-here L! 4 code-allot ;
: code-q,       ( n -- )  code-here q! 8 code-allot ;
: code-align    ( -- ) (  ) ; immediate

: sys-here      ( -- a )  sdp @ ;

\ -------------------- Vocabulary/header support -----------------------

\ LAST is the NT (name token) of the last named word created, so 
\ :NONAME and [: do not set this variable as they have no names.
\ LATESTXT is set to the xt of the current word being defined.
\ Includes :NONAME CODE : CREATE and all its children.

variable last         \ nt of last header created
variable last-link              \ address of last link for last header created
variable latestxt               \ xt of last definition

\ --------------------------- Compiling words -------------------------------

: >ct            ( xt -- ct )       dup cell- @ + ;    \ given an xt, get the ct (rel addr)
: >comp          ( xt -- comp )     >ct [ 0 head.comp 0 head.ct - ] literal + ; \ point to the comp field
: >name          ( xt -- nt )       >ct [ 0 head.nt 0 head.ct - ]   literal + ; \ get the name
: link>name      ( lfa -- nt )      [ 0 head.nt 0 head.link - ]     literal + ;
: compile,       ( xt -- )          dup >comp perform ;     \ compile xt on the stack
: immediate      ( -- )             ['] execute last @ head.ct ! ; \ last header is immediate word
: name>interpret ( nt -- xt )       head.xtptr @ ;          \ nt to xt
: name>compile   ( nt -- xt1 xt2 )  head.ct 2@ ;            \ nt to compilation token
: name>string    ( nt -- c-addr u ) head.nt count ; inline  \ nt to name string
: compiles       ( xt1 xt2 -- )     >comp ! ;               \ set the compile field of xt2 to xt1
: compiles-me    ( xt -- )          latestxt @ compiles ;   \ sets xt as compilation for last name
: tfa!           ( type -- )        last @ head.tfa c! ;    \ set the type
: tfa@           ( nt -- type )     head.tfa c@ ;           \ get type

: (comp-only) ( -- ) throw_componly throw ;   \ compile only message

\ ------------------------- Code generation words ---------------------------

\ sync-code is called prior to generating any code, to allow optimisers and
\ other code generation routines to delay generating code; they can take
\ a call to sync-code as an indication that they need to generate now.
\ Not all code generation routines need to call sync-code; only those
\ that are publically available, such as routines in the comp action field.

\ This should be replaced in optimizing code; it's effectively a not very
\ smart basic-block boundary & exit indicator.

defer sync-code ' noop is sync-code         \ ***

: gen-2op  ( n op2 -- ) sync-code code-w, code-L, ; \ generate pending code then op n
: gen-2opc ( n op2 -- ) sync-code code-w, code-b, ; \ generate pending code then op n
: gen-2opi ( n op1 -- ) sync-code code-b, code-L, ; \ generate pending code then op n

: user-addr ( n -- )
    ?dup if
      dup $FF > if $8B8D gen-2op else $4B8D gen-2opc then ( lea ecx { n ebx }  )
    else
      sync-code $CB8B code-w, ( mov ecx ebx )
    then ;

: add-ebp,n    ( n -- ) \ equiv of ndrop
    ?dup if
      dup 0> if $C583 else negate $ED83 then
      gen-2opc
    then ;
: mov-eax,n    ( n -- )   
    ?dup if
      dup 1+ if $B8 gen-2opi else $C883 gen-2opc then
    else
      sync-code $C033 code-w,
    then ;
: mov-ecx,n  ( n -- )
    $B9 gen-2opi ;        

0 value tail-call

: xt-rel,     ( xt op -- )         \ generate opcode and rel adjusted xt
    sync-code code-b, 
    code-here - cell- code-L, ;
: xt-call,  ( xt -- )              \ core routine for generation a call
    $e8 xt-rel,                    \ compile call to xt on the stack
    code-here to tail-call ;       \ possible tail call
    
: xt-jmp,     ( xt -- )    $e9 xt-rel, ;     \ generate jump to xt on the stack
: xt-call[],  ( addr -- )  $15FF gen-2op ;   \ generate call through address
: xt-jmp[],   ( addr -- )  $25FF gen-2op ;   \ generate jump through address

\ change a call into a jmp; $e8->$e9, $15->$25
\ change a jmp into a call; $e9->$e8, $25->$15
: call>jmp ( addr -- ) dup c@ $e8 = if $e9 else $25 then swap c! ;
: jmp>call ( addr -- ) dup c@ $e9 = if $e8 else $15 then swap c! ;

\ The kernel has no assembler, so there's no "postponed assembly" possible.
\ Code is copied by marking as inline

: copy-code ( addr len -- ) \ copy a code word
    sync-code               \ ensure outstanding code generated
    code-here swap dup code-allot move ;

: inline, ( xt -- ) \ inline a word
    dup >name head.ofa w@ copy-code ; \ get the length, inline it

: inline ( -- ) \ code will be inlined
    tail-call 0= if      \ if no calls, inlineable
      ['] inline, compiles-me      \ as tail-call is set for any call
    then ;

\ ------------------------ Branching primitives ----------------------------

\ Control stack is implemented on the data stack. Entries are 2 cells; first is
\ the mark, the second is the type of mark.

: roll ( xu xu-1 . . . x0 u -- xu-1 . . . x0 xu )
    dup>r pick sp@ dup cell+     \ not optimal but rarely used
    r> cells cell+ move drop ;

: cs-pick 2* 1+ dup>r pick r> pick ;  \ cs is 2 cell items
: cs-roll 2* 1+ dup>r roll r> roll ;  \ 1 cs-roll is 2swap

: ?pairs ( n1 n2 -- ) xor throw_mismatch ?throw ;

: mark>    ( -- n ) code-here ;          \ mark for resolve
: <resolve ( n -- ) code-here - code-here 4 - L! ; \ backward jump resolve
: >resolve ( n -- ) code-here over - swap 4 - L! ; \ forward jump resolve

( equates for marks )
-1 equ mark-if
-2 equ mark-begin
-3 equ mark-do
-4 equ mark-?do
-5 equ mark-leave

\ ----------------------------- Branching ----------------------------------

: ahead ( c: -- orig )                     only-compiles> postpone bra   mark> mark-if ; 
: if ( c: -- orig ) ( x -- )               only-compiles> postpone ?bra  mark> mark-if ; 
: -if ( c: -- orig ) ( x -- x )            only-compiles> postpone -?bra mark> mark-if ; 
: then ( c: orig -- )                      only-compiles> sync-code mark-if ?pairs >resolve ; 
: else ( c: orig1 -- orig2 )               only-compiles> postpone ahead 2swap postpone then ; 
: begin ( c: -- dest )                     only-compiles> sync-code mark> mark-begin ; 
: while ( c: dest -- orig dest ) ( x -- )  only-compiles> postpone if 2swap ; 
: again ( c: dest -- )                     only-compiles> mark-begin ?pairs postpone bra <resolve ; 
: until ( c: dest -- ) ( x -- )            only-compiles> mark-begin ?pairs postpone ?bra <resolve ; 
: repeat ( c: orig dest -- )               only-compiles> postpone again postpone then ;  
: recurse ( -- )                           only-compiles> latestxt @ xt-call, ; ( over xt-call, )

\ --------------------  DO/I/J/LOOP  ---------------------------------

code bounds     ( adr len -- lim first ) \ calculate loop bounds from adr,len
    2 2 in/out
      xadd    { ebp } eax             \ exchange & add equiv of over + swap
      next; inline

: unloop     2rdrop     ; inline

code bra-?do ( -- ) ( non-executable, for do )
      cmp     edx ecx
      jz      0
      next; inline

code do-part1 ( n n -- )
    2 0 in/out
      mov     edx eax
      mov     ecx { ebp }
      mov     eax { cell ebp }
      next; inline

code do-part2 ( -- )
      add     ecx $80000000      \ bias ecx
      sub     edx ecx            \ adjust n2 by ecx
      push    ecx
      push    edx
      next; inline

: do-entry ( -- )      sync-code postpone do-part1 ;
: do-bias ( -- )       postpone do-part2 mark> mark-do ;
: do ( c: -- do-sys )  only-compiles> do-entry do-bias ; 2 0 in/out

: ?do ( c: -- ?do-sys do-sys )
    only-compiles>
      do-entry
      postpone bra-?do mark> mark-?do        \ for "don't do this loop"
      do-bias ; 2 0 in/out                   \ the repeat location

code i ( -- n )
    0 1 in/out
      mov     { -cell ebp } eax
      mov     eax {      esp }
      add     eax { cell esp }
      next; inline

code j ( -- n )
    0 1 in/out
      mov     { -cell ebp } eax
      mov     eax { 2 cells esp }
      add     eax { 3 cells esp }
      next; inline

: leave ( do-sys ... -- orig do-sys ... )       \ forward branch to unloop in loop
    only-compiles>
    0 begin over mark-do <> while
        1+ -rot 2>r                 \ strip control stack down to do-sys
      repeat >r
      postpone ahead drop mark-leave 2swap    \ branch, put leave-sys under do-sys
      r> begin ?dup while 1- 2r> rot repeat   \ restore the leave entries
      ; 

: ?leave ( f -- ) ( r: n1 n2 -- )
    only-compiles>
      postpone if
      postpone leave
      postpone then ; 1 0 in/out

: (loop-then) ( x -- )
      drop mark-if postpone then ;

: (loop) ( cs: i*x addr -- )
      mark-do ?pairs <resolve       \ for "do loop again"
      begin dup mark-leave = while  \ resolve the leave entries
        (loop-then)                 \ fixup the branch(es)
      repeat
      postpone unloop               \ now generate the unloop
      dup mark-?do = if
        (loop-then)                 \ and resolve the ?do "don't do this loop" jump
      then ;

code _loop
      add     dword { esp } 1
      jno     0
      next; inline

code _+loop
      add     ebp cell
      add     { esp } eax
      mov     eax { -cell ebp }
      jno     0
      next; inline

code _-loop
      add     ebp cell
      sub     { esp } eax
      mov     eax { -cell ebp }
      jno     0
      next; inline

: loop  ( -- ) ( c: do-sys -- ) ( r: n1 n2 -- )
    only-compiles>
      postpone _loop
      (loop) ; 

: -loop ( n -- ) ( c: do-sys -- ) ( r: n1 n2 -- )
    only-compiles>
      postpone _-loop
      (loop) ; 1 0 in/out

: +loop ( n -- ) ( c: do-sys -- ) ( r: n1 n2 -- )
     only-compiles>
       postpone _+loop
       (loop) ; 1 0 in/out

\ -------------------- Various support words --------------------------

variable ste-i -1 ste-i !               \ # of input cells, -ve is unknown
variable ste-o -1 ste-o !               \ # of output cells, -ve is unknown

\ if else then ahead again need work to correctly caluclate the stack effects
\ probably best done in the optimiser

: ste-reset ( -- ) ste-i on  ste-o on  ; \ reset stack effects

: ste-adjust ( -- )       \ generate adjustment offset
    ste-i @ ste-o @
    2dup or 0< not -rot - cells and \ zero if either -ve
    add-ebp,n      \ add or sub ebp , n
    ste-reset ;

: ste-calc  ( in out -- )           \ calculate stack effects
    2dup or ste-i @ or ste-o @ or 0< \ if either is -ve
    if
      2drop ste-reset       \ set both -ve
    else over ste-o @ - dup 0>  \ get in stk value
      if
        dup ste-i +! ste-o +! \ adjust
      else drop then
      swap - ste-o +!
    then ;

: (in/out@) ( nt -- in out )         \ get the ste values
    head.stk dup sb@ swap 1+ sb@ ;

: in/out@   ( -- in out )             \ get the ste values
    last @ (in/out@) ;

: (in/out!)  ( -- )          \ set the ste values in the last xt
    ste-i @ ste-o @           \ get calc values
    last @ head.stk
    dup>r 1+ c!
    r> c! ;

: in/out    ( in out -- )             \ set the ste values
    ste-o ! ste-i ! (in/out!) ; \ set calc values

: (ofa-calc)    ( ofa -- )
    sync-code              \ ensure all generated
    code-here swap -       \ length of the code
    last @ head.ofa w! ;   \ save length

: ofa-calc      ( -- ) 
    latestxt @ (ofa-calc) ;

\ ---------------------------- Defining Words --------------------------------

1 equ body-off            \ the offset where a body is

code >body
    1 1 in/out
      mov     eax { body-off eax }   \ body-off + @
      next; inline

: literal   ( n -- ) ( r: -- n )
    only-compiles>
      postpone dup
      mov-eax,n ; 0 1 in/out

: plit postpone literal ;

: 2literal  ( n m -- )              \ run-time skeleton for 2literal
    only-compiles> swap plit plit ; 0 2 in/out

: p2lit postpone 2literal ;

: (docon,) ( xt const -- )
    mov-ecx,n xt-jmp, ofa-calc ;

: (gen-here)   ( xt type-of-name <-name-> -- )   \ generate do code
    header tfa! here (docon,) ;            \ header

: (comp-cons) ( xt -- ) >body plit ;
: (comp-val)  ( xt -- ) (comp-cons) postpone @ ; \ generate @

code (exec-val)  ( -- n )                \ variable @
    0 1 in/out
      mov     { -4 ebp } eax
      mov     eax { ecx }
      next; inline

: value     ( n -<name>- )         \ self fetching value
    ( -- n )                \ run time
    ['] (exec-val) tval (gen-here) ,
    ['] (comp-val) compiles-me      \ make the defined word compile this
    0 1 in/out ; 1 0 in/out

: create    ( -<name>- )           \ pointer
    ( -- n )                \ run time
    ['] dovar tcre (gen-here)
    ['] (comp-cons) compiles-me     \ doesn't work because of DOES> , needs fixed ???
    0 1 in/out ; 

: constant  ( -<name>- )           \ pointer
    ( -- n )                \ run time
    header tcon tfa! ['] dovar swap (docon,)
    ['] (comp-cons) compiles-me     \ doesn't work because of DOES> , needs fixed ???
    0 1 in/out ; 

: variable  ( "name")              \ compile time
    ( -- n )                \ run time
    ['] dovar tvar (gen-here) 0 ,
    ['] (comp-cons) compiles-me
    0 1 in/out ; 

: user      ( n -<name>- )          \ create a user variable
    header tusr tfa!
    user-addr ['] dovar xt-jmp, ofa-calc
    0 1 in/out
    ; 1 0 in/out

: +user     ( n -<name>- )      \ creates a user. a user can be
              \ a byte, cell, float, string or stack
    next-user @ swap over + next-user !
    user ;

\ -------------------- Link Operations (Single Linked)  --------------------

\ usage: i*x node ' xt-to-apply list-apply
\ Follows link at offset 0 in each node, for each link executes xt-to-apply
\ The xt-to-apply sees ( i*x node ), returns ( i*x' )
\ Safe to use even if the iterator destroys next link and can be used recursively

: list-apply   ( i*x node iterator -- i*x' )     \ list-apply over a linked list of nodes
    >r            \ the nodes have a next pointer at offset 0
    begin ?dup
    while
      r@ over @
      >r execute r>         \ iterator sees ( i*x node), returns ( i*x'))
    repeat rdrop ;

: add-link  ( addr node -- )  2dup @ swap ! ! ;      \ add a node to a node
: link,     ( addr -- )       align here over @ , swap ! ;

code un-link    ( addr link -- f1 )           \ unlink from list link
    2 1 in/out
      mov     edx { ebp }            \ eax=link, edx=addr
@@1:  mov     ecx { eax }            \ link @
      or      ecx ecx              \ if zero
      jz      short @@8             \ failed to find
      cmp     edx  ecx              \ my link?
      je      short @@2             \ yes, go unlink
      mov     eax ecx              \ next link
      jmp     short @@1             \ back round
@@2:  mov     ecx { edx }            \ fetch me
      mov     { eax } ecx          \ prev=me
      xor     eax eax              \ tos=0
      jmp     short @@9             \ exit
@@8:  or      eax -1             \ tos=-1
@@9:  next;

\ -----------------------  Input Source Definitions  -----------------------

\ The following are all the areas that need save and restored by
\ save-input and restore-input.

create (source)     \ input stream
  here
      0 ,           \ length of input stream
      0 ,           \ address of input stream
      0 ,           \ >in
      0 ,           \ source-id
      0 ,           \ sourceline#
      0 ,           \ sourcefilename
      0 ,           \ pair: value of map-len
      0 ,           \       value of map-base
      0 ,           \ pair: length of last parse
      0 ,           \       offset of last parse
here swap - equ (source-len)

(source) 2 cells+ constant >in            \ pointer to >in
(source) 3 cells+ constant (source-id)    \ pointer to source-id
(source) 4 cells+ constant (source-l#)    \ source line number
(source) 5 cells+ constant (source-nm)    \ source file name
(source) 6 cells+ constant (source-map)   \ source file map
(source) 8 cells+ constant (source-parse) \ source start of parsed

: source         ( -- addr len ) (source) 2@ ;
: tib            ( -- addr )     (source) cell+ @ ;
: source-id      ( -- id )       (source-id) @ ;
: sourcefilename ( -- addr len ) (source-nm) @ count ;
: sourceline#    ( -- n )        (source-l#) @ ;
: source-remain  ( -- addr len ) source >in @ /string ; \ remaining string

\ -------------------- save/restore file input ------------------------------

: save-input    ( -- ... cells n )             \ save input
    sp@ (source-len) - dup>r sp!
    (source) r> (source-len) move
    [ (source-len) 2 rshift ] literal ;

: restore-input ( ... cells n -- 0 )
    cells >r sp@ (source) r@ move  \ copy from stack to dest
    sp@ r> + sp! 0 ;               \ adjust stack (n drops)

\ ----------------- Return Stack Operators ----------------------------

code n>r        ( ... n -- r: ... n )   \ move from stack to rstack
\      pop     edx             \ return address
      mov     ecx eax         \ # of cells
      neg     eax             \ make negative (stacks grow down)
      mov     esi ebp         \ esi is source (stack)
      lea     esp { -cell esp eax *cell } \ adjust rstack
      lea     ebp {  cell ebp ecx *cell } \ adjust stack
      mov     { esp } ecx     \  # cells
      lea     edi { cell esp }   \ destination
      rep     movs dword      \ move the cells
      mov     eax { -cell ebp }
\      jmp     edx
      next; inline            \ must be inlined 

code nr>        ( r: ... n -- ... n )   \ move from rstack to stack
\      pop     edx             \ return address
      mov     { -cell ebp } eax   \ save previous eax
      pop     eax             \ # of cells
      mov     ecx eax        \ into ecx
      neg     eax             \ make negative (stacks grow down)
      lea     ebp { -cell eax *cell ebp } \ same for stack
      mov     esi esp        \ esi is source (rstack)
      mov     eax ecx
      mov     edi ebp
      lea     esp { esp eax *cell } \ adjust rstack
      rep     movs dword           \ move the cells
\      jmp     edx
      next; inline            \ must be inlined

\ Due to the way that throw and catch work, and the restore of the source area
\ on an error, the source of the error is not available at the point where a
\ message may be issued. To support better diagnostics, an error where source
\ information is needed can save the error source and the message code will use
\ that to provide better diagnostics. For an example, see included.

variable (errsrc) 0 (errsrc) ! \ error source for diagnostics

: (save-errsrc) ( n -- )       \ copy source buffers
    ?dup if (errsrc) @ 0= if
      source buf-allot dup>r swap cmove \ copy the input line
      buf-allot (source) over (source-len) cmove \ copy (source)
      dup (errsrc) !         \ save address of buffer
      r> swap cell+ !        \ modify source address of input in copy
    then then ;

: (restore-errsrc) ( -- )      \ for message, restore error source
    (errsrc) @ (source) (source-len) cmove ;

\ -------------------- Parse Input Stream --------------------

code parse-name ( <spaces>name<space|eol> -- c-addr u )
    0 2 in/out
      mov     { -cell ebp } eax          \ save prev eax tos
      xor     eax eax                    \ zero eax = length found
      mov     edx { (source) }           \ edx = input length
      mov     edi { (source) cell+ }     \ edi = input pointer
      add     edx edi                    \ edx points to end of buffer
      add     edi { >in }                \ point edi at string to parse
      mov     esi edi                    \ potential start of string returned

@@1:  cmp     edx edi                    \ check buffer > input ptr
      jbe     short @@8                  \ no, so done
      add     edi 1                      \ bump to following char
      cmp     byte { -1 edi } $20        \ trim off leading leading spaces
      jbe     short @@1                  \ loop

      lea     esi { -1 edi }             \ esi points at start of non-bl string
      mov     ecx esi
      sub     ecx { (source) cell+ }     \ make an offset
      mov     { (source-parse) cell+ } ecx \ save

@@2:  add     eax 1                      \ increment length
      cmp     edx edi                    \ are we past end?
      jbe     short @@8                  \ yes, exit
      add     edi 1                      \ bump to following char
      cmp     byte { -1 edi } $20        \ check for space
      ja      short @@2                  \ no so keep counting

@@8:  sub     edi { (source) cell+ }     \ distance scanned
      mov     { >in } edi                \ update in
      mov     { (source-parse) } eax     \ length of string
      mov     { -2 cells ebp } esi       \ return addr len
      next;

: parse     ( char "ccc<char>" -- c-addr u )    \ parse to delimiter
    >r source-remain          \ source to parse is >in to end
    2dup r> scan nip          \ scan, only interested in length
    - dup char+ >in @ +       \ point past delim (if there is one)
    (source) @ min >in ! ;    \ adjust >in, return parsed string without delims

: parse"    ( -- addr len ) '"' parse ;

: parse-str ( -- a n )      \ parse possibly quoted string
    parse-name              \ parse name, skips leading blanks
    dup if
      over c@ '"' = if      \ starts with a " ?
        drop char+          \ start of string
        tib - >in !         \ reset parse area
        parse"              \ parse for delim
    then then ;

: char      ( -- char ) parse-name drop c@ ;
: [char]    ( -- char ) only-compiles> char plit ;

\ -------------------- String Literals --------------------------------------

: ", ( a1 n1 -- )             here over 2+ allot place ;
: sliteral  ( a1 n1 -- )      only-compiles> here -rot ", count p2lit ;
: (p") ( -- buff )            parse" buf-allot dup>r place r> ;
: s" ( -<string">- -- a1 n1 ) (p") count compiles> drop parse" postpone sliteral ;
: ," ( -<string">- )          parse" ",  compiles> drop postpone s" postpone ", ;
: c" ( -<string">- -- a1 )    (p")       compiles> drop here ,"    plit ;
: z" ( -<string">- -- a1 )    (p") 1+    compiles> drop here ," 1+ plit ;

: base@         ( n -- base n )   base @ ;
: base!         ( base -- )       base ! ;

\ -------------------- console i/o ------------------------------------------

defer init-console
defer init-screen
defer init-title

defer hide-console
defer show-console

defer console

defer accept
defer key
defer key?
defer ekey
defer ekey>char
defer ekey>fkey

defer emit
defer cr
defer ?cr
defer type
defer tab
defer bs
defer cls         ' cls alias page
defer beep
defer set-xy      ' set-xy alias at-xy

defer get-xy
defer getcolrow
defer cursor-inview
defer setcolrow
defer set-cursor
defer get-cursor
defer setrowoff
defer getrowoff
defer getmaxcolrow
defer setmaxcolrow
defer cols
defer rows
defer #col
defer #tab

8 value tab-size

: .(        ( -- ) ')' parse type ; immediate

: ."        ( -<string">- -- )
    parse" type
    compiles> drop
      postpone s" postpone type ;

: space     ( -- )  bl emit ;

: _seps        ( n char -- )
    buf-allot dup>r maxstring rot fill
    0max begin dup
    while   dup maxstring min
            r@ over type -
    repeat  drop rdrop ;

: spaces ( n -- ) bl  _seps ;
: dashes ( n -- ) '-' _seps ;

\ -------------------- number output ----------------------------------------

: decimal #10 base! ;
: hex     #16 base! ;
: binary   #2 base! ;
: octal    #8 base! ;

: <# ( -- ) pad hld ! ;

code hold       ( char -- )
    1 0 in/out
      mov     ecx { hld ebx }
      sub     ecx 1
      mov     { ecx } al
      mov     { hld ebx } ecx
      mov     eax { ebp }
      next;

: holds ( addr len -- ) dup negate hld +! hld @ swap move ;
: sign  ( f1 -- )       0< if '-' hold then ;

code # ( d1 -- d2 )
    2 2 in/out
      mov     ecx { base ebx }
      xor     edx edx
      div     ecx
      mov     esi eax
      mov     eax { ebp }
      div     ecx
      mov     { ebp } eax
      mov     eax edx
      cmp     eax 9
      jbe     short @@1
      add     eax 7
@@1:  add     eax '0'
      mov     ecx { hld ebx }
      sub     ecx 1
      mov     { ecx } al
      mov     { hld ebx } ecx
      mov     eax esi
      next;

: #s ( d1 -- d2 )       begin # 2dup or 0= until ;
: #> ( d1 -- addr len ) 2drop hld @ pad over - ;

: (d.)       ( d -- addr len ) tuck dabs <# #s rot sign #> ;
: d.         ( d -- )          (d.) type space ;
: d.r        ( d w -- )        >r (d.) r> over - spaces type ;
: (.)        ( n1 -- a1 n1 )   s>d (d.) ; \ convert number n1 to an ascii string
: .          ( n -- )          s>d d. ;
: .r         ( n w -- )        >r s>d r> d.r ;
: u.         ( u -- )          0 d. ;
: u.r        ( u w -- )        0 swap d.r ;
: #.         ( n -- )          base@ swap decimal . base! ;    \ display number in decimal
: .n         ( n -- )          0 <# swap 0 ?do # loop #> type base! ; \ display n digits
: h.         ( u -- )          base@ swap hex u. base! ;
: h.base>hex ( n1 n2 -- base n1 n2 ) base@ -rot hex ;
: h.r        ( u w -- )        h.base>hex >r 0 <# #s #> r> over - spaces type base! ;
: h.n        ( u n -- )        h.base>hex .n ; \ display n1 as a hex number of n2 digits
: h.2        ( u -- )          2 h.n ;         \ two digit hex number
: h.4        ( u -- )          4 h.n ;         \ four digit hex number
: h.8        ( u -- )          8 h.n ;         \ eight digit hex number
: ($.)       ( -- )            '$' emit ;
: $.         ( u -- )          ($.) h. ;        \ display $hex
: $.8        ( u -- )          ($.) h.8 ;        \ display $hexhexhe
: $s.        ( n -- )          ($.) base@ swap hex . base! ;   \ display signed $hex
: b.n        ( u n -- )        base@ -rot binary .n ;
: b.32       ( u -- )          32 b.n ;
: b.8        ( u -- )          8  b.n ;
: U+.        ( xchar -- )      s" U+" type dup $FFFF <= if h.4 else h.8 then ;
: ?          ( addr -- )       @ . ;

\ -------------------- Dictionary Search ------------------------------------

: voc#threads ( voc-address -- #threads ) voc.thrd @ ;

: wid>thread ( addr len wid -- addr len thread ) \ get the wordlist thread
    3dup voc#threads thread-hash + voc.#0 ;   \ get the hash (addr len thread-entry)

\ (std-search) should not be used directly; it's a pointer in the vocabulary
\ structure that is executed by (search-self). The exception is search-wordlist,
\ where we can't guarantee that the wordlist is a vocabulary; it might just
\ be a bare wordlist. Case insensitive.

defer (wid-compare) ' istr= is (wid-compare)

: (std-search)  ( addr len wid -- 0 | nt )      \ this is the standard wordlist vsrch
    wid>thread            \ get the hash (addr len thread-entry)
    -rot 2>r              \ save the string
    begin @ dup while               \ follow the link
      dup link>name name>string 
        2r@ (wid-compare)           \ compare the names
      0= while            \ no match, repeat
    repeat
      link>name           \ return nt
    then 2rdrop ;        \ drop the string

: (asis-search) ( addr len wid -- 0 | nt )      \ this is the asis wordlist vsrch
    ['] (wid-compare) >body dup @ >r            \ save & set (wid-compare)
    ['] str= over ! >r
    (std-search)
    2r> ! ;                 \ restore

\ Searches a vocabulary using its own search, which is in the vocabulary header
\ when it's built. The default search is (std-search). Case depends on the search.

: (search-self) ( addr len vocwid -- 0 | nt )   \ search the vocabulary
    dup voc.srch perform ;

: name>xtimm    ( nt -- 0 | xt imm )            \ change from nt to xt imm flag
    dup if
      dup name>interpret swap
      head.ct @ ['] execute =       \ if it's an execute
      if 1 else -1 then             \ set immediate else not so
    then ;

\ search-wordlist is case dependant on the underlying vocabulary (see case-asis)

: search-wordlist ( addr len wid -- 0 | xt flag ) (search-self) name>xtimm ;

: hide   ( -- ) last @ head.link @ last-link @ ! ;
: reveal ( -- ) last @ head.link   last-link @ ! ;

\ ----------------- Context & current --------------------------

#vocs 2 + cells allot here 0 ,  \ backmost marker guard
    equ context-base            \ rightmost entry
context-base value context      \ the context
variable current                \ current wordlist for definitions
variable voc-link               \ linked list of wordlists (vocabularies)

: get-current   ( -- wid ) current @ ;
: set-current   ( wid -- ) current ! ;
: swap-current  ( wid1 - wid2 ) get-current swap set-current ;

\ ----------------------- Find name in vocabulary ---------------------------

\ find-name is borrowed from Gforth, where find-name has largely obsoleted find
\ for which there are a number of problems (not the least being that the string
\ is a counted string). find-name can return the nt or 0. Unlike gforth,
\ the nt is the nfa; there's no separate hash required to get from the nt to the
\ header entry for the field. The nfa is a counted string. The case is dependant
\ on the search defined in the vocabulary itself.

: (find-context) ( addr len -- nt | 0) \ as in gforth, return name token
    dup if                  \ check for string *** should use get-order & do
      2>r context           \ search through the context list
      begin dup @ dup while           \ not at end of list
        over cell+ @ over <> if       \ and not the same vocab as next
          2r@ rot (search-self) ?dup  \ search the wordlist
        else drop 0 then              \ else try next entry
        0= while cell+      \ next entry
      repeat                \ back to begin
      then 2rdrop           \ tidy up the rstack
    then nip ;              \ drop excess

defer find-name ' (find-context) is find-name

\ ANS version of find; its use is limited due to the return values.
\ Search order for find is standard vocabularies, including IMPORTS
\ Don't use it; it's a brain dead implementation here deliberately

: find ( addr -- addr false | xt -1 | xt 1 )
    dup count find-name dup if        \ find the name
      nip name>xtimm                  \ xt imm
    then ;                            \ else str 0

\ -------------------- Compiling words ----------------------------

\ REMOVED as immediacy is difficult to determine using smart compile,
\ and marked as obsolescent in Forth2012 standard.
\ : [compile] ( -<name>- )            \ compile immediate word

: undefined ( -- ) throw_undefined throw ;
: defined? ( "name" -- 0 | nt ) parse-name find-name ;
: definite-find ( "name" -- nt ) defined? dup 0= if undefined then ;

: ' ( -- xt ) definite-find name>interpret ; \ tick of word following
: ['] ( -<name>- ) only-compiles> ' plit ;   \ tick of word following
: compiles-for ( xt <name> -- ) ' compiles ; \ parsing; set the compilation word

\ -------------------- Build header -----------------------------------------

: (common-header) ( a1 n1 -- ) \ build a hashed header from a1,n1 in voc
    get-current                \ get wid ( addr len wid )
    warning @ if over if       \ warning and not null
      3dup search-wordlist if  \ issue not unique message
        drop
        warn_notunique warnmsg
      then
    then then
    wid>thread                 \ get the thread this name lives on
    dup last-link !            \ last link
    link,                      \ lfa
    here xt-jmp,               \ the ptr to ct pair field in the xt
    ['] compile, ,             \ standard word compiles (xt2 field)
    code-here dup , latestxt ! \ the xt
    ['] xt-call, ,             \ how to compile
    0 ,                        \ recognizer
    sourceline# w,             \ vfa
    0 w, -1 w, 0 c,            \ ofa ste tfa
    here last !                \ nt is last
    ", align                   \ nfa
    ;

: (asis-header) ( a1 n1 -- )   \ no case translation
    in-sys
    (common-header)            \ build head in current
    in-prev ;                  \ back to original dictionary pointer

: (std-header) ( a1 n1 -- )    \ std build header in same dict as wordlist
    buf-allot dup>r            \ local buffer
    place r> lowercase count   \ lowercase in the buffer
    (asis-header) ;

: header    ( -<name>- )           \ build a header
    parse-name dup 0= throw_namereqd ?throw
    in-app align in-prev           \ align the data space
    get-current voc.head perform ; \ perfrom the word at voc.head

\ If we modify the xt ptr in the header to point to another xt, then we need to
\ remove the ct-ptr that (common-header) created, as it's superfluous.
\ Used by alias (it points to another xt) and synonym

: xtptr!    ( xt -- )       \ set xtptr to xt
    -5 code-allot           \ remove the xt-ptr word, it's equiv of jmp
    last @ head.xtptr ! ;

: alias ( xt -<name>- ) header xtptr! ; \ make another 'name' for 'xt'

: synonym ( -<newname> <oldname>- )
    header hide
    definite-find
    name>xtimm 1 = if immediate then         \ make synonym immediate
    xtptr! reveal ;                \ set the xt pointer of header

\ ----------------- Vocabulary & wordlist support --------------------------

\ The standard iteration over a vocabulary. The xt has the stack effect of
\ ( nt -- x | 0 ). Returning zero terminates the loop.
\ Example;
\ The stack is clean on entry to the xt, so this
\  : x drop 1+ true ;
\  0 ' x ' forth >body traverse-wordlist
\ will count the number of vocabulary entries.

\ The code returns the nts in newest to oldest order. Due to the way that the
\ vocabularies are built, the highest address represents the latest entry. We copy
\ the thread list to a temp part of storage, then find the largest and hence
\ latest entry on a thread list. It is unlinked from the list, and we repeat until
\ all the entries have been processed.

code largest    ( a1 n1 --- a2 n2 )
    2 2 in/out
      mov     ecx eax          \ count of array to search
      mov     eax { ebp }        \ starting address of array
      xor     edi edi          \ starting highest value = 0
      mov     edx  eax          \ highest value addr = start address
@@1:  cmp     { eax } edi      \ compare 32bit words
      jle     short @@2         \ branch if not greater
      mov     edx eax          \ if greater, save offset in edx
      mov     edi { eax }        \ and contents in edi
@@2:  add     eax cell          \ bump to next element
      sub     ecx 1
      jnz     short @@1
      mov     ecx edx
      mov     { ebp } ecx
      mov     eax edi
      next;

: (std-iter)    ( xt voc -- u )
    dup voc.#0 swap voc#threads      \ get voc.#0, thread count
    sp@ 4096 - swap 2dup 2>r cells move \ copy vocabulary threads to stack !!! *** awful
    begin 2r@ largest dup            \ find latest thread
    while   dup -rot       \ dup the nt
      @ swap !             \ unlink this, the largest, from list
      link>name swap dup             \ point at name, get xt
      >r execute r> swap while       \ execute & repeat if xt returns non-zero
    repeat 2dup then       \ stack to same level
    2rdrop 3drop ;

: traverse-wordlist ( x*i xt wid -- x'*i u )
    dup voc.iter perform ;

\ : dovoc         ( -- ) dovar context ! ;         \ no DOES> here...
code dovoc
      mov     edx { ' context >body }
      mov     { edx } ecx
      next;

\ Vocabularies; currently, these MUST be defined as specified

#rthreads #vocabulary root              \ root vocab
#ithreads #vocabulary imports case-asis \ case sensitive imports
           vocabulary forth             \ main vocabulary
#fthreads #vocabulary files             \ files vocab
#hthreads #vocabulary hidden            \ hidden words

37 equ vthreads        \ default number of threads for a vocabulary

: #wordlist     ( #threads -- wid )
    here swap
    0 ,             \ not linked
    0 ,             \ xt ptr, none in a wordlist
    ['] (std-header) ,        \ to create entries
    ['] (std-search) ,        \ to search the wordlist
    ['] (std-iter) ,          \ to iterate the wordlist
    1 max 511 min dup ,       \ thread count
    cells allot&zero ;

: wordlist      ( -- wid ) vthreads #wordlist ;

\ -------------------- Search order & context --------------------

: also          ( -- )      \ duplicates topmost vocabulary in the search order
    context dup @         \ ( context wid)
    swap cell-            \ ( wid context-4)
    dup to context ! ;

: only          ( -- )      \ removes all vocs from order down to root & import
    context-base to context
    also root also imports also ;

: get-order     ( -- widn .. wid1 n )      \ fetch widn .. wid1 n
    context-base context -
    2 rshift dup>r
    0 ?do
      context-base i 1+ cells - @
    loop r> ;

: set-order     ( widn .. wid1 n -- )      \ set widn .. wid1
    dup 0< if        \ -1: min search order
      drop only
    else
      context-base over cells - to context
    0 ?do          \ n to 0 do
        context i cells+ !     \ save it
      loop           \ and loop
    then ;

: definitions   ( -- )      \ sets the topmost context vocabulary as current
    context @ set-current ;

: previous      ( -- )      \ removes the topmost vocabulary from the search order
    cell +to context ;    \ just bump up

: .id           ( nt -- ) name>string type ;
: .name         ( xt -- ) >name .id ;      \ show name or "noname"

: .voc-name     ( voc -- )
    dup voc.xt @ ?dup if .name drop
    else s" wid-" type $. then ;

: order         ( -- )
    cr s" context:     " type
    get-order
    0 ?do .voc-name space loop
    cr s" current:     " type
    get-current .voc-name ;

\ : forth-wordlist ( -- wid ) [ ' forth >body ] literal ;   
' forth >body constant forth-wordlist

voc-root       \ meta switch to root voc

' forth             alias forth
' forth-wordlist    alias forth-wordlist
' set-order         alias set-order

voc-forth      \ meta switch to forth voc

\ -------------------- Number Input --------------------

((
code digit      ( char base -- n flag )
    2 2 in/out
      mov     ecx { ebp }
      sub     ecx '0'
      jb      short @@1
      cmp     ecx 9
      jbe     short @@2
      sub     ecx 7
      cmp     ecx 10
      jb      short @@1
@@2:  cmp     ecx eax
      jae     short @@1
      mov     { ebp } ecx
      or      eax -1
      ret     \ jmp short @@3 ( ret because 2 2 in/out )
@@1:  xor     eax eax
@@3:  next;
))

code >number    ( ud1 c-addr1 u1 –– ud2 c-addr2 u2 )
    4 4 in/out
      test    eax eax              \ check if anything to convert
      je      short @@4            \ zero, so skip
      mov     ecx eax              \ len in ecx
      mov     esi { ebp }          \ esi = address
      mov     edi { base ebx }     \ get the number base
@@1:  movzx   eax byte { esi }     \ get next digit
      cmp     eax '0'
      jb      short @@3            \ if below '0' branch to done
      cmp     eax '9'
      jbe     short @@2            \ go convert it
      and     eax $df              \ convert to uppercase
      cmp     eax 'A'              \ if below 'A'
      jb      short @@3            \ then branch to done
      sub     eax 7      \ adjust to be contiguous numeric
@@2:  sub     eax '0'              \ de-ASCIIfy
      cmp     eax edi              \ test against current base
      jae     short @@3            \ out of base range
      mov     edx  eax
      mov     eax { cell ebp }     \ swap eax <-> { cell ebp }
      mov     { cell ebp } edx
      mul     edi        \ multiply by base
      mov     edx eax
      mov     eax { 2 cells ebp }  \ swap eax <-> { 2 cells ebp }
      mov     { 2 cells ebp } edx
      mul     edi        \ multiply by base
      add     eax { cell ebp }     \ add
      adc     edx { 2 cells ebp }  \ and +carry for the double
      mov     { 2 cells ebp } eax  \ store result
      mov     { cell ebp } edx     \ store result
      add     esi 1
      sub     ecx 1
      jnz     short @@1
@@3:  mov     eax ecx
      mov     { ebp } esi          \ address of unconvertable digit
@@4:  next;


\ -------------------- Library Procedures -----------------------------------

\ On execution, the imports aren't set up properly; in particular, we need to
\ get the entry points of LoadLibrary and GetProcAddress. They are stored
\ in the IAT built by the image manager imageman.f, and are used to
\ pre-load those entry points, then load GetLastError and VirtualAlloc which
\ are needed very early in the code before we can use standard imports.

\ -------------------- Early IMPORT functions -----------------------

variable lib-link       \ linkage for libraries

: imp-load   ( import -- import )          \ resolve proc address, search all libs
    lib-link
    begin @ ?dup                 \ loop through libraries
    while              \ ( import lib )
      over imp.name @ over       \ ( import lib name lib )
      imp-trylib                 \ now load the import
      ?dup if                    \ success ( import lib ep )
        >r             \ save entry point
        over imp.lib !           \ store lib ptr in import
        r> over !                \ store the ep in the import address
        exit                     \ and exit
      then
    repeat
    throw_impnotfound throw ;              \ it's an error, couldn't load the function

code imp-resolve ( [args...] [ep] -- result )  \ called to resolve from import:
      xchg    esp ebp      \ swap regs back again
      mov     eax ecx      \ recover the tos
      call    ' imp-load             \ resolve the library this proc is in
      xchg    esp ebp      \ swap the regs back again
      jmp     { eax }      \ call the ep
      next;

code imp-call   ( -- )
      xchg    ebp esp      \ swap the regs
      push    eax          \ save tos
      call    { ecx }      \ call ep
      xchg    ebp esp      \ swap back regs
      next;

\ -------------------- Required imports -------------------------------------

0 value appinst     \ the application instance (origin of code), set in main

2 import: GetProcAddress
1 import: LoadLibrary
1 import: GetLastError
4 import: VirtualAlloc
1 import: ExitProcess

: imp-trylib ( name lib -- proc-ep | 0 )        \ helper to get import address
    lib.handle @
    over char+ over call GetProcAddress    \ name, lib handle
    ?dup if
      nip nip                    \ found, so leave EP
    else
      swap count                 \ get procname len
      buf-allot dup>r place      \ allocate buffer, copy string
      S" A" r@ +place            \ add an "A<null>" after last char
      r> char+                   \ point at it and lib
      swap call GetProcAddress
    then ;

: get-ior     ( return-code -- ior )           \ ior is any windows error, mainly for IO
    if   0                 \ non-zero, so OK
    else call GetLastError          \ get windows last error
    then dup ior ! ;       \ save in ior, return

code init-k32   ( -- )         \ initialize for windows calls
      push    ebp              \ save regs, these get wiped otherwise
      push    ebx
      push    edi
      push    esi
      push    eax

      mov     eax { ' appinst >body }  \ get appinst (dos header) in eax
      mov     ebx { $3c eax }          \ from dos header to pe header
      add     eax { $d8 eax ebx }      \ from pe header to iat
      mov     ebx { eax }              \ ep of loadlibrary from iat
      mov     edi { cell eax }         \ ep of getprocaddress from iat +4
      mov     { ' GetProcAddress >body imp.ep } edi  \ save in the proc for getprocaddress
      mov     { ' LoadLibrary    >body imp.ep } ebx  \ save in the proc for loadlibrary
      mov     eax { kernel32.dll lib.name }  \ fetch the name string ptr
      add     eax 1          \ point at the name (past the count)
      push    eax
      call    ebx            \ call loadlibrary
      mov     { kernel32.dll lib.handle } eax \ save the address in dll
      mov     esi  eax       \ dll in esi

      mov     eax { ' GetLastError >body imp.name } \ point at getlasterror
      add     eax 1
      push    eax
      push    esi
      call    edi            \ get the address
      mov     { ' GetLastError >body imp.ep } eax          \ save ep

      mov     eax { ' VirtualAlloc >body imp.name } \ point at virtualalloc
      add     eax 1
      push    eax
      push    esi
      call    edi            \ get the address
      mov     { ' VirtualAlloc >body imp.ep } eax  \ save ep

((
      mov     eax { ' ExitThread >body imp.name } \ point at ExitThread
      add     eax 1
      push    eax
      push    esi
      call    edi            \ get the address
      mov     { ' ExitThread >body imp.ep } eax  \ save ep
))
      mov     eax kernel32.dll         \ point at kernel dll structure
      mov     { ' GetProcAddress >body imp.lib } eax  \ and save in getprocaddress
      mov     { ' LoadLibrary    >body imp.lib } eax  \ and save in loadlibrary
      mov     { ' GetLastError   >body imp.lib } eax  \ and save in getlasterror
      mov     { ' VirtualAlloc   >body imp.lib } eax  \ and save in virtualalloc
\      mov     { ' ExitThread     >body imp.lib } eax  \ and save in exitthread

      pop     eax
      pop     esi
      pop     edi
      pop     ebx
      pop     ebp
      next;

\ -------------------- Memory Management functions --------------------------

\ Windows heap function calls
1 import: GetProcessHeap
3 import: HeapAlloc
3 import: HeapFree
4 import: HeapReAlloc

\ The memory allocation code uses HEAP_GENERATE_EXCEPTIONS rather than returning
\ false on the stack. The reason is simple; crashing on a known exception is
\ easier than a crash further down the line on some other exception. Early &
\ informative is best. But if you want ANS behavior, call ANS-MEMORY

create process-heap           \ process heap address
    0 ,            \  heap address
    HEAP_GENERATE_EXCEPTIONS ,  \ flag should be zero to pass ANS test

: ans-memory   ( -- )          process-heap cell+ off ;
: heap&flag@   ( -- fl heap )  process-heap 2@ ;
: allocate     ( n -- a f )    heap&flag@ call HeapAlloc dup 0= ;
: malloc       ( n -- a )      allocate throw_memallocfail ?throw ;
: free         ( a -- f )      heap&flag@ call HeapFree 0= ;
: release      ( a -- )        free throw_memrelfail ?throw ;
: realloc      ( n a -- a' f ) dup>r heap&flag@ call HeapReAlloc
                               ?dup if rdrop 0 else r> -1 then ;
: resize       ( a n -- a' f ) swap ?dup if realloc else allocate then ;
: init-malloc  ( -- )          call GetProcessHeap process-heap ! ;

\ -------------------- ANS File Functions --------------------

7 import: CreateFile
1 import: CloseHandle
4 import: ReadFile
4 import: WriteFile
1 import: DeleteFile
2 import: MoveFile
4 import: SetFilePointer
1 import: FlushFileBuffers
2 import: SetEndOfFile

:            bin ; immediate         \ BIN
GENERIC_READ  constant r/o           \ GENERIC_READ
GENERIC_WRITE constant w/o           \ GENERIC_WRITE
r/o w/o +     constant r/w           \ READ/WRITE

: ascii-z     ( addr len buff -- buff-z )      \ make an ascii string
   dup>r place r> 1+ ;

: (open-file) ( adr slen fmode type -- fileid ior )
   2swap buf-allot dup>r place 2>r             \ ( r: buff fmode type )
   0 FILE_FLAG_SEQUENTIAL_SCAN r> 0
   [ FILE_SHARE_READ FILE_SHARE_WRITE or ] literal
   r> r> char+
   call CreateFile dup INVALID_HANDLE_VALUE <> get-ior ; \ fileid ior = 0 = success

: open-file ( adr slen fmode -- fileid ior )    OPEN_EXISTING (open-file) ;
: create-file ( adr slen fmode -- fileid ior )  CREATE_ALWAYS (open-file) ;
: close-file ( fileid -- ior ) call CloseHandle get-ior ; \ hObject 0 = success

: fparms-rw     ( addr len fileid -- 0 0 ptr len addr fileid ) \ parms for read/write
\ ptr points here:         ^
    >r swap 2>r 0 sp@ 0 swap 2r> r> ;

: read-file     ( b-adr b-len fileid -- len ior )
    fparms-rw call ReadFile get-ior ;       \ len ior = 0 = success

: write-file    ( adr slen fileid -- ior )
    fparms-rw call WriteFile nip get-ior ;            \ ior = 0 = success

: delete-file ( adr len -- ior )
    buf-allot ascii-z call DeleteFile get-ior ;       \ ior - 0 = success

: rename-file ( adr1 len adr2 len -- ior )
    buf-allot ascii-z -rot
    buf-allot ascii-z call MoveFile get-ior ;         \ adr1a adr2a

code fparms-fp  ( len-ud fileid move --  \ parms for file-position words using setfilepointer
\      -- movehigh move ptrmovehigh movelow fileid ) \ results
\ ptr points here:   ^
      4 5 in/out
      mov     ecx { ebp }        \ fileid
      mov     edx { cell ebp }      \ movehigh
      mov     edi { 2 cells ebp }      \ movelow
      mov     { 2 cells ebp } edx      \ movehigh
      mov     { cell ebp } eax      \ movehigh move
      lea     edx { 2 cells ebp }      \
      mov     { ebp } edx        \ movehigh move ptrmovehigh
      mov     { -cell ebp } edi     \ movehigh move ptrmovehigh movelow
      mov     eax ecx          \ fileid
      next;

: setfp        ( parms -- len-ud 0 | len-ud err )
    fparms-fp call SetFilePointer dup -1 ( INVALID_SET_FILE_POINTER ) =
    if call GetLastError dup NO_ERROR =
      if   drop swap 0           \ return len-ud 0=success
      else nip 0 swap then       \ return 0 0 ior=err
    else swap 0 then ;           \ return len-ud 0=success

: file-position ( fileid -- len-ud ior )
    >r 0 0 r> FILE_CURRENT setfp ;

: advance-file ( len-ud fileid -- ior )   \ RELATIVE position file, not ANS
    FILE_CURRENT setfp nip nip ;           \ ior - 0 = success

: reposition-file ( len-ud fileid -- ior )
    FILE_BEGIN setfp nip nip ;             \ ior - 0 = success

: file-append   ( fileid -- ior )
    >r 0 0 r> FILE_END setfp nip nip ;       \ ior - 0 = success

2 import: GetFileSize

: file-size     ( fileid -- len-ud ior )
    0 sp@ rot call GetFileSize
    tuck INVALID_FILE_SIZE <> if
      false
    else
      call GetLastError NO_ERROR <>
      if 2drop 0 0 true then
    then ;

: flush-file    ( fileid -- ior )
    call FlushFileBuffers get-ior ;      \ ior - 0 = success

((
code adj-lens ( buff len buff' -- len len' )  \ adjust lengths: rot - tuck swap - 1+ ;
     3 2 in/out
      mov     ecx { ebp }              \ length
      mov     edx { cell ebp }            \ buff
      sub     eax edx      \ subtract where found (buff - buff')
      mov     { cell ebp } eax            \ len read
      sub     eax ecx      \ length read
      add     eax 1           \ 1+
      next;
))

: adj-lens      ( buff len buff' -- len len' )
    rot - tuck swap - 1+ ;          \ len len-buff-buff'+1

: read-line-CRLF ( buff len -- len len true | false ) \ return len of string, len to move file, flag
    2dup $D scan          \ look for cr
    if
      dup>r adj-lens r>             \ adjust length & length back
      char+ c@ $A = if 1+ then      \ check next for LF, adjust for it
      true exit           \ leave with found
    then                  \ cr not found, perhaps it's an LF only
    drop
    2dup $A scan          \ look for LF
    if
      adj-lens            \ length back
      true exit
    then
    3drop false  ;        \ no just a plain string

: read-line    ( adr len fileid -- len eof ior )
    >r                    \ save the fileid
    0max dup rllen !      \ save length requested
    1+ 2dup r@ read-file ?dup       \ read requested chars+1
    if                    \ if read not ok
      rdrop               \ drop fileid
      >r 3drop 0 -1 r> exit         \ ior <> 0 = error
    then
    2dup = rlneof !       \ if req=read is equal , not end of file
    min                   \ if read any characters
    dup if
      rllen @ min dup rllen !       \ reset length read
      read-line-crlf      \ scan for line break characters
      if                  \ if line break
        rlneof @ if 1- 0 min then   \ if not end, need to adjust for extra char read
        ?dup if           \ if it's ok to positiom
          s>d r> advance-file       \ position file for next time
         -1 swap exit              \ len -1 ior
        else
          rdrop -1 0 exit           \ len -1 ior
        then
      then
      rlneof @ if         \ correct if not eof (bill mccarthy fix)
       -1 -1 r@ advance-file ?dup  \ we over-read, so step back 1 char
        if rdrop 0 0 rot exit       \ reposition-file error
        then
      then
      rdrop rllen @ -1 0            \ no line break, so len -1 0
    else
      2drop rdrop 0 0 0             \ nothing read return 0=len, eof=false ior=false
    then    ;

create crlf$ 2 c, 13 c, 10 c, 0 c,  \ counted crlf string

: write-line    ( adr len fileid -- ior )
    dup>r write-file
    crlf$ count r> write-file or ; \ ior - 0 = success

: resize-file   ( len-ud fileid -- ior )
    dup>r reposition-file drop
    r> call SetEndOfFile get-ior ;       \ ior - 0 = success

: file-status   ( adr len -- x ior )
    r/o open-file
    dup if nip
    else    swap close-file drop
    then    0 swap ;      \ ior - 0 = success

: fsave-file    ( addr len filename -- )
    count r/w create-file throw_filecreatefail ?THROW
    dup>r write-file throw_filewritefail ?THROW
    r> close-file drop ;

\ -------------------- Memory mapped I/O for reading -----------------------

5 import: MapViewOfFile
1 import: UnmapViewOfFile
6 import: CreateFileMapping

: slurp         ( addr len -- addr' len' )      \ return a read file map from name
    r/o open-file
    throw_filenotfound ?throw >r    ( r: file-handle )
    0 0 0 PAGE_READONLY 0 r@ call CreateFileMapping \ map handle
    r@ file-size 2drop              \ length of file
    dup>r swap >r         ( r: file-handle len map-handle )
    0 0 FILE_MAP_READ r@ call MapViewOfFile ( addr r: file-handle len map-handle )
    r> close-file drop              \ close the map handle
    r> r> close-file drop ;         \ close the file handle

: burp          ( addr -- flag )      \ free read file map
    call UnmapViewOfFile get-ior ;

\ --------------------  State ------------------------------------------

variable state

: [  state off   ; immediate
: ]  state on    ;

\ --------------------  Aux Stack words  -------------------------------

\ A stack is 

\ begin-structure stk%
\  cell-
\   lfield:  stk.max     \ maximum stack size
\   lfield:  stk.count   \ count (offset at 0)
\   0 +field stk.ent0    \ entries
\  cell+
\ end-structure

: stack    ( n "name" -- )   dup , here 0 , swap 1- 0max cells allot constant ;
: sdepth   ( stack -- n )    @ ;
: sempty?  ( stack -- f )    @ 0= ;
: smax     ( "name" -- n )   cell- @ ;
: spush    ( x stack -- )    dup>r 1+! r@ r> @ cells+ ! ;
: s2push   ( n m stack -- )  tuck spush spush ;
: spop     ( stack -- x )    dup dup>r @ dup 0= throw_auxstacku ?throw cells+ @ r> 1-! ;
: s2pop    ( stack -- n m  ) dup spop swap spop ;
: -stack   ( stack -- )      off ;

: get-stack ( rec-addr -- recn .. rec1 n )
    dup @ dup 0= if nip exit then
    cells bounds swap ?do
      i @
    -cell +loop ;

: set-stack ( recn .. rec1 n rec-addr -- )
    2dup cell- @ u> throw_auxstacko ?throw
    over cells cell+ bounds ?do
      i !
    cell +loop ;

\ ----------------- Recognizer stack --------------------------

$20 dup , cells here swap allot value forth-recognizer

: get-recognizers ( -- xt1 .. xtn n )
    forth-recognizer get-stack ;
: set-recognizers ( xt1 .. xtn n )
    forth-recognizer set-stack ;

: .recognizers ( -- )
    cr s" recognizers: " type
    get-recognizers 0 ?do .name space loop ;

\ -------------------- Recognizers ------------------------------------------

: dt-token: ( xt-interpret xt-compile xt-postpone "<spaces>name" -- )
    create swap rot , , , ;

  ' undefined    \ the default actions
  ' undefined
  ' undefined
dt-token: dt:null \ the default datatype token

: recognize ( addr len stack-id -- i*x dt:type | dt:null )
    \ apply a recognizer stack to a string, delivering a token
    dup cell+ swap @ cells bounds ?do
      2dup i -rot 2>r
      perform \ @ execute
      dup dt:null <> if
        2rdrop unloop exit
      then drop 2r>
    cell +loop
    2drop dt:null ;

\ -------------------- Names   ------------------------------------------

  :noname name>interpret execute ;
  :noname name>compile   execute ;
  :noname name>compile   swap plit compile, ;
dt-token: dt:name ( nt -- )

: rec:name ( addr u -- nt dt:name | dt:null )
    find-name ?dup if dt:name else dt:null then ;

\ -------------------- Numbers ------------------------------------------

  ' d>s
  :noname d>s plit ;
  :noname d>s plit postpone plit ;
dt-token: dt:number ( n 0 -- )

  ' noop
  ' p2lit
  :noname p2lit postpone p2lit ;
dt-token: dt:double ( n m -- )

\ Can be used to force number recognition in any base
\ $ -- hex prefix
\ # -- decimal
\ % -- binary
\ & -- octal
\                #     $     %    &    \ '
create num-bases 10 c, 16 c, 2 c, 8 c, \ 0 c,

: num-base! ( addr u -- addr' u' ) \ set the base
    over c@ '#' - dup 4 u< if
      >r 1 /string r>
      num-bases + c@ base!
    else drop then ;

: num-ve? ( addr u -- addr' u' flag ) \ 0=+ve, -1=-ve
    over c@ '-' = dup>r negate /string r> ;

: rec:num ( addr u -- d r:type | dt:null ) \ [-][base][-]number[.]
    base@ >r                      \ save the base
    2dup slastchar '.' = if       \ check if double
      1- dt:double                \ remove . set as double
    else
      dt:number                   \ plain ole number
    then >r
    num-ve? >r                    \ check & flag for -ve
    num-base!                     \ get & set base
    r@ 0= if                      \ if not negative
      rdrop num-ve? >r            \ sign might be here
    then
    0 0 2swap >number             \ convert the number
    nip if                        \ should be nothing left
      2drop 2rdrop dt:null        \ fail, not a number
    else
      r> if dnegate then r>       \ negate it if reqd
    then r> base! ;

: quoted? ( str k -- flag )       \ check for '.' strings
    3 = if @ $ff00ff and $270027 = else drop 0 then ;

: rec:quot ( addr u -- n dt:num | dt:null ) \ 'x' type numbers
    2dup quoted? if
      drop 1+ c@ s>d dt:number
    else
      2drop dt:null
    then ;

\ initialise recognizer list
forth-recognizer 3 over !  
    cell+ ' rec:name over !
    cell+ ' rec:num  over !
    cell+ ' rec:quot swap !

\ -------------------- Interpreter ------------------------------------------

: ?stack ( -- ) \ check the data stack for stack underflow
    depth 0< throw_stackunder ?throw ;

: default-recognize ( addr u -- i*x xt )
    forth-recognizer recognize state @ cells- @ ;

: interpreter ( -- )                \ parse and interpret
    begin 
      ?stack parse-name dup
    while
      default-recognize execute
    repeat 2drop ;                  \ end of line

: postpone  ( -<name>- )            \ postpone next word
    only-compiles>
      -2 state dup @ >r !           \ save & set state
      parse-name default-recognize execute
      r> state ! ;                  \ reset state

\ -------------------- Colon Compiler ---------------------------------------

\ Support for local variables and local allocations.

defer locals-init ' noop is locals-init \ see locals.fs

: exit          ( -- )       \ exit current word
    only-compiles>
      sync-code              \ ensure everything generated
      code-here tail-call = if \ possible call/ret sequence
        tail-call 5 - call>jmp \ change to a jump; tail call optimise
      then
      postpone unnest        \ generate the ret (needed; may be a branch target)
      ; 

variable csp                 \ current stack pointer variable
: csp! ( -- ) sp@ csp ! ;    \ for stack depth check
: ?csp ( -- ) sp@ csp @ xor throw_stackchg ?throw ;

\ Words to support semi-colon ( ; )
\ ; is generic, and covers ; for : :NONAME,  ;] for [: and terminates DOES>
\ COLON-SYS is [ parms ] xt mark, and ; swaps and executes the xt

: ; ( c: colon-sys -- ) \ generic ;
    only-compiles>
      sync-code
      swap catch if throw_stackchg throw then ; 

: exit&[ ( -- xt ) \ generate an exit, stop compiling
    postpone exit
    postpone [
    latestxt @ ;

: init&] ( -- ) \ initialise locals, start compiling
    locals-init
    0 to tail-call         \ will be non-zero if we have any calls
    ] ;                    \ start compiling

: : ( c: -<name>- -- colon-sys )
    header hide            \ sets latestxt
    tcol tfa!              \ type is a colon-def
    ste-reset
    csp!                   \ not strictly required?
    [: ( c: colon-sys -- ) \ what to do at ;
      tcol ?pairs reveal (in/out!)
      exit&[ 1+ (ofa-calc) \ accounts for RET
      ?csp
    ;] tcol
    init&] ;

\ ------------------------ :NONAME -------------------------------------

  0 ,                      \ see structure head in gstructs
  0 , 0 , ' xt-call, ,     \ ct and comp
  0 , 0 w, 0 w, -1 w,      \ recog & 3 word fields
  tnon c,                  \ noname
  here head.ct
  ," :noname"
constant anonymous        \ anon compilation token  

: :noname ( c: -- colon-sys ) ( s: -- xt )    \ defining for headerless
    anonymous xt-jmp,           \ offset back to anon header
    code-here latestxt !
    [: ( c: colon-sys -- ) ( –– xt ) \ what to do at ;
      tnon ?pairs exit&[
    ;] tnon
    init&] ;

\ ------------------------ Quotations ------------------------------------

: ;]  ( c: orig xt colon-sys -- ) ( -- xt )  \ end an anonymous quotation
    only-compiles>
      postpone ;             \ end and stop compiling
      over if
        2>r postpone then 2r>
        plit latestxt !
        init&]       \ stack depth, start compiling
      else nip then ;

: [:  ( c: -- orig xt colon-sys ) \ start an anonymous quotation
    0 :noname
    compiles> drop
      postpone ahead
      latestxt @ :noname ;

\ ------------------------ compiles> -------------------------------

: compiles>  ( -- xt )              \ for alternative compilation semantics
    only-compiles>
       postpone exit          \ stop current definition
\       ?csp         \ check no rubbish on the stack
       locals-init            \ can have its own locals
       code-here compiles-me ; \ make the defined word compile this

: only-compiles> ( -- )        \ shorthand for (comp-only) compiles> drop
    only-compiles>
       postpone (comp-only)
       postpone compiles>
       postpone drop ;

\ ---------------------------- Defer -------------------------------------

: defer@    ( addr -- xt ) >body @ ;          \ fetch xt pointed
: defer!    ( addr xt -- ) >body ! ;          \ set defer

: (exec-def) ( -- ) dovar perform ;
: (comp-def) ( xt -- ) >body xt-call[], ;  \ compile a call indirect

: defer-err throw_defernoval throw ;

: defer     ( -<name>- )              \ create a deferred word
    ['] (exec-def) tdef (gen-here)
    ['] defer-err ,         \ the xt that's deferred, default is err
    ['] (comp-def) compiles-me        \ compile this
    ofa-calc ;              \ length calculation

: action-of ( "name" )              \ get action for defer
    ' defer@
    compiles> drop
      postpone ['] postpone defer@ ;

\ ---------------------------- DOES> -----------------------------------

\ Step A. DOES> generates an in-word call to (;code), a ret and a
\ dovar. The code generated by this
\
\   : X CREATE , DOES> @ ;
\
\ looks like this;
\
\   create ,     | call create  | call ,   |
\   does>        | call (;code) | ret*     | dovar |
\   @            | call @       |
\   ;            | ret          |
\
\ Step B. When (;code) is called, the address of the does code is 1
\ byte following the return address on the stack; just past the ret*
\ generated by DOES>. The address just CREATEd points at the original
\ code. (;code) saves the CREATE address on the stack, and compiles the
\ jump to the does> code in the definition. This
\
\   10 X Y
\
\ generates this for Y through (;code);
\
\   | Y's CREATE addr in ecx | jmp does> code in X |
\
\ Step C. When Y is executed, it stacks the address of the CREATE and
\ jumps to the DOES> code (in the example, dovar | call @). The
\ dovar ensures the address of the create is correctly loaded prior to
\ the user's code, and that it is still fetchable through >BODY.
\
\ DOES> has a dependancy on mov-ecx,n which generates this code;
\
\   ( $...... B9nnnnnnnn )      mov     ecx $nnnnnnnn
\   ( $...... E9xxxxxxxx )      jmp     $xxxxxxxx
\
\ The $xxxxxxxx address is at byte 7 up this construct, and is
\ modified by (;code). ECX $nnnnnnnnn is the address of here when create
\ is called.
\
\ NOTE: The call to (;code) MUST NOT be tail-call optimised, even though
\ call/ret sequences can normally be optimised away to a jmp. (;code)
\ uses the return address to calculate the body of does>.

body-off 4 + equ jmp-addr      \ the offset of the jump instruction

: (;code)       ( -- )              \ compile code for does>
    ['] xt-call, compiles-me      \ reset the standard compile word
    r@ 1+               \ code for does> (after ret)
    latestxt @ [ jmp-addr 1+ ] literal +
    dup>r - cell- r> !            \ xt for create, jump part
    -1 -1 in/out ;      \ stack effects unknown at this point

: does> ( c: -- colon-sys )
    ['] xt-call, compiles-me
    latestxt @ jmp-addr + jmp>call  \ see create for code, turn jump into a call
    :noname               \ start compiling
    compiles> ( c: colon-sys xt -- colon-sys does-sys ) \ compile
      drop                \ xt for does>, get rid of it
      sync-code
      postpone (;code) postpone unnest postpone dovar \ to fetch create value
      latestxt @ code-here latestxt ! \ save latestxt recurse will come here
      [: ( c: colon-sys does-sys -- ) \ what to do at ;
         tdos ?pairs latestxt ! postpone ;
      ;] tdos
      locals-init
    0 to tail-call ;       \ will be non-zero if we have any calls

\ -------------------------- Vocabulary -------------------------------

: #vocabulary ( #threads -<name>- -- )
    in-sys 
    create tvoc tfa!
    #wordlist
    in-app
    latestxt @ over voc.xt !
    voc-link add-link 
      does> context ! ;

: vocabulary    ( -<name>- -- )
    vthreads #vocabulary
    ;

: case-asis     ( -- )         \ case insensitive vocabulary
    voc-link @       \ last vocabulary
    ['] (asis-header) over voc.head !   \ set header word
    ['] (asis-search) swap voc.srch ! ; \ set search word

\ -------------------- abort" -------------------------------------

: abort"   ( n -<string">- -- )
    only-compiles>
      postpone if
      postpone c"
      postpone abort!
      postpone then ;

variable (excval) -4095 (excval) !

: exception ( addr len -- n ) ( add in exception text for throws )
    (excval) dup 1+! @
    dup>r throw_msgs link, , ", r> ;

\ -------------------- Import Functions -----------------------------------
\ This can be extended to support MPE's EXTERN: definitions
\
\ Imports are placed in a case sensitive IMPORTS vocabulary, which is always
\ in the minimal search order along with ROOT (see ONLY). User code does not
\ need to refer to the vocabulary to be able to call imported routines.
\   n IMPORT: CaseSensitiveName
\             import a function from a DLL (the DLL must be available)
\   CaseSensitiveName
\             call the imported function. C calls of the form
\               Func(a,b,c)
\             are invoked with
\               c b a Func
\
\  LIB structure (12 bytes long)
\
\          begin-structure lib%
\            lfield:  lib.link    \ link to next DLL
\            lfield:  lib.handle  \ handle of the DLL
\            lfield:  lib.name    \ ptr to counted string
\          end-structure
\
\  IMPORT structure (12 bytes long)
\
\          begin-structure imp%
\          lfield:  imp.ep      \ entry point
\          lfield:  imp.lib     \ ptr to lib entry
\          lfield:  imp.name    \ ptr to counted string
\          end-structure

\ -------------------------- Library Routines --------------------------------

1 import: FreeLibrary

: lib-free ( lib-entry -- )              \ free a lib entry
    lib.handle dup @
    call FreeLibrary drop off ;      \ free & zero it

: lib-load ( lib-entry --  )            \ load a library
    dup lib.name @ char+            \ to null terminated library name
    call LoadLibrary swap lib.handle ! ; \ store address

: library ( -- <name> )               \ create import library
    >in @ defined? if         \ check if defined
      drop             \ duplicate, ignore it
    else
      >in !
      ['] dovar tlib (gen-here)
      here
        lib-link link,        \ link field
      0 , last @ ,           \ handle & the name string
      lib-load                    \ load it now
    then ;

variable (cdecl) (cdecl) off
: __cdecl (cdecl) on ;

: import: ( n <name> -- )             \ create import in imports vocabulary
    >in @ defined? if         \ check if defined
      2drop             \ duplicate, ignore it
    else
      >in !
      [ ' imports >body ] literal swap-current
      header timp tfa!
      set-current                  \ original definitions
      dup ( n ) 1 in/out           \ set stack effects
      here mov-ecx,n
      ['] imp-call
      over 0> (cdecl) @ and if               \ build call and adjust stack
        xt-call,
        cells add-ebp,n
        postpone unnest
      else
        xt-jmp, drop               \ just jump, 0 or not __cdecl
      then ofa-calc
      ['] imp-resolve ,     \ entry point
      0 ,                   \ library
      last @ ,              \ the name string
    then (cdecl) off ;

: call ; immediate ( support identifying import with a call statement )

\ ---------------------------- Structures -------------------------------------

: begin-structure ( -- addr 0 ) 0 constant latestxt @ body-off + 0 ;
: end-structure   ( addr n -- ) swap ! ;  \ set size of constant above

: (offset)   ( n -- ) ?dup if plit postpone + then ;

: offset     ( n1 <-name-> -- )        \ compiling
             ( n2 -- n2+n1 )           \ runtime
    >r : r@ (offset) postpone ; rdrop
    toff tfa! 1 1 in/out
    [: 0 swap execute (offset) ;] compiles-me ;

: +field     ( n1 n2 <-name-> -- n1+n2 )  \ n1+n2 stored offset=n1
             ( addr -- addr+n1 )          \ runtime
    over offset + ;

: field:     ( n1 <"name"> -- n2 ) ( addr -- 'addr )  aligned cell +field ;
: cfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  1 +field ;
: bfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  1 +field ;
: wfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  2 +field ;
: lfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  4 +field ;
: xfield:    ( n1 <"name"> -- n2 ) ( addr -- 'addr )  8 +field ;

\ -------------------- Messages ----------------------------

: (.viewtype)   ( line# addr len -- )
    s" in " type type
    ?dup if s"  at line " type #. then ; \ not from console or evaluate

defer edit-error ( line# file count -- ) ' 3drop is edit-error

: (message)     ( n addr len -- )
    (errsrc) @ if                 \ if there's an error
      save-input n>r              \ save the current input source
      (restore-errsrc)            \ restore the error source
    then
    source-id if cr source type then         \ print source if not console
    cr (source-parse) Q@ spaces '^' _seps
    cr type dup #.           \ print (Error, Warning) and err #
    sourceline# sourcefilename (.viewtype) s" : " type
    (source-parse) Q@ tib + swap type space
    throw_msgs @
    [:
      2dup cell+ @ = if
        [ 2 cells ] literal + count type
      else drop then
    ;]
    list-apply drop                \ find & print matching messages if any
    msg @ ?dup if cr count type msg off then      \ if extra message set, print it
    (errsrc) @ if                  \ check if we need to restore
      source-id 1+ if
        sourceline# sourcefilename edit-error
      then nr> restore-input drop            \ restore the input
      (errsrc) off                 \ clear the error source
    then ;

: warnmsg       ( n -- ) s" Warning " (message) ;         \ prints warning:

: errmsg        ( n -- )             \ prints error:
    dup 1+ if s" Error " (message)            \ only do this for real errors, not -1 throw
    else drop then ;

\ -------------------- Text Interpreter Loop --------------------------------

\ note: reset-stacks shouldn't be redefined in an application.
\       use the reset-stack-chain instead.

: _reset-stacks ( i*x -- ) \ reset the stacks
    sp0 @ sp! ;

defer reset-stacks ' _reset-stacks is reset-stacks ( i*x -- ) \ reset the stack
' reset-stacks alias ..

: refill-console ( -- f )
    tib maxcounted accept (source) ! \ read a line from the console
    true ;                 \ never end of file from console

: refill-file ( -- f )
    tib maxcounted 2- source-id read-line \ read a line
    throw_filereadfail ?throw       \ deal with ior
    swap (source) ! ;               \ length read

: refill-mem ( -- f )
    (source-map) 2@ dup if          \ anything left?
      2dup $0A scan       \ find newline
      2dup 1 /string (source-map) 2! \ save rest
      nip -               \ length
      $0D -skip           \ trim off any CRs
      (source) 2!         \ save as line
      true
    else nip then ;       \ nothing left

: refill ( -- f )              \ refill source
    (source-parse) off     \ zero parsed name len
    source-id dup 1+ if    \ -1=evaluate, 0=console, n=file
      >in off if            \ n=file
        (source-l#) 1+!       \ line ctr
        [ (source-map) cell+ ] literal @ if \ any map buffer?
          refill-mem 
        else refill-file then
      else refill-console then      \ from terminal
    else 1+ then ;     \ turn -1 into false

: .[depth] ( -- )
    depth ?dup if
      base@ >r decimal
    0 <# ']' hold #s '[' hold #> type
      r> base!
    then ;

: .ok ( -- )
    s"  ok" type .[depth] ;

\ -------------------- execute-parsing & evaluate -------------------------
\ Make addr u the current input source, execute xt ( ... -- ... ), then restore
\ the previous input source. Borrowed from gforth.

: execute-parsing ( addr len xt -- i*x )
    -rot
    save-input n>r
    (source) 2!
    >in off
    -1 (source-id) !
    c" (evaluate)" (source-nm) !
    ['] execute catch
    dup (save-errsrc)
    nr> restore-input drop
    throw ;

: evaluate ( addr len -- i*x ) \ interpret string addr,len
    ['] interpreter execute-parsing ;

: nextword     ( -- addr len )            \ get next word in input stream
    begin parse-name dup 0=
    while
      drop refill dup 0= if exit then
      2drop
    repeat ;

: definite-word ( -- add len )              \ find definite word in stream
    nextword dup 0= throw_earlyeof ?throw ;

\ -------------------- File Loading -----------------------------------------

2 import: PathAddExtension
2 import: PathFindOnPath

here char+ ," src/objects"
here char+ ," src/asm"
here char+ ," src/lib"
here char+ ," src"
here char+ ," ."

create pathlist , , , , , 
       here 0 , 0 , 0 , 0 , -1 ,
         value pathnext

: +path ( a l -- )
    pathnext dup cell+
    dup @ throw_plistfull ?throw
    to pathnext
    here 1+ swap ! ", ;

: \to/ ( addr len -- ) ( !!! bug in PathFindOnPath )
    bounds ?do i c@ '\' = if '/' i c! then loop ;

: file-rel>abs ( addr len -- addr len )
    buf-allot dup>r place
    r> dup 1+ >r count \to/
    z" .fs" r@ call PathAddExtension drop
    pathlist r@ call PathFindOnPath 0=
    r> zcount 2dup lower 2dup \to/ rot drop ;

\ -------------------- Include file support ---------------------------------

variable echo  \ "echo on" echoes everything to the console that's included

: .echo         ( -- )
      echo @  if
        cr source -trailing type space
      then ;

variable including?     \ set on to stop this file, as in /s

: include-start ( a l -- xt )              \ create name for the current file
    [ ' files >body ] literal swap-current >r
    ['] create execute-parsing        \ create filename
    tfil tfa!
    r> set-current
    0 , cdp @ ,             \ set the low code area ranges
    latestxt @ ;            \ return the xt

: include-end  ( xt -- )
    execute cdp @ swap ! ;            \ get the ranges & complete the header high ranges

: included      ( addr len -- )   \ compile file into current dictionary
    file-rel>abs             \ make absolute
    2dup 2>r
    slurp over if            \ file might be null
      2r> include-start >r   \ generate a filename in the files voc

      save-input n>r         \ save source to rstack
      over (source-id) !     \ fileid is map handle
      (source-map) 2!
      (source-l#) off        \ clear the sourceline# counter
      last @ (source-nm) !   \ current sourcefile

      [:
        including? on        \ read & interpret file
        begin including? @ refill and  \ guarded by catch
        while
          .echo
          interpreter
        repeat
        including? on
      ;] catch               \ read file and catch errors

      dup (save-errsrc)      \ potentially save error source
      source-id burp drop    \ burp the map
      nr> restore-input drop \ restore the src
      r> include-end         \ fill in the file extents
      throw                  \ throw if there was error
    else 2rdrop then ;

: include ( -<filename>- ) parse-str included ;  \ load "filename" into app dictionary

: "loaded?      ( addr len -- flag )               \ flag if loaded
    file-rel>abs             \ extend to full path
    [ ' files >body ] literal search-wordlist    \ search for file name
    dup if nip then ;

: loaded? ( -<name>- -- flag ) parse-str "loaded? ;  \ flag if loaded
    
: required ( addr len -- )
    2dup "loaded? if 2drop             \ search for file name
    else included then ;               \ load the file

: require ( -<filename>- ) parse-str required ;

\ -------------------- Comment words ----------------------------------------

: \             ( -- )  \ comment to end of line
    (source) @ >in ! ; immediate

: (             ( -- )
    ')' source-id if
      >r begin
        source-remain r@ scan nip 0=
      while refill 0= throw_earlyeof ?throw
      repeat r>
    then parse 2drop ; immediate

: _))           ( a1 n1 -- )   \ everything is a comment up to the string a1,n1
    begin
      2dup definite-word
      str= if 2drop exit then
    again ;

: \s ( comment to end of file ) including? off postpone \ ; immediate
     
: \\ postpone \ ; immediate \ comment to end of line
: (( s" ))" _)) ; immediate        \ comment till ))
: /* s" */" _)) ; immediate        \ comment till */

\ --------------------  TO and +TO  ----------------------------------------

\ Tables are indexed on types; see tval etc.

create (to-interp)   ( 8 cells ) 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
create (to-compile)  ( 8 cells ) 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
create (+to-interp)  ( 8 cells ) 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,
create (+to-compile) ( 8 cells ) 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,

: (to) ( table <-name-> -- ) \ lookup entry based on type & execute it
    >r ' dup >body swap
    >name tfa@
    dup $1 $8 between 0= throw_notvalue ?throw
    1- r> +cells perform ;

: to ( <-name-> )
    (to-interp) (to)
    compiles> drop
      (to-compile) (to) ;

: +to ( <-name-> )
    (+to-interp) (to)
    compiles> drop
      (+to-compile) (to) ;

: (to!)   ( body -- ) plit postpone ! ;
: (+to!)  ( body -- ) plit postpone +! ;

' !       (to-interp)   tval 1- cells + !
' +!      (+to-interp)  tval 1- cells + !
' !       (to-interp)   tdef 1- cells + !
' (to!)   (to-compile)  tval 1- cells + !
' (to!)   (to-compile)  tdef 1- cells + !
' (+to!)  (+to-compile) tval 1- cells + !

' to alias is              \ is or to works with defer & value   

\ -------------------- Version and Build -------------------------------------

#version# constant version#
#build#   constant build#

: ((version)) ( version# build# -- addr len )
     base@ >r decimal
     <# 0 #s s"  Build: " holds 2drop
    0 # # '.' hold # # '.' hold #s
     s" STC Experimental 32bit: " holds #>
     r> base! ;

: (version) version# build# ((version)) ;

: .version ( -- ) (version) type ;
' .version alias wf32
 
\ -------------------- Main Entry Point -------------------------------------

\ The main entry point is at exem. This calls task-entry, a word
\ used both here and in the multi-tasking support module. Then main is
\ called; this is the start of the Forth initialisation. Once the environment
\ for Forth is complete, then the deferable word boot is executed. This word
\ can be pointed to the user code to execute; in the kernel, this simply
\ initialises the console, types the version, and allows QUIT to run.

: reset-quit ( -- ) ( reset to console )
    [ sdp data.top ] literal @               \ allocate a slice from top of SDP
    maxbuffer - 0 (source) 2!      \ set the console src buffer
    >in [ (source-len) 2 cells- ] literal erase
    reset-stacks
    state @ -2 = if forth-recognizer spop drop then \ remove macro recognizer
    postpone [          \ interpret state
    c" (console)" (source-nm) !
    console ;        \ select the forth console

: quit ( -- )             \ main loop of forth
    begin                 \ polling loop
      begin
        cr [: refill drop interpreter ;] catch ?dup 0=
      while
        state @ 0= if .ok then
      repeat
      console errmsg
      reset-quit rp0 @ rp!          \ reset all the stacks
    again ;

0 import: GetCommandLine

: cmdline ( -- addr len )       \ prepare the command line
    call GetCommandLine zcount  \ get the commandline
    over c@ '"' = if            \ first char a " ?
    1 /string '"'             \ find next " character
    else bl then                \ else look for a blank
    scan 1 /string ;            \ scan for & bump past

: (boot)        ( -- )
    init-screen
   .version ;

: init-imports ( -- )              \ initialize all procedure libraries
    [: ( nt -- true )             \ init imports
      name>interpret >body
      ['] imp-resolve over !       \ set resolver ep
    0 swap imp.lib !             \ zero library
      true
    ;] [ ' imports >body ] literal traverse-wordlist  \ zero the imports
                         \ NOTE! run-time rather than compile-time
    init-k32             \ early EP support
    lib-link @ ['] lib-free list-apply          \ zero out app libraries
    lib-link @ ['] lib-load list-apply ;        \ reload them

defer init-forth   ' noop is init-forth      \ things to do at start
defer boot         ' (boot) is boot

: main ( -- )            \ main start forth main entry point
                         \ !!! inits must be in order shown !!!
    only forth also definitions    \ set context & current
    init-imports
    init-malloc
    init-forth                     \ extended initialisation
    reset-quit
    ['] boot catch 0= if           \ do boot (which may never return)
      cmdline ['] evaluate catch ?dup if console errmsg then
    then
    quit ;

\ 3 import: VirtualFree
: bye ( ? -- )
\    MEM_RELEASE 0 up0 call VirtualFree drop \ release memory
    call ExitProcess ;         \ exit forth with whatever is top of stack

\ -------------------- Thread support & initialisation ------------------------

\ Exception code set in entry. Used by this code and tasks.fs to provide
\ memory commit for pages that cause an exception C0000005 (access violation)
\ for writes to memory. Allows VirtualAlloc MEM_RESERVE, and the system
\ will automatically turn that into MEM_COMMIT. Type 2 per thread handler.
\ This is the top level handler; there's a generic handler defined
\ below this (also type 2) that performs "generic" recovery and is called after
\ this handler through an indirect jump. This should be set to a callback as
\ the calling sequence is OS and not wf32 based.

\ At the time of the call to the per-thread handler, ESP points to three
\ structures as follows:-
\    ESP+4 Pointer to structure EXCEPTION_RECORD
\    ESP+8 Pointer to own ERR structure
\    ESP+C Pointer to structure CONTEXT record

: exc-default   cr throw_fatal errmsg           \ issue fatal message
    1 ;                   \ default (1, let system handle)

code exc-route  ( except err context -- 0|1)
    3 cb-entry            \ generate entry code
      call    { win-handler ebx }     \ call the routine
    0 cb-exit             \ *note* caller drops params
      next;

code exc-vcommit ( except err context -- 0|1)           \ type2, per-thread
      xor     ecx ecx               \ const 0
      lea     eax { 1 ecx }         \ eax=1 default is next handler
      mov     edx { cell esp }      \ get exception record
      cmp     { cell edx } ecx      \ continuable?
      jne     short @@9             \ next handler
      cmp     dword { edx } EXCEPTION_ACCESS_VIOLATION \ is it an access violation?
      jne     short @@9             \ no, carry on with search
      cmp     { 20 edx } ecx        \ is the violation a read?
      je      short @@9             \ don't bother, continue
      add     ecx { 24 edx }        \ address to commit (ecx is zero, set cc)
      jz      short @@9             \ if zero continue error
      push    PAGE_EXECUTE_READWRITE \ read/write/execute
      push    MEM_COMMIT            \ commit
      push    eax                   \ one byte will do
      push    ecx                   \ address to commit
      call    { ' VirtualAlloc >body imp.ep } \ loaded in kernel, call indirect
      cmp     eax 1                 \ 0= ?
      sbb     eax eax               \ 0 = done OK, -1 = failed
      neg     eax
@@9:  next;

2  4096 * equ rstacksize \ rstack size
3  4096 * equ usr-size   \ user area size for task variables

rstacksize 4096 + constant image-commit \ amount to commit to the stack

\ -------------------- EXEM Start Entry Point -------------------------------

code init-thread ( -- ) ( initialise thread & regs )
      mov     ebx { fs: $14 }            \ get previous user base if any
      test    ebx ebx                    \ check if zero
      jnz     short @@1                  \ no, so just set stacks

      push    PAGE_READWRITE             \ read/write
      push    MEM_COMMIT MEM_RESERVE +             \ commit & reserve
      push    usr-size buf-size +        \ user area and buffers
      push    ebx              \ ebx=0 system assigns address
      call    { ' VirtualAlloc >body imp.ep }      \ loaded in kernel, call indirect
      mov     ebx eax                    \ assign to user area
      lea     esi { usr-size ebx }       \ and the buffer area

      mov     dword { base ebx } #10               \ base 10
      mov     { buf-base ebx } esi       \ [3] init buffer & buf-off
      and     dword { buf-off ebx } 0
      mov     dword { win-handler ebx } ' exc-default \ default handler

      mov     edx { fs: $18 }            \ base of tib
      mov     { edx $14 } ebx            \ [1] save user base in tib at pvarbitrary
@@1:
      pop     edi              \ return address
      mov     ecx { edx }                \ set up err handlers
      push    ' exc-route                \ err handler routing
      push    ecx              \ previous error handler
      mov     ecx esp
      push    ' exc-vcommit              \ err handler commit
      push    ecx              \ previous error handler
      mov     { edx } esp                \ [2] error handler now in tib
      and     esp $fffffff0              \ align to 16bytes
      lea     ebp { rstacksize negate esp }        \ set stack register
      mov     { rp0 ebx } esp            \ save new rp0
      mov     { sp0 ebx } ebp            \ save new sp0
      jmp     edi
      next;

code exem-ep    ( -- addr )
      mov     eax { esp }
      next;

code exem       ( -- )               \ exe entry point
      call    ' exem-ep              \ get base address
      sub     eax ' exem image-origin - 5 + \ subtract to get loadpoint (length of call is 5)
      mov     { ' appinst >body } eax
      call    ' init-k32                 \ initialize libraries
      call    ' init-thread              \ initialise thread
      jmp     ' main
      next;

' exem image-origin - constant img-entry       \ the offset of the ep

\ --------------------------- Console I/O ------------------------------------

: k_noop1 0 ;
: k_noop2 0 dup ;

1 import: GetStdHandle

: stdin  ( -- n ) STD_INPUT_HANDLE  call GetStdHandle ;
: stderr ( -- n ) STD_OUTPUT_HANDLE call GetStdHandle ;
: stdout ( -- n ) STD_ERROR_HANDLE  call GetStdHandle ;

: d_type   ( addr n -- )    stdout write-file throw ;
: d_emit   ( char -- )      sp@ 1 type drop ;
: d_cr     ( -- )           crlf$ count type ;
: d_tab    ( -- )           9 emit ;
: d_bs     ( -- )           8 emit ;
: d_accept ( addr max -- n ) 2- stdin read-line throw 0= if bye then ;

' noop       is init-console
' noop       is init-screen
' noop       is init-title
' noop       is hide-console
' noop       is show-console
' noop       is console

' d_accept   is accept
' k_noop1    is key
' true       is key?
' key        is ekey
' true       is ekey>char
' false      is ekey>fkey

' d_emit     is emit
' d_cr       is cr
' drop       is ?cr
' d_type     is type
' d_tab      is tab
' d_bs       is bs
' noop       is cls
' noop       is beep
' k_noop2    is set-xy
' k_noop2    is get-xy
' k_noop2    is getcolrow
' noop       is cursor-inview
' 2drop      is setcolrow
' drop       is set-cursor
' k_noop1    is get-cursor
' drop       is setrowoff
' k_noop1    is getrowoff
' k_noop2    is getmaxcolrow
' 2drop      is setmaxcolrow
' k_noop1    is cols
' k_noop1    is rows
' drop       is #col
' drop       is #tab

\ -------------------- Tools --------------------

8 equ .smax       \ max number of stack entries to show

: .s            ( -- )
    ?stack
    depth if
       .[depth] space
       depth .smax
       2dup > if s" ... " type then
       min
       begin
          dup pick
          base@
            dup 16 = if '$' emit else
            dup  8 = if '&' emit else
            dup  2 = if '%' emit then then then drop
          .
          1- dup 0=
       until drop
    else s" [empty]" type
    then ;

: dump          ( adr len -- )  ( hex byte format with ascii )
    over + dup rot
    ?do
      cr i 8 h.r s"  | " type            ( the address )
      i 16 + over umin i       ( do up to 16 bytes in each line )
      2dup ?do
        i c@ h.2 space         ( hex value of byte )
        i j 7 + = if space then          ( add space every 8 )
      loop
      2dup
      - 16 over - 3 * swap 8 < - spaces s" |" type ( out to the end of the hex area )
      ?do
        i c@ dup 127 bl within if drop '.' then emit ( a byte as a char )
      loop s" |" type
    16 +loop    drop    ;
    
[debug] [if]

\ -------------------- debug stuff, temporary --------------------

: break         sync-code $cc code-b, ; immediate

: kwords        ( -- "match-str" )
    parse-name pad place pad lowercase drop
    0 [: ( count nt -- count' true )
        dup count pad count search nip nip if
          cr dup name>interpret $. .id
          1+
        else drop
        then true
      ;] context @ traverse-wordlist
    cr . s" words in voc " type ;

0 value .olly-fileid
0 value .olly-vocname

: .olly-write
        .olly-fileid write-file drop ;

: .olly-line ( lfa -- )                \ debug: write a single entry
        link>name dup name>interpret          \ nt xt
      0 (d.) .olly-write             \ write out hex string
        9 sp@ 1 .olly-write drop       \ tab
        .olly-vocname count .olly-write
        s" ." .olly-write
        count .olly-fileid write-line drop ;     \ then name

: .olly-thread ( thread-ptr -- )       \ debug: chase a thread
        begin @ dup
        while
          dup .olly-line
        repeat drop ;

: .olly-voc    ( voc-ptr -- )          \ chase a vocabulary
        dup voc.#0 swap voc#threads
      0 ?do
          dup i cells + .olly-thread
        loop drop ;

: .olly   ( -- )
        base@ >r hex
        s" gkernel32.txt" w/o create-file drop to .olly-fileid
        voc-link
        begin   @ dup
        while
          dup voc.xt @ >name dup to .olly-vocname count
          s" locals" str= not if
            dup .olly-voc
          then
        repeat drop
        .olly-fileid close-file drop r> base! ;

[then]

in-app

\ ------------------------ Miscellaneous words ------------------------

\ ---------------------------------------------------------------------
\
\       These definitions must be last so they are not used
\       during meta compilation
\
\ ---------------------------------------------------------------------

\ None defined currently

\ ---------------------------------------------------------------------
\ Forward reference resolutions, auto resolved in this version
\ because the names match. To override where names don't match,
\ ' xt1 resolves xt2

\ None defined currently

\ ------------------- The End -----------------------------------------



