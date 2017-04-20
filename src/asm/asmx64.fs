\ --------------------------- Change Block -------------------------------
\
\ arm 5/23/2012 11:11:51 PM
\     First version 0.1 x86-32 and x86-64 assembler
\ arm 7/1/2012 6:33:21 PM
\     0.2 changes to syntax; see text for details
\
\
\
\ ------------------------- End Change Block -----------------------------
\
((

License (http://www.opensource.org/licenses/BSD-2-Clause)

Copyright (c) 2012, Alex McDonald
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
   
    Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (
INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

))
\ ------------------------------------------------------------------------

cr .( Loading x86 32/64 bit assembler ... ) sourcefilename type

base @

\ Harness words

only forth
    definitions
    vocabulary assembler
also assembler definitions
vocabulary asm-hidden

: in-hidden ( -- ) ( words defined in hidden )
            also asm-hidden definitions previous ;
: in-forth  ( -- ) ( words defined in forth )
            also forth      definitions previous ;
: in-asm    ( -- ) ( words defined in assembler )
            also assembler  definitions previous ;

only forth
  also assembler
  also asm-hidden
  also forth

in-hidden

( defer memory words, set for mode32 only here )  ( *** )
defer     asm-c, '     b, is     asm-c, ( x -- )
defer     asm-w, '     w, is     asm-w, ( x -- )
defer     asm-d, '     L, is     asm-d, ( x -- )
defer     asm-c! '     b! is     asm-c! ( x a -- )
defer     asm-w! '     w! is     asm-w! ( x a -- )
defer     asm-d! '     L! is     asm-d! ( x a -- )
defer     asm-b@ '     b@ is     asm-b@ ( a -- x )
defer    asm-sb@ '    sb@ is    asm-sb@ ( a -- x )
defer     asm-w@ '     w@ is     asm-w@ ( a -- x )
defer     asm-d@ '     L@ is     asm-d@ ( a -- x )
defer    data-c, '     b, is    data-c, ( x -- )
defer    data-w, '     w, is    data-w, ( x -- )
defer    data-d, '     L, is    data-d, ( x -- )
defer    data-c! '     b! is    data-c! ( x a -- )
defer    data-w! '     w! is    data-w! ( x a -- )
defer    data-d! '     L! is    data-d! ( x a -- )
defer    data-b@ '     b@ is    data-b@ ( a -- x )
defer    data-w@ '     w@ is    data-w@ ( a -- x )
defer    data-d@ '     L@ is    data-d@ ( a -- x )
defer   asm-here '   here is   asm-here ( -- a )
defer  asm-align '  align is  asm-align ( -- )

in-asm

include asmx64-msgs.fs
include asmx64-core.fs
include asmx64-extend.fs
include asmx64-sse4.fs
\ include asmx64-sse.fs

in-asm

  ' code-b,     also asm-hidden is asm-c,     previous   ( x -- )
  ' code-w,     also asm-hidden is asm-w,     previous   ( x -- )
  ' code-L,     also asm-hidden is asm-d,     previous   ( x -- )
  ' code-here   also asm-hidden is asm-here   previous   ( -- a )

  ' end-code alias ;c
  ' end-code alias c;
  
include src\kernel\gkernext.fs     \ load exec/next words
  
only forth also definitions
base !
