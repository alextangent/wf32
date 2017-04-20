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

cr .( Loading global structures... ) sourcefilename type

begin-structure head%  \ length to head.nt
  6 cells - 2- 1-      \ most words return head.nt
  lfield:  head.link   \        [ link field       ]         lfa
  lfield:  head.ct     \        [ ' compile,       ] <---+ 1 ct: compile, or execute (if immediate)
  lfield:  head.xtptr  \  +---- [ xt ptr field     ]     | 2 xt-ptr
  lfield:  head.comp   \  |     [ ' xt-call,       ]     |   how to compile, normally xt-call,
  lfield:  head.rec    \  |     [ recognizer       ]     |   currently 0
  wfield:  head.vfa    \  |     [ view field       ]     |   vfa (line#)
  wfield:  head.ofa    \  |     [ optimize field   ]     |   ofa (length of code for inline)
  wfield:  head.stk    \  |     [ stack effects    ]     |   ste (will be removed)
  bfield:  head.tfa    \  |     [ type flag        ]     |   tfa
  0 +field head.nt     \  |     [ the name letters ]     |   counted string; the name token
  6 cells 2+ +         \  |                              |
end-structure          \  |                              |
                       \  |                              |
                       \  |     [ ct-off           ] ----+   rel offset to head.ct
                       \  +---> [ xt field         ]         code (the xt)

((
begin-structure rec%   \ recognizer
  lfield:  rec.int     \ interpret
  lfield:  rec.comp    \ compile
  lfield:  rec.post    \ postpone
end-structure

begin-structure stk%
 cell-
  lfield:  stk.max     \ maximum stack size
  lfield:  stk.count   \ count
  0 +field stk.ent0    \ entries
 cell+ 
end-structure
))

begin-structure voc%
  lfield:  voc.link
  lfield:  voc.xt
  lfield:  voc.head    \ defining word for this vocabulary
  lfield:  voc.srch    \ search word
  lfield:  voc.iter    \ iteration word
  lfield:  voc.thrd
 0 +field  voc.#0
end-structure

begin-structure lib%
  lfield:  lib.link    \ link to next DLL
  lfield:  lib.handle  \ handle of the DLL
  lfield:  lib.name    \ ptr to counted string
end-structure

begin-structure imp%
  lfield:  imp.ep      \ entry point
  lfield:  imp.lib     \ ptr to lib entry
  lfield:  imp.name    \ ptr to counted string
end-structure

begin-structure data%
  lfield:  data.here   \ Current pointer to area
  lfield:  data.origin \ Address of the area (origin)
  lfield:  data.top    \ Highest address of area (origin + length)
  lfield:  data.link   \ Link of all the DP areas; set in DP-LINK
 0 +field  data.name   \ Counted name of the area
end-structure

begin-structure exc%   \ exception record
  field:   exc.code
  field:   exc.flag
  field:   exc.record
  field:   exc.addr
  field:   exc.#parms
  field:   exc.parms
  field:   exc.access
\  13 cells+
end-structure

begin-structure ctx%   \ context record
  39 cells+
  field:   ctx.edi
  field:   ctx.esi
  field:   ctx.ebx
  field:   ctx.edx
  field:   ctx.ecx
  field:   ctx.eax
  field:   ctx.ebp
  field:   ctx.eip
  1 cells+
  field:   ctx.eflags
  field:   ctx.esp
\  1 cells+
end-structure

\s ------------------------- Vocabulary Structures -------------------------

  Vocabulary dictionary structure
  -------------------------------

  Vocbularies are self-defining, self-searching and iterable. In other
  words, the internal structure of a vocabulary is hidden from the end user by
  a simple OO type technique. The header contains entry points for

    head -- define a word in this vocabulary
    srch -- find a word in this vocabulary
    iter -- iterate over the contents of the vocabulary

  This allows vocabularies and wordlists to be independant of any structure;
  they could be held on disk, in SQL tables, defined as trees etc.

\ ---------------------------- Header Structures ---------------------------

  The compile token is a structure for stateless compilation. For more details, see
  the document https://www.complang.tuwien.ac.at/forth/header-ideas.html

  HEAD.CT is a 2 cell structure, the "compilation token".

      head.comp    compile time code (default is ptr to xt-call, generates a CALL)
      head.ct      for semantics (either EXECUTE or COMPILE,)
      head.xtptr   pointer to the xt

  The XT contains a back offset at XT CELL- that points at the NT (header, or name token).

  For standard words (example given for DUP);
      head.comp    ' XT-CALL, \ generates a CALL
      head.ct      ' COMPILE,
      head.xtptr   ' DUP

  IMMEDIATE (an ANS word) just sets HEAD.CT to EXECUTE; (example for \ comment);
      head.comp    ' XT-CALL, \ never gets used
      head.ct      ' EXECUTE
      head.xtptr   ' \

  INLINE sets the compilation field to inline the code
      head.comp    ' INLINE,

  For words that have no compilation semantics;
      head.comp    ' (COMP-ONLY) \ executes -13 THROW

  NAME>INTERPRET and NAME>COMPILE work as in the Forth 2012 standard.

  The word >CT takes the xt, and uses ct-ptr to find the compilation token (note that
  ct-ptr is a relative address, not a direct pointer back to the header). When we do
  NAME>COMPILE EXECUTE of the NT, this fetches head.xtptr and head.ct; for DUP
  that's

      ' DUP ' COMPILE,

  which when executed compiles DUP. COMPILE, fetches & executes the head.comp field 
  (DUP >COMP @ EXECUTE) executing XT-CALL, ( xt -- ) generating the code for DUP.

  For \, NAME>COMPILE EXECUTE executes ' \ ' EXECUTE. The HEAD.COMP field is ignored.

  COMPILES> sets comp to the compiles> part. For words with special compilation
  semantics, such as IF, the ct looks like this;
      head.comp    ' IF compiles> part
      head.cta     ' COMPILE,
      head.xtptr   ' IF interprets part


  
  Examples;
    : IF -14 throw
      compiles> ...stuff to do IF at compile... ;
    ' opt-swap   compiles-for swap
    ' noop       compiles-for chars
    ' opt-inline compiles-for dup
    : x swap ; inline

  ct-ptr: offset to the ct pair (arrow not shown)

  tfa:  type of name

  ste: byte 1    stk-i effects input
       byte 2    stk-o effects output
       -ve indicates stack effects are unknown.

  ofa: 15-0    length of the xt code, used for inlining

  vfa: unsigned word (0-64k) line number in file.
       Allows input files of up to 64k lines; >64k simply wraps around.




