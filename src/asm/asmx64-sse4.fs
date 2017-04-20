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

only forth
  also assembler
  also asm-hidden
  also forth
in-hidden

\ ------------------ POPCNT opcode ---------------------
\ Population Count; number of bits
\ 363 ($F3) is REP opcode
\   363 017 270 xrm    popcnt reg, reg/mem   16 32 or 64 bit operands

:noname ( op -- op' ) ?opnd-16>mode ;
    2 rreg rreg/mem 0 0 ( inline opcode gen )
    opgrp: s4-popcnt

$f3000fb8 ' s4-popcnt        op: popcnt

\ ------------------ CRC32 opcode ---------------------
\ CRC32 — Accumulate CRC32 Value
\ 362 ($F2) is REPNZ opcode
\   362 046 360+w xrm   crc32 reg, reg/mem

:noname ( op -- op' )
    op+width ;
    2 rreg rreg/mem 0 0 ( inline opcode gen )
    opgrp: s4-crc32

$f20f38f0 ' s4-crc32         op: crc32

