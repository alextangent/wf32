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

in-hidden

\ Assembler errors in extended range

: asmmsg: ( addr len <-name-> -- )
    exception create ,
    does> @ ?throw ;

s" invalid in 64bit mode"                 asmmsg: _?inv64b
s" invalid in 32bit mode"                 asmmsg: _?inv32b
s" register operand not allowed"          asmmsg: _?notreg
s" memory operand not allowed"            asmmsg: _?notmem
s" memory operand missing"                asmmsg: _?mempty
s" memory operand required"               asmmsg: _?memreq
s" immediate operand not allowed"         asmmsg: _?notimm
s" unexpected immediate operand"          asmmsg: _?unexim
s" RIP register not allowed"              asmmsg: _?notrip
s" modifier not allowed"                  asmmsg: _?notmod
s" operand duplicated"                    asmmsg: _?dupopr
s" invalid operand size"                  asmmsg: _?invosz
s" operand size required"                 asmmsg: _?reqosz
s" mismatched operand size"               asmmsg: _?misosz
s" relative address too big"              asmmsg: _?invrsz
s" invalid branch operand size"           asmmsg: _?invbsz
s" invalid address size"                  asmmsg: _?invasz
s" mismatched address sizes"              asmmsg: _?misasz
s" invalid register type"                 asmmsg: _?invrty
s" invalid register #"                    asmmsg: _?invreg
s" can't use ESP/RSP as index"            asmmsg: _?espinx
s" scale with no index register"          asmmsg: _?sclnoi
s" no opening { found"                    asmmsg: _?misop{
s" no closing } found"                    asmmsg: _?miscl}
s" invalid 64bit displacement"            asmmsg: _?not/64
s" requires 64bit immediate"              asmmsg: _?misi64
s" requires 64bit displacement"           asmmsg: _?misd64
s" too few operands"                      asmmsg: _?2fewop
s" too many operands"                     asmmsg: _?2mnyop
s" register must be first operand"        asmmsg: _?reg1st
s" too many open fwd refs"                asmmsg: _?fwdref
s" too many macro labels"                 asmmsg: _?labels
s" unresolved fwd refs"                   asmmsg: _?unresf
s" FAR is unsupported"                    asmmsg: _?unsfar

in-asm
