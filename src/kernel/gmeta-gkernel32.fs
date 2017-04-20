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

cr .( Loading META GKERNEL Wrapper ) sourcefilename type

only forth also definitions also vimage

warning off

vocabulary meta         \ metacompiler implementation
only forth also meta definitions

kern-ver count included      \ get kernel version

also vimage                    

1024 4096 * $1000 naligned to image-asize  \  size of kernel application dictionary
1024 4096 * $1000 naligned to image-csize  \  size of kernel code dictionary
1024 4096 * $1000 naligned to image-ssize  \  size of kernel system dictionary

0                                       $1000 + to image-csep \ separations
image-csize                             $1000 + to image-asep
image-asize image-csize +               $1000 + to image-ssep

 $10000 dup malloc to image-codeptr image-codeptr swap  $90 fill \ where code image-appptr is built
 $10000 dup malloc to image-appptr  image-appptr  swap erase \ where target image-appptr is built
 $10000 dup malloc to image-sysptr  image-sysptr  swap erase \ where target heads are built

previous 

401 constant #threads                        \ # of threads in forth-wordlist
 17 constant #ithreads                       \ # of threads in procs lexicon
  3 constant #ethreads                       \ # of threads in exports lexicon
  1 constant #fthreads                       \ # of threads in files lexicon
 59 constant #hthreads                       \ # of threads in hidden lexicon
  1 constant #rthreads                       \ # of threads in root lexicon

[debug]     [if] cr .( [debug]     generating basic kernel debug) [then]
[tail-call] [if] cr .( [tail-call] optimising tail calls)  [then]
[meta-dbg]  [if] cr .( [meta-dbg]  message level ) [meta-dbg] . [then]

\ ===================================================================

\ ** NOTE From this point on, everything is meta compilation, so no 
\ ** colon definitions or code. Has to be all interpreted.

\ ======================= LOAD COMPILER =============================

kern-next count included              \ load kernel extension for next, exec code
kern-cmp  count included              \ load the compiler

CR
CR .( Build information )
CR .(   Directory:    ) current-dir$ type
CR .(   Compiler:     ) kern-cmp  count type
CR .(   NEXT macros:  ) kern-next count type
CR .(   Source:       ) kern-src  count type
CR .(   Version from: ) kern-ver  count type
CR .(   Version:      ) #version# #build# ((version)) type
CR .(   Build Image:  ) kern-name count type
   .(   Type: ) Z" GUI DLL CUI " exetype dup . 1- cells+ 4 space type
   

CR CR .( Compiling... ) \ time-reset

\ ======================= COMPILED CODE =============================

kern-src  count included              \ load & compile the kernel source

\ add this file into the files list

voc-files   \ switch to files vocabulary
  create src/kernel/gkernel32.fs \ dummy filename with extent descriptors
         tcode-here , appinst ,
voc-forth

\ ===================== END COMPILED CODE ===========================

cr .( Compile complete, doing fixups)

\ NOTE: We're still in compile mode, still only interpreted after this

\ Initialize list head ptr variables
cr .( Fixup list heads...)
libs-list-t @ lib-link t-!
voc-link-t @ voc-link t-!

\ Copy over the threads in the vocabularies

cr .( Move vocabulary threads...)      
voc-threads imports                                  \ correct the vocabs
  imports-threads swap tsys-there #ithreads cells move \ move the threads
voc-threads files                                 \ correct the vocabs
  files-threads   swap tsys-there #fthreads cells move \ move the threads
voc-threads hidden                                  \ correct the vocabs
  hidden-threads  swap tsys-there #hthreads cells move \ move the threads
voc-threads root                                 \ correct the vocabs
  root-threads    swap tsys-there #rthreads cells move \ move the threads
voc-threads forth                                  \ correct the vocabs
  forth-threads   swap tsys-there #threads cells move \ move the threads

\ Calculate lengths of sections to write

cr .( Fixup data pointers & section lengths...)

tapp-here  image-origin image-asep + - to image-aactual
tsys-here  image-origin image-ssep + - to image-sactual
tcode-here image-origin image-csep + - to image-cactual

\ Initialises the data pointers (see gstructs.fs):
\ begin-structure data%
\   lfield:  data.here   \ Current pointer to area
\   lfield:  data.origin \ Address of the area (origin)
\   lfield:  data.top    \ Highest address of area (origin + length)
\   lfield:  data.link   \ Link of all the DP areas; set in DP-LINK
\  0 +field  data.name   \ Counted name of the area
\ end-structure

tapp-here   adp data.here t-!                                 \ init the data pointers
tsys-here   sdp data.here t-!
tcode-here  cdp data.here t-!

image-origin image-csep + cdp data.origin t-!
image-origin image-asep + adp data.origin t-!
image-origin image-ssep + sdp data.origin t-!

image-origin image-csep + image-csize + cdp data.top t-!
image-origin image-asep + image-asize + adp data.top t-!
image-origin image-ssep + image-ssize + sdp data.top t-!

' exem image-origin - to image-entry          \ entry point

\ Make sure words all resolved

.fixup [if] cr c" *** Errors in compile" abort!
            [else]
              cr image-stats
              [image-save] [if]
                kern-name count std-img2exe
              [then]
            cr .( Compilation complete, ) .elapsed
            [then]

recover  \ tries to reset for debugging purposes
