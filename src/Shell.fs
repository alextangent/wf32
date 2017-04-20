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


cr .( Loading Shell Words...)

only forth also definitions

internal

10 import: CreateProcess
2 import: WaitForSingleObject

create startupinfo
    here 0 ,                \ cb
    0 ,                     \ lpReserved
    0 ,                     \ lpDesktop
    0 ,                     \ lpTitle
    373 ,                   \ dwX
    3 ,                     \ dwY
    0 ,                     \ dwXSize
    0 ,                     \ dwYSize
    80 ,                    \ dwXCountChars
    50 ,                    \ dwYCountChars
    0 ,                     \ dwFillAttribute
    STARTF_USEPOSITION
    STARTF_USECOUNTCHARS +
    STARTF_USESHOWWINDOW +
\    STARTF_USESTDHANDLES +
    ,                       \ dwFlags
    SW_SHOWNORMAL W,        \ wShowWindow
    0 W,                    \ cbReserved2
    0 ,                     \ lpReserved2
    0 ,                     \ hStdInput
    0 ,                     \ hStdOutput
    0 ,                     \ hStdError
    here over - swap !

create procinfo
    0 ,                     \ hProcess
    0 ,                     \ hThread
    0 ,                     \ dwPriocessId
    0 ,                     \ dwThreadId

: (createprocess) ( a1 -- f1 )  \ pass to NT without any interpretation
                                \ Note: a1 must point to a zString !!!
    procinfo 4 cells erase  \ clear procinfo
    >r                      \ null terminated parameter string
    procinfo                \ lppiProcInfo
    startupinfo             \ lpsiStartInfo
    0                       \ lpszCurDir
    0                       \ lpvEnvironment
    0                       \ fdwCreate
    1                       \ fInheritHandles
    0                       \ lpsaThread
    0                       \ lpsaProcess
    r>                      \ lpszCommandLine
    0                       \ lpszImageName
    CreateProcess 0= ;

: exec-close-all ( -- )
    [ procinfo cell+ ] literal @ CloseHandle
    procinfo @ CloseHandle 2drop ;

external

: exec-process ( a l -- b )
    buf-allot 2dup + off dup>r swap cmove r>
    (createprocess) 0= ;

: exec-cmd      ( a l -- f )
    exec-process dup if exec-close-all then ;

: exec-cmd-wait ( a l -- f )
    exec-process dup if
      INFINITE procinfo @ WaitForSingleObject drop
      exec-close-all
    then ;
' exec-cmd-wait alias shell

internal

\ external editor,
\ provides (editor) ( line# addr len -- addr len )
\ where result can be passed to exec-cmd

include editor

external

: edit ( <-name-> )         \ edit definition
    ' >name (.viewinfo)
    3dup (.viewtype) 
    (editor) exec-cmd drop ;

: editfile ( <-filename-> ) \ edit filename
    0 parse-str
    (editor) exec-cmd drop ;

internal 

: (cmd) ( a -- )
    count exec-cmd-wait drop ;

external

: cmd s" cmd /k prompt [w32f] $p$g" 
    buf-allot dup>r place
    r> (cmd) ;
: dir s" cmd /c dir " 
    buf-allot dup>r place
    source-remain r@ +place
    postpone \
    cr r> (cmd) ;
' dir alias ls


module

