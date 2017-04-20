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

cr .( Loading Image Manager ) sourcefilename type

((

IMAGEMAN builds Windows EXE images.

For documentation on the PECOFF format, see
http://www.microsoft.com/hwdev/hardware/PECOFF.asp. Note: not included
because of copyright restrictions, but freely downloadable. Also see "
Peering Inside the PE: A Tour of the Win32 Portable Executable File
Format" by Matt Pietrek, and "An In-Depth Look into the Win32 Portable
Executable File Format", same author, both findable on http://msdn.
microsoft.com.

There is very little documentation on the loader. An exception is "What
Goes On Inside Windows 2000: Solving the Mysteries of the Loader" by
Russ Osterlund, also from MSDN.

This quite complex code has 6 major parts.

1. Field definitions using FLDBASE and FLD words.
2. File handling for both .IMG and .EXE file words
3. Section building and writing words
4. Import library building and writing words
5. Header building and writing words
[ 6. Resource copying from module to module (not yet written) ]
[ 7. Building DLLs (not yet written) ]

Example:
--------

See STD-IMG2EXE for an example at the foot of this code
                                                                               
Commentary:
-----------

VIMAGE                   IMAGEMAN has its own dictionary (because of possible name collisions)
                         so a separate dictionary is used.
COMPACT                  Standard file is built with 4096 (4KBYTE) file sections. COMPACT
                         specifies 512 ($200) file sections, which builds a smaller EXE file
                         at possibly the expense of slightly longer load times.
s" name" BUILD           Identifies the new image name.
X SUBSYSTEM              where X is CUI (DOS application) or GUI | GRAPHICAL | EXE
                         (Windows), or DLL (Windows DLL)
n LOADPOINT              The loadpoint of the program (default is $400000). Must be aligned to
                         a 64K boundary.
n ENTRYPOINT             The (relative) entrypoint address in the section declared CODE
N M STACKSIZE            The reserved and committed stack size. Default is 1Mb, 4K
N M HEAPSIZE             The reserved and committed heap size. Default is 1Mb, 4K
s" name" SECTION         The name of the section, case sensitive and 8 chars max.
n SECTIONTYPE            Section characterisics: S-CODE S-EXECUTE S-READ S-WRITE
                                                 S-INIT S-UNINIT S-DISCARD S-SHARE
                         S-CODE S-EXECUTE: section contains executable code
                         S-READ: section can be read
                         S-WRITE: section can be written
                         S-INIT: section contains initialised data
                         Default is none
                         Can appear multiple times for one section
                         STD-CODE is S-INIT S-CODE S-EXECUTE OR OR
                         STD-DATA is S-INIT S-READ S-WRITE OR OR
N M SECTIONDATA          Address and length of the section
N SECTIONRVA             Not normally used, but can set the (relative) address of the section
N SECTIONSIZE            Size of section. Must be equal to or longer than the sectiondata. When
                         loaded, the section will zeroed and padded out to this length with zeros.
                         Optional, set to size of the section data
ENDSECTION               Required to end a section
s" dll" IMPORT-DLL       Import library name
N s" name" IMPORT-FUNC   Import the procedure <-name-> with a hint of N. N can be zero, but the
                         correct hint will speed (marginally) load time.
ENDIMPORTS ( -- N M )    Required to end the imports sections, follow with SECTIONDATA
ENDBUILD                 Creates the image from the information given above.
s" name" IMAGE-LOAD      Loads a .IMG file for subsequent conversion. Sets the following
                         words as a side effect;
                           IMAGE-CODEPTR   pointer to the code section
                           IMAGE-CACTUAL   real size of code section
                           IMAGE-CSIZE     virtual size of code section
                           IMAGE-APPPTR IMAGE-AACTUAL IMAGE-ASIZE ditto APP values
                           IMAGE-SYSPTR IMAGE-SACTUAL IMAGE-SSIZE and SYS values
                           IMAGE-ENTRY     relative offset of entry point

Requirements/restrictions:
--------------------------

Keyword order is important. The following MUST appear in the following order:

  BUILD SECTION ENDBUILD

  SECTION / ENDSECTION must enclose SECTIONDATA and optionally
  SECTIONTYPE and SECTIONRVA keywords.

  SECTION / ENDSECTION must enclose IMPORT-DLL and IMPORT-FUNC

  ENDIMPORTS must terminate IMPORT-DLL and IMPORT-FUNC, and must be followed by SECTIONDATA

Other keywords can appear in any order anywhere between BUILD and ENDBUILD.

Sections are always aligned in memory on 4K boundaries. Each section follows the next in
memory. So, the following sections:

                 SECTION .code
        a $1234 SECTIONDATA
                 ENDSECTION
                 SECTION .extra
        a $1120 SECTIONDATA
                 ENDSECTION

The first section always starts on $1000. Section #1 will start at $1000, and be
$1234 bytes padded out to the next 4K boundary (out to $2FFF). Section #2 will start
at $3000, be $1120 bytes padded out to $4FFF, etc. The .idata section should be
written last.

The entry point is calculated from the start of the last section marked as S-CODE.

No resource section is built (section .rsrc). To be addressed. (Tough one, as the format
isn't documented properly.)

The DLL type is currently ignored.

WATCH OUT! for name collisions. Only specify VIMAGE as a vocabulary if you're not
compiling code, please.


Defaults:  See std-img2exe
---------

Saving an image:
----------------

FSAVE   Creates a new copy of Win32Forth on your harddrive, that is a
        duplicate of the Forth you are already running, and will still
        startup running Forth when the new program is executed.  FSAVE is
        followed by the name of the program to create.  The name passed
        to FSAVE must not be the same name as the Forth program you are
        currently running. FSAVE is used as follows;

        FSAVE newforth <enter>

        The amount of available dictionary space available will remain the
        same as before FSAVE was performed.  If you need to change the
        amount of application dictionary or system dictionary space
        available for a subsequent program compile, you must perform a
        meta-compile.

))

7 #vocabulary vimage
also vimage definitions

base @ decimal

$1000   constant 4kbyte
$100000 constant 1mbyte

4kbyte value head-len                               \ header length, see compact
4kbyte value file-align                             \ file alignment, see compact
0 value zero-buff                                   \ binary zeros for padding

-1 value pe32?

: pe32 -1 to pe32? ;
: pe64  0 to pe32? ;

( Pointers used by image-prep and the meta kernel build )
0 value image-codeptr      \ pointers to areas to write to new image
0 value image-appptr
0 value image-sysptr
0 value image-cactual      \ actual area lengths
0 value image-aactual
0 value image-sactual
0 value image-csize        \ size of area when loaded
0 value image-asize
0 value image-ssize
0 value image-csep         \ offset of start from app load point
0 value image-asep
0 value image-ssep
0 value image-entry        \ RVA of entry point
0 value image-origin       \ load point

begin-structure dos%       \ prebuilt; length is fixed & minimum.
    $3c +                  \ MZ at +0, RVA of PE at +$3c
    lfield: dos.pe         \ RVA of pe
end-structure

begin-structure pe% ( PE header; offset from loadpoint )
4 + \ lfield: pe.pe        \ "PE\0\0"
2 + \ wfield: pe.cpu       \ CPU type 0x14c, x86
    wfield: pe.sect#       \ count of sections
    lfield: pe.dt          \ seconds since December 31st, 1969, at 4:00 P.M.
4 + \ lfield: pe.symb      \ ptr to symbol table
4 + \ lfield: pe.#symb     \ # of symbols
2 + \ wfield: pe.sz        \ size of "optional" header $E0 (PE32) $F0 (PE32+)
    wfield: pe.char        \ characteristics, $0203 (no debug, executable, relocs stripped )
2 + \ wfield: pe.type      \ 0x10B (PE32), 0x20B (PE32+)
1 + \ bfield: pe.majlinkver  \ we set to kernel ver (1)
1 + \ bfield: pe.minlinkver  \ we set to kernel .rev (0)
    lfield: pe.codesz      \ size of all code sections
    lfield: pe.initsz      \ size of all initialised
    lfield: pe.uninitsz    \ size of all uninitialised
    lfield: pe.ep          \ entry point
    lfield: pe.codebase    \ base of code
  dup
    lfield: pe32.database  \ base of data (PE32 only )
    lfield: pe32.load      \ loadpoint of image (PE32 only )
  drop
    xfield: pe64.load      \ loadpoint of image (PE32+ only )
    lfield: pe.sectalign   \ section alignment
    lfield: pe.filealign   \ file alignment
2 + \ wfield: pe.majos     \ major os (5)
2 + \ wfield: pe.minos     \ minor os (2) ( XP or better )
2 + \ wfield: pe.majimgver   \ major img ver (#version)
2 + \ wfield: pe.minimgver   \ minor img ver (#build)
2 + \ wfield: pe.majsubos  \ major os subsys (5)
2 + \ wfield: pe.minsubos  \ minor os subsys (2) ( XP or better )
4 +                        \ reserve (0)
    lfield: pe.imgsz       \ image size
    lfield: pe.headsz      \ headersize
    lfield: pe.chksum      \ checksum
    wfield: pe.subsys      \ subsystem, GUI=2
    wfield: pe.dllchar     \ dll sys, 0
end-structure

begin-structure pe32%
    pe% +
    lfield: pe32.resstack    \ reserve stack
    lfield: pe32.comstack    \ commit stack
    lfield: pe32.resheap     \ reserve heap
    lfield: pe32.comheap     \ commit heap
4 + \ lfield: pe32.lflags    \ loader flags (0)
4 + \ lfield: pe32.dd#       \ number of dict entries (16)
end-structure

begin-structure pe64%
    pe% +
    xfield: pe64.resstack    \ reserve stack
    xfield: pe64.comstack    \ commit stack
    xfield: pe64.resheap     \ reserve heap
    xfield: pe64.comheap     \ commit heap
4 + \ lfield: pe64.lflags    \ loader flags (0)
4 + \ lfield: pe64.dd#       \ number of dict entries (16)
end-structure

begin-structure pedd% ( PE data dictionary, 16 of 8 bytes )
    xfield: pedd.export
    xfield: pedd.import      \ set by endimports word
    xfield: pedd.resource
8 + \ xfield: pedd.exception
8 + \ xfield: pedd.security
8 + \ xfield: pedd.fixup
8 + \ xfield: pedd.debug
8 + \ xfield: pedd.architecture
8 + \ xfield: pedd.globalptr
8 + \ xfield: pedd.tls
8 + \ xfield: pedd.loadconfig
8 + \ xfield: pedd.boundimport
    xfield: pedd.iat         \ set by endimport word
8 + \ xfield: pedd.delayimport
8 + \ xfield: pedd.com+
8 +                          \ reserved
end-structure

begin-structure sect%    ( section descriptor )
    xfield: sect.name     \ name, null terminated (if not= 8)
    lfield: sect.virtsz   \ size in memory                     
    lfield: sect.rva      \ address in memory                  
    lfield: sect.datasz   \ size of data                       
    lfield: sect.fpdata   \ file pointer to data
    lfield: sect.fpreloc  \ file pointer to relocs (0)         
4 + \ lfield: sect.fpline \ fp to linenums
    wfield: sect.#reloc   \ set to zero for exe
2 + \ wfield: sect.#line  \ set to zero
    lfield: sect.char     \ characteristics, see SECTIONTYPE
end-structure

IMAGE_SCN_MEM_READ IMAGE_SCN_CNT_INITIALIZED_DATA or     constant std-read \ abbreviation
STD-READ IMAGE_SCN_MEM_WRITE                      or     constant std-data \ abbreviation
STD-DATA IMAGE_SCN_CNT_CODE IMAGE_SCN_MEM_EXECUTE or or  constant std-code \ abbreviation

appinst    value std-exeload                     \ standard .exe loadpoint

1 constant gui
2 constant dll
3 constant cui


\ ---------------- Pointers to build areas -------------------------

0 value dos-ptr           \ %dos  base of dos header
0 value pe-ptr            \ %pe   pointer to base of PE
0 value pedd-ptr          \ %pedd pointer to base of PE data dictionary
0 value sect-ptr          \ %sect pointer to base of sections
0 value imp-ptr           \ %imp  pointer to import section
0 value img-name          \ image name

\ ---------------- File handling PE image --------------------------

-1 value pef-hndl
              
: ?pef-error  ( flag -- ) if WinErrMsg ON GetLastWinErr then ;

: pef-create  ( -- ) ( create the image file )
    img-name count r/w create-file ?pef-error to pef-hndl ;
     
: pef-open    ( -- ) ( open the image file )
    img-name count r/w open-file ?pef-error to pef-hndl ;

: pef-close   ( -- ) ( close the file )
    pef-hndl close-file ?pef-error -1 to pef-hndl ;

: pef-write   ( addr len -- ) ( write a buffer )
    pef-hndl write-file ?pef-error ;

: pef-repos   ( n -- ) ( reposition file )
    s>d pef-hndl reposition-file ?pef-error ;

: pef-pos     ( -- n ) ( file position? )
    pef-hndl file-position ?pef-error d>s ;

: peimg-padwrite ( addr len -- ) ( write zero padded block )
    dup>r pef-write
    zero-buff r@ pe-ptr pe.filealign @ naligned \ align to image file alignment
    r> - pef-write ;                  \ what's left to write

\ ------------------------- Section building --------------------------

0 value prev-sectrva                                  \ previous section's rva
variable sect#
0 value sect-adr

: sect-next ( -- ) ( next section )
    sect# dup
      @ 1+ dup [ sect% ] literal * sect-ptr + to sect-adr
      swap ! ;

: sectinit   ( -- )                                   \ initialise
    sect# off                                \ zero section count
    sect-ptr to sect-adr
    4kbyte to prev-sectrva                   \ previous rva
    ;

: sectionrva ( n -- ) sect-adr sect.rva ! ;                    \ rva of section

: sectiontype ( n -- )                                \ section characteristics
    sect-adr sect.char dup @ rot or swap ! ;

: section    ( addr len -- )                          \ section name code
    sect-adr >r                              \ base of section
    8 min 2dup lower                         \ parse out the string
    r@ sect.name swap cmove                  \ and move to dest
    prev-sectrva r> sect.rva ! ;             \ in case no rva specified

: sectionsize ( n -- )                                \ section virtual size
    sect-adr dup>r
    sect.virtsz @ negate +to prev-sectrva    \ remove existing length
    4kbyte naligned dup r> sect.virtsz !     \ align to 4k boundary
    +to prev-sectrva ;                       \ add in to length

: sectiondata ( addr len -- )                         \ section data, some fields get filled later
    sect-adr >r
    dup r@ sect.datasz !                     \ length of real data
    sectionsize                              \ and it's the default section size too
    r> sect.fpdata ! ;                       \ will become file ptr when written

: endsection ( -- )                                   \ tidy up section
    sect-adr sect.datasz @
    pe-ptr pe.sectalign @ naligned >r        \ put aligned datasize on rstack
    sect-adr sect.char @
    dup IMAGE_SCN_CNT_CODE and if            \ is section code?
      image-csize pe-ptr pe.codesz +!        \ add to code size
      sect-adr sect.rva @ pe-ptr pe.codebase !        \ set codebase rva
    then
    dup IMAGE_SCN_CNT_INITIALIZED_DATA and if \ is section init? (
      r@ pe-ptr pe.initsz +!                 \ add to init size
      pe32? if
        sect-adr sect.rva @ pe-ptr pe32.database !      \ set database rva only for 32bit)
      then
    then
    IMAGE_SCN_CNT_UNINITIALIZED_DATA and if  \ is this uninitialized?
      r@ pe-ptr pe.uninitsz +!               \ add to uninit size
    then
    r>drop

    sect-next
    sect-adr dos-ptr - file-align naligned   \ update length of the header
      to head-len ;

: sectwrite ( -- )                                   \ write out sections
    sect-ptr sect% sect# @ * bounds ?do     \ now each section, i is offset
      pef-pos                               \ get where we are
      i sect.fpdata @                       \ address of data
      i sect.datasz @                       \ len to write
      peimg-padwrite                         \ write out the section padded
      i sect.fpdata !                       \ update header where we wrote the data
      i sect.datasz dup @
        file-align naligned swap !          \ adjust header
    sect% +loop
    sect-adr sect% -                        ( back off to last entry )
    dup sect.virtsz @ swap sect.rva @ +     ( pick up size + rva )
      pe-ptr pe.imgsz !                     \ imagesize is max address when loaded
    sect# @ pe-ptr pe.sect# w!             \ number of sections in header
    ;

\ -------------------------- RVA calculation -------------------------

0 value  rva-buff
: ->rva      ( n -- n' )                              \ convert to rva
    rva-buff sect-adr sect.rva @ - - ;       \ from start of header

\ -----------------------Import Function building --------------------

((

 Add imports a stand-alone section normally called .idata. Section must be declared.

 IID1:                      ILT          FUNCS              IAT
   IID-RVA-ILT ----------> |   | ---> | hint | func | <--- |   |  <--1
   IID-TIMEDATE            | 0 |                           | 0 |
   IID-FORWARDER      +--> |   | ---> | hint | func | <--- |   |  <--+
   IID-RVA-NAME --... |    |   | ---> | hint | func | <--- |   |     |
   IID-RVA-IAT -->1   |    | 0 |                           | 0 |     |
 IID2:                |     ...                             ...      |
   IID-RVA-ILT -------+                                              |
   IID-TIMEDATE                                                      |
   IID-FORWARDER                                                     |
   IID-RVA-NAME ---> libname                                         |
   IID-RVA-IAT ------------------------------------------------------+
 ...

 Note that the entries are built back-to-front from the declaration order -- the last function
 in the last library appears first, and the first appears last.

 hint|func  is 2 byte hint, zero terminated ASCII name
 
 In a 64bit system, the ILT and IAT are 8 byte entries.

))

begin-structure iid%
  4 +                  \ iid.ilt       ( RVA pointer to ilt )
  4 +                  \ iid.timedate  ( time&date )
  4 +                  \ iid.fwd       ( forwarder )
  lfield: iid.name      ( RVA pointer to iid.name )
  lfield: iid.iat       ( RVA pointer to iat )
end-structure

begin-structure imp% ( shared structure )
  cell +               \ field: imp.link
  field: imp.func    ( IMPORT-FUNC: hint for function, IMPORT-DLL: link for IMPORTs )
  0 +field imp.name
end-structure

variable lib-link      \ head of IMPORT-FUNC-LIBRARYs
variable lib-count     \ count of IMPORT-FUNC-LIBRARYs
variable func-count    \ count of imports

( Uses the locals data area to build temporary structures *** fix )
: impinit    ( -- )  ( initialise import )
    [ ldp data.origin ] literal @ ldp !           \ reset origin
    lib-link off
    lib-count off
    func-count off ;

: IMPORT-DLL ( addr len -- ) ( define library name )
    lib-count 1+!
    in-loc
      lib-link link, 0 , ", align
    in-app ;

: IMPORT-FUNC ( n addr len -- ) ( define import in the above library )
    func-count 1+!
    in-loc
      lib-link @ imp.func link, ( point from lib to function )
      rot , ", align
    in-app ;
    
variable iat-ptr
variable ilt-ptr
variable iid-ptr
variable cur-ptr 
4 value iat-len                             ( length of 1 IAT, 4 or 8 )

: bump-iat/ilt ( -- ) ( bump the ptrs )
    iat-len dup iat-ptr +! ilt-ptr +! ;     ( next IAT and ILT ) ( 8 for 64bit )

: build-str ( link -- ) ( store name at cur-ptr )
    imp.name count                          ( name from link )
    >r cur-ptr @ r@ cmove                   ( move the name )
    r> 1+ 2 naligned cur-ptr +! ;           ( update the ptr, keep word aligned )

: build-func ( link -- ) ( deal with one function )
    cur-ptr @ ->rva dup iat-ptr @ !         ( point at hint, func name )
                        ilt-ptr @ !
    dup imp.func @ cur-ptr @ w!             ( the hint )
    2 cur-ptr +!                            ( next byte )
    build-str bump-iat/ilt ;                ( store name & bump )

: build-lib ( link -- ) ( deal with one lib )
    iid-ptr @                               ( modify the IID )
    iat-ptr @ ->rva over iid.iat !          ( point IID at IAT )
    ilt-ptr @ ->rva swap ( iid.ilt ) !      ( point at ILT )
    dup imp.func @ ['] build-func list-apply   ( for-each function )
    cur-ptr @ ->rva iid-ptr @ iid.name !    ( point at name here )
    build-str bump-iat/ilt                  ( store name & bump )
    iid% iid-ptr +! ;                       ( next IID )

: endimports ( -- addr len ) ( build the imports )
    imp-ptr                                 ( local area? )
    dup to rva-buff                         ( so rva calculation is correct )
    iat-ptr !                               ( this is the start of the IAT )
    lib-count @ dup>r                       ( dup for start )
      func-count @ + iat-len *              ( counts 4 * ) ( *** 8 for 64bit )
    dup iat-ptr @ + ilt-ptr !               ( ILT ptr )
    dup ilt-ptr @ + iid-ptr !               ( IID ptr )
        iat-ptr @ ->rva pedd-ptr pedd.iat 2!    ( point dictionary at IAT & length )
    r> 1+ iid% *                            ( length of all IIDs )
    dup iid-ptr @ ->rva pedd-ptr pedd.import 2! ( point dictionary at IIDs & length )
        iid-ptr @ + cur-ptr !               ( current pointer )
    lib-link @ ['] build-lib list-apply        ( for-each library )
    imp-ptr cur-ptr @ over - ;

\ ------------------------- Header building --------------------------

2 import: SystemTimeToFileTime
1 import: GetSystemTimeAsFileTime

create bdt            \ base date struct for FileTimeToSystemTime
        1970 w,       \ +0  wyear   wed 1st jan 1970 at 00:00:00 hrs
        1    w,       \ +2  wmonth
        4    w,       \ +4  wdayofweek
        1    w,       \ +6  wdayofmonth
        0    w,       \ +8  whour
        0    w,       \ +10 wminute
        0    w,       \ +12 wsecond
        0    w,       \ +14 wmilliseconds

: linktime  ( -- n )                                \ secs since date above
             0 0 SP@ GetSystemTimeAsFileTime drop swap \ get current time
             0 0 SP@ bdt SystemTimeToFileTime drop swap \ get Wed 1st Jan 1970
             d- 10000000 sm/rem nip                 \ adjust as # secs (was 100ns) ??? possible bug here
             ;

0 value buildtype                                   \ exe or dll

: subsystem  ( m -- )                                 \ declare subsystem
    dup dll = if
      drop $210f pe-ptr pe.char
    else
      cui = if 3 else 2 then pe-ptr pe.subsys
    then w! ;

: loadpoint  ( n -- ) \ declare loadpoint
    pe-ptr pe32? if pe32.load else pe64.load then ! ;

: entrypoint ( n -- ) pe-ptr pe.ep ! ; \ declare entrypoint

: stacksize  ( r c -- )
    pe-ptr tuck 
    pe32? if
      pe32.comstack ! 
      pe32.resstack !
    else
      pe64.comstack ! 
      pe64.resstack !
    then ;

: heapsize  ( r c -- )
    pe-ptr tuck pe32? if
      pe32.comheap  ! 
      pe32.resheap  !
    else
      pe64.comheap  ! 
      pe64.resheap  !
    then ;

: compact    ( -- ) $200 dup to file-align to head-len ; \ build a compact header

create pe-img32 ( 32bit PE header with defaults )
    align here
      'P' c, 'E' c, 0 c, 0 c,
      $014c        w, \ cpu type x86
          0        w, \ section count
          0         , \ seconds since December 31st, 1969, at 4:00 P.M.
          0         , \ ptr to symbol table
          0         , \ number of symbols
        $e0        w, \ size of optional header
      $0203        w, \ characteristics of header
      $010b        w, \ magic $010b PE32
          1 c, 1 c,   \ major/minor linker version#
          0         , \ size of all code sections
          0         , \ size of all initialised
          0         , \ size of all uninitialised
          0         , \ entry point
          0         , \ base of code
          0         , \ base of data
    appinst         , \ base of image
     4kbyte         , \ section alignment
     4kbyte         , \ file alignment
          5        w, \ major os (5)
          2        w, \ minor os (2)
    version#       w, \ major image version
      build#       w, \ minor image version
          5        w, \ major subsys ver (5)
          2        w, \ minor subsys ver (2)
          0         , \ reserved
          0         , \ image size
     4kbyte         , \ header size
          0         , \ checksum
          2        w, \ subsystem, GUI=2, CUI=3
          0        w, \ DLL flags
     1mbyte         , \ reserve stack
     4kbyte         , \ commit stack
     1mbyte         , \ reserve heap
     4kbyte         , \ commit heap
          0         , \ reserve
         16         , \ dd# entries
    here swap - constant pe-img32%

create pe-img64 ( 64bit PE header with defaults )
    align here
      'P' c, 'E'  c, 0 c, 0 c,
      $8664        w, \ cpu type x64
          0        w, \ section count
          0         , \ seconds since December 31st, 1969, at 4:00 P.M.
          0         , \ ptr to symbol table
          0         , \ number of symbols
        $f0        w, \ size of optional header
      $0203        w, \ characteristics of header
      $020b        w, \ magic $020b PE32+
          1 c, 1 c,   \ major/minor linker version#
          0         , \ size of all code sections
          0         , \ size of all initialised
          0         , \ size of all uninitialised
          0         , \ entry point
          0         , \ RVA base of code
        0 0        2, \ base of image
     4kbyte         , \ section alignment
     4kbyte         , \ file alignment
          5        w, \ major os (5)
          2        w, \ minor os (2)
    version#       w, \ major image version
      build#       w, \ minor image version
          5        w, \ major subsys ver (5)
          2        w, \ minor subsys ver (2)
          0         , \ reserved
          0         , \ image size
     4kbyte         , \ header size
          0         , \ checksum
          2        w, \ subsystem, GUI=2, CUI=3
          0        w, \ DLL flags
     0 1mbyte      2, \ reserve stack
     0 4kbyte      2, \ commit stack
     0 1mbyte      2, \ reserve heap
     0 4kbyte      2, \ commit heap
          0         , \ reserve
         16         , \ dd# entries
    here swap - constant pe-img64%

: build ( addr len -- ) \ declare new image

    here 4kbyte + 4kbyte naligned
    dup [ 4kbyte 5 * 256 + ] literal erase \ temp buffers
    dup
      dup                         to dos-ptr
      [ 4kbyte 2* ] literal + dup to imp-ptr
      [ 4kbyte 2* ] literal + dup to zero-buff
      [ 4kbyte    ] literal +     to img-name
    -rot img-name place          \ image name

( Initialise the MS DOS header )
    [ 'M' 'Z' 8 lshift or ] literal over w!  ( MZ )
    dos% over dos.pe !                       ( RVA to PE )

( Initialise pointers to PE areas )
    dos% + dup to pe-ptr  ( point at PE )

( move in standard PE for this image type )
    pe32? if
      pe-img32 over pe32% 4
    else
      pe-img64 over pe64% 8
    then to iat-len
    dup>r cmove r> +                       \ move point at data dicts
    dup to pedd-ptr                        \ data dictionaries

( point at data dictionaries and sections )
    pedd% + to sect-ptr                      \ section start
    linktime pe-ptr pe.dt !                  \ time of build
    impinit                                  \ initialise
    sectinit
    ;

((

library imagehlp.dll
3 import: MapFileAndCheckSum             \ helper; checksum for PE

: checksum   ( -- n )
    0 sp@ >r                       \ pass two dwords
    0 sp@ r>
    img-name 1+                  \ name of file
    MapFileAndCheckSum 0<> abort" Failed to checksum file"
    nip ;
))

: endbuild   ( -- )                                   \ fixup all the missing info
    cr ." Building image " img-name count type
    pef-create                            \ create the file
    
    file-align  pe-ptr pe.filealign !        \ n byte file align (mult of 512 bytes)
    head-len    pe-ptr pe.headsz !           \ n byte header size (mult of filealign)
    zero-buff head-len peimg-padwrite        \ write out a dummy header

    sectwrite                                \ write out sections
    pef-pos >r                            \ get file position
    0 pef-repos                           \ position to 0

    dos-ptr head-len peimg-padwrite        \ rewrite corrected header
    pef-close

((
    cr ." generating checksum "
    checksum dup h.
    pef-open                              \ re-open file
    dos% pe.checksum pef-repos            \ offset of checksum field in file
    sp@ 4 pef-write drop                  \ write out changed checksum
    pef-close
))
    
    r> cr ." Built length " dup . ."  bytes (" 10 arshift . ." KB)"
    dos-ptr [ 4kbyte 5 * 256 + ] literal erase ;

\ ------------------ Standard EXE build from .IMG --------------------------

cui value exetype ( used in meta compiler )

: std-img2exe ( addr len -- )            \ compose std image, addr/len is name

    \  COMPACT                            \ COMPACT HEADER
      BUILD                              \ exe name

        EXETYPE          SUBSYSTEM       \ normally gui
        STD-EXELOAD      LOADPOINT
        1MBYTE IMAGE-COMMIT STACKSIZE       \ size and commit
        1MBYTE  $8000       HEAPSIZE
        IMAGE-ENTRY      ENTRYPOINT
        S" .code"                  SECTION
        STD-CODE                     SECTIONTYPE
        IMAGE-CODEPTR IMAGE-CACTUAL  SECTIONDATA
        IMAGE-CSIZE                  SECTIONSIZE
                                   ENDSECTION

        S" .app"                   SECTION
        STD-DATA                     SECTIONTYPE
        IMAGE-APPPTR IMAGE-AACTUAL   SECTIONDATA
        IMAGE-ASIZE                  SECTIONSIZE
                                   ENDSECTION

        S" .sys"                   SECTION
        STD-DATA                     SECTIONTYPE
        IMAGE-SYSPTR IMAGE-SACTUAL   SECTIONDATA
        IMAGE-SSIZE                  SECTIONSIZE
                                   ENDSECTION

        S" .idata"                 SECTION
        STD-READ                     SECTIONTYPE
        S" kernel32.dll"               IMPORT-DLL
          0 S" GetProcAddress"           IMPORT-FUNC
          0 S" LoadLibraryA"             IMPORT-FUNC
                                       ENDIMPORTS
                                     SECTIONDATA
                                   ENDSECTION
      endbuild ;

\ ---------------- In memory image handling ---------------------

: calc ( dp -- offset size addr len ) ( calculate sizes )
    dup data.origin @ appinst - swap       ( the offset )
    dup data.origin 2@ - swap              ( max possible size of area )
    ( data.here ) 2@ over - ;              ( address & length of area )

((
: calc&move ( addr len -- addr' len ) ( calculate then move buffer )
    calc
    dup 4kbyte naligned malloc             ( allocate big enough buffer 4K aligned )
    2dup swap 2>r cmove 2r> ;              ( move it, return address & actual size )
))

only forth definitions also vimage

((
  image-prep marks up variables to build a set of image pointers suitable for
  FSAVEing. There are some things that can't be saved;
    - the state of any files or their file handles
    - any dynamically allocated areas
    - transient areas
    - and there may be others
  Initialization in the kernel makes sure the system dynamic areas are reinstated.
  The sizes and addresses are taken from the DP structures in the kernel.
  It also doubles as dot-word to decribe the current image.
))
: .image ( -- )
    cr ." Load point " appinst $.
    cr ." Entry point +" img-entry dup $. ." at " appinst + .name
    ['] boot defer@ ." , boot point is " .name
    .mem
    ;
    
definitions

: image-prep ( -- ) ( copy the current image )
    cdp calc to image-cactual to image-codeptr to image-csize to image-csep
    adp calc to image-aactual to image-appptr  to image-asize to image-asep
    sdp calc to image-sactual to image-sysptr  to image-ssize to image-ssep
    img-entry to image-entry
    std-exeload to image-origin
    .image ;

only forth definitions also vimage

\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Fsave stuff:    copy a Forth image
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

: ("FSAVE)    ( addr len )                       \ build .exe
              buf-allot dup 1+ >r place
              z" .exe" r@ PathAddExtension drop
              r> zcount STD-IMG2EXE               \ make image
              ;

: "FSAVE      ( addr len  -- )                   \ use current image & build .exe
              cr image-prep                         \ create memory .img
              ("FSAVE) ;                         \ save memory .img

: FSAVE       ( -- <-exename-> )                 \ parse filename and save
              pe32 parse-str "FSAVE
              ;

PREVIOUS

BASE !

