# wf32
A 32 bit Forth for Windows; conforms with minor omissions to the Forth 2012 standard
documented at http://forth-standard.org/

To install WF32, run rebuild32.bat. This creates the full system wf32.exe.

Some useful utility words (and more doc will follow):

WORDS      Shows all the words in the system
EDIT name  Edit a word. For this, you will need an editor. The system as built
           uses CONTEXT, which can be obtained from here: http://www.contexteditor.org/index.php
           If you want to use your own editor, then change the word (editor) in file src/editor.fs 
           to point to your favourite editor.
SEE name   Shows a description and disassembly of the word.

