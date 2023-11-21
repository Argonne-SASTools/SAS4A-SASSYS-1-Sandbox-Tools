This directory contains files that can be used with the TextWrangler text editor on a Mac.

= SASInput.plist:

    Place this file in ~/Library/Application\ Support/TextWrangler/Language\ Modules/

    This file defines basic syntax for a SAS input file, which allows TextWrangler to perform syntax highlighting, to support input block (code) folding, and to define function pop-up navigation. The property list file assumes that SAS input files end with a `.inp` extension. As an alternative, the syntax type of a file can be selected within TextWrangler using the status bar at the bottom of a text window.

= Compare with BASE:

    Place this file in ~/Library/Application\ Support/TextWrangler/Scripts

    Placing this file in the TextWrangler Scripts folder makes it available under the "script" menu in TextWrangler. When selected, the script will fetch the BASE revision of the current document and display a comparison between the two files. The script assumes that the current document is under Subversion version control. The comparison is similar to performing `svn diff` on the file, but allows dynamic updating of the working copy.
