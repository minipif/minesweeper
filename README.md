Minesweeper
===========

Jonathan Lepage  
December 2010

Haskell implementation of the minesweeper game, using a graphical user interface.  
Features a naive auto-resolution algorithm:

* Auto-flagging: Flags the n unopened cells around a opened n-cell.
* Auto-opening: Opens all unflagged cells around an n-cell surrounded by n flags.
