Simple prolog interpreter.

To compile: ghc Main.hs

How to use:
Main.exe <FILES>

where FILES is a list of all files that have to be loaded and interpreted.

There are two test files - test.pl test2.pl
To load them:
Main.exe test.pl test2.pl

The you can enter Goals or exit the app. by entering exit.

>> ?-grandfather(a,X).
                          **** ANSWER ****
**** If there are multiple solutions, press enter to see the next ****

>> exit
