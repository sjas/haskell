# haskell

Doing the NPP lab exercises for fun.

## contents

    - `Test.hs` contains test function
    - `Main.hs` contains test code for fast validation
    - the other .hs files are actual implementations
    
## compile

Since code is being put into Modules, that are automically loaded, only the main file has to be listed for compiling.

    ghc --make Main.hs && ./Main
    
This line being put to a script or a shortcut of your editor and you are good to go. Forget reloads in the REPL.

## comment

Using a Mainfile containing imperative testcode with printlines to STDOUT is used.
A full blown unit test framework would have been overhead here.
First write the test code in the mainfile, then implement. 
This is TDD in its simpliest form.

Hardest part was figuring monads out on my own (and not having understood them until now...)
Maybe this will help somebody with similar problems, I would not want to have to work without this for now.

The idea stemmed from erlier scheme experiences. 
Back then each function was put into a single file, along with comments and corresponding testcode.
Running a file provided fast feedback when running the scripts.
As opposed to pure REPL usage the test code was retained, too.

Not needing to reload scripts after having made changes to it and not having an extra step is also further development streamlining.
