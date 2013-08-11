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

Using a Mainfile containing imperative testcode with printlines to STDOUT was put to use, since 
full blown unit testing would have been overhead IMHO. This here is after all just like playing in a sandbox.
The approach would have been trivial to implement if the course introduction or any other documention on google 
had shown how to do it, instead of having to guess how IO Monads might have to work.
Either there exists some misunderstanding of how to work with haskell on my side, or this workflow is just extremely 
unpopular?

The idea stemmed from my scheme experiences. Back then each defined function was put into a single file, along with 
comments and corresponding testcode, which provided fast feedback when running the scripts, not loosing code 
(compared to coding solely in the REPL and closing the interpreter), and not needing to reload scripts after having made 
changes to it AND running them in separate step.

Maybe this will help somebody with similar problems, I would not want to have to work without this for now.
