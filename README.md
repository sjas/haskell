# haskell

Doing the NPP lab exercises for fun and profit.

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
had shown how to do it, instead of having to guess how IO Monads have might work.
Either there exists some misunderstanding of how to work with haskell on my side, or nobody figured it out/used this 
workflow yet. (I know how unlikely this is... ?)
