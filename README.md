# haskell

Doing the NPP lab exercises for fun and profit.

## contents

    - `Tests` contains test function
    - `Main` contains test code for fast validation
    - the other files are actual implementations
    
## compile

Since code is being put into Modules, that are automically loaded, only the main file has to be listed for compiling.

    ghc --make Main.hs && ./Main
    
This line being put to a script or a shortcut of your editor and you are good to go. Forget reloads in the REPL.

This would be trivial if the course introduction or any other documention on google had shown how to do it...
