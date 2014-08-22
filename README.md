# Elm-Redex

Elm-redex is a lambda calculus toy that I wrote in 8 hours in Elm and a little Javascript.

It is available live on my website [here](http://liamoc.net/redex.html).

To build it, currently a bug in Elm means that it must be built against revision 1c6dfdeb or later of the Elm compiler.

Parsimmon, the JS library, is also used for parsing lambda terms.

To build, just invoke the Elm compiler as shown:

    $ elm --only-js Main.elm

And then open `build/Main.html`. Have fun!
