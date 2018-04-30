# Type of html static

This little companion library of `Type of html` provides just one
little function: `static :: Document a => a -> Q Exp`.  Using this
template haskell function on any part of your html document will
escape, render and lift to a Symbol at compile time.

It increases performance a lot by avoiding any runtime computation.
By producing Proxy Symbol, it will fuse at compiletime with adjacent
elements in your document.
