{ nixpkgs ? import ./nixpkgs.nix, compiler ? "ghc842" }:

nixpkgs.haskell.packages.${compiler}.callCabal2nix "type-of-html-static" (nixpkgs.lib.sourceFilesBySuffices ./. [".cabal" ".hs" "LICENSE" ".md"]) {}
