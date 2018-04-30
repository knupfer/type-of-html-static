{ nixpkgs ? import ./nixpkgs.nix }:

with nixpkgs.haskell.lib;
with nixpkgs.lib;
let tested = [ "ghc822" "ghc842" ];
    eval = x: import ./default.nix { compiler = x; };
in
{ sdist = sdistTarball (eval (last tested));
} // genAttrs tested (x: buildStrictly (eval x))
