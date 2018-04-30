{ nixpkgs ? import ./nixpkgs.nix }:

(import ./default.nix {}).env
