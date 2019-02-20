{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  contiguous = (
    with rec {
      contiguousSource = pkgs.lib.cleanSource ../.;
      contiguousBasic  = self.callCabal2nix "contiguous" contiguousSource { };
    };
    overrideCabal contiguousBasic (old: {
    })
  );
}
