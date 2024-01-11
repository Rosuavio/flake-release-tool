{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

{
  name = "nix-shell";

  packages = [
    pkgs.ghcid
  ];
  preCommit = {
    cabal-fmt.enable = true;
    stylish-haskell.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
