{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

{
  name = "nix-shell";

  preCommit = {
    cabal-fmt.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
