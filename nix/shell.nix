{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

{
  name = "nix-shell";

  packages = [
    pkgs.ghcid
    (pkgs.writeShellScriptBin "downward" "exec -a $0 ${pkgs.fast-downward}/bin/fast-downward $@")
  ];
  preCommit = {
    cabal-fmt.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
