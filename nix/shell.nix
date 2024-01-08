{ repoRoot, inputs, pkgs, lib, system }:

cabalProject:

{
  name = "nix-shell";

  # prompt = null;

  # welcomeMessage = null;

  # packages = [];

  # scripts = {};

  # env = {};

  # shellHook = "";

  preCommit = {
    cabal-fmt.enable = true;
    nixpkgs-fmt.enable = true;
  };
}
