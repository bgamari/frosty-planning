{
  description = "frosty-planning Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hlint
            pkgs.haskellPackages.ghcid
            pkgs.chromium
          ];
          shellHook = ''export PATH=$PATH:$(dirname $(which cabal))'';
        };

        packages.frosty-planning = pkgs.haskellPackages.callCabal2nix "frosty-planning" ./. {};
      }
    );
}