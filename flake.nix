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
        wkhtmltopdfBin = "${pkgs.wkhtmltopdf}/bin";
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hlint
            pkgs.haskellPackages.ghcid
            pkgs.wkhtmltopdf
          ];
          shellHook = ''export PATH=$PATH:$(dirname $(which cabal)):${wkhtmltopdfBin}'';
        };

        packages.frosty-planning = pkgs.haskellPackages.callCabal2nix "frosty-planning" ./. {};
      }
    );
}