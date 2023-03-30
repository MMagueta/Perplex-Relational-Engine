{
  description = "ExpressDB";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:

    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [ mlton ];

        shellHook = ''
          ${pkgs.mlton}/bin/mlton
        '';
      };
      packages.default = pkgs.callPackage ./default.nix {};
    });
}
