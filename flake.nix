# kracht
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>

{
  description = "Kracht - utility library for GNU Guile";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          guile-kracht = pkgs.callPackage ./default.nix { };
          default = self.packages.${system}.guile-kracht;
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.guile-kracht ];
            packages = with pkgs; [
              guile
              # The suite is Veritas, which needs these two, so that
              # `nix develop' can run it without a Guix environment.
              guile-json
              guile-fibers
            ];
            shellHook = ''
              export GUILE_LOAD_PATH="$PWD/src''${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}"
            '';
          };
        });
    };
}
