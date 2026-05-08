{
  description = "Penultimate - Idris 2 Terminal Library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};
    in {
      devShells = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              idris2
              gmp
              chez
              gnumake
            ];
            shellHook = ''
              export IDRIS2_PREFIX=$PWD/.idris2
              echo "Welcome to Penultimate Development Environment"
              echo "Run 'make build' to build the library"
              echo "Run 'make install' to install locally to .idris2/"
              echo "Run 'make build-examples' to build the examples runner"
            '';
          };
        }
      );
    };
}
