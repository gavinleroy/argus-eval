{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        devShells.default = with pkgs; pkgs.mkShell {
          buildInputs = [
            # For evaluation notebook
            julia-bin
            python3
          ] ++ lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.SystemConfiguration
          ];
          PYTHON = python3;
        };

        devShell = self.devShells.${system}.default;
      });
}
