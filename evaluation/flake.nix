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
        ci-publish = pkgs.writeScriptBin "ci-publish" ''
          julia -e 'using Pkg
            Pkg.activate(mktempdir())
            Pkg.add([
              Pkg.PackageSpec(name="PlutoSliderServer", version="0.3.2-0.3"),
            ])

            import PlutoSliderServer

            PlutoSliderServer.github_action(".";
              Export_cache_dir="pluto_state_cache",
              Export_baked_notebookfile=false,
              Export_baked_state=false,
              # more parameters can go here
            )'
        '';
      in {
        devShells.default = with pkgs; pkgs.mkShell {
          buildInputs = [
            ci-publish
            julia-bin
            python3
            libertine
          ] ++ lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.SystemConfiguration
          ];
          PYTHON = python3;
          LIBERTINE_PATH = "${pkgs.libertine}/share/fonts";
        };

        devShell = self.devShells.${system}.default;
      });
}
