{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      in {
        devShells.default = with pkgs; pkgs.mkShell {
          buildInputs = [
            # Rust
            toolchain
            llvmPackages_latest.llvm
            llvmPackages_latest.lld
          ] ++ lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.SystemConfiguration
          ] ++ lib.optionals stdenv.isLinux [
            alsa-lib.dev
            udev.dev
          ];

          RUSTC_LINKER = "${llvmPackages.clangUseLLVM}/bin/clang";
          #RUSTFLAGS="-Z next-solver";
        };

        devShell = self.devShells.${system}.default;
      });
}
