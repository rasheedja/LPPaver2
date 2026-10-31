{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hsPkgs = pkgs.haskell.packages.ghc96;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            # Haskell Tools
            hsPkgs.ghc
            pkgs.cabal-install
            pkgs.stack
            hsPkgs.fourmolu
            hsPkgs.hlint
            hsPkgs.haskell-language-server
            # Frontend tools
            pkgs.nodejs
            # C libraries needed by Haskell deps and GHC runtime
            pkgs.zlib
            # Services, need npm, and a wrapper for redis:
            pkgs.redis
            pkgs.webdis
            # Other tools
            pkgs.bash # Workaround for copilot breaking in nix+direnv shells
            pkgs.git
            pkgs.gh
          ];
        };
      });
}
