{
  description = "A simple PaidRight library project";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
  };

  outputs = { self, nixpkgs }:
    let
      config = { allowUnfree = true; };

      overlays = [
        (import ./nix/overlays/haskell.nix)
      ];

      # System types to support.
      supportedSystems = [ 
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin" 
      ];

      # Helper function to generate an attrset 
      # `{ x86_64-linux = f "x86_64-linux"; ... }`.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: 
        import nixpkgs { inherit config overlays system; }
      );

    in
    {
      # you can use this for debugging overlays within a nix repl
      # `nix repl`
      # `:lf .#'
      # `_pkgs.<arch>.<package-name>`
      _pkgs = forAllSystems (s: nixpkgsFor(s)); 

      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in import ./nix/shell.nix { inherit pkgs; }
      );

      # As this is a simple library, we only expose a default package
      packages = forAllSystems (system:
        let 
          pkgs = nixpkgsFor.${system};
          name = "paidright-library";
          src = ./.;
          args = {};
        in {
          default = pkgs.haskellPackages.callCabal2nix name src args;
        }
      );
    };
}
