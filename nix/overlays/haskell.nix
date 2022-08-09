final: prev: 
let
  ghcVersion = "ghc922"; 
  cabal2nix = final.haskellPackages.callCabal2nix;
in
{
  haskellPackages = 
    let 
      withNoTests = prev.haskell.lib.dontCheck;
      withJailbreak = prev.haskell.lib.doJailbreak;
    in prev.haskell.packages.${ghcVersion}.override {
      overrides = new: old: rec {

        # a specific version from Hackage
        servant-server = new.callHackage "servant-server" "0.19.1" { };

        # As of the time of writing, cabal-fmt's latest version on hackage is 0.1.5.1, 
        # which will not compile with Cabal 3.6 library, which is default with GHC 9.2
        # However, master (which is version 0.1.6) does compile with Cabal 3.6.
        # so we explicitly pin it here. Note that this can be removed once 'cabal-fmt' 0.1.6
        # is pushed to hackage and makes its way in nixpkgs 
        # (and we also update flake.lock to a nix commit that includes cabal-fmt 0.1.6)
        cabal-fmt = 
          withJailbreak (
            cabal2nix "cabal-fmt" (prev.fetchgit {
              url = "https://github.com/phadej/cabal-fmt.git";
              sha256 = "qeBMWynb1GTOU0GvhT5bVIKtiWkNuy88Zg5+GpXB/qc=";
              rev = "6651ffdccdfce71330f2b5cde9f8f23b616abf82";
              fetchSubmodules = true;
            }) { }
          );

        # --------
        # Examples
        # --------

        # cabal2nix
        # something = cabal2nix "something" ../../something {};

        # needs to be jailbroken
        # google-oauth2-jwt =
        #   withJailbreak (new.callHackage "google-oauth2-jwt" "0.3.3" { });

        # don't run tests
        # slugify = withNoTests (old.slugify);
      };
    };
}

