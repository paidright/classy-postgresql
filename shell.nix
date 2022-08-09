(import
  ( let 
      lock = builtins.fromJSON (builtins.readFile ./flake.lock); 
      rev = lock.nodes.flake-compat.locked.rev;
    in fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    }
  )
  { src = ./.; }
).shellNix

