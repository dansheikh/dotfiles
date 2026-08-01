{ inputs }:
[
  (import ./unstable.nix { inherit inputs; })
  (import ./overrides.nix { inherit inputs; })
]
