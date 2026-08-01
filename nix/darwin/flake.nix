{
  description = "Nix Darwin flake.";

  inputs = {
    nixpkgs.url          = "github:nixos/nixpkgs?ref=nixpkgs-25.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    nix-darwin.url       = "github:nix-darwin/nix-darwin?ref=nix-darwin-25.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Pinned to match the stable channel — same pattern as NixOS flake.
    # inputs.nixpkgs.follows = "nixpkgs-unstable" ensures HM and the unstable
    # overlay resolve packages from the same nixpkgs evaluation.
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-unstable
    , nix-darwin
    , home-manager
    }:
    let
      mkConfig = { system, hostname, user }: nix-darwin.lib.darwinSystem {
        inherit system;
        specialArgs = {
          inherit inputs self hostname user;
          # Use stdenv.hostPlatform.system rather than the bare `system`
          # string to silence the 'system has been renamed' eval warning.
          system = system;
        };
        modules = [
          # Shared modules
          ./modules/core.nix
          ./modules/system.nix
          ./modules/packages.nix

          # Per-host entrypoint
          ./hosts/${hostname}/configuration.nix

          # Home-manager as a nix-darwin module — mirrors NixOS pattern exactly.
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs   = true;
              useUserPackages = true;
              extraSpecialArgs = { inherit inputs; };
              users.${user}  = import ./hosts/${hostname}/home/home.nix;
            };
          }
        ];
      };
    in
    {
      darwinConfigurations = {
        "PL-LM-DSheikh" = mkConfig {
          system   = "aarch64-darwin";
          hostname = "PL-LM-DSheikh";
          user     = "dansheikh";
        };
      };
    };
}
