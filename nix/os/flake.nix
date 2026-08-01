{
  description = "NixOS configuration";

  nixConfig = {
    extra-substituters = [ "https://noctalia.cachix.org" ];
    extra-trusted-public-keys = [
      "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="
    ];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

  };

  outputs =
    inputs@{ self, nixpkgs, home-manager, ... }:
    let
      mkConfig =
        { arch, hostname, user }:
        nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs self hostname user; };
          modules = [
            # Set hostPlatform via module rather than the deprecated top-level
            # system argument to nixosSystem — eliminates the evaluation warning.
            { nixpkgs.hostPlatform = arch; }
            ./modules/core.nix
            ./modules/system.nix
            ./modules/packages.nix
            ./modules/fonts.nix
            ./hosts/${hostname}/configuration.nix

            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit inputs; };
                users.${user} = import ./hosts/${hostname}/home/home.nix;
              };
            }
          ];
        };
    in
    {
      nixosConfigurations = {
        danish-dell-precision7550 = mkConfig {
          arch = "x86_64-linux";
          hostname = "danish-dell-precision7550";
          user = "danish";
        };
      };
    };
}
