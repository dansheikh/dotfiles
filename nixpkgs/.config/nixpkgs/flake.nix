# Flake-based Home Manager: https://nix-community.github.io/home-manager/index.html#ch-nix-flakes
{
    description = "Home Manager Configuration";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, home-manager, ... }: {
        homeConfigurations = {
            dan.sheikh = home-manager.lib.homeManagerConfiguration {
                system = "aarch64-darwin";
                pkgs = nixpkgs.legacyPackages.${system}
                username = "dan.sheikh";
                homeDirectory = "Users/dan.sheikh";
                modules = [
                    ./modules/home.nix
                ]
            };
        };
    };
}