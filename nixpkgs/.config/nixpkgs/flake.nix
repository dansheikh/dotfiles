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

    outputs = inputs: {
        homeConfigurations = {
            dan.sheikh = inputs.home-manager.lib.homeManagerConfiguration {
                inherit pkgs;
                system = "aarch64-darwin";
                username = "dan.sheikh";
                homeDirectory = "Users/dan.sheikh";
                modules = [
                    ./modules/darwin/home.nix
                ]
            };
        };
    };
}