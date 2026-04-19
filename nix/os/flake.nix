{
  description = "NixOS flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs @ { nixpkgs, nixpkgs-unstable, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      pkgs-unstable = import nixpkgs-unstable { inherit system; config.allowUnfree = true; };
    in
    {
      nixosConfigurations = {
        danish-dell-precision7550 = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs pkgs pkgs-unstable; };
          modules = [
            ./configuration.nix
          ];
        };
      };
    };
}
