{ inputs }:
final: prev: {
  # pkgs.unstable is available in every module — no specialArgs threading needed.
  # Usage: pkgs.unstable.bat, pkgs.unstable.neovim, etc.
  unstable = import inputs.nixpkgs-unstable {
    system = final.stdenv.hostPlatform.system;
    config.allowUnfree = true;
  };
}
