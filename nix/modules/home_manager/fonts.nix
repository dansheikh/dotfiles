{ config, lib, pkgs, ... }:
let
  inherit (lib) lists mkEnableOption;
  cfg = config.fonts;
in
{
  options.fonts = {
    nerdfonts.enable = mkEnableOption "nerdfonts";
    powerlineFonts.enable = mkEnableOption "powerline-fonts";
  };
  config = {
    home.packages = with pkgs;
      (lists.optional (cfg.nerdfonts.enable) nerdfonts) ++
      (lists.optional (cfg.powerlineFonts.enable) powerline-fonts);
  };
}
