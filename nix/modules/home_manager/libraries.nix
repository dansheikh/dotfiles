{ config, lib, pkgs, ... }:
let
  inherit (lib) lists mkEnableOption;
  cfg = config.libraries;
in
{
  options.libraries = {
    gettext.enable = mkEnableOption "gettext";
    libnotify.enable = mkEnableOption "libnotify";
    libtool.enable = mkEnableOption "libtool";
    ncurses.enable = mkEnableOption "ncurses";
  };
  config = {
    home.packages = with pkgs;
      (lists.optional (cfg.gettext.enable) gettext) ++
      (lists.optional (cfg.libnotify.enable) libnotify) ++
      (lists.optional (cfg.libtool.enable) libtool) ++
      (lists.optional (cfg.ncurses.enable) ncurses);
  };
}
