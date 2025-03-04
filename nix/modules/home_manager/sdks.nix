{ config, lib, pkgs-unstable, ... }:
let
  inherit (lib) lists mkEnableOption;
  cfg = config.sdks;
in
{
  options.sdks = {
    dotnet.enable = mkEnableOption "dotnet";
    groovy.enable = mkEnableOption "groovy";
    scala.enable = mkEnableOption "scala";
    java.enable = mkEnableOption "java";
  };
  config = {
    home.packages =
      (lists.optional (cfg.dotnet.enable) pkgs-unstable.dotnet-sdk_8) ++
      (lists.optional (cfg.groovy.enable) pkgs-unstable.groovy) ++
      (lists.optional (cfg.java.enable) pkgs-unstable.temurin-bin) ++
      (lists.optional (cfg.scala.enable) pkgs-unstable.scala);
  };
}
