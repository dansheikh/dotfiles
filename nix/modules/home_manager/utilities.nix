{ config, lib, pkgs, pkgs-unstable, ... }:
let
  inherit (lib) lists mkEnableOption mkIf;
  cfg = config.utilities;
in
{
  options.utilities = {
    asciidoctor.enable = mkEnableOption "asciidoctor";
    bat.enable = mkEnableOption "bat";
    chafa.enable = mkEnableOption "chafa";
    clang.enable = mkEnableOption "clang";
    coursier.enable = mkEnableOption "coursier";
    cmake.enable = mkEnableOption "cmake";
    delta.enable = mkEnableOption "delta";
    direnv.enable = mkEnableOption "direnv";
    dive.enable = mkEnableOption "dive";
    dockerCredentialHelpers.enable = mkEnableOption "docker-credential-helpers";
    duDust.enable = mkEnableOption "du-dust";
    duckdb.enable = mkEnableOption "duckdb";
    eza.enable = mkEnableOption "eza";
    fd.enable = mkEnableOption "fd";
    ffmpegthumbnailer.enable = mkEnableOption "ffmpegthumbnailer";
    fzf.enable = mkEnableOption "fzf";
    gawk.enable = mkEnableOption "gawk";
    gcc.enable = mkEnableOption "gcc";
    git.enable = mkEnableOption "git";
    githubCli.enable = mkEnableOption "github-cli";
    glow.enable = mkEnableOption "glow";
    gnugrep.enable = mkEnableOption "gnugrep";
    gnupg.enable = mkEnableOption "gnupg";
    gnutar.enable = mkEnableOption "gnutar";
    httpie.enable = mkEnableOption "httpie";
    imagemagick.enable = mkEnableOption "imagemagick";
    jq.enable = mkEnableOption "jq";
    kakouneLsp.enable = mkEnableOption "kakoune-lsp";
    kubectl.enable = mkEnableOption "kubectl";
    kubernetesHelm.enable = mkEnableOption "kubernetes-helm";
    lazygit.enable = mkEnableOption "lazygit";
    libtool.enable = mkEnableOption "libtool";
    libvterm.enable = mkEnableOption "libvterm";
    lsd.enable = mkEnableOption "lsd";
    mediainfo.enable = mkEnableOption "mediainfo";
    neofetch.enable = mkEnableOption "neofetch";
    ohMyFish.enable = mkEnableOption "oh-my-fish";
    ohMyPosh.enable = mkEnableOption "oh-my-posh";
    openssl.enable = mkEnableOption "openssl";
    opentofu.enable = mkEnableOption "opentofu";
    prettierd.enable = mkEnableOption "prettierd";
    podman.enable = mkEnableOption "podman";
    poppler.enable = mkEnableOption "poppler";
    qemu.enable = mkEnableOption "qemu";
    ranger.enable = mkEnableOption "ranger";
    ripgrep.enable = mkEnableOption "ripgrep";
    rlwrap.enable = mkEnableOption "rlwrap";
    starship.enable = mkEnableOption "starship";
    stow.enable = mkEnableOption "stow";
    tmux.enable = mkEnableOption "tmux";
    tree.enable = mkEnableOption "tree";
    unzip.enable = mkEnableOption "unzip";
    vale.enable = mkEnableOption "vale";
  };
  config = {
    home.packages = with pkgs;
      (lists.optional (cfg.asciidoctor.enable) asciidoctor) ++
      (lists.optional (cfg.chafa.enable) pkgs-unstable.chafa) ++
      (lists.optional (cfg.clang.enable) pkgs-unstable.clang) ++
      (lists.optional (cfg.coursier.enable) pkgs-unstable.coursier) ++
      (lists.optional (cfg.cmake.enable) cmake) ++
      (lists.optional (cfg.delta.enable) pkgs-unstable.delta) ++
      (lists.optional (cfg.dive.enable) dive) ++
      (lists.optional (cfg.dockerCredentialHelpers.enable) docker-credential-helpers) ++
      (lists.optional (cfg.duDust.enable) du-dust) ++
      (lists.optional (cfg.duckdb.enable) pkgs-unstable.duckdb) ++
      (lists.optional (cfg.eza.enable) pkgs-unstable.eza) ++
      (lists.optional (cfg.fd.enable) pkgs-unstable.fd) ++
      (lists.optional (cfg.ffmpegthumbnailer.enable) ffmpegthumbnailer) ++
      (lists.optional (cfg.gawk.enable) gawk) ++
      (lists.optional (cfg.gcc.enable) pkgs-unstable.gcc) ++
      (lists.optional (cfg.git.enable) pkgs-unstable.git) ++
      (lists.optional (cfg.githubCli.enable) pkgs-unstable.gh) ++
      (lists.optional (cfg.glow.enable) glow) ++
      (lists.optional (cfg.gnugrep.enable) gnugrep) ++
      (lists.optional (cfg.gnupg.enable) gnupg) ++
      (lists.optional (cfg.gnutar.enable) gnutar) ++
      (lists.optional (cfg.httpie.enable) httpie) ++
      (lists.optional (cfg.imagemagick.enable) imagemagick) ++
      (lists.optional (cfg.jq.enable) pkgs-unstable.jq) ++
      (lists.optional (cfg.kakouneLsp.enable) kakoune-lsp) ++
      (lists.optional (cfg.kubectl.enable) pkgs-unstable.kubectl) ++
      (lists.optional (cfg.kubernetesHelm.enable) pkgs-unstable.kubernetes-helm) ++
      (lists.optional (cfg.lazygit.enable) pkgs-unstable.lazygit) ++
      (lists.optional (cfg.libtool.enable) libtool) ++
      (lists.optional (cfg.libvterm.enable) libvterm) ++
      (lists.optional (cfg.lsd.enable) pkgs-unstable.lsd) ++
      (lists.optional (cfg.mediainfo.enable) mediainfo) ++
      (lists.optional (cfg.neofetch.enable) neofetch) ++
      (lists.optional (cfg.ohMyFish.enable) pkgs-unstable.oh-my-fish) ++
      (lists.optional (cfg.ohMyPosh.enable) pkgs-unstable.oh-my-posh) ++
      (lists.optional (cfg.openssl.enable) openssl) ++
      (lists.optional (cfg.opentofu.enable) pkgs-unstable.opentofu) ++
      (lists.optional (cfg.prettierd.enable) pkgs-unstable.prettierd) ++
      (lists.optional (cfg.podman.enable) pkgs-unstable.podman) ++
      (lists.optional (cfg.poppler.enable) poppler) ++
      (lists.optional (cfg.qemu.enable) qemu) ++
      (lists.optional (cfg.ranger.enable) ranger) ++
      (lists.optional (cfg.ripgrep.enable) pkgs-unstable.ripgrep) ++
      (lists.optional (cfg.rlwrap.enable) rlwrap) ++
      (lists.optional (cfg.stow.enable) pkgs-unstable.stow) ++
      (lists.optional (cfg.tmux.enable) pkgs-unstable.tmux) ++
      (lists.optional (cfg.tree.enable) tree) ++
      (lists.optional (cfg.unzip.enable) unzip) ++
      (lists.optional (cfg.vale.enable) pkgs-unstable.vale);
    programs.bat = mkIf cfg.bat.enable
      {
        config = {
          pager = "less -FR";
          theme = "Dracula";
        };
        enable = true;
        extraPackages = with pkgs.bat-extras; [ batdiff batgrep batman batpipe batwatch prettybat ];
      };
    programs.direnv = mkIf cfg.direnv.enable
      {
        enable = true;
        enableBashIntegration = true;
        enableNushellIntegration = true;
        enableZshIntegration = true;
        nix-direnv = {
          enable = true;
        };
      };
    programs.fzf = mkIf cfg.fzf.enable
      {
        changeDirWidgetCommand = "fd --type d --hidden";
        changeDirWidgetOptions = [ "--preview 'tree -C {}'" ];
        defaultCommand = "fd --type f --hidden";
        defaultOptions = [ "--cycle" "--info=inline" "--layout=reverse" ];
        enable = true;
        enableFishIntegration = true;
        enableZshIntegration = true;
        fileWidgetCommand = "fd --type f --hidden";
        fileWidgetOptions = [ "--preview 'bat -n --color=always {}'" ];
        package = pkgs-unstable.fzf;
        tmux = {
          enableShellIntegration = true;
          shellIntegrationOptions = [ "-p 80%,60%" ];
        };
      };
    programs.starship = mkIf cfg.starship.enable
      {
        enable = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        enableZshIntegration = true;
        settings = { command_timeout = 3000; };
      };
  };
}
