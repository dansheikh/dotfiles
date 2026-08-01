{ pkgs, ... }:

# pkgs.unstable is available via the overlay declared in modules/core.nix.
# Stable packages:   pkgs.<name>
# Unstable packages: pkgs.unstable.<name>
#
# Principle: install binaries here; configuration lives under $XDG_CONFIG_HOME
# managed by stow. No HM module involvement for stow-managed tools.

let
  # Caffeine toggle script
  idle-toggle = pkgs.writeShellScriptBin "idle-toggle" ''
    PATH="${pkgs.systemd}/bin:${pkgs.libnotify}/bin:$PATH"

    if systemctl --user is-active hypridle.service >/dev/null; then
        systemctl --user stop hypridle.service
        notify-send -u critical -i "coffee" "Caffeine Active" "Idle sleep is disabled. Your computer will stay awake!"
    else
        systemctl --user start hypridle.service
        notify-send -u normal -i "battery-full" "Caffeine Deactivated" "Idle sleep settings have been restored."
    fi
  '';
in
{
  environment.systemPackages = with pkgs; [
    # ── Core utilities ──────────────────────────────────────────────────────
    btop
    file
    hyprshot
    idle-toggle
    pciutils # lspci —  PCI device inspection
    powertop
    stow
    usbutils # lsusb —  USB device inspection
    unzip
    zip

    # ── Media utilities ─────────────────────────────────────────────────────
    exfat
    libwebp
    ntfs3g
    v4l-utils

    # ── Desktop shell ───────────────────────────────────────────────────────
    noctalia

    # ── Shells ──────────────────────────────────────────────────────────────
    # zsh and fish are registered via programs.<shell>.enable in system.nix.
    # nushell has no NixOS module — installed here and registered via
    # environment.shells in system.nix. Config managed by stow.
    unstable.nushell

    # ── Terminals ───────────────────────────────────────────────────────────
    # Configs managed by stow under ~/.config/kitty and ~/.config/wezterm
    unstable.kitty
    unstable.wezterm

    # ── Browsers ────────────────────────────────────────────────────────────
    # Firefox is managed via `programs` in system.nix.
    (unstable.chromium.override {
      commandLineArgs = [
        # Prevents WebRTC from hiding local devices inside Wayland sandboxes
        "--disable-features=WebRtcHideLocalIpsWithMdns"
        # Enable PipeWire portal for screen sharing ONLY.
        # Keeping WebRTCPipeWireCamera OUT forces direct Linux V4L2 camera access!
        "--enable-features=WebRTCPipeWireCapturer"
        # Hardware video acceleration
        "--enable-gpu-rasterization"
        "--enable-hardware-overlays"
        # Native Wayland rendering
        "--ozone-platform=wayland"
      ];
    })
    unstable.brave
    unstable.google-chrome
    unstable.microsoft-edge

    # ── File managers ───────────────────────────────────────────────────────
    nemo # GUI file manager — Super+E keybind
    unstable.yazi # Terminal file manager

    # ── Media players ───────────────────────────────────────────────────────
    unstable.mpv
    unstable.vlc

    # ── Document viewers ────────────────────────────────────────────────────
    unstable.foliate # eBook reader
    unstable.zathura # Keyboard-driven, vim-like navigation
    unstable.zathuraPkgs.zathura_pdf_mupdf # MuPDF rendering backend for zathura
    unstable.sioyek # Research-oriented PDF viewer, vim keybindings

    # ── Editors ─────────────────────────────────────────────────────────────
    # Configs managed by stow under ~/.config/nvim and ~/.emacs.d
    unstable.antigravity-cli
    unstable.antigravity-ide
    unstable.emacs
    unstable.neovim
    unstable.vscode

    # ── Nix tooling ─────────────────────────────────────────────────────────
    unstable.nil
    nixpkgs-fmt

    # ── Wayland / Session utilities ─────────────────────────────────────────
    cliphist
    unstable.networkmanagerapplet
    wl-clipboard
    xdg-utils

    # ── Desktop utilities ───────────────────────────────────────────────────
    unstable.libnotify # Provides notify-send.

    # ── CLI essentials ──────────────────────────────────────────────────────
    chafa
    gnupg
    micro
    zprint

    unstable.babashka
    unstable.bat
    unstable.carapace
    unstable.clojure
    unstable.clojure-lsp
    unstable.delta
    unstable.direnv
    unstable.efm-langserver
    unstable.eza
    unstable.fd
    unstable.fzf
    unstable.git
    unstable.jq
    unstable.lazygit
    unstable.lua-language-server
    unstable.nerdfetch
    unstable.rar
    unstable.ripgrep
    unstable.starship
    unstable.tmux
    unstable.tree
    unstable.tree-sitter
    unstable.unrar

    # ── Build / Language runtimes ────────────────────────────────────────────
    unstable.go
    javaPackages.compiler.temurin-bin.jdk-21
    unstable.gcc
  ];
}
