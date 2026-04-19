# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  pkgs-unstable,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "danish-dell-precision7550"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  # You can disable this if you're only using the Wayland session.
  services.xserver.enable = true;

  # Enable the Gnome Desktop Environment.
  services.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;
  services.gnome.gnome-keyring.enable = true;
  programs.dconf.enable = true;

  # Pluggable Authentication Modules (PAM) services.
  security.pam.services = {
    gdm = {
      enableGnomeKeyring = true;
    };
    hyprlock = {
      enableGnomeKeyring = true;
    };
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.danish = {
    isNormalUser = true;
    description = "Dan Sheikh";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    # packages = with pkgs; [ ];
  };

  # Install zsh.
  programs.zsh.enable = true;

  # Install firefox.
  programs.firefox.enable = true;

  # Install hyprland, hyprlock, & hypridle.
  hardware.graphics.enable = true;
  hardware.nvidia.modesetting.enable = true;
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
  };
  programs.hyprlock.enable = true;
  services.hypridle.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };

  environment.shells = with pkgs; [ zsh ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    antidote
    (catppuccin-gtk.override {
      accents = [
        "blue"
        "flamingo"
        "green"
        "lavender"
        "maroon"
        "mauve"
        "peach"
        "pink"
        "red"
        "rosewater"
        "sapphire"
        "sky"
        "teal"
        "yellow"
      ];
      size = "standard";
      tweaks = [
        "black"
        "rimless"
      ];
      variant = "mocha";
    })
    babashka
    chafa
    cliphist
    direnv
    emacs
    file-roller
    gcc
    ghostty
    gnome-themes-extra
    gnome-tweaks
    go
    javaPackages.compiler.temurin-bin.jdk-21
    micro
    nil
    nixpkgs-fmt
    pkgs-unstable.bat
    pkgs-unstable.clojure
    pkgs-unstable.clojure-lsp
    pkgs-unstable.delta
    pkgs-unstable.efm-langserver
    pkgs-unstable.eza
    pkgs-unstable.fd
    pkgs-unstable.file
    pkgs-unstable.font-awesome
    pkgs-unstable.fzf
    pkgs-unstable.gcc
    pkgs-unstable.git
    pkgs-unstable.jq
    pkgs-unstable.lazygit
    pkgs-unstable.libnotify
    pkgs-unstable.lua-language-server
    pkgs-unstable.nerdfetch
    pkgs-unstable.networkmanagerapplet
    pkgs-unstable.nwg-look
    pkgs-unstable.hyprpaper
    pkgs-unstable.hyprshot
    pkgs-unstable.rar
    pkgs-unstable.ripgrep
    pkgs-unstable.tree
    pkgs-unstable.unrar
    pkgs-unstable.unzip
    pkgs-unstable.waybar
    pkgs-unstable.neovim
    pkgs-unstable.vscode
    pkgs-unstable.zip
    powerline-fonts
    starship
    stow
    swaynotificationcenter
    tmux
    wl-clipboard
    wofi
    zprint
  ];

  fonts.packages = with pkgs-unstable; [
    nerd-fonts.iosevka
    nerd-fonts.jetbrains-mono
    nerd-fonts.victor-mono
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
}
