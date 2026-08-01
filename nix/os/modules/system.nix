{ config, pkgs, lib, hostname, user, ... }:

{
  # ── Boot ──────────────────────────────────────────────────────────────────
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Make v4l2loopback modules visible to current active kernel package
  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback
  ];

  # Force boot-time invocation of the loopback layer
  boot.kernelModules = [
    "v4l2loopback"
  ];

  # CRITICAL CAPABILITY CONFIGURATION:
  # `exclusive_caps=1` forces the virtual video device to present itself as a standard web
  # capture device instead of a processing sink.
  # `video_nr=10` pushes it to /dev/video10 so your physical webcam cleanly claims /dev/video0.
  boot.extraModprobeConfig = ''
    options v4l2loopback exclusive_caps=1 card_label="Virtual Camera" video_nr=10
  '';

  # ── Networking ────────────────────────────────────────────────────────────
  networking.hostName = hostname;
  networking.networkmanager.enable = true;

  # ── Display Manager —  greetd + tuigreet ──────────────────────────────────
  services.greetd = {
    enable = true;
    settings = {
      terminal.vt = 1;
      default_session = {
        command = ''
          ${pkgs.tuigreet}/bin/tuigreet \
            --time \
            --asterisks \
            --greeting "Welcome back, ${user}" \
            --remember \
            --remember-session \
            --sessions /run/current-system/sw/share/wayland-sessions
        '';
        user = "greeter";
      };
    };
  };

  services.displayManager.defaultSession = "hyprland";

  # /var/cache/tuigreet must exist for --remember / --remember-session to persist.
  systemd.tmpfiles.rules = [ "d /var/cache/tuigreet 0755 greeter greeter -" ];

  # Prevent greetd from restarting on nixos-rebuild switch — it is the parent
  # process of the running compositor and a restart would kill the session.
  systemd.services.greetd = {
    serviceConfig.Restart = lib.mkForce "no";
    stopIfChanged = false;
  };

  systemd.user.services.emacs-daemon = {
    description = "Emacs text editor daemon";
    documentation = [ "man:emacs(1)" ];
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];

    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.uwsm}/bin/uwsm app -- ${pkgs.unstable.emacs}/bin/emacs --daemon";
      ExecStop = "${pkgs.unstable.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
      Restart = "on-failure";
      # Wait 5 seconds before attempting any restart.
      RestartSec = "5s";
      # Prevent infinite loops: if it crashes 3 times within 30 seconds, stop.
      StartLimitBurst = 3;
      StartLimitIntervalSec = "30s";
    };
  };

  # ── Compositors ───────────────────────────────────────────────────────────
  hardware.graphics.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
  };

  # ── Session environment ───────────────────────────────────────────────────
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    # XDG base dirs in sessionVariables ensure they are available to all
    # PAM session processes. programs.xonsh.config additionally sets them
    # in /etc/xonsh/xonshrc for non-interactive xonsh subprocesses (tmux).
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    XDG_CACHE_HOME = "$HOME/.cache";
  };

  # ── PAM & Keyring ─────────────────────────────────────────────────────────
  security.pam.services.noctalia = { };
  security.pam.services.greetd.enableGnomeKeyring = true;
  services.gnome.gnome-keyring.enable = true;

  # ── Audio ─────────────────────────────────────────────────────────────────
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  # ── Bluetooth / Power / UPower ────────────────────────────────────────────
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;
  services.power-profiles-daemon.enable = true;
  services.upower.enable = true;

  # ── Printing ──────────────────────────────────────────────────────────────
  services.printing.enable = true;

  # ── Shells ────────────────────────────────────────────────────────────────
  # programs.zsh / programs.fish handle vendor completions and /etc/shells.
  # nushell has no NixOS module — installed via packages.nix, registered below.
  # All shell configuration lives under $XDG_CONFIG_HOME, managed by stow.
  programs.zsh.enable = true;
  programs.fish.enable = true;

  # ── Xonsh ─────────────────────────────────────────────────────────────────
  # programs.xonsh.enable:
  #   - installs the xonsh wrapper with xontribs into environment.systemPackages
  #   - registers xonsh in /etc/shells via environment.shells
  #   - generates /etc/xonsh/xonshrc which sources set-environment via bash
  #
  # The xontrib derivation lives in nix/lib/xontrib-prompt-starship.nix and
  # is shared with the darwin configuration to avoid duplication.
  #
  # programs.xonsh.config injects a snippet into /etc/xonsh/xonshrc that:
  #   - sets XDG base dirs for non-interactive xonsh subprocesses (tmux TPM)
  #   - restores Noctalia's bin to PATH after set-environment reconstructs it
  #     (set-environment drops flake-input packages from PATH)
  programs.xonsh = {
    enable = true;
    package = pkgs.unstable.xonsh;

    # extraPackages is consumed by the programs.xonsh module which calls
    # cfg.package.override { inherit (cfg) extraPackages; } internally,
    # producing the wrapped binary added to environment.systemPackages.
    # This is the only correct path — overriding package directly does not
    # result in the xontrib appearing in NIX_PYTHONPATH.
    extraPackages = ps: [
      ((import ../../lib/xontrib-prompt-starship.nix) pkgs ps)
    ];

    config = ''
      import os as _os

      # Set XDG base dirs for all xonsh invocations
      _home = _os.path.expanduser("~")
      _os.environ.setdefault("XDG_CONFIG_HOME", _os.path.join(_home, ".config"))
      _os.environ.setdefault("XDG_DATA_HOME",   _os.path.join(_home, ".local", "share"))
      _os.environ.setdefault("XDG_STATE_HOME",  _os.path.join(_home, ".local", "state"))
      _os.environ.setdefault("XDG_CACHE_HOME",  _os.path.join(_home, ".cache"))

      # Inject the exact current Noctalia path from flake inputs
      _noctalia_bin = "${pkgs.noctalia}/bin/noctalia"
      if _noctalia_bin not in _os.environ.get("PATH", ""):
          _os.environ["PATH"] = _noctalia_bin + ":" + _os.environ.get("PATH", "")

      del _os, _home, _noctalia_bin
    '';
  };

  # ── Users ─────────────────────────────────────────────────────────────────
  users.users.${user} = {
    isNormalUser = true;
    description = "Dan Sheikh";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    # Shell is set as a string — programs.xonsh.enable registers the wrapped
    # binary at this path in /etc/shells. config.programs.xonsh.package
    # returns the pre-override input derivation, not the post-override wrapped
    # result that has NIX_PYTHONPATH set, so a derivation reference would
    # point at the wrong binary.
    shell = "/run/current-system/sw/bin/xonsh";
  };

  # ── Browsers ──────────────────────────────────────────────────────────────
  programs.firefox.enable = true;
}
