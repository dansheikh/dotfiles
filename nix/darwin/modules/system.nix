{ self, hostname, user, pkgs, ... }: {
  networking.hostName = hostname;
  networking.computerName = hostname;

  programs.zsh.enable = true;

  # Register xonsh in /etc/shells so it can be set as a login shell.
  # The xonsh binary itself is installed via home-manager (home.packages).
  # nix-darwin's environment.shells accepts package derivations; the binary
  # path is resolved automatically from the package output.
  environment.shells = with pkgs; [ zsh unstable.xonsh ];

  # Set XDG base dirs as launchd session environment variables so they are
  # available to ALL processes in the user session — including non-interactive
  # xonsh subprocesses spawned by tmux for `run` commands. Without this,
  # $XDG_CONFIG_HOME is unset when tmux evaluates run "${XDG_CONFIG_HOME}/...".
  launchd.user.envVariables = {
    XDG_CONFIG_HOME = "/Users/${user}/.config";
    XDG_DATA_HOME   = "/Users/${user}/.local/share";
    XDG_STATE_HOME  = "/Users/${user}/.local/state";
    XDG_CACHE_HOME  = "/Users/${user}/.cache";
  };

  security.pam.services.sudo_local = {
    reattach = true;
    touchIdAuth = true;
  };

  system.defaults = {
    dock = {
      autohide = true;
      mru-spaces = true;
    };
    finder = {
      AppleShowAllExtensions = true;
      FXPreferredViewStyle = "clmv";
    };
  };
  system.configurationRevision = self.rev or self.dirtyRev or null;
  system.primaryUser = user;

  users.users."${user}" = {
    home = "/Users/${user}";
    description = user;
  };
}

