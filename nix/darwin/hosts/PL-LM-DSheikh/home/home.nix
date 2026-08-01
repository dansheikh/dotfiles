{ inputs, pkgs, lib, ... }:

let
  # Shared xontrib derivation — same source as NixOS to prevent drift.
  mkXontrib = import ../../../../lib/xontrib-prompt-starship.nix;

  xontrib-prompt-starship =
    mkXontrib pkgs pkgs.unstable.xonsh.pythonEnv.pkgs;

  # Wrap xonsh with PYTHONPATH prepended to include the xontrib.
  # No programs.xonsh module exists in nix-darwin; this replicates the
  # mechanism the NixOS programs.xonsh module uses via makeWrapper.
  xonsh-with-xontribs = pkgs.symlinkJoin {
    name        = "xonsh-with-xontribs";
    paths       = [ pkgs.unstable.xonsh ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild   = ''
      wrapProgram $out/bin/xonsh \
        --prefix PYTHONPATH : "${xontrib-prompt-starship}/${pkgs.unstable.xonsh.pythonEnv.sitePackages}"
    '';
  };
in

{
  # ── Identity ──────────────────────────────────────────────────────────────
  home.username      = "dansheikh";
  home.homeDirectory = "/Users/dansheikh";

  # ── Stow boundary — HM does NOT manage these paths ────────────────────────
  # ~/.config/kitty/        — managed by stow
  # ~/.config/wezterm/      — managed by stow
  # ~/.config/nvim/         — managed by stow
  # ~/.emacs.d/ / emacs.org — managed by stow
  # ~/.config/zsh/          — managed by stow
  # ~/.config/starship.toml — managed by stow
  # ~/.config/nushell/      — managed by stow
  # ~/.config/fish/         — managed by stow
  # ~/.config/xonsh/        — managed by stow

  # ── Xonsh + xontribs ─────────────────────────────────────────────────────
  # No programs.xonsh module in nix-darwin — wrap xonsh manually via
  # symlinkJoin + makeWrapper, prepending the xontrib to PYTHONPATH.
  # Note: set xonsh as login shell after first activation:
  #   chsh -s $(which xonsh)
  home.packages = [ xonsh-with-xontribs ];

  # ── State Version ─────────────────────────────────────────────────────────
  home.stateVersion = "25.11";
}
