{ config, lib, pkgs-unstable, ... }:
let
  inherit (lib) lists mkEnableOption mkIf;
  cfg = config.shells;
in
{
  options.shells = {
    carapace.enable = mkEnableOption "carapace";
    fish.enable = mkEnableOption "fish";
    nushell.enable = mkEnableOption "nushell";
    powershell.enable = mkEnableOption "powershell";
    xonsh.enable = mkEnableOption "xonsh";
    zsh.enable = mkEnableOption "zsh";
  };
  config = {
    home.packages = (lists.optional (cfg.powershell.enable) pkgs-unstable.powershell ++
      lists.optional (cfg.xonsh.enable) pkgs-unstable.xonsh);
    programs.carapace = mkIf cfg.carapace.enable
      {
        enable = true;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        enableZshIntegration = true;
        package = pkgs-unstable.carapace;
      };
    programs.fish = mkIf cfg.fish.enable
      {
        enable = true;
        functions = {
          help = {
            body = "\"$argv\" --help 2>&1 | bat --plain --language=help";
          };
        };
        interactiveShellInit = ''
          if test -f '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
              fenv source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
          end

          set -gx ALTERNATE_EDITOR "nvim"
          set -gx DOTNET_TOOLS "$HOME/.dotnet/tools"
          set -gx DOTNET_CLI_TELEMETRY_OPTOUT 1
          set -gx EDITOR "emacsclient -c -a \"\""
          set -gx LANG en_US.UTF-8
          set -gx LC_TYPE en_US.UTF-8
          set -gx MIX_XDG true
          set -gx XDG_CACHE_HOME "$HOME/.cache"
          set -gx XDG_CONFIG_HOME "$HOME/.config"
          set -gx XDG_DATA_HOME "$HOME/.local/share"
          set -gx XDG_STATE_HOME "$HOME/.local/state"
          set -gx MPLCONFIGDIR "$XDG_CONFIG_HOME/matplotlib"
          set -gx POETRY_CACHE_DIR "$XDG_CACHE_HOME/pypoetry"
          set -gx POETRY_CONFIG_DIR "$XDG_CONFIG_HOME/pypoetry"
          set -gx POETRY_DATA_DIR "$XDG_DATA_HOME/pypoetry"
          set -gx VISUAL "emacsclient -c -a \"\""

          fish_add_path "$HOME/.local/bin"
          fish_add_path (go env GOPATH)/bin
          fish_add_path "$DOTNET_TOOLS"

          command -v tmux &> /dev/null && [ -z $TMUX ] && [ -z $INSIDE_EMACS ] && { tmux has &> /dev/null && tmux attach || exec tmux new && exit; }

          neofetch
        '';
        loginShellInit = ''
          if test -f '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
              fenv source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
          end
        '';
        shellAbbrs = {
          ".." = "cd ..";
          "..." = "cd ../..";
          ect = "emacsclient -t -a \"\"";
          ftk = "fzf-tmux -p 80%,60%  --cycle --info=inline --layout=reverse --print0 | xargs -0 -o kak";
          ftm = "fzf-tmux -p 80%,60%  --cycle --info=inline --layout=reverse --print0 | xargs -0 -o micro";
          gad = "git add";
          gch = "git checkout";
          gcl = "git clone";
          gco = "git commit";
          gdi = "git diff";
          ghi = "git history";
          gst = "git status";
          ktl = "kubectl";
          lg = "lazygit";
          ls = "lsd --group-dirs first -la";
          rg = "rg -S";
          tf = "tofu";
        };
        shellAliases = {
          biff =
            "git diff --name-only --relative --diff-filter=d | xargs bat --diff";
          cp = "cp -iv";
          fb = "fd --exec-batch";
          mkdir = "mkdir -pv";
          mv = "mv -iv";
          rm = "rm -iv";
        };
        package = pkgs-unstable.fish;
      };
    programs.nushell = mkIf cfg.nushell.enable
      {
        configFile = {
          text = ''
            command -v tmux &> /dev/null and ($env.INSIDE_EMACS | is-empty) and { tmux has &> /dev/null && tmux attach || exec tmux new && exit; }
            ("$TERM_PROGRAM" != "Apple_Terminal") and eval "$(oh-my-posh init nu --config $env.XDG_CONFIG_HOME/oh-my-posh/oh-my-posh.toml)"

            neofetch
          '';
        };
        enable = true;
        envFile = {
          text = ''
            $env.ALTERNATE_EDITOR = "nvim"
            $env.COURSIER_BIN_DIR = "$env.HOME/.local/share/coursier/bin"
            $env.COURSIER_JVM_CACHE = "$env.HOME/.cache/coursier/jvm"
            $env.DOTNET_TOOLS = "$env.HOME/.dotnet/tools"
            $env.DOTNET_CLI_TELEMETRY_OPTOUT = 1
            $env.EDITOR = "emacsclient -c -a \"\""
            $env.LANG = en_US.UTF-8
            $env.LC_TYPE = en_US.UTF-8
            $env.MIX_XDG = true
            $env.XDG_CACHE_HOME = "$env.HOME/.cache"
            $env.XDG_CONFIG_HOME = "$env.HOME/.config"
            $env.XDG_DATA_HOME = "$env.HOME/.local/share"
            $env.XDG_STATE_HOME = "$env.HOME/.local/state"
            $env.MPLCONFIGDIR = "$env.XDG_CONFIG_HOME/matplotlib"
            $env.POETRY_CACHE_DIR = "$env.XDG_CACHE_HOME/pypoetry"
            $env.POETRY_CONFIG_DIR = "$env.XDG_CONFIG_HOME/pypoetry"
            $env.POETRY_DATA_DIR = "$env.XDG_DATA_HOME/pypoetry"
            $env.SDKMAN_DIR = "$env.HOME/.sdkman"
            $env.VISUAL = "emacsclient -c -a \"\""
            $env.PATH = (
              $env.PATH
              | split row (char esep)
              | append /usr/local/bin
              | append ($env.HOME | path join .local bin)
              | append $env.$DOTNET_TOOLS
              | append (go env GOPATH | path join bin)
              | uniq
            )
          '';
        };
        package = pkgs-unstable.nushell;
      };
    programs.zsh = mkIf cfg.zsh.enable
      {
        antidote = {
          enable = true;
          plugins = [
            "ohmyzsh/ohmyzsh path:lib"
            "ohmyzsh/ohmyzsh path:plugins/colored-man-pages"
            "ohmyzsh/ohmyzsh path:plugins/extract"
            "olets/zsh-abbr kind:defer"
            "romkatv/powerlevel10k kind:fpath"
            "sindresorhus/pure kind:fpath"
            "zsh-users/zsh-autosuggestions"
            "zsh-users/zsh-completions"
            "zsh-users/zsh-history-substring-search"
            "zsh-users/zsh-syntax-highlighting"
          ];
        };
        autocd = true;
        completionInit = "";
        dotDir = ".config/zsh";
        enable = true;
        autosuggestion.enable = true;
        enableCompletion = true;
        envExtra = ''
          if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
              source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
          fi

          export ALTERNATE_EDITOR="nvim"
          export CARAPACE_BRIDGES="zsh,fish,bash,inshellisense"
          export COURSIER_BIN_DIR="$HOME/.local/share/coursier/bin"
          export COURSIER_JVM_CACHE="$HOME/.cache/coursier/jvm"
          export DOTNET_TOOLS="$HOME/.dotnet/tools"
          export DOTNET_CLI_TELEMETRY_OPTOUT=1
          export EDITOR="emacsclient -c -a \"\""
          export LANG=en_US.UTF-8
          export LC_TYPE=en_US.UTF-8
          export MIX_XDG=true
          export XDG_CACHE_HOME="$HOME/.cache"
          export XDG_CONFIG_HOME="$HOME/.config"
          export XDG_DATA_HOME="$HOME/.local/share"
          export XDG_STATE_HOME="$HOME/.local/state"
          export MPLCONFIGDIR="$XDG_CONFIG_HOME/matplotlib"
          export POETRY_CACHE_DIR="$XDG_CACHE_HOME/pypoetry"
          export POETRY_CONFIG_DIR="$XDG_CONFIG_HOME/pypoetry"
          export POETRY_DATA_DIR="$XDG_DATA_HOME/pypoetry"
          export SDKMAN_DIR="$HOME/.sdkman"
          export VISUAL="emacsclient -c -a \"\""
        '';
        history = {
          expireDuplicatesFirst = true;
          extended = true;
          ignoreAllDups = false;
          ignoreDups = true;
          ignoreSpace = true;
          path = "${config.xdg.dataHome}/zsh/zsh_history";
          save = 1000;
          size = 1000;
        };
        initExtra = ''
          command -v tmux &> /dev/null && [ -z $TMUX ] && [ -z $INSIDE_EMACS ] && { tmux has &> /dev/null && tmux attach || exec tmux new && exit; }

          autoload -Uz promptinit && promptinit

          zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
          zstyle ':completion:*:git:*' group-order 'main commands' 'alias commands' 'external commands'

          [ "$TERM_PROGRAM" != "Apple_Terminal" ] && eval "$(oh-my-posh init zsh --config $XDG_CONFIG_HOME/oh-my-posh/oh-my-posh.toml)"

          # [ ! -f "$XDG_CONFIG_HOME/zsh/.p10k.zsh" ] || source "$XDG_CONFIG_HOME/zsh/.p10k.zsh"

          command -v kubectl &> /dev/null && source <(kubectl completion zsh)

          [ -s "$HOME/.sdkman/bin/sdkman-init.sh" ] && source "$HOME/.sdkman/bin/sdkman-init.sh"

          neofetch
        '';
        initExtraFirst = ''
          autoload -Uz compinit && compinit

          export PATH="$HOME/.local/bin:$(go env GOPATH)/bin:$DOTNET_TOOLS:$PATH"
        '';
        package = pkgs-unstable.zsh;
        shellAliases = {
          biff =
            "git diff --name-only --relative --diff-filter=d | xargs bat --diff";
          cp = "cp -iv";
          fb = "fd --exec-batch";
          mkdir = "mkdir -pv";
          mv = "mv -iv";
          rm = "rm -iv";
        };
      };
  };
}
