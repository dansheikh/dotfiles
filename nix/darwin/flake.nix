{
  description = "Nix Darwin flake.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-25.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin?ref=nix-darwin-25.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ self
    , nixpkgs
    , nixpkgs-unstable
    , nix-darwin
    ,
    }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
      };
      configuration =
        { pkgs, pkgs-unstable, ... }:
        {
          # List packages installed in system profile. To search by name, run:
          # $ nix-env -qaP | grep wget
          environment.systemPackages = with pkgs; [
            babashka
            bat
            cargo
            chafa
            clippy
            clojure
            cljstyle
            coreutils-full
            cq
            delta
            efm-langserver
            fd
            file
            font-awesome
            gcc
            # ghostty
            git
            git-lfs
            glow
            golangci-lint-langserver
            google-cloud-sdk
            hadolint
            httpie
            htop
            k3d
            kind
            lua51Packages.lua
            lua51Packages.luarocks
            marksman
            moreutils
            nixpkgs-fmt
            nodejs_22
            pkgs-unstable.bun
            pkgs-unstable.biome
            pkgs-unstable.bruno
            pkgs-unstable.bruno-cli
            pkgs-unstable.claude-code
            pkgs-unstable.clojure-lsp
            pkgs-unstable.cmake
            pkgs-unstable.deno
            pkgs-unstable.direnv
            pkgs-unstable.emacs
            pkgs-unstable.eza
            pkgs-unstable.fzf
            pkgs-unstable.gemini-cli
            pkgs-unstable.gettext
            # pkgs-unstable.goose-cli
            pkgs-unstable.hoppscotch
            pkgs-unstable.jq
            pkgs-unstable.kitty
            pkgs-unstable.ktlint
            pkgs-unstable.kotlin
            pkgs-unstable.kotlin-interactive-shell
            # pkgs-unstable.kotlin-native
            pkgs-unstable.kotlin-language-server
            pkgs-unstable.kubectl
            pkgs-unstable.lazygit
            pkgs-unstable.libtool
            pkgs-unstable.llama-cpp
            pkgs-unstable.lua-language-server
            pkgs-unstable.neovim
            pkgs-unstable.nil
            pkgs-unstable.ollama
            pkgs-unstable.opencode
            # pkgs-unstable.open-webui
            pkgs-unstable.pandoc
            pkgs-unstable.pandoc-lua-filters
            pkgs-unstable.pandoc-plantuml-filter
            pkgs-unstable.podman
            pkgs-unstable.podman-tui
            pkgs-unstable.rlwrap
            pkgs-unstable.rio
            pkgs-unstable.sbcl
            pkgs-unstable.sqlite
            pkgs-unstable.texliveSmall
            pkgs-unstable.tree-sitter
            pkgs-unstable.thunderbird
            pkgs-unstable.vscode
            pkgs-unstable.wezterm
            prettierd
            powerline-fonts
            R
            ranger
            ripgrep
            rustc
            rustfmt
            shellcheck
            shfmt
            starship
            stow
            stylua
            sqlfluff
            taplo
            temurin-bin
            tilt
            tmux
            tree
            ueberzugpp
            viu
            vscode-json-languageserver
            yaml-language-server
            yamllint
            zprint
          ];

          fonts.packages = with pkgs; [
            nerd-fonts.iosevka
            nerd-fonts.jetbrains-mono
            nerd-fonts.symbols-only
            nerd-fonts.victor-mono
          ];

          # Necessary for using flakes on this system.
          nix.settings.experimental-features = "nix-command flakes";

          # Enable alternative shell support in nix-darwin.
          # programs.fish.enable = true;
          programs.zsh.enable = true;

          security.pam.services.sudo_local = {
            reattach = true;
            touchIdAuth = true;
          };

          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null;

          system.defaults = {
            dock = {
              autohide = true;
              mru-spaces = true;
            };
          };

          system.primaryUser = "dansheikh";

          # Used for backwards compatibility, please read the changelog before changing.
          # $ darwin-rebuild changelog
          system.stateVersion = 6;

          # The platform the configuration will be used on.
          nixpkgs.hostPlatform = "aarch64-darwin";
        };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#PL-LM-DSheikh
      darwinConfigurations."PL-LM-DSheikh" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
        specialArgs = { inherit inputs pkgs pkgs-unstable; };
      };
    };
}
