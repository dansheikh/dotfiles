{ config, pkgs, ... }:

{
    home.packages = with pkgs; [
        awscli
        bat
        curlie
        docker
        du-dust
        fd
        fish
        httpie
        git
        gnupg
        go
        gpg
        jq
        kitty
        lsd
        neofetch
        neovim
        openssl
        pandoc
        qutebrowser
        ripgrep
        rustup
        starship
        tmux
        tree
        vscode
        yarn
    ];

    programs.home-manager.enable = true;
    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
}