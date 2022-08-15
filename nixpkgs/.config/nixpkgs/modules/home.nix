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
        nerdfonts
        openssl
        pandoc
        qutebrowser
        ripgrep
        rustup
        starship
        stow
        tmux
        tree
        vscode
        yarn
    ];

    programs.home-manager.enable = true;
    programs.direnv.enable = true;
    programs.direnv.nix-direnv.enable = true;
    programs.fish = {
        enable = true;
        interactiveShellInit = "
            set EDITOR nvim
            set -x LANG en_US.UTF-8
            set -x LC_TYPE en_US.UTF-8

            starship init fish | source
        ";
        shellAbbrs = {
            .. = "cd ..";
            ... = "cd ../..";
            giad = "git add";            
            gich = "git checkout";
            gicl = "git clone";
            gico = "git commit";            
            gidi = "git diff";
            gihi = "git history";           
            gist = "git status";
        };
        shellAliases = {
            cp = "cp -iv"
            mkdir = "mkdir -pv"
            mv = "mv -iv"
            rm = "rm -iv"
        };
    };
}