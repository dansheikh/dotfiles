{ config, lib, pkgs-unstable, ... }:
let
  inherit (lib) lists mkEnableOption mkIf;
  cfg = config.editors;
in
{
  options.editors = {
    emacs.enable = mkEnableOption "emacs";
    kakoune.enable = mkEnableOption "kakoune";
    micro.enable = mkEnableOption "micro";
    neovim.enable = mkEnableOption "neovim";
    vscode.enable = mkEnableOption "vscode";
    vscodium.enable = mkEnableOption "vscodium";
    zed.enable = mkEnableOption "zed";
  };
  config = {
    home.packages =
      (lists.optional (cfg.emacs.enable) pkgs-unstable.emacs30) ++
      (lists.optional (cfg.micro.enable) pkgs-unstable.micro) ++
      (lists.optional (cfg.neovim.enable) pkgs-unstable.neovim) ++
      (lists.optional (cfg.vscode.enable) pkgs-unstable.vscode) ++
      (lists.optional (cfg.vscodium.enable) pkgs-unstable.vscodium) ++
      (lists.optional (cfg.zed.enable) pkgs-unstable.zed-editor);
    programs.kakoune = mkIf cfg.kakoune.enable
      {
        config = {
          alignWithTabs = true;
          autoComplete = [ "insert" "prompt" ];
          autoInfo = [ "command" "onkey" ];
          autoReload = "ask";
          colorScheme = "catppuccin_mocha";
          incrementalSearch = true;
          indentWidth = 2;
          numberLines = {
            enable = true;
            relative = true;
          };
          showMatching = true;
          showWhitespace = {
            enable = false;
          };
          tabStop = 2;
          ui = {
            enableMouse = true;
            statusLine = "bottom";
          };
          wrapLines = {
            enable = true;
          };
        };
        enable = true;
        extraConfig = ''
            define-command -docstring "fuzzy-find file" -params 0 fuzzy-edit %{
              evaluate-commands %sh{
                file=$(fd --type f --hidden | fzf-tmux $FZF_TMUX_OPTS --cycle --info=inline --layout=reverse --preview "bat --color=always --style=numbers {}" --print0)
                if [ -n "$file" ]; then
                  printf 'edit "%s"\n' "$file"
                fi
              }
            }

            define-command -docstring "fuzzy-find buffer" -params 0 fuzzy-buffer %{
              evaluate-commands %sh{
                buffer=$(
                  (
                    eval "set -- $kak_buflist"
                    while [ $# -gt 0 ]
                    do
                      printf "%s\0" "$1"
                      shift
                    done
                  ) | fzf-tmux $FZF_TMUX_OPTS --cycle --info=inline --layout=reverse --preview "bat --color=always --style=numbers {}" --read0
                )
                buffer=''${buffer/\'/\'\'}
                if [ -n "$buffer" ]; then
                  printf 'buffer "%s"\n' "$buffer"
                fi
              }
            }

            define-command -docstring "grep-find file" -params 0 grep-edit %{
              evaluate-commands %sh{
                initial_query="''${*:-}"
                rg_prefix="rg --color=always --column --hidden --line-number --no-heading --smart-case "
                file=$(: | fzf-tmux $FZF_TMUX_OPTS --ansi --bind "start:reload:''$rg_prefix {q}" --bind "change:reload:sleep 0.1; $rg_prefix {q} || true" --cycle --delimiter : --disabled --info=inline --layout=reverse --preview "bat --color=always --highlight-line {2} --style=numbers {1}" --print0 --query "$initial_query")
                if [ -n "$file" ]; then
                  printf 'edit "%s"\n' "$file"
                fi
              }
            }

            evaluate-commands %sh{kak-lsp --kakoune -s $kak_session}

            hook global WinSetOption filetype=(go|javascript|nix|python|rust|typescript) %{
              lsp-enable-window
              lsp-auto-hover-enable
            }

            hook global WinSetOption filetype=(go|nix|rust) %{
              hook window BufWritePre .* lsp-formatting-sync
            }

            hook global WinSetOption filetype=sql %{
              evaluate-commands %sh{
                if command -v jq &> /dev/null && command -v sqlfluff &> /dev/null; then
                	printf 'set-option window lintcmd %%{run() { cat "$1" | sqlfluff lint --format json - | jq -r '\'''map(.violations[] + {filepath: .filepath}) | .[] | "\\(.filepath):\\(.line_no):\\(.line_pos): error: \\(.description)"'\'''; } && run}\n'
                fi
              }
            }

            hook global WinSetOption filetype=python %{
              evaluate-commands %sh{
            	  if command -v jq &> /dev/null && command -v ruff &> /dev/null; then
            	    printf 'set-option window lintcmd %%{run() { cat "$1" | ruff check --format json - 2> /dev/null | jq -r '\'''.[] | "\\(.filename):\\(.location.row):\\(.location.column): error: \\(.message)"'\'''; } && run}\n'
                fi
          		}
            }

            hook global WinSetOption filetype=go %{
              evaluate-commands %sh{
                if command -v golangci-lint &> /dev/null; then
                	printf 'set-option window lintcmd %%{run() { cat "$1" | golangci-lint run --out-format=line-number --print-issued-lines=false "$kak_buffile" | awk '\'''BEGIN { FS=":" } { printf "%%s:%%d:%%d:%%s:%%s", $1, $2, $3, " error", $4 }'\'''; } && run}\n'
                fi
              }
            }

            hook global WinSetOption filetype=(javascript|typescript) %{
              evaluate-commands %sh{
                if command -v prettier &> /dev/null; then
                	printf 'set-option window formatcmd "prettier --stdin-filepath %s"\n' "$kak_buffile"
                fi
              }
            }

            hook global WinSetOption filetype=python %{
              evaluate-commands %sh{
                if command -v black &> /dev/null; then
                	printf 'set-option window formatcmd "black --quiet -"\n'
                fi
              }
            }

            hook global BufWritePre .* %{
              evaluate-commands %sh{
                if [ -n "$kak_opt_formatcmd" ]; then
                	printf 'format\n'
                fi
              }
            }

            hook global BufWritePost .* %{
              evaluate-commands %sh{
                if [ -n "$kak_opt_lintcmd" ]; then
                	printf 'lint\n'
                fi
              }
            }

            map global user f :fuzzy-edit<ret>
            map global user b :fuzzy-buffer<ret>
        '';
      };
  };
}

