p'/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'.exists() and source-bash /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

xontrib load abbrevs

$CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense'
$COMPLETIONS_CONFIRM=True
$COMPLETIONS_DISPLAY = 'readline'
$DOTNET_TOOLS = "$HOME/.dotnet/tools"
$DOTNET_CLI_TELEMETRY_OPTOUT = 1
$EDITOR = 'nvim'
$GOPATH = $(go env GOPATH).strip()
$LANG = 'en_US.UTF-8'
$LC_TYPE = 'en_US.UTF-8'
$XDG_CACHE_HOME = pf"$HOME/.cache"
$XDG_CONFIG_HOME = pf"$HOME/.config"
$XDG_DATA_HOME = pf"$HOME/.local/share"
$XDG_STATE_HOME = pf"$HOME/.local/state"
$MPLCONFIGDIR = pf"$XDG_CONFIG_HOME/matplotlib"
$PNPM_HOME = pf"$XDG_DATA_HOME/pnpm"
$POETRY_CACHE_DIR = pf"$XDG_CACHE_HOME/pypoetry"
$POETRY_CONFIG_DIR = pf"$XDG_CONFIG_HOME/pypoetry"
$POETRY_DATA_DIR = pf"$XDG_DATA_HOME/pypoetry"

$PATH.add("$PNPM_HOME", front=True)
$PATH.add("$GOPATH/bin", front=True)
$PATH.add("/usr/local/bin", front=True)
$PATH.add("$HOME/.local/bin", front=True)
$PATH.add("$DOTNET_TOOLS")

abbrevs['..'] = 'cd ..'
abbrevs['...'] = 'cd ../..'
abbrevs['giad'] = 'git add'
abbrevs['gich'] = 'git checkout'
abbrevs['gicl'] = 'git clone'
abbrevs['gico'] = 'git commit'
abbrevs['gidi'] = 'git diff'
abbrevs['gihi'] = 'git history'
abbrevs['gist'] = 'git status'
abbrevs['ktl'] = 'kubectl'
abbrevs['ls'] = 'lsd --group-dirs first -la'
abbrevs['rg'] = 'rg -S'
abbrevs['tf'] = 'terraform'

aliases['biff'] = 'git diff --name-only --relative --diff-filter=d | xargs bat --diff'
aliases['cp'] = 'cp -iv'
aliases['fb'] = 'fd --exec-batch'
aliases['mkdir'] = 'mkdir -pv'
aliases['mv'] = 'mv -iv'
aliases['rm'] = 'rm -iv'

!(command -v tmux &> /dev/null) and 'TMUX' not in ${...} and exec tmux new

execx($(oh-my-posh init xonsh --config $XDG_CONFIG_HOME/oh-my-posh/oh-my-posh.toml))
exec($(carapace _carapace))
