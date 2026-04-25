# Nushell Configuration

Idiomatic two-file Nushell config ported from `zsh_improved`. Nix-compatible.

## Structure

```
~/.config/nushell/
├── env.nu          # XDG, PATH, tool env vars, starship prompt init
├── config.nu       # Everything else: settings, completions, keybindings,
│                   # aliases, functions, integrations, vendor sources
└── vendor/         # Generated init files (create with commands below)
    ├── zoxide.nu   # zoxide init nushell | save -f vendor/zoxide.nu
    └── atuin.nu    # atuin init nu       | save -f vendor/atuin.nu
```

Two files. No numbered modules, no generation script, no `source` machinery
within your own config. This is the standard Nu convention.

## Install

```sh
# 1. Back up existing config
[ -d ~/.config/nushell ] && mv ~/.config/nushell ~/.config/nushell.bak

# 2. Copy files
mkdir -p ~/.config/nushell/vendor
cp env.nu config.nu ~/.config/nushell/

# 3. Install core tools
#   macOS:
brew install nushell carapace fzf fd bat eza starship direnv
#   Nix:
nix-env -iA nixpkgs.nushell nixpkgs.carapace nixpkgs.fzf nixpkgs.fd \
           nixpkgs.bat nixpkgs.eza nixpkgs.starship nixpkgs.direnv

# 4. Generate vendor files (if you use zoxide and/or atuin)
zoxide init nushell | save -f ~/.config/nushell/vendor/zoxide.nu
atuin init nu       | save -f ~/.config/nushell/vendor/atuin.nu

# 5. Uncomment the `source vendor/zoxide.nu` and/or `source vendor/atuin.nu`
#    lines at the bottom of config.nu.
```

Re-run the `save` commands after upgrading zoxide or atuin.

## Nix compatibility

### PATH

`env.nu` covers all standard Nix profile locations:

| Profile type | Path |
|---|---|
| Single-user (`nix-env`) | `~/.nix-profile/bin` |
| Flakes / `nix profile` | `~/.local/state/nix/profiles/profile/bin` |
| Multi-user default | `/nix/var/nix/profiles/default/bin` |
| NixOS system packages | `/run/current-system/sw/bin` |

When `$NIX_PROFILES` is set (NixOS, home-manager, multi-profile setups),
`env.nu` splits it and adds each profile's `bin` to PATH automatically.
This is the authoritative source for active profile stacks and handles
system + user + home-manager combinations without hardcoded paths.

All candidates are guarded by `path exists` — the config is portable
across non-Nix machines without modification.

### LOCALE_ARCHIVE

On NixOS, locale data lives in the Nix store. `env.nu` detects the presence
of `$NIX_PROFILES` on Linux and sets `LOCALE_ARCHIVE` from the Nix store
glibc-locales path, preventing locale warnings from tools like `date` and
`grep`.

### home-manager

If Nu is managed by home-manager, `env.nu` and `config.nu` may be generated
and marked read-only. Do not edit them directly. Instead:

```nix
programs.nushell = {
  enable = true;
  extraEnv   = builtins.readFile ./env.nu;
  extraConfig = builtins.readFile ./config.nu;
};
```

### nix develop / nix-shell

Nix dev environments spawn bash by default. To use Nu:

```sh
nix develop --command nu        # one-off
```

Or in your flake's `devShell`:

```nix
devShells.default = pkgs.mkShell {
  shellHook = "exec nu";
};
```

With direnv and `use flake` in `.envrc`, the `pre_prompt` hook in `config.nu`
injects all flake env vars automatically on each prompt — no extra steps.

## Tool requirements

| Tool | Purpose | Nix attribute |
|------|---------|---------------|
| `carapace` | Tab completion (500+ commands) | `nixpkgs.carapace` |
| `fzf` | Interactive pickers | `nixpkgs.fzf` |
| `fd` | Fast file finder | `nixpkgs.fd` |
| `bat` | Syntax-highlighted previews | `nixpkgs.bat` |
| `eza` | Modern `ls` | `nixpkgs.eza` |
| `starship` | Prompt | `nixpkgs.starship` |
| `direnv` | Per-directory env vars | `nixpkgs.direnv` |

Optional: `zoxide`, `atuin`, `mise`, `ripgrep`, `delta`, `nerdfetch`

## Design notes

### Two files only
The multi-file `conf.d/` structure was a Zsh convention for managing a shell
that has no module system. Nu doesn't need it — `config.nu` is readable at
a few hundred lines, and splitting introduces `source` parse-time constraints,
circular import risk, and load-order fragility with no benefit.

### vendor/ — why only zoxide and atuin
Tools that emit Nu source code (mise, zoxide, atuin, pyenv, rbenv) cannot
be initialised inline because they define custom commands, and `source`
requires parse-time constant paths. However:

- **mise / pyenv / rbenv**: only need PATH shims and a small number of env
  vars — both handled in `env.nu` without any generated file.
- **fnm**: provides a `--json` flag that returns clean structured output,
  loadable directly with `from json | load-env`.
- **zoxide / atuin**: define Nu commands (`z`, `zi`, Ctrl-R override) that
  cannot be expressed as env vars. Vendor files are genuinely necessary.

### `source` is a parse-time keyword
`source` paths must be literals or `const` variables resolved at parse time.
They cannot appear inside `if` blocks, functions, or loops. All `source`
calls in this config are top-level literals. This is why vendor files exist
and why `nureload` (which tried to `source` from inside a `def`) was
replaced with `alias reload = exec nu`.

### `reload` vs `nureload`
`exec nu` replaces the current process with a fresh Nu session, re-reading
all config from scratch. It is the correct reload mechanism. `source
($nu.config-path)` inside a `def` causes a circular import error.

### `merge deep` throughout
All `$env.config` mutations use `merge deep` so nested records (hooks, menus,
keybindings, completions) compose correctly across multiple calls rather than
overwriting each other.

### Aliases vs `def` commands
Nu aliases cannot take parameters. Commands requiring arguments (`hg`,
`psgrep`, `killport`, `findtext`, etc.) are `def` commands. `source` and
`exec` are parser keywords — `reload` aliases `exec nu` directly, which Nu
permits.

### Nu builtins shadowed with `^`
`rm`, `cp`, `mv`, `mkdir`, and `ls` are Nu builtins. Aliases that pass flags
use `^` to invoke the system binary instead (e.g. `alias rm = ^rm -iv`).

### Keybinding changes from Zsh
| Zsh | Nu | Reason |
|-----|----|--------|
| `Ctrl-T` | `Ctrl-T` | file picker — unchanged |
| `Alt-C` | `Alt-C` | dir picker — unchanged |
| `^Gb` | `Ctrl-B` | branch switcher — no chained bindings in Nu |
| `^Gl` | `Ctrl-G` | git log — frees Ctrl-L for clear screen |
| `^K` | `Ctrl-K` | kill — unchanged |
| `^X^E` | `Alt-E` | edit in editor — Nu `openEditor` event |
| `Ctrl-L` | `Ctrl-L` | clear screen — now unambiguous |
