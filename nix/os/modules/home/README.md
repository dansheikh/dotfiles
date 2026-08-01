# `modules/home/`

Shared Home Manager modules — configuration that is genuinely common across
multiple hosts and users. Nothing lives here until it is needed by more than
one host; per-host HM config belongs in `hosts/<hostname>/home/`.

---

## What belongs here

- Program config shared verbatim across all hosts (e.g. a common `git` identity
  base, a shared `programs.direnv` setup, universal shell aliases)
- Shared theme primitives that every host's `home.nix` builds on (e.g. a common
  Catppuccin color set referenced by multiple HM modules)
- Cross-host font preferences if they ever diverge from the system-level
  `modules/fonts.nix`

## What does NOT belong here

- Anything host-specific: compositor config (Hyprland, Niri), display scaling,
  monitor layout, machine-specific autostart
- Anything managed by stow under `$XDG_CONFIG_HOME`: shell config (zsh, fish,
  nushell), terminal config (kitty, wezterm), editor config (neovim, emacs)
- The HM entrypoint (`home.nix`) — that always lives in `hosts/<hostname>/home/`

---

## How to add a shared module

1. Create the module file in this directory, e.g. `modules/home/git.nix`:

   ```nix
   { ... }:
   {
     programs.git = {
       enable    = true;
       userName  = "Dan Sheikh";
       userEmail = "dan@example.com";
     };
   }
   ```

2. Import it in every relevant host's `home.nix`
   (`hosts/<hostname>/home/home.nix`):

   ```nix
   {
     imports = [
       ../../../../modules/home/git.nix   # shared
       ./fonts.nix                        # host-local
       ./noctalia.nix
       ./hyprland.nix
       ./niri.nix
     ];
     # ...
   }
   ```

3. If the module needs `inputs`, `pkgs-unstable`, or other `extraSpecialArgs`,
   those are already available in every HM module via the `extraSpecialArgs`
   declaration in `flake.nix` — no additional wiring required.

---

## Naming conventions

| File | Purpose |
|---|---|
| `git.nix` | `programs.git` shared base config |
| `direnv.nix` | `programs.direnv` shared config |
| `theme.nix` | Shared color/theme primitives |
| `fonts.nix` | Shared user-level font preferences (if diverging from system) |

Keep modules focused — one concern per file, named after the program or concept
it configures.
