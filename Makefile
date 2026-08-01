# Target directory defaults to $HOME
TARGET ?= $(HOME)
STOW_FLAGS := --verbose=1 --target=$(TARGET) --no-folding

# Package groups by environment
COMMON_PKGS := asdf bin git helix kitty lazygit nvim shells ssh starship tmux wezterm
LINUX_PKGS  := hyprland noctalia
MACOS_PKGS  :=

# XDG path to TPM binaries
TPM_DIR := $(HOME)/.config/tmux/plugins/tpm
TPM_BIN := $(TPM_DIR)/bin

.PHONY: adopt all common dry-run help linux macos tpm-clean tpm-init tpm-install tpm-update unstow

## Help: Display available commands
help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Targets:"
	@echo "  adopt       - Adopt target files into repo on fresh setups"
	@echo "  all         - Stow common + auto-detected OS packages"
	@echo "  common      - Stow cross-platform packages"
	@echo "  dry-run     - Simulate stowing without making changes"
	@echo "  linux       - Stow Linux specific packages"
	@echo "  macos       - Stow macOS specific packages"
	@echo "  tpm-clean   - Remove plugins no longer present in tmux.conf"
	@echo "  tpm-install - Install newly added plugins from tmux.conf"
	@echo "  tpm-init    - Initialize Git submodules"
	@echo "  tpm-update  - Update all currently installed plugins"
	@echo "  unstow      - Unstow all packages"

## Detect OS and run appropriate target
all: common
ifeq ($(shell uname -s), Linux)
	@echo "Detected Linux, applying Linux packages..."
	$(MAKE) linux
else ifeq ($(shell uname -s), Darwin)
	@echo "Detected macOS, applying macOS packages..."
	$(MAKE) macos
endif

## Package groups
common:
	stow $(STOW_FLAGS) $(COMMON_PKGS)

linux:
	stow $(STOW_FLAGS) $(LINUX_PKGS)

macos:
	stow $(STOW_FLAGS) $(MACOS_PKGS)

## Dry Run (Simulation)
dry-run:
	stow -n $(STOW_FLAGS) $(COMMON_PKGS)

## Unstow (Clean up symlinks for the current OS)
unstow:
	stow -D $(STOW_FLAGS) $(COMMON_PKGS)
ifeq ($(shell uname -s), Linux)
	@echo "Unstowing Linux packages..."
	stow -D $(STOW_FLAGS) $(LINUX_PKGS)
else ifeq ($(shell uname -s), Darwin)
	@echo "Unstowing macOS packages..."
	stow -D $(STOW_FLAGS) $(MACOS_PKGS)
endif

## Adopt (Overwrite repo files with target machine files during initial setup)
adopt:
	stow --adopt $(STOW_FLAGS) $(COMMON_PKGS)
	git status

## TPM Targets

# 1. Initialize TPM if not present, then install plugins
tpm-init:
	@if [ ! -d "$(TPM_DIR)" ]; then \
		echo "Initializing TPM..."; \
		git submodule update --init --recursive; \
	fi
	@$(MAKE) tpm-install

# 2. Install newly added plugins from tmux.conf
tpm-install:
	@if [ -x "$(TPM_BIN)/install_plugins" ]; then \
		echo "Installing TPM plugins..."; \
		$(TPM_BIN)/install_plugins; \
	else \
		echo "TPM not found. Run 'make tpm-init' first."; \
	fi

# 3. Update all currently installed plugins
tpm-update:
	@if [ -x "$(TPM_BIN)/update_plugins" ]; then \
		echo "Updating TPM plugins..."; \
		$(TPM_BIN)/update_plugins all; \
	else \
		echo "TPM not found. Run 'make tpm-init' first."; \
	fi

# 4. Remove plugins no longer present in tmux.conf
tpm-clean:
	@if [ -x "$(TPM_BIN)/clean_plugins" ]; then \
		echo "Removing unused TPM plugins..."; \
		$(TPM_BIN)/clean_plugins; \
	else \
		echo "TPM not found. Run 'make tpm-init' first."; \
	fi
