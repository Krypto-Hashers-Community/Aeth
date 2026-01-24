# Makefile for Aeth
#
# This Makefile provides targets for building, installing, uninstalling,
# and managing dependencies for the Aeth Haskell project across various
# Linux distributions.
#
# Usage:
#   make help          - Display this help message.
#   make deps          - Install system-level dependencies (GHC, Cabal) for your distribution.
#   make build         - Build the Aeth project.
#   make install       - Install the Aeth executable to ~/.local/bin (or where cabal puts it).
#   make uninstall     - Uninstall the Aeth executable.
#   make run           - Run the Aeth project.
#   make clean         - Clean build artifacts.

.PHONY: all help deps build install uninstall run clean

# --- Configuration ---
PROJECT_NAME := Aeth
CABAL_EXE_NAME := $(strip Aeth)# As defined in aeth.cabal
INSTALL_DIR := $(strip $(HOME)/.local/bin)# Default installation directory for user binaries

# --- Distribution Detection ---
# Try to detect the distribution using lsb_release or /etc/os-release
DISTRO := $(shell lsb_release -si 2>/dev/null || cat /etc/os-release | grep -E "^ID=" | cut -d'=' -f2 | tr -d '"')
# Convert to lowercase for easier matching
DISTRO := $(shell echo $(DISTRO) | tr '[:upper:]' '[:lower:]')

# --- Dependency Commands ---
ifeq ($(DISTRO),ubuntu)
    DEPS_CMD := sudo apt-get update && sudo apt-get install -y build-essential ghc cabal-install libvty-dev
else ifeq ($(DISTRO),debian)
    DEPS_CMD := sudo apt-get update && sudo apt-get install -y build-essential ghc cabal-install libvty-dev
else ifeq ($(DISTRO),arch)
    DEPS_CMD := sudo pacman -Syu --noconfirm base-devel ghc cabal-install vty
else ifeq ($(DISTRO),fedora)
    # dnf check-update returns 100 if updates are available, so we ignore the exit code
    DEPS_CMD := sudo dnf check-update || true; sudo dnf install -y @development-tools ghc cabal-install ncurses-devel
else ifeq ($(DISTRO),centos)
    DEPS_CMD := sudo dnf check-update || true; sudo dnf install -y @development-tools ghc cabal-install ncurses-devel
else ifeq ($(DISTRO),gentoo)
    DEPS_CMD := sudo emerge --noreplace dev-lang/ghc dev-haskell/cabal-install sys-libs/ncurses
else ifeq ($(DISTRO),nixos)
    DEPS_CMD := @echo "NixOS detected. For NixOS, it's recommended to define a Nix expression for your project."; \
                echo "This Makefile will not attempt to install system dependencies via Nix."; \
                echo "You might need to use 'nix-shell -p ghc cabal-install' or create a default.nix file."
else
    DEPS_CMD := @echo "Unknown distribution: $(DISTRO). Please install 'ghc' and 'cabal-install' manually."; \
                echo "You may also need development headers for ncurses or vty if you encounter build issues."
endif

# --- Targets ---

all: build

help:
	@echo "Usage:"
	@echo "  make help          - Display this help message."
	@echo "  make deps          - Install system-level dependencies (GHC, Cabal) for your distribution."
	@echo "  make build         - Build the Aeth project."
	@echo "  make install       - Install the Aeth executable to $(INSTALL_DIR) (or where cabal puts it)."
	@echo "  make uninstall     - Uninstall the Aeth executable."
	@echo "  make run           - Run the Aeth project."
	@echo "  make clean         - Clean build artifacts."
	@echo ""
	@echo "Detected Distribution: $(DISTRO)"
	@echo "Note: The 'deps' target requires sudo for package installation."

deps:
	@echo "Installing system dependencies for $(DISTRO)..."
	@echo "This target requires sudo permissions."
	@echo "Please enter your password if prompted."
	$(DEPS_CMD)
	@echo "Updating cabal package list..."
	cabal update
	@echo "Installing vty-unix..."
	cabal install --lib vty-unix

build:
	@echo "Building $(PROJECT_NAME)..."
	cabal build all

install: build
	@echo "Installing $(PROJECT_NAME) to cabal's default location (likely ~/.cabal/bin or similar)..."
	mkdir -p "$(INSTALL_DIR)"
	@echo "DEBUG: INSTALL_DIR='$(INSTALL_DIR)'"
	@echo "DEBUG: CABAL_EXE_NAME='$(CABAL_EXE_NAME)'"
	cp $(shell find dist-newstyle -name $(CABAL_EXE_NAME) -type f) "$(INSTALL_DIR)/$(CABAL_EXE_NAME)"

uninstall:
	@echo "Attempting to uninstall $(PROJECT_NAME)..."
	@echo "Note: Cabal does not have a direct 'uninstall' command."
	@echo "This target will try to remove the installed executable from common locations."
	@echo "You may also need to manually remove the package from your cabal store if you no longer need it."
	-rm -f $(INSTALL_DIR)/$(CABAL_EXE_NAME)
	-rm -f $(HOME)/.cabal/bin/$(CABAL_EXE_NAME) # Cabal's default for user installs
	@echo "If $(PROJECT_NAME) was installed system-wide (e.g., /usr/local/bin), you might need to remove it manually with 'sudo rm /usr/local/bin/$(CABAL_EXE_NAME)'."

run: build
	@echo "Running $(PROJECT_NAME)..."
	cabal run $(CABAL_EXE_NAME) -- $(ARGS)

clean:
	@echo "Cleaning build artifacts for $(PROJECT_NAME)..."
	cabal clean
