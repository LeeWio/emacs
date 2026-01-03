# AGENTS.md

This file provides guidance to Qoder (qoder.com) when working with code in this repository.

## Codebase Overview

This is a high-performance, modular Emacs configuration focused on performance optimization with a Catppuccin Mocha theme and transparent background. The configuration uses a modular architecture with separate modules for different functionality.

## Architecture

### Directory Structure
```
.
├── early-init.el          # Pre-startup initialization (GUI disable, etc.)
├── init.el                # Main configuration entry point
├── config/                # Personal configuration files
│   └── custom.el          # Custom settings
└── modules/               # Feature modules
    ├── core/              # Core configuration
    ├── ui/                # UI configuration  
    ├── lang/              # Language support
    ├── tools/             # Tool configuration
    ├── perf/              # Performance optimization
    ├── lsp/               # LSP and completion system
    ├── lsp/lang/          # Language-specific LSP config
    └── keybindings/       # Keybinding configuration
```

### Module Structure
- **core/**: Core Emacs settings (encoding, backup, history, line numbers, highlighting, smooth scrolling)
- **ui/**: User interface (Catppuccin theme, transparency, VS Code-style mode line)
- **lang/**: Programming language support configuration
- **tools/**: Development tools (which-key, git integration with Magit)
- **perf/**: Performance tuning and optimization settings
- **lsp/**: LSP client (currently using eglot) and completion system (corfu)
- **keybindings/**: Keyboard shortcuts and keymap configuration

### Main Entry Points
- `init.el`: Loads all modules in sequence
- Each module has an `*-init.el` file that requires the individual components
- Package management via `use-package` with MELPA/ELPA sources

## Development Commands

### Testing Configuration
- To test configuration changes: Start Emacs normally - configuration loads automatically
- To test specific modules: `(require 'module-name)` in Emacs eval
- To test configuration syntax: `emacs --batch --eval "(load-file \"init.el\")"`
- To start Emacs in terminal mode: `emacs -nw`

### Performance Testing
- Performance metrics are built into the configuration via `perf-init.el`
- GC threshold and process output limits are optimized for performance
- Large file handling with automatic feature disabling

### LSP/Completion System
- Current setup: eglot (LSP client) + corfu (completion) + corfu-popupinfo + orderless (filtering) + yasnippet + nerd-icons + consult-eglot + eglot-inlay-hints (built-in) + eglot-booster (optional)
- Language servers configured in `modules/lsp/lang/` (currently C/C++, Java, TypeScript/JavaScript with corresponding language servers)
- Performance settings in `modules/lsp/lsp-perf.el`
- Icons for completion provided by nerd-icons with safe loading
- Mode line icons also use nerd-icons

### Development and Debugging
- To check package installation status: `M-x list-packages`
- To reload configuration: `M-x eval-buffer` on the config file you're editing
- To check configuration loading time: Performance metrics are displayed during startup
- To debug issues: Check `*Messages*` buffer (`C-h e`)
- To enable debug mode: Add `(setq debug-on-error t)` temporarily to init.el

### Common Package Management
- To install a new package: `(package-install 'package-name)` or use `use-package` declaration
- To update packages: `M-x package-list-packages` then `U` to mark all for upgrade, `x` to execute
- To check installed packages: `M-x package-list-packages`