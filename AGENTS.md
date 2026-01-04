# AGENTS.md

This file provides guidance to Qoder (qoder.com) when working with code in this repository.

## Project Overview

This is an Emacs configuration directory (`.emacs.d`) optimized for C/C++ development with advanced features including LSP support, completion, and project navigation.

## Architecture

- **Package Management**: Uses `straight.el` with `use-package` for declarative configuration
- **Core Configuration**: Split between `early-init.el` (startup optimizations) and `init.el` (main configuration)
- **Language Support**: C/C++ with `eglot` LSP client and `clangd` server
- **Completion**: `corfu` for completion UI with `orderless` for flexible matching
- **Project Navigation**: Built-in project.el with `find-file-in-project` extension

## Key Features

- C/C++ development with Eglot + clangd
- Advanced completion with Corfu
- Project navigation and file switching
- Tree-sitter syntax highlighting
- Flymake for on-the-fly syntax checking

## Development Commands

Since this is an Emacs configuration:

- **Start Emacs**: `emacs` or `emacs -q -l init.el` to load this configuration
- **Update packages**: `M-x straight-pull-all` within Emacs
- **Install new packages**: `M-x straight-use-package` or add to init.el with use-package
- **Debug configuration**: `M-x toggle-debug-on-error` for troubleshooting

## Key Bindings

- `C-c o` - Switch between header/source files
- `C-c C-o` - Enhanced header/source switching with project root
- `C-c f` - Format buffer or region using LSP
- `M-.` - Jump to definition (xref)
- `M-,` - Return from definition (xref)
- `M-n` - Next Flymake error
- `M-p` - Previous Flymake error
- `C-'` - Toggle imenu-list sidebar
- `TAB` - Navigate completion candidates in Corfu
- `S-TAB` - Previous completion candidate in Corfu

## Configuration Structure

- `early-init.el` - Startup optimizations (GC settings, disable startup screen)
- `init.el` - Main configuration with sections for:
  - Package management (straight.el + use-package)
  - Basic editing experience (indentation, backup settings)
  - Completion (Corfu, orderless)
  - C/C++ specific setup
  - LSP configuration (Eglot with clangd)
  - Project navigation tools