#!/bin/bash

# Script to install tree-sitter grammars for React/TypeScript development

EMACS_TREE_SITTER_DIR="$HOME/.emacs.d/tree-sitter"
mkdir -p "$EMACS_TREE_SITTER_DIR"

echo "Installing tree-sitter grammars for React/TypeScript development..."

# Install tree-sitter CLI if not available
if ! command -v tree-sitter &> /dev/null; then
    echo "Installing tree-sitter CLI..."
    if command -v brew &> /dev/null; then
        brew install tree-sitter
    else
        echo "Please install tree-sitter CLI manually:"
        echo "  - macOS: brew install tree-sitter"
        echo "  - Or download from: https://github.com/tree-sitter/tree-sitter/releases"
        exit 1
    fi
fi

# Clone and build common grammars
cd "$EMACS_TREE_SITTER_DIR" || exit 1

# JavaScript/TypeScript grammars
grammars=(
    "https://github.com/tree-sitter/tree-sitter-javascript"
    "https://github.com/tree-sitter/tree-sitter-typescript"
    "https://github.com/tree-sitter/tree-sitter-html"
    "https://github.com/tree-sitter/tree-sitter-css"
    "https://github.com/Wilfred/tree-sitter-xml"
)

for repo in "${grammars[@]}"; do
    name=$(basename "$repo")
    if [ ! -d "$name" ]; then
        echo "Cloning $name..."
        git clone "$repo" "$name"
        cd "$name" || continue
        if [ -f "Makefile" ]; then
            echo "Building $name..."
            make
        elif [ -f "package.json" ]; then
            echo "Building $name with npm..."
            npm install
            npm run build
        fi
        cd ..
    else
        echo "$name already exists, skipping..."
    fi
done

# Handle TypeScript specifically (it has separate TSX parser)
if [ -d "tree-sitter-typescript" ]; then
    cd tree-sitter-typescript || exit 1
    # Build both typescript and tsx parsers
    if [ -f "Makefile" ]; then
        make
    fi
    cd ../..
fi

echo "Tree-sitter grammars installation completed!"
echo "Grammars installed in: $EMACS_TREE_SITTER_DIR"