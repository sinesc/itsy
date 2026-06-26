#!/bin/sh
# Installs the opt-in post-commit hook that saves a benchmark report after every commit.
#
#   bash scripts/install-bench-hook.sh
#
# The hook runs a full benchmark (slow), so it is not installed by default.

set -e

repo_root=$(git rev-parse --show-toplevel 2>/dev/null) || {
    echo "error: not inside a git repository" >&2
    exit 1
}

src="$repo_root/scripts/post-commit"
hooks_dir=$(git rev-parse --git-path hooks)
dst="$hooks_dir/post-commit"

if [ ! -f "$src" ]; then
    echo "error: $src not found" >&2
    exit 1
fi

if [ -e "$dst" ]; then
    echo "warning: $dst already exists; backing it up to $dst.bak"
    mv "$dst" "$dst.bak"
fi

mkdir -p "$hooks_dir"
cp "$src" "$dst"
chmod +x "$dst"
echo "installed post-commit hook -> $dst"
echo "a benchmark report will now be saved to itsy/bench/reports/ after each commit."
echo "to uninstall: rm \"$dst\""
