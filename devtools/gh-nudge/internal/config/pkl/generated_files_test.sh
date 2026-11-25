#!/usr/bin/env bash
# Verify checked-in Pkl-generated Go files match fresh output from pkl run @pkl-go/gen.pkl.
set -euo pipefail

pkl_bin="$1"
pkl_project="$2"
pkl_project_deps="$3"
config_pkl="$4"
shift 4

# @pkl_project//:packages outputs both the cache directory and its helper script;
# locate the directory entry.
cache_dir=""
for entry in "$@"; do
    if [ -d "$entry" ]; then
        cache_dir="$entry"
        break
    fi
done
if [ -z "$cache_dir" ]; then
    echo "Error: could not find Pkl package cache directory in args: $*" >&2
    exit 1
fi

case "$pkl_bin" in
    /*) ;;
    *) pkl_bin="$PWD/$pkl_bin" ;;
esac
case "$cache_dir" in
    /*) ;;
    *) cache_dir="$PWD/$cache_dir" ;;
esac

tmp_root="${TEST_TMPDIR:-$(mktemp -d)}"
work_dir="$tmp_root/pkl_gen_check"
rm -rf "$work_dir"
project_dir="$work_dir/project"
orig_dir="$work_dir/orig"
gen_dir="$work_dir/gen"
mkdir -p "$project_dir" "$orig_dir" "$gen_dir"

cp -L "$pkl_project" "$project_dir/PklProject"
cp -L "$pkl_project_deps" "$project_dir/PklProject.deps.json"
cp -L "$config_pkl" "$project_dir/Config.pkl"

for f in devtools/gh-nudge/internal/config/pkl/*.pkl.go; do
    cp -L "$f" "$orig_dir/"
done

"$pkl_bin" run @pkl-go/gen.pkl \
    --cache-dir "$cache_dir" \
    --project-dir "$project_dir" \
    --base-path github.com/jaeyeom/experimental/devtools/gh-nudge/internal/config/pkl \
    --output-path "$gen_dir" \
    "$project_dir/Config.pkl"

if ! diff -ru "$orig_dir" "$gen_dir"; then
    echo "Error: Pkl-generated Go files are out of date. Run 'make generate-pkl' to regenerate." >&2
    exit 1
fi
