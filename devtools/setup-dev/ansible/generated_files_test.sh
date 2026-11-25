#!/usr/bin/env bash
# Verify checked-in Ansible generated files match fresh output from generate_packages.
set -euo pipefail

gen_bin="$1"
case "$gen_bin" in
    /*) ;;
    *) gen_bin="$PWD/$gen_bin" ;;
esac

tmp_root="${TEST_TMPDIR:-$(mktemp -d)}"
work_root="$tmp_root/ansible_gen_check"
rm -rf "$work_root"
orig_dir="$work_root/orig"
gen_dir="$work_root/gen"
mkdir -p "$orig_dir" "$gen_dir"

src_dir="devtools/setup-dev/ansible"
cp -L "$src_dir/BUILD.bazel" "$src_dir/README.org" "$orig_dir/"
for f in "$src_dir"/*.yml "$src_dir"/qubes-templatevm-*.sh; do
    [ -e "$f" ] || continue
    cp -L "$f" "$orig_dir/"
done

cp -R "$orig_dir"/. "$gen_dir/"
chmod -R u+w "$gen_dir"

(cd "$gen_dir" && "$gen_bin")

if ! diff -ru "$orig_dir" "$gen_dir"; then
    echo "Error: Ansible generated files are out of date. Run 'make generate-ansible' to regenerate." >&2
    exit 1
fi
