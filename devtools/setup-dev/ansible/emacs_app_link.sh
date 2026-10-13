#!/bin/sh
# Point /Applications/Emacs.app at $(brew --prefix emacs-plus)/Emacs.app.
#
# The default creates a Finder alias, then a symlink if Finder cannot.
# Pass "symlink" to skip Finder. Prints "unchanged" or "created".
# Exit 2 means the destination directory is not writable. A real
# Emacs.app bundle is left in place.
#
# EMACS_APP_LINK_DIR overrides /Applications in tests.
set -eu

mode="${1:-alias}"
dest_dir="${EMACS_APP_LINK_DIR:-/Applications}"
dest_dir="${dest_dir%/}"
dest="${dest_dir}/Emacs.app"

if ! prefix=$(brew --prefix emacs-plus 2>/dev/null); then
    printf '%s\n' unchanged
    exit 0
fi
src="${prefix%/}/Emacs.app"
if [ ! -d "$src" ]; then
    printf '%s\n' unchanged
    exit 0
fi

# macOS stat does not follow symlinks. -L compares the app both paths open.
same_app() {
    candidate="${1%/}"
    [ -n "$candidate" ] || return 1
    [ "$candidate" = "$src" ] && return 0
    [ -e "$candidate" ] || return 1
    [ "$(stat -L -f '%d:%i' "$candidate")" = "$(stat -L -f '%d:%i' "$src")" ]
}

# Print the path dest points at. Empty means dest is absent, or symlink
# mode will replace a non-directory. Exit 1 for a real bundle or a file
# that is not a Finder alias.
link_target() {
    if [ -L "$dest" ]; then
        got=$(readlink "$dest" || true)
        case "$got" in
            "" | /*) ;;
            *) got="${dest_dir}/${got}" ;;
        esac
        printf '%s\n' "$got"
        return 0
    fi
    if [ -d "$dest" ]; then
        printf '%s\n' "${dest} is a real bundle, not an alias to ${src}" >&2
        return 1
    fi
    if [ ! -e "$dest" ] || [ "$mode" = "symlink" ]; then
        return 0
    fi
    # Finder returns an application file for Emacs.app. POSIX path of that
    # file fails, so cast the original item back to an alias first.
    got=$(osascript -e "tell application \"Finder\" to POSIX path of ((original item of (POSIX file \"${dest}\" as alias)) as alias)" 2>/dev/null || true)
    if [ -z "$got" ]; then
        printf '%s\n' "${dest} exists and is not a Finder alias to ${src}" >&2
        return 1
    fi
    printf '%s\n' "$got"
}

if ! target=$(link_target); then
    exit 1
fi
if same_app "$target"; then
    printf '%s\n' unchanged
    exit 0
fi
if [ ! -w "$dest_dir" ]; then
    exit 2
fi
rm -f "$dest"

if [ "$mode" != "symlink" ] && osascript -e "tell application \"Finder\" to make alias file to posix file \"${src}\" at posix file \"${dest_dir}\" with properties {name:\"Emacs.app\"}" >/dev/null 2>&1; then
    printf '%s\n' created
    exit 0
fi
if ln -s "$src" "$dest"; then
    printf '%s\n' created
    exit 0
fi
printf '%s\n' "could not create ${dest} pointing at ${src}" >&2
exit 1
