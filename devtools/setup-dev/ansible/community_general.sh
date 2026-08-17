#!/bin/sh
# community.general collection install policy helpers. Sourced by ensure.sh.
#
# Policy:
# - macOS / Termux: latest from Galaxy; --force after an ansible-core upgrade.
# - Debian/Ubuntu (apt): use the collection shipped with the *active* ansible
#   unless ansible-core was just upgraded via pip. If there is no bundled
#   collection, or after an upgrade, install a Galaxy version pinned to core.
# - Other Linux: latest from Galaxy; --force after an upgrade.

# Galaxy spec compatible with a given ansible-core version (e.g. 2.16.3).
community_general_spec_for_core() {
    case "$1" in
        2.15.*) echo 'community.general:>=10.0.0,<11.0.0' ;;
        2.16.*) echo 'community.general:>=11.0.0,<12.0.0' ;;
        2.17.*) echo 'community.general:>=12.0.0,<13.0.0' ;;
        2.18.*) echo 'community.general:>=13.0.0,<14.0.0' ;;
        *) echo 'community.general' ;;
    esac
}

# True if the collection lives next to the active ansible python module.
# $1 = "ansible python module location" from `ansible --version`.
community_general_has_bundled() {
    [ -n "$1" ] && [ -d "$1/../ansible_collections/community/general" ]
}

# Decide how to obtain community.general.
# $1 OS (uname -s), $2 termux (0/1), $3 upgraded (0/1),
# $4 has_apt (0/1), $5 has_bundled (0/1), $6 ansible-core version.
# Prints: use_bundled | install SPEC | install SPEC --force
community_general_plan() {
    _cgp_os="$1"
    _cgp_termux="$2"
    _cgp_upgraded="$3"
    _cgp_has_apt="$4"
    _cgp_has_bundled="$5"
    _cgp_core="$6"
    _cgp_force=""
    if [ "$_cgp_upgraded" = 1 ]; then
        _cgp_force=" --force"
    fi

    if [ "$_cgp_termux" = 1 ] || [ "$_cgp_os" = "Darwin" ]; then
        echo "install community.general$_cgp_force"
        return
    fi

    if [ "$_cgp_has_apt" = 1 ]; then
        if [ "$_cgp_upgraded" = 1 ]; then
            echo "install $(community_general_spec_for_core "$_cgp_core") --force"
            return
        fi
        if [ "$_cgp_has_bundled" = 1 ]; then
            echo "use_bundled"
            return
        fi
        echo "install $(community_general_spec_for_core "$_cgp_core") --force"
        return
    fi

    echo "install community.general$_cgp_force"
}

# True when a user Galaxy copy would shadow the bundled collection on
# Debian/Ubuntu. After a pip upgrade, return false so the caller installs
# instead of deleting Galaxy.
# $1 has_apt, $2 termux, $3 OS, $4 has_bundled, $5 has_user_galaxy, $6 upgraded.
community_general_needs_repair() {
    [ "$6" = 1 ] && return 1
    [ "$1" = 1 ] || return 1
    [ "$2" = 1 ] && return 1
    [ "$3" = "Darwin" ] && return 1
    [ "$4" = 1 ] || return 1
    [ "$5" = 1 ]
}
