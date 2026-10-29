#!/bin/bash
# Remove regenerable caches, logs and trash entries to keep disk usage low.
set -uo pipefail


# -----------------------------------------------------------------------------
# ------------------------------- Constants -----------------------------------
# -----------------------------------------------------------------------------
OPENCODE_AGE_DAYS=14
TMP_AGE_DAYS=7
TRASH_AGE_DAYS=30
CACHE_AGE_DAYS=30
BROWSER_AGE_DAYS=60

OC_SHELL="$HOME/.local/share/opencode/shell"
OC_LOG="$HOME/.local/share/opencode/log"
TMP_WORK="/tmp/opencode"
TRASH_FILES="$HOME/.local/share/Trash/files"
TRASH_INFO="$HOME/.local/share/Trash/info"
POETRY_DIR="$HOME/.cache/pypoetry"
PIP_DIR="$HOME/.cache/pip"
NPM_DIR="$HOME/.npm/_cacache"
ARDUINO_STAGING="$HOME/.arduino15/staging"
THUMB_DIR="$HOME/.cache/thumbnails"
BROWSER_DIRS=(
    "$HOME/.cache/ms-playwright"
    "$HOME/.cache/ms-playwright-mcp"
    "$HOME/.cache/puppeteer"
    "$HOME/.cache/selenium"
)

DRY_RUN=0
DELETE_ALL=0


# -----------------------------------------------------------------------------
# -------------------------------- Interface ----------------------------------
# -----------------------------------------------------------------------------
usage() {
    echo "usage: $(basename "$0") [--dry-run] [--all]"
    echo "  --dry-run  show targets without deleting"
    echo "  --all      ignore age limits (browser caches stay age based)"
}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------------ Implementation -------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# ---------------------------------- Main -------------------------------------
# -----------------------------------------------------------------------------
main() {
    parse_args "$@"
    avail_before=$(available_kb)
    echo "available before: $(human_kb "$avail_before")"
    remove_tmp_work
    remove_opencode_logs
    remove_trash_entries
    remove_package_caches
    prune_uv_cache
    remove_thumbnail_entries
    remove_old_browsers
    remove_empty_dirs "$OC_SHELL"
    remove_empty_dirs "$OC_LOG"
    avail_after=$(available_kb)
    echo "available after:  $(human_kb "$avail_after")"
    echo "freed:            $(human_kb $((avail_after - avail_before)))"
}

parse_args() {
    for arg in "$@"; do
        case "$arg" in
            --dry-run) DRY_RUN=1 ;;
            --all) DELETE_ALL=1 ;;
            -h|--help) usage; exit 0 ;;
            *) echo "unknown option: $arg" >&2; usage; exit 1 ;;
        esac
    done
}


# -----------------------------------------------------------------------------
# -------------------------------- Targets ------------------------------------
# -----------------------------------------------------------------------------
remove_tmp_work() {
    clean_old_entries "$TMP_WORK" "$TMP_AGE_DAYS"
}

remove_opencode_logs() {
    clean_old_entries "$OC_SHELL" "$OPENCODE_AGE_DAYS"
    clean_files "$OC_LOG" "$OPENCODE_AGE_DAYS"
}

remove_trash_entries() {
    clean_entries "$TRASH_FILES" "$TRASH_AGE_DAYS"
    clean_trash_info
}

remove_package_caches() {
    clean_files "$POETRY_DIR/artifacts" "$CACHE_AGE_DAYS"
    clean_files "$POETRY_DIR/cache" "$CACHE_AGE_DAYS"
    clean_files "$PIP_DIR" "$CACHE_AGE_DAYS"
    clean_files "$NPM_DIR" "$CACHE_AGE_DAYS"
    clean_files "$ARDUINO_STAGING" "$CACHE_AGE_DAYS"
}

prune_uv_cache() {
    command -v uv >/dev/null 2>&1 || return 0
    if [ "$DRY_RUN" -eq 1 ]; then
        echo "[dry-run] uv cache prune"
        return 0
    fi
    if [ "$DELETE_ALL" -eq 1 ]; then
        uv cache clean >/dev/null
    else
        uv cache prune >/dev/null
    fi
    echo "[pruned]  uv cache"
}

remove_thumbnail_entries() {
    clean_entries "$THUMB_DIR" "$CACHE_AGE_DAYS"
}

remove_old_browsers() {
    for dir in "${BROWSER_DIRS[@]}"; do
        clean_old_entries "$dir" "$BROWSER_AGE_DAYS"
    done
}


# -----------------------------------------------------------------------------
# --------------------------------- Helpers -----------------------------------
# -----------------------------------------------------------------------------
# Delete top level entries older than the age limit, honoring --all.
clean_entries() {
    local target=$1 days=$2
    local find_args=(-mindepth 1 -maxdepth 1)
    [ -d "$target" ] || return 0
    [ "$DELETE_ALL" -eq 0 ] && find_args+=(-mtime "+$days")
    while IFS= read -r -d '' path; do
        remove_entry "$path"
    done < <(find "$target" "${find_args[@]}" -print0 2>/dev/null)
}

# Delete top level entries older than the age limit, never honoring --all.
clean_old_entries() {
    local target=$1 days=$2
    local find_args=(-mindepth 1 -maxdepth 1 -mtime "+$days")
    [ -d "$target" ] || return 0
    while IFS= read -r -d '' path; do
        remove_entry "$path"
    done < <(find "$target" "${find_args[@]}" -print0 2>/dev/null)
}

# Delete files whose mtime exceeds the age limit, honoring --all.
clean_files() {
    local target=$1 days=$2
    local find_args=(-type f)
    [ -d "$target" ] || return 0
    [ "$DELETE_ALL" -eq 0 ] && find_args+=(-mtime "+$days")
    while IFS= read -r -d '' path; do
        remove_entry "$path"
    done < <(find "$target" "${find_args[@]}" -print0 2>/dev/null)
}

remove_entry() {
    local path=$1 size
    # Never recurse into mounted filesystems such as stale fuse mounts.
    if mountpoint -q "$path" 2>/dev/null; then
        echo "[skip]    mount point $path"
        return 0
    fi
    size=$(du -sh "$path" 2>/dev/null | cut -f1)
    if [ "$DRY_RUN" -eq 1 ]; then
        echo "[dry-run] ${size:-?}  $path"
        return 0
    fi
    rm -rf -- "$path" && echo "[removed] ${size:-?}  $path"
}

# Drop trash info files whose corresponding payload is gone.
clean_trash_info() {
    local info base
    [ -d "$TRASH_INFO" ] || return 0
    for info in "$TRASH_INFO"/*.trashinfo; do
        [ -e "$info" ] || continue
        base=$(basename "$info" .trashinfo)
        [ -e "$TRASH_FILES/$base" ] && continue
        remove_entry "$info"
    done
}

remove_empty_dirs() {
    local target=$1
    [ -d "$target" ] || return 0
    [ "$DRY_RUN" -eq 1 ] && return 0
    find "$target" -mindepth 1 -type d -empty -delete 2>/dev/null
}

available_kb() {
    df --output=avail -k / | tail -n 1 | tr -d ' '
}

human_kb() {
    local kb=$1 sign=""
    if [ "$kb" -lt 0 ]; then
        sign="-"
        kb=$((-kb))
    fi
    echo "${sign}$(numfmt --to=iec --suffix=B --from-unit=1024 -- "$kb")"
}


main "$@"


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
