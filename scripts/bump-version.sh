#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ELISP_FILE="$REPO_ROOT/space-tree.el"
EASK_FILE="$REPO_ROOT/Eask"

# --- Read current version from space-tree.el header ---
CURRENT_VERSION=$(sed -n 's/^;; Version: \([0-9]*\.[0-9]*\.[0-9]*\)/\1/p' "$ELISP_FILE")
if [[ -z "$CURRENT_VERSION" ]]; then
  echo "Error: could not read version from $ELISP_FILE" >&2
  exit 1
fi

IFS='.' read -r MAJOR MINOR PATCH <<< "$CURRENT_VERSION"

# --- Determine commit range ---
LATEST_TAG=$(git -C "$REPO_ROOT" describe --tags --match 'v*' --abbrev=0 2>/dev/null || true)
if [[ -n "$LATEST_TAG" ]]; then
  RANGE="${LATEST_TAG}..HEAD"
else
  RANGE="HEAD"
fi

# --- Scan commits for conventional commit prefixes ---
BUMP=""
while IFS= read -r subject; do
  case "$subject" in
    *'!':*|*BREAKING\ CHANGE*)
      BUMP="major"
      break
      ;;
    feat:*|feat\(*)
      if [[ "$BUMP" != "major" ]]; then
        BUMP="minor"
      fi
      ;;
    fix:*|fix\(*)
      if [[ -z "$BUMP" ]]; then
        BUMP="patch"
      fi
      ;;
  esac
done < <(git -C "$REPO_ROOT" log --format='%s' "$RANGE" --)

# Also check commit bodies for BREAKING CHANGE
if [[ "$BUMP" != "major" ]]; then
  if git -C "$REPO_ROOT" log --format='%b' "$RANGE" -- | grep -q '^BREAKING CHANGE'; then
    BUMP="major"
  fi
fi

# --- No bump needed ---
if [[ -z "$BUMP" ]]; then
  exit 0
fi

# --- Compute new version ---
case "$BUMP" in
  major) NEW_VERSION="$((MAJOR + 1)).0.0" ;;
  minor) NEW_VERSION="${MAJOR}.$((MINOR + 1)).0" ;;
  patch) NEW_VERSION="${MAJOR}.${MINOR}.$((PATCH + 1))" ;;
esac

# --- Idempotency: if already at this version, do nothing ---
if [[ "$NEW_VERSION" == "$CURRENT_VERSION" ]]; then
  exit 0
fi

# --- Update files ---
sed -i'' -e "s/^;; Version: ${CURRENT_VERSION}/;; Version: ${NEW_VERSION}/" "$ELISP_FILE"
sed -i'' -e "s/\"${CURRENT_VERSION}\"/\"${NEW_VERSION}\"/" "$EASK_FILE"

# Update version.txt if it exists
if [[ -f "$REPO_ROOT/version.txt" ]]; then
  printf '%s\n' "$NEW_VERSION" > "$REPO_ROOT/version.txt"
fi

# Update .release-please-manifest.json if it exists
MANIFEST="$REPO_ROOT/.release-please-manifest.json"
if [[ -f "$MANIFEST" ]]; then
  sed -i'' -e "s/\"${CURRENT_VERSION}\"/\"${NEW_VERSION}\"/" "$MANIFEST"
fi

echo "$NEW_VERSION"
