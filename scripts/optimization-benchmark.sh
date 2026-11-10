#!/usr/bin/env bash
# Reproducible clone/fetch benchmark for haskell-git.
#
# Generates a synthetic repository (many small files, a history that produces
# deltas), serves it over a fake SSH command that runs `git-upload-pack`
# locally (so the measurement isolates CPU/IO work rather than the network),
# and reports wall-clock clone timings plus repository invariants.
#
# Usage:
#   scripts/optimization-benchmark.sh --binary ./result/bin/haskell-git
#   scripts/optimization-benchmark.sh --binary $(cabal list-bin exe:haskell-git) \
#       --files 4000 --commits 200 --runs 3 --fetch
#
# Options:
#   --binary PATH   haskell-git executable (or set HASKELL_GIT_BIN)
#   --files N       number of files in the synthetic tree (default 2000)
#   --commits N     number of commits in the history (default 120)
#   --runs N        clone runs (default 3; reports every run and the median)
#   --fetch         after cloning, make an upstream commit and time a fetch
#   --keep          keep the work directory and print its path
#
# The reference `git` binary is measured too when it is on PATH. The script
# requires git, python3 and a POSIX shell; it never touches the network.

set -euo pipefail

BINARY=${HASKELL_GIT_BIN:-}
FILES=2000
COMMITS=120
RUNS=3
DO_FETCH=0
KEEP=0

while [ $# -gt 0 ]; do
    case "$1" in
        --binary) BINARY=${2:?--binary needs a path}; shift 2 ;;
        --files) FILES=${2:?--files needs a number}; shift 2 ;;
        --commits) COMMITS=${2:?--commits needs a number}; shift 2 ;;
        --runs) RUNS=${2:?--runs needs a number}; shift 2 ;;
        --fetch) DO_FETCH=1; shift ;;
        --keep) KEEP=1; shift ;;
        -h|--help) sed -n '2,30p' "$0"; exit 0 ;;
        *) echo "unknown option: $1" >&2; exit 2 ;;
    esac
done

if [ -z "$BINARY" ]; then
    echo "error: pass --binary PATH or set HASKELL_GIT_BIN" >&2
    exit 2
fi
BINARY=$(cd "$(dirname "$BINARY")" && pwd)/$(basename "$BINARY")

WORK=$(mktemp -d "${TMPDIR:-/tmp}/haskell-git-bench.XXXXXX")
cleanup() {
    if [ "$KEEP" -eq 1 ]; then
        echo "work directory kept: $WORK"
    else
        rm -rf "$WORK"
    fi
}
trap cleanup EXIT

SOURCE="$WORK/source"
SERVE="$WORK/serve"
BARE="$SERVE/repo.git"
FAKE_SSH="$WORK/fake-ssh.sh"

echo "== generating synthetic repository ($FILES files, $COMMITS commits) =="
mkdir -p "$SOURCE" "$SERVE"
git -C "$SOURCE" init -q -b main
git -C "$SOURCE" config user.email ada@example.com
git -C "$SOURCE" config user.name Ada

python3 - "$SOURCE" "$FILES" "$COMMITS" <<'PY'
import os
import random
import subprocess
import sys

root, file_count, commit_count = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
random.seed(20261109)
words = ("lorem ipsum dolor sit amet consectetur adipiscing elit sed do "
         "eiusmod tempor incididunt ut labore magna aliqua").split()
dirs = [f"d{i:02d}" for i in range(40)]
for d in dirs:
    os.makedirs(os.path.join(root, d), exist_ok=True)

def new_blob(seed):
    rng = random.Random(seed)
    target = rng.randint(8 * 1024, 24 * 1024)
    lines, size, index = [], 0, 0
    while size < target:
        line = f"{index:06d} " + " ".join(rng.choice(words) for _ in range(rng.randint(6, 12)))
        lines.append(line)
        size += len(line) + 1
        index += 1
    return "\n".join(lines) + "\n"

def write(path, contents):
    with open(os.path.join(root, path), "w") as handle:
        handle.write(contents)

paths = [os.path.join(dirs[i % 40], f"file{i:04d}.txt") for i in range(file_count)]
for i, path in enumerate(paths):
    write(path, new_blob(i))

def git(*args):
    subprocess.run(["git", "-C", root, *args], check=True, stdout=subprocess.DEVNULL)

git("add", "-A")
git("commit", "-qm", "initial tree")
for commit in range(1, commit_count):
    # Touch a spread of files per commit so the pack contains delta chains.
    touched = random.sample(paths, max(1, file_count // 40))
    for path in touched:
        write(path, new_blob(commit * 1000003 + paths.index(path)))
    if commit % 17 == 0:
        write(os.path.join(dirs[commit % 40], f"new-{commit:04d}.txt"),
              new_blob(commit * 7919))
    git("add", "-A")
    git("commit", "-qm", f"commit {commit}")
print("generated", file_count, "files and", commit_count, "commits")
PY

git -C "$SOURCE" clone -q --bare "$SOURCE" "$BARE"
git -C "$BARE" gc -q
PACK_BYTES=$(du -sk "$BARE/objects/pack"/*.pack | awk '{s+=$1} END {print s*1024}')
OBJECTS=$(git -C "$BARE" count-objects -v | awk -F': ' '/in-pack/ {print $2}')
COMMITS_IN_PACK=$(git -C "$BARE" rev-list --all --count)
echo "pack: $((PACK_BYTES / 1024)) KiB, $OBJECTS objects, $COMMITS_IN_PACK commits"

cat > "$FAKE_SSH" <<EOF
#!/bin/sh
exec git-upload-pack "$BARE" 2>/dev/null
EOF
chmod +x "$FAKE_SSH"
# The clone and fetch both go through the same local transport.
export GIT_SSH_COMMAND="$FAKE_SSH"

measure() {
    local start end
    start=$(date +%s%N)
    "$@" >/dev/null 2>&1
    end=$(date +%s%N)
    echo $(( (end - start) / 1000000 ))
}

clone_once() {
    local binary=$1 destination=$2
    measure "$binary" clone ssh://example/repo.git "$destination"
}

results=()
for run in $(seq 1 "$RUNS"); do
    destination="$WORK/clones/haskell-git-$run"
    rm -rf "$destination"
    ms=$(clone_once "$BINARY" "$destination")
    loose=$(find "$destination/.git/objects" -type f ! -path '*/pack/*' ! -path '*/info/*' | wc -l)
    packs=$(find "$destination/.git/objects/pack" -name '*.pack' | wc -l)
    fsck=ok
    git -C "$destination" fsck --strict >/dev/null 2>&1 || fsck=FAIL
    echo "haskell-git run $run: ${ms}ms loose=$loose packs=$packs fsck=$fsck"
    results+=("$ms")
done
median=$(printf '%s\n' "${results[@]}" | sort -n | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}')
echo "haskell-git median: ${median}ms"

if command -v git >/dev/null 2>&1; then
    ref_results=()
    for run in $(seq 1 "$RUNS"); do
        destination="$WORK/clones/git-$run"
        rm -rf "$destination"
        ms=$(GIT_SSH_COMMAND="$FAKE_SSH" measure git clone --quiet ssh://example/repo.git "$destination")
        echo "git run $run: ${ms}ms"
        ref_results+=("$ms")
    done
    ref_median=$(printf '%s\n' "${ref_results[@]}" | sort -n | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}')
    echo "git median: ${ref_median}ms"
fi

if [ "$DO_FETCH" -eq 1 ]; then
    echo "== thin-pack fetch =="
    destination="$WORK/clones/fetch"
    rm -rf "$destination"
    clone_once "$BINARY" "$destination" >/dev/null
    python3 - "$SOURCE" <<'PY'
import os
import sys
root = sys.argv[1]
changed = 0
for directory, _dirs, names in sorted(os.walk(root)):
    if ".git" in directory.split(os.sep):
        continue
    for name in sorted(names):
        if not name.endswith(".txt"):
            continue
        with open(os.path.join(directory, name), "a") as handle:
            handle.write("fetch-update\n")
        changed += 1
        if changed >= 40:
            break
    if changed >= 40:
        break
print("modified", changed, "files for the fetch")
PY
    git -C "$SOURCE" add -A
    git -C "$SOURCE" commit -qm "upstream change" >/dev/null
    git -C "$SOURCE" push -q "$BARE" main:main
    start=$(date +%s%N)
    if ! (cd "$destination" && "$BINARY" fetch origin >"$WORK/fetch.out" 2>"$WORK/fetch.err"); then
        echo "fetch failed:" >&2
        cat "$WORK/fetch.err" >&2
        exit 1
    fi
    end=$(date +%s%N)
    loose=$(find "$destination/.git/objects" -type f ! -path '*/pack/*' ! -path '*/info/*' | wc -l)
    packs=$(find "$destination/.git/objects/pack" -name '*.pack' | wc -l)
    fsck=ok
    git -C "$destination" fsck --strict >/dev/null 2>&1 || fsck=FAIL
    echo "fetch: $(( (end - start) / 1000000 ))ms loose=$loose packs=$packs fsck=$fsck"
fi
