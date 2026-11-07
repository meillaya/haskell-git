#!/usr/bin/env bash
# Measure the peak RSS of clone and verify on a pack much larger than a
# sensible heap cap. This is a manual acceptance check, not part of cabal test.
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
bin=${HASKELL_GIT:-$root/dist-newstyle/build/x86_64-linux/ghc-9.10.3/haskell-git-0.2.0.0/x/haskell-git/build/haskell-git/haskell-git}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

size_mb=${SIZE_MB:-64}
mem_limit_kb=${MEM_LIMIT_KB:-131072}
time_bin=$(type -P time || true)
if [ -z "$time_bin" ]; then
    echo "GNU time is not installed; install pkgs.time or skip this benchmark" >&2
    exit 0
fi

git init -q -b main "$work/source"
git -C "$work/source" config user.email bench@example.com
git -C "$work/source" config user.name Bench
head -c "$((size_mb * 1024 * 1024))" /dev/urandom > "$work/source/big.bin"
git -C "$work/source" add -A
git -C "$work/source" commit -qm big
git -C "$work/source" gc -q
git clone -q --bare "$work/source" "$work/serve.git"

port=$(python3 - <<'PY'
import socket
s = socket.socket()
s.bind(("127.0.0.1", 0))
print(s.getsockname()[1])
s.close()
PY
)
git daemon --reuseaddr --export-all --base-path="$work" --port="$port" --listen=127.0.0.1 >/dev/null 2>&1 &
daemon=$!
trap 'kill "$daemon" 2>/dev/null || true; rm -rf "$work"' EXIT
sleep 0.5

"$time_bin" -v "$bin" clone "git://127.0.0.1:$port/serve.git" "$work/clone" 2>"$work/clone.time"
"$time_bin" -v "$bin" verify "$work/clone" 2>"$work/verify.time"

max_clone=$(awk '/Maximum resident set size/ {print $6}' "$work/clone.time")
max_verify=$(awk '/Maximum resident set size/ {print $6}' "$work/verify.time")
printf 'pack source: %s MiB\nclone peak RSS: %s KiB\nverify peak RSS: %s KiB\nlimit: %s KiB\n' \
    "$size_mb" "$max_clone" "$max_verify" "$mem_limit_kb"
if [ "$max_clone" -gt "$mem_limit_kb" ] || [ "$max_verify" -gt "$mem_limit_kb" ]; then
    echo "memory benchmark failed" >&2
    exit 1
fi
echo "memory benchmark passed"
