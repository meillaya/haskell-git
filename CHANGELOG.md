# Revision history

## Unreleased

- Clone indexes a self-contained pack in one streaming pass and retains it
  directly, resolving delta bases through a temporary uncompressed object
  cache instead of writing, re-reading, and deleting every object as a loose
  file. Checkout and index generation share one payload read per file. Large
  clones are several times faster.
- Pack index parsing is linear rather than quadratic, so opening an existing
  object store is fast for large repositories. Pack trailer checksums are now
  verified when a pack is retained, after a fetch, and by `verify`, instead of
  on every store open.
- Loose objects are compressed at zlib level 1, matching git's default for
  loose objects.
- A Nix flake packages the CLI and library and provides a dev shell, an
  overlay, and NixOS and Home Manager modules (`programs.haskell-git.enable`).
- Nested `.gitignore` files are evaluated by `status`.
- `submodule add`, `submodule deinit`, and branch-aware
  `submodule update --remote` are implemented.
- Checkout refuses trees containing `.git` components and refuses to write
  through symlinked parent directories.
- Merge detects renames whose contents also changed when the text similarity
  is at least 50%.
- HTTP(S) transport negotiates protocol v2 using `ls-refs` and `fetch`, with
  fallback to v0 when the server does not support it.
- SSH and git:// transports negotiate protocol v2 as well: the SSH transport
  forwards `GIT_PROTOCOL` through OpenSSH's `SendEnv` mechanism and the git://
  transport sends the `version=2` extra parameter. Both fall back to v0 when
  the server does not advertise version 2, and `protocol.version=0` forces the
  v0 path.
- Clone and fetch retain self-contained packs together with a v2 index and
  remove the loose object duplicates; thin packs continue to use loose
  objects.

## 0.3.0.0 - 2026-11-09

- Merge now performs a line-level diff3 and detects exact-content renames.
- `merge --no-ff` / `--ff-only`, `pull --ff-only`, and `rebase --onto` are
  implemented.
- Fetch sends `have` lines and auto-follows reachable tags; `--tags` and
  `--no-tags` control the behaviour.
- `status` evaluates root `.gitignore` and `.git/info/exclude`.
- `push --set-upstream`, `push --force-with-lease`, and git:// receive-pack are
  implemented, and pushes update remote-tracking refs.
- Refs are written under lock files and `HEAD`/branch reflogs are recorded.
- `submodule sync` is implemented.
- SHA-256 fetch and push work end to end, including the `object-format`
  capability on receive-pack.
- The test suite adds diff3, rename, merge-flag, rebase-onto, tag auto-follow,
  SSH/git:// push, force-with-lease, `.gitignore`, reflog, and pack
  round-trip coverage.
- GitHub Actions runs the suite on GHC 9.6 and 9.10, `cabal check`, `cabal
  sdist`, and a bounded-memory benchmark.

## 0.2.0.0 - 2026-11-07

- SHA-256 repositories are supported alongside SHA-1. `init --object-format`
  writes the repository extension, clone adopts the advertised format, and the
  loose object, tree, commit, tag, index, pack index, pack, ref, and verify
  layers all use the repository's hash algorithm.
- Pack files are streamed through a temporary file and imported incrementally;
  the store reads packed objects through file handles and a bounded memo.
- A transport layer supports Smart HTTP, SSH (through OpenSSH), and git://.
- `fetch`, `push`, `pull`, and `pull --rebase` are implemented, including
  refspecs, `FETCH_HEAD`, tag auto-following, pruning, fast-forward checks,
  deletion, and HTTP Basic/Bearer authentication.
- `checkout`, `switch`, `branch`, `status`, and `reset --hard` manage the index
  and working tree with conflict safety.
- `merge` performs fast-forward and three-way tree merges, writes conflict
  stages and markers, and supports `--continue` and `--abort`. `commit -m`
  commits the index, including merge commits.
- `rebase` replays commits non-interactively and supports `--continue`,
  `--skip`, and `--abort`.
- Submodules can be cloned recursively and initialised or updated with
  `submodule update --init [--recursive]`; `submodule status` reports their
  state. Repositories with a `.git` gitfile are discovered.
- New test coverage includes streaming, authenticated HTTP, SSH/git://,
  push, checkout/status, merge, rebase, and submodules.

## 0.1.0.0 - 2026-11-06

- Clone over Smart HTTP fetches every branch and tag, sets the default branch,
  writes remote tracking refs and `refs/remotes/origin/HEAD`, and checks out
  regular files, executable files, symlinks, and submodule directories.
- Clone writes the index in version 2, so `git status` is clean after a clone.
- New commands: `add`, `ls-tree`, `log`, `rev-parse`, `write-tree`,
  `update-ref`, and `verify`.
- `clone` reads packs with offset deltas and with reference delta bases that
  appear later in the pack.
- Objects are read from pack files as well as loose files, through the new
  store layer.
- The tree codec handles all five entry modes and writes git's canonical order.
- Annotated tag objects are read, written, and fetched.
- References are read from `packed-refs` as well as loose files, and ref names
  are validated before they become paths.
- Repository discovery walks up from a subdirectory and recognises bare
  repositories.
- Object and ref writes go through a temporary file and a rename.
- `devenv.nix` provides the development shell; dependency bounds accept GHC 9.6
  through 9.14.
- The test suite now starts a real `git http-backend` server for clone tests,
  covers packed repositories, and checks corruption detection.
