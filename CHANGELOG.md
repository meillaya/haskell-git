# Revision history

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
