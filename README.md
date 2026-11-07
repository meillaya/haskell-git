# haskell-git

haskell-git clones Git repositories over Smart HTTP, then reads, writes, and
checks them. It implements the formats itself, so it does not need a C Git
library and does not call `git` for repository work. Use it to inspect a
repository from code you can read, or to check that an object store is intact.

The first command:

```console
$ haskell-git clone https://github.com/githubtraining/hellogitworld hello
Cloning into 'hello'...
HEAD is ef7bebf8bdb1919d947afe46ab4b2fb4278039b3 on master
```

Then `cd hello` and `git status --porcelain` prints nothing. The clone wrote the
index as well as the working tree, so git sees a clean checkout.

## Build it

The development environment is a devenv shell. It provides GHC, cabal, git,
python3, and the C libraries the zlib binding needs.

```console
$ devenv shell
haskell-git dev shell
ghc   9.10.3
cabal 3.16.1.0

build      cabal build
unit-test  cabal test
check      cabal check and cabal test
$ cabal build
```

Install the executable somewhere on your path:

```console
$ cabal install exe:haskell-git --overwrite-policy=always --installdir="$HOME/.local/bin"
```

Without Nix you need GHC 9.6 or newer, cabal-install, and the zlib development
headers. `cabal build` then behaves the same way.

## Clone

```console
$ haskell-git clone https://github.com/githubtraining/hellogitworld hello
Cloning into 'hello'...
HEAD is ef7bebf8bdb1919d947afe46ab4b2fb4278039b3 on master
$ cd hello
$ git branch -a | head -5
* master
  remotes/origin/HEAD -> origin/master
  remotes/origin/bisect
  remotes/origin/fbaddfiles
  remotes/origin/fbmaster
$ git tag
RELEASE_1.0
RELEASE_1.1
$ git fsck --strict
```

Clone fetches every advertised branch and tag, gives the default branch a local
ref, mirrors the other branches under `refs/remotes/origin/`, sets
`refs/remotes/origin/HEAD`, and writes the remote and branch sections of the
config. The checkout restores regular files, executable files, symlinks, and
empty directories for submodule entries, then writes the index from the same
tree.

An empty remote clones too. The result is an empty repository with the remote
config and the advertised default branch, which is what `git clone` leaves
behind.

A server that advertises SHA-256 object ids is refused by name, and a remote
that is not an HTTP or HTTPS URL is refused before any request. HTTP
authentication is not implemented: a server that demands it fails with its
HTTP status.

## Inspect

Read commands take a revision: `HEAD`, a branch or tag name, a full ref, or a
40-character object id.

```console
$ haskell-git ls-tree -r HEAD | head -4
100644 blob 412eeda78dc9de1186c2e0e1526764af82ab3431	.gitattributes
100644 blob 7c820833a9ad5fbfc96efd533d55f5edc65dc977	.gitignore
100644 blob 17eccd68b7cafa718d53c8b4db666194646e2bd9	.travis.yml
100644 blob adfd6da3c0a3fb038393144becbf37f14f780087	README.txt
$ haskell-git cat-file -p HEAD | head -6
tree f724b12649f1f531c66c1caa65fa3215accfaeb7
parent ebbbf773431ba07510251bb03f9525c7bab2b13a
author Jordan McCullough <jordan@github.com> 1415384839 -0700
committer Jordan McCullough <jordan@github.com> 1415384839 -0700

Fix groupId after package refactor
$ haskell-git log -n 1
commit ef7bebf8bdb1919d947afe46ab4b2fb4278039b3
Author: Jordan McCullough <jordan@github.com>
Date:   Fri Nov  7 11:27:19 2014 -0700

    Fix groupId after package refactor

$ haskell-git rev-parse HEAD
ef7bebf8bdb1919d947afe46ab4b2fb4278039b3
```

`cat-file` prints the stored payload for commits and tags, so a signed commit
keeps its signature. `-t` prints the type and `-s` prints the payload length.

Objects are read from loose files or from pack files. The pack index, the pack
object header, and both delta encodings are parsed here, so a repository that
`git gc` packed is as readable as one with loose objects.

## Verify

```console
$ haskell-git verify
verify: 306 objects, 13 refs, 0 problems
```

`verify` decompresses and decodes every loose object, rehashes it, checks the
pack trailer checksum, resolves every packed object and compares it with its id,
and checks that every ref and HEAD point at an object that exists. Each problem
prints on its own line. The exit status is 0 for a clean repository, 1 when
problems were found, and 2 for a usage error.

The index and working tree are derived state, so `verify` does not check them.
`git status` is the tool for that.

## Write a commit

The plumbing commands compose the same way git's do.

```console
$ haskell-git init
Initialized empty Git repository in /tmp/write-demo/.git/
$ git config user.name "Ada"
$ git config user.email "ada@example.com"
$ printf 'hello\n' > hello.txt
$ mkdir docs && printf 'notes\n' > docs/notes.txt
$ haskell-git add .
$ TREE=$(haskell-git write-tree)
$ echo "$TREE"
5b083868b1b325fb1e029e3934baeec6787998e6
$ COMMIT=$(haskell-git commit-tree "$TREE" -m "First commit")
$ haskell-git update-ref refs/heads/main "$COMMIT"
$ git log --format=%s
First commit
$ git status --porcelain
$ git fsck --strict
```

The tree id depends only on the file contents, so that value is reproducible.
A commit id also covers the author and committer timestamps, so it changes from
run to run.

`commit-tree` reads the author and committer from `GIT_AUTHOR_*` and
`GIT_COMMITTER_*` when set, and from the repository `user.name` and `user.email`
otherwise. It prints the new commit id and leaves HEAD alone, like git.

## How it works

Git stores four kinds of object. A blob holds file bytes, a tree holds names and
object ids, a commit points at a tree and its parents, and a tag points at
another object. An object id is the SHA-1 of the object header and payload, so
the name of an object is derived from its bytes.

![Object graph](docs/figures/object-model.png)

A reference is a file that contains an object id, or a symbolic reference to
another reference. `HEAD` points at a branch in a normal checkout and directly
at a commit in a detached one. Tags and branches live under `refs/`.

The four files the tool reads and writes have fixed layouts. The figure below
shows the loose object, the tree entry, the index entry, and the pack object
header. The formulas are the rules this implementation follows: the loose
object id is the SHA-1 of the encoded object, a tree sorts directories as their
name plus a slash, an index entry is padded to an eight-byte boundary, and an
offset delta base is found by subtracting the decoded distance from the entry's
own offset.

![File formats](docs/figures/formats.png)

The clone pipeline turns an advertisement and a pack stream into a repository.
A pkt-line frame is four hex digits of length and the payload, `want` lines name
the object ids the client needs, and the server answers with a pack. The pack
reader resolves full objects and both delta kinds, then writes loose objects,
refs, the index, and the working tree.

![Clone flow](docs/figures/clone-flow.png)

Every command goes through one store. `clone` writes through it, `cat-file`,
`ls-tree`, and `log` read through it, `verify` walks it, and the write commands
build objects and hand them to it. The store keeps the loose object directory
and every pack index open for the life of one command.

![Module layout](docs/figures/modules.png)

The index is written in version 2. Each entry is a 62-byte header, the path, and
padding to the next eight-byte boundary, and the file ends with a SHA-1. The
stat fields are left as zero: git treats a zero size as "hash the file and
compare", so a fresh checkout is clean and git fills the stat fields in on the
next status.

Pack reads load each pack file once per command and keep resolved deltas in a
memo table. Large packs cost memory in proportion to their size, which is the
main reason `clone` and `verify` are aimed at small and medium repositories.

## Limits

- Smart HTTP only. SSH and the git protocol are not implemented, and there is
  no authentication.
- SHA-1 only. A server that advertises `object-format=sha256` is refused.
- `clone`, `verify`, and the read commands load whole pack files into memory.
- Submodule entries become empty directories and are not initialised.
- `push`, `fetch`, `pull`, `merge`, `rebase`, and a general `checkout` are out
  of scope.
- Windows is untested. Symlinks and executable bits assume a POSIX filesystem.

## Tests

```console
$ cabal test
```

The suite uses git as the reference. Interoperability tests read and write the
same repository from both sides, and the clone tests start a real
`git http-backend` server, clone from it, and then check the result with
`git status`, `git fsck --strict`, and `git ls-tree`. Unit tests cover the
codecs, the pack and delta readers, the index, the refs, and the verify
commands, including corrupted objects.

`devenv shell -- unit-test` runs the same suite from the shell, and
`devenv shell -- check` adds `cabal check`.
