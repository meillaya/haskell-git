# haskell-git

A learning-oriented reimplementation of selected Git internals in Haskell.

The project is intentionally **Git-compatible first** and **feature-complete second**. The goal is to understand how Git stores objects, links trees and commits, tracks refs, and performs a bounded Smart HTTP clone, all from Haskell code rather than shelling out to `git` for the implementation.

## Project status

The repository currently implements the following milestones:

- repository initialization
- loose blob read/write
- tree read/write
- commit creation
- minimal refs and `HEAD` support
- pkt-line parsing
- Smart HTTP discovery parsing
- pack header/object-header parsing
- delta parsing/application
- bounded end-to-end **Smart HTTP + SHA-1 clone** for the supported scenario

The implementation is still intentionally narrow. It is designed to be understandable and correct for the current scope, not to match all of Git's transport, storage, or porcelain behavior.

## Theory

Git is fundamentally a **content-addressed object database**. Each object is stored under the SHA-1 of its canonical byte representation. That means the hash is not metadata layered on top of an object; it is derived from the exact bytes that define the object.

A canonical Git object is encoded as:

```text
<type><space><size><NUL><payload>
```

For example, a blob uses `blob <size>\0...`, a tree uses `tree <size>\0...`, and a commit uses `commit <size>\0...`. Hashing these bytes yields the object ID.

### Loose objects

Git's simplest storage format is the **loose object**. A loose object is the canonical object byte stream compressed with zlib and stored at:

```text
.git/objects/aa/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
```

The first two hex characters become the directory name and the remaining 38 characters become the file name.

### Trees

A tree object is a compact directory listing. Each entry stores a mode, a file name, a NUL byte, and the raw 20-byte object ID of the referenced object.

Conceptually:

```text
100644 hello.txt\0<20 raw bytes of blob id>
```

A tree is therefore not just a list of paths. It is a byte-level structure that Git expects in a very particular format.

### Commits

A commit links history together. It references exactly one tree and optionally one or more parent commits. It also stores author/committer identity metadata and a commit message.

Conceptually:

```text
tree <tree-id>
parent <parent-id>
author Name <email> <timestamp> <timezone>
committer Name <email> <timestamp> <timezone>

<message>
```

This project currently supports first commits and single-parent commits, which is enough to model linear history and validate interoperability.

### Refs and HEAD

Git refs are named pointers to object IDs. `HEAD` is usually a **symbolic reference** that points to a branch ref such as `refs/heads/main` or `refs/heads/master`.

The minimal model implemented here is:

```text
.git/HEAD                 -> ref: refs/heads/<branch>
.git/refs/heads/<branch>  -> <commit-id>
```

That is enough to make generated commits visible to Git tools like `git rev-parse HEAD` and `git cat-file -p HEAD`.

### Smart HTTP, pkt-line, packfiles, and deltas

Clone requires a different layer of Git theory.

Git's Smart HTTP transport uses a pkt-line framing format. The server advertises refs and capabilities, and the client sends an upload-pack request describing what it wants. The server responds with pack data.

A pkt-line frame looks like this:

```text
<4-hex-digit total-length><payload>
```

with `0000` reserved for a flush packet.

Packfiles bundle many objects together. Each pack begins with a `PACK` signature, a version, and an object count. Each object then has its own packed header plus compressed content. Some entries are stored as deltas instead of full objects.

This project currently implements a **bounded checkpoint** version of that machinery and then uses it in a bounded clone path for the supported Smart HTTP scenario.

## Architecture

The code is split into a thin CLI layer and a library layer.

### CLI layer

`app/Main.hs` is intentionally tiny. It delegates all command handling to `app/Git/CLI.hs`.

That CLI currently exposes a small plumbing-style interface:

```text
init [PATH]
hash-object [-w] FILE
cat-file -p OID
commit-tree TREE [-p PARENT]... -m MESSAGE
clone URL DEST
```

The CLI is responsible for argument parsing and user-visible command behavior, but the object, ref, pack, and clone logic lives in `src/`.

### Library layout

The current library modules are organized by responsibility:

```text
src/Git/Hash.hs                 -- SHA-1 object ids and path rendering
src/Git/Object.hs               -- canonical Git object wrapper/codec
src/Git/Object/Blob.hs          -- blob conversions
src/Git/Object/Tree.hs          -- tree entry codec and pretty rendering
src/Git/Object/Commit.hs        -- commit codec and ident metadata
src/Git/Object/Loose.hs         -- zlib-compressed loose object IO
src/Git/Refs.hs                 -- symbolic HEAD + ref read/write
src/Git/Repo.hs                 -- repository creation and basic repo paths
src/Git/Protocol/PacketLine.hs  -- pkt-line framing
src/Git/Protocol/SmartHTTP.hs   -- advertisement parsing
src/Git/Pack.hs                 -- pack header/object-header parsing
src/Git/Pack/Delta.hs           -- delta parsing and application
src/Git/Clone.hs                -- bounded Smart HTTP clone orchestration
```

The design principle is simple: keep binary-format logic close to the module that owns the format, keep byte-oriented boundaries explicit, and keep the CLI thin.

## Implementation details so far

### 1. Repository initialization

`Git.Repo.initRepository` creates a minimal Git-readable repository structure and writes the base files that Git expects.

It currently creates:

```text
.git/
.git/objects/
.git/objects/info/
.git/objects/pack/
.git/refs/
.git/refs/heads/
.git/refs/tags/
.git/HEAD
.git/config
.git/description
```

`HEAD` begins as a symbolic ref and `config` is written with a minimal `[core]` section.

### 2. Blob storage

A blob is represented as bytes, converted to a canonical Git object, hashed, compressed, and written as a loose object.

The essential flow is:

```haskell
payload
  -> blobToObject
  -> encodeGitObject
  -> hashBytes
  -> zlib compress
  -> .git/objects/xx/yyyy...
```

Reading a loose object reverses that process and now normalizes decompression failures into a Git-layer IO error rather than leaking raw zlib exceptions.

### 3. Trees

Trees are encoded using raw 20-byte object IDs, not hex text. Entries are sorted by name before serialization, which matches Git's expectation for tree ordering in the supported scenario.

Pretty-printing via `cat-file -p` is implemented for trees by decoding the tree payload and rendering entries in a Git-like format.

### 4. Commits

Commits are encoded and decoded with byte-oriented author/committer metadata and message payloads. The implementation keeps the commit codec separate from ref updates.

That separation matters because it keeps object construction independent from how the repository chooses to point `HEAD` or a branch at the resulting object.

### 5. Minimal refs

The refs layer implements only what was needed for the current milestones:

- read/write branch refs
- read/write symbolic `HEAD`
- update the current `HEAD` target
- resolve the current head commit

This deliberately avoids broader refspec, checkout, index, or porcelain behavior.

### 6. Protocol and pack foundations

Pkt-line, Smart HTTP discovery parsing, pack header parsing, object-header parsing, and delta parsing/application were implemented first in isolation and backed by focused tests.

That checkpoint exists because clone is the hardest milestone and it is safer to validate the binary/protocol foundations before wiring them into end-to-end behavior.

### 7. Bounded Smart HTTP clone

The current clone implementation is intentionally narrow.

Supported scenario:

- SHA-1 repositories
- Smart HTTP transport
- bounded upload-pack exchange
- tested against a local Smart HTTP fixture server
- includes a fixture pack with at least one deltified object

The clone flow is:

```text
GET  /info/refs?service=git-upload-pack
  -> parse advertised refs/capabilities
  -> choose default branch from advertised HEAD
POST /git-upload-pack
  -> send a minimal want/done request
  -> receive pack bytes
  -> parse and import pack objects
  -> write local branch + remote-tracking branch + HEAD
  -> append minimal clone config
  -> materialize the working tree from the fetched commit tree
```

The implementation now rewrites `HEAD` to the discovered default branch and preserves the base object kind when resolving ref-delta entries, which avoids incorrectly turning deltified trees/commits into blobs.

## What has been achieved so far

The project has moved from a placeholder executable to a working Haskell implementation that can already interoperate with real Git across multiple layers.

### Achievements

- Git recognizes repositories created by this project.
- Git can read blobs written by this project.
- This project can read blobs written by Git.
- Git can inspect trees written by this project.
- This project can read trees written by Git.
- Git can inspect commits written by this project.
- The project maintains symbolic `HEAD` and branch refs well enough for Git to resolve `HEAD`.
- pkt-line, Smart HTTP discovery, pack header parsing, and delta parsing/application are implemented and tested.
- A bounded end-to-end Smart HTTP clone works for the supported SHA-1 scenario, including a deltified pack fixture.

## Build, run, and test

### Build

```bash
cabal build
```

### Run the CLI

```bash
cabal run haskell-git -- <command>
```

Examples:

```bash
cabal run haskell-git -- init .
cabal run haskell-git -- hash-object -w hello.txt
cabal run haskell-git -- cat-file -p <object-id>
cabal run haskell-git -- commit-tree <tree-id> -m "Initial commit"
cabal run haskell-git -- clone http://127.0.0.1:PORT/repo.git clone-dir
```

### Test

```bash
cabal test
```

### Package check

```bash
cabal check
```

At the moment `cabal check` still emits a non-blocking warning about a missing `source-repository` stanza.

## Test strategy

The test suite uses both focused codec tests and interoperability tests.

### Unit / codec tests

These validate that the byte-level encodings round-trip and reject malformed inputs:

- blobs
- trees
- commits
- pkt-line
- pack headers
- delta parsing/application
- refs behavior

### Interoperability tests

These validate that Git and the Haskell implementation agree on observable behavior:

- Git recognizes `initRepository` output
- Git reads blobs/trees/commits written by this project
- this project reads blobs/trees written by Git
- clone produces a Git-readable repository in the supported scenario

### End-to-end clone test

The clone test launches a tiny local Smart HTTP fixture server that serves:

- a Smart HTTP discovery response
- a generated upload-pack response containing a real packfile

This allows the project to exercise its own clone implementation without depending on an external network service.

## Next major milestones

The major milestones left are mostly about widening the supported clone surface and making the implementation more Git-complete.

### 1. Broader pack and clone coverage

The clone path is currently bounded to the supported scenario. Future work should expand support for more realistic pack layouts, richer object mixes, and stricter protocol behavior.

### 2. Stronger upload-pack negotiation

The current request path is intentionally minimal. A more complete implementation would negotiate capabilities and object wants in a more Git-like way.

### 3. More repository and transport realism

The clone path still intentionally avoids broad transport parity. Smart HTTP is supported; broader transport support is not.

### 4. More Git plumbing and porcelain

Still explicitly out of scope or deferred:

- index/staging
- checkout as a general command
- status/diff
- merge/rebase/cherry-pick
- push/fetch beyond the current bounded clone flow
- SHA-256 repositories
- broad transport parity

## Limitations

The current implementation should be read as a **bounded, learning-first Git core** rather than a drop-in replacement for Git.

Important current limits:

- only SHA-1 repositories are supported
- clone support is bounded to the tested Smart HTTP scenario
- transport support is narrow
- object support is focused on blob/tree/commit
- the project does not yet aim for broad real-world repository compatibility

## References

These are the core resources that should be used when implementing the project from scratch.

- Git documentation: [`gitformat-pack`](https://git-scm.com/docs/gitformat-pack)
- Git documentation: [`gitprotocol-pack`](https://git-scm.com/docs/gitprotocol-pack)
- Git documentation: [`gitprotocol-http`](https://git-scm.com/docs/gitprotocol-http)
- Git documentation: [`gitrepository-layout`](https://git-scm.com/docs/gitrepository-layout)
- Git documentation: [`git-cat-file`](https://git-scm.com/docs/git-cat-file)
- Git documentation: [`git-hash-object`](https://git-scm.com/docs/git-hash-object)
- Git documentation: [`git-write-tree`](https://git-scm.com/docs/git-write-tree)
- Git documentation: [`git-commit-tree`](https://git-scm.com/docs/git-commit-tree)
- Git documentation: [`git-symbolic-ref`](https://git-scm.com/docs/git-symbolic-ref)
- RFC 1950: ZLIB compressed data format specification
- RFC 1951: DEFLATE compressed data format specification
- Git source code, especially the object, pack, and upload-pack implementations
- Pro Git, chapters covering Git internals, object storage, refs, and packfiles
- `zlib` Haskell package documentation, especially streaming decompression behavior
- `http-conduit` / `http-client` documentation for request construction and response handling
