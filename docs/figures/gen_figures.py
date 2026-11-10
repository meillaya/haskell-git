#!/usr/bin/env python3
"""Generate the four haskell-git README figures as Excalidraw scenes.

The scene geometry is generated, so this script is the source of truth. Run it
from the repository root, then render every scene with the Excalidraw CLI:

    python3 docs/figures/gen_figures.py
    EXCALIDRAW_BROWSER_PATH=... excalidraw render docs/figures/object-model.excalidraw \
        --out docs/figures/object-model.png --scale 2 --max-width 3200

Rules follow /home/mei/nixos/agents/excalidraw.nix: 1520x940 scenes, 40 unit
margin, hand-drawn style (roughness 2, hachure fill, stroke width 2), font
family 1, body 22, title 32, notes 18, house palette, one claim per figure,
every arrow labelled next to the arrow.

The reflection figures (optimization-pipeline, optimization-cost, thin-pack,
pack-math, ...) follow the v2 worklog rules in docs/figures/RULES.md: semantic
palette aliases, measured values in the scene, module tags, churn rows, and one
equation or measurement per figure. Keep both rule sets in sync.
"""

import json
import os

# House palette: role -> (stroke, fill).
PALETTE = {
    "compiler": ("#1971c2", "#d0ebff"),
    "cuda": ("#e8590c", "#ffe8cc"),
    "hip": ("#2f9e44", "#d3f9d8"),
    "runtime": ("#9c36b5", "#f3d9fa"),
    "analysis": ("#f08c00", "#fff3bf"),
    "risk": ("#e03131", "#ffe3e3"),
    "model": ("#0c8599", "#c5f6fa"),
    "neutral": ("#495057", "#f1f3f5"),
    # Performance-worklog aliases (reflection figures, see RULES.md v2).
    "before": ("#e03131", "#ffe3e3"),
    "after": ("#2f9e44", "#d3f9d8"),
    "store": ("#0c8599", "#c5f6fa"),
    "math": ("#f08c00", "#fff3bf"),
    "measure": ("#9c36b5", "#f3d9fa"),
    "network": ("#1971c2", "#d0ebff"),
    "code": ("#212529", "#ffffff"),
}

INK = "#1e1e1e"
MUTED = "#5c5f66"
ARROW_INK = "#495057"

BODY = 22
TITLE = 32
NOTE = 18

# Glyphs verified to render in the hand-drawn (font family 1) face. The list
# includes the house-tested set plus the two glyphs the briefs require: the
# triple bar (git status) and the Unicode minus in the pack formula. Both were
# checked in a render probe; see the report.
GLYPHS = set("‖⊕∈⌊⌋≤≠∅≈→↦ΔΣ≡−")
# Extended set for the reflection (worklog-style) figures. Verified with the
# render probe in RULES.md; keep this list in sync with docs/figures/RULES.md.
EXTENDED_GLYPHS = set("↑↓×·²³⁻√∞¬∑∏")
GLYPHS = GLYPHS | EXTENDED_GLYPHS


def _text_width(text, size):
    """Approximate advance width of one line in the hand-drawn face."""
    narrow = set("iljtfr.,:;'\"|!`()[]{}")
    wide = set("mwMW@%")
    glyph = GLYPHS
    total = 0.0
    for ch in text:
        if ch in narrow:
            total += 0.36
        elif ch in wide:
            total += 1.0
        elif ch in glyph:
            total += 0.72
        elif ch == " ":
            total += 0.36
        elif ch.isupper():
            total += 0.72
        elif ch.isdigit():
            total += 0.6
        else:
            total += 0.55
    return total * size * 1.02


def _line_height(size):
    return size * 1.25


class Scene:
    """Tiny Excalidraw scene builder with the full element field set."""

    def __init__(self, name, width=1520, height=940):
        self.name = name
        self.width = width
        self.height = height
        self.elements = []
        self._seed = 20261106

    # -- low level ---------------------------------------------------------

    def _base(self, etype, x, y, w, h):
        self._seed += 17
        return {
            "id": "%s-%02d" % (self.name, len(self.elements) + 1),
            "type": etype,
            "x": x,
            "y": y,
            "width": w,
            "height": h,
            "angle": 0,
            "strokeColor": INK,
            "backgroundColor": "transparent",
            "fillStyle": "hachure",
            "strokeWidth": 2,
            "strokeStyle": "solid",
            "roughness": 2,
            "opacity": 100,
            "groupIds": [],
            "frameId": None,
            "roundness": None,
            "seed": self._seed,
            "version": 1,
            "versionNonce": self._seed,
            "isDeleted": False,
            "boundElements": [],
            "updated": 1,
            "link": None,
            "locked": False,
        }

    def _text(self, x, y, text, size, color, align="left", container=None,
              width=None, height=None, vertical="top"):
        lines = text.split("\n")
        if width is None:
            width = max(_text_width(line, size) for line in lines)
        if height is None:
            height = _line_height(size) * len(lines)
        el = self._base("text", x, y, width, height)
        el.update({
            "strokeColor": color,
            "text": text,
            "originalText": text,
            "fontSize": size,
            "fontFamily": 1,
            "textAlign": align,
            "verticalAlign": vertical,
            "containerId": container,
            "lineHeight": 1.25,
            "autoResize": True,
        })
        self.elements.append(el)
        return el

    # -- shapes ------------------------------------------------------------

    def box(self, x, y, w, h, role, label=None, font=BODY, stroke_width=2):
        stroke, fill = PALETTE[role]
        el = self._base("rectangle", x, y, w, h)
        el.update({
            "strokeColor": stroke,
            "backgroundColor": fill,
            "strokeWidth": stroke_width,
            "roundness": {"type": 3},
        })
        self.elements.append(el)
        if label is not None:
            self._bind_label(el, label, font)
        return el

    def _bind_label(self, container, label, font):
        lines = label.split("\n")
        tw = max(_text_width(line, font) for line in lines)
        th = _line_height(font) * len(lines)
        x = container["x"] + (container["width"] - tw) / 2
        y = container["y"] + (container["height"] - th) / 2
        text = self._text(x, y, label, font, INK, align="center",
                          container=container["id"], width=tw, height=th,
                          vertical="middle")
        container["boundElements"] = [{"type": "text", "id": text["id"]}]

    def note(self, text, x, y, size=NOTE, color=INK):
        return self._text(x, y, text, size, color)

    def title(self, text):
        tw = _text_width(text, TITLE)
        self._text((self.width - tw) / 2, 44, text, TITLE, INK)

    def rule(self, x1, y1, x2, y2, color=ARROW_INK):
        """A plain line (no arrowhead), used for charts and callouts."""
        dx = x2 - x1
        dy = y2 - y1
        el = self._base("line", x1, y1, abs(dx), abs(dy))
        el.update({
            "strokeColor": color,
            "backgroundColor": "transparent",
            "roundness": None,
            "points": [[0, 0], [dx, dy]],
            "lastCommittedPoint": None,
            "startBinding": None,
            "endBinding": None,
            "startArrowhead": None,
            "endArrowhead": None,
        })
        self.elements.append(el)
        return el

    def churn(self, x, y, cells, width=18, height=18, role="risk", gap=8):
        """A row of small repeated cells followed by an ellipsis cell."""
        for index in range(cells):
            self.box(x + index * (width + gap), y, width, height, role)
        self.note("...", x + cells * (width + gap) + 2, y - 2, NOTE, MUTED)

    def banner(self, x, y, w, text, role="neutral", font=BODY):
        """A full-width labelled banner (bands, sections, axes headers)."""
        return self.box(x, y, w, 44, role, text, font=font)

    def arrow(self, x1, y1, x2, y2, label=None, label_x=None, label_y=None,
              label_size=NOTE, color=ARROW_INK):
        dx = x2 - x1
        dy = y2 - y1
        el = self._base("arrow", x1, y1, abs(dx), abs(dy))
        el.update({
            "strokeColor": color,
            "backgroundColor": "transparent",
            "roundness": {"type": 2},
            "points": [[0, 0], [dx, dy]],
            "lastCommittedPoint": None,
            "startBinding": None,
            "endBinding": None,
            "startArrowhead": None,
            "endArrowhead": "arrow",
            "elbowed": False,
        })
        self.elements.append(el)
        if label is not None:
            lx = label_x if label_x is not None else x1 + dx * 0.5
            ly = label_y if label_y is not None else y1 + dy * 0.5
            self._text(lx, ly, label, label_size, INK)
        return el

    # -- output ------------------------------------------------------------

    def save(self, out_dir):
        self._validate()
        scene = {
            "type": "excalidraw",
            "version": 2,
            "source": "https://excalidraw.com",
            "elements": self.elements,
            "appState": {
                "viewBackgroundColor": "#ffffff",
                "exportBackground": True,
                "exportWithDarkMode": False,
                "theme": "light",
                "gridModeEnabled": False,
            },
            "files": {},
        }
        path = os.path.join(out_dir, self.name + ".excalidraw")
        with open(path, "w", encoding="utf-8") as handle:
            json.dump(scene, handle, indent=1)
            handle.write("\n")
        return path

    def _validate(self):
        required = {
            "id", "type", "x", "y", "width", "height", "angle", "strokeColor",
            "backgroundColor", "fillStyle", "strokeWidth", "strokeStyle",
            "roughness", "opacity", "groupIds", "frameId", "roundness", "seed",
            "version", "versionNonce", "isDeleted", "boundElements", "updated",
            "link", "locked",
        }
        ids = set()
        for el in self.elements:
            missing = required - set(el)
            assert not missing, (el["id"], missing)
            assert el["id"] not in ids, el["id"]
            ids.add(el["id"])
            assert el["fontFamily"] == 1 if el["type"] == "text" else True
            if el["type"] == "text":
                assert "\u2014" not in el["text"], "em dash in %r" % el["text"]
                bad = set(el["text"]) - GLYPHS - set(
                    " \n\"#$%&'()*+,-./0123456789:;<=>?@"
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`"
                    "abcdefghijklmnopqrstuvwxyz{|}~"
                )
                assert not bad, "unsupported glyphs %r in %r" % (bad, el["text"])


# ---------------------------------------------------------------------------
# Figure 1: object-model
# ---------------------------------------------------------------------------

def figure_object_model(out_dir):
    s = Scene("object-model")
    s.title("Object ids name the bytes, and refs point into the graph")

    s.box(80, 200, 180, 90, "neutral", "HEAD")
    s.box(80, 380, 240, 90, "neutral", "refs/heads/main")
    s.box(440, 300, 260, 130, "compiler", "commit")
    s.box(800, 290, 240, 120, "runtime", "tree")
    s.box(800, 530, 240, 120, "runtime", "subtree")
    s.box(1180, 220, 240, 110, "hip", "blob a.txt")
    s.box(1180, 570, 240, 110, "hip", "blob b.txt")

    # refs point into the graph
    s.arrow(170, 290, 170, 380, "symbolic ref", label_x=190, label_y=318)
    s.arrow(320, 425, 440, 390, "oid", label_x=362, label_y=372)
    s.arrow(700, 375, 800, 350, "tree", label_x=726, label_y=330)
    s.arrow(920, 410, 920, 530, "subtree", label_x=938, label_y=446)
    s.arrow(1040, 350, 1180, 300, "entry", label_x=1072, label_y=288)
    s.arrow(1040, 590, 1180, 620, "entry", label_x=1072, label_y=558)

    # object ids name the bytes
    s.box(400, 690, 820, 170, "neutral",
          "oid = H(type ‖ size ‖ 0x00 ‖ payload)\n"
          "H = SHA-1 (20 B) or SHA-256 (32 B)\nref → oid")
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 2: formats
# ---------------------------------------------------------------------------

def figure_formats(out_dir):
    s = Scene("formats")
    s.title("Formats are byte layouts parameterised by the hash")

    panels = [
        dict(role="compiler", x=60, y=140, header="Loose object",
             record="type SP size NUL payload",
             formulas=["oid = H(bytes)", "H = SHA-1 or SHA-256"],
             caption=".git/objects/xx/yyyy, zlib compressed"),
        dict(role="runtime", x=780, y=140, header="Tree entry",
             record="mode SP name NUL oid[h]",
             formulas=['key = name ‖ ("/" if tree)',
                       "h = 20 B (SHA-1) or 32 B (SHA-256)"],
             caption="payload of the tree object"),
        dict(role="analysis", x=60, y=520, header="Index entry",
             record="header[h] + name + pad",
             formulas=["header = 62 B (SHA-1) or 74 B (SHA-256)",
                       "len(entry) ≡ 0 (mod 8)"],
             caption=".git/index, one entry per path"),
        dict(role="hip", x=780, y=520, header="Pack object",
             record="header MSB ‖ type[3] ‖ size[4]",
             formulas=["base = offset − decode(bytes)",
                       "trailer = H(pack bytes)"],
             caption=".git/objects/pack/*.pack"),
    ]
    for p in panels:
        x, y = p["x"], p["y"]
        w, h = 680, 330
        s.box(x, y, w, h, p["role"])
        s.note(p["header"], x + 26, y + 20, BODY)
        s.box(x + 26, y + 80, w - 52, 84, p["role"], p["record"])
        for i, formula in enumerate(p["formulas"]):
            s.note(formula, x + 26, y + 196 + i * 40, BODY)
        s.note(p["caption"], x + 26, y + 296, NOTE, MUTED)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 3: clone-flow
# ---------------------------------------------------------------------------

def figure_clone_flow(out_dir):
    s = Scene("clone-flow")
    s.title("A transport streams an advertisement and a pack into one store")

    s.note("Remote", 60, 112, NOTE, MUTED)
    s.note("Decode and import", 560, 112, NOTE, MUTED)
    s.note("Local", 1080, 112, NOTE, MUTED)

    # Remote column: the same exchange over HTTP(S), SSH, or git://.
    s.box(60, 180, 300, 100, "model", "GET info/refs")
    s.box(60, 520, 300, 120, "model", "POST upload-pack")
    s.note("HTTP(S) | SSH | git://", 60, 664, NOTE, MUTED)

    # Decode column.
    s.box(560, 190, 340, 110, "runtime", "pkt-line decoder")
    s.box(560, 490, 340, 140, "runtime", "spooled pack +\nstreaming importer")

    # Local column.
    s.box(1080, 180, 320, 110, "neutral", "loose objects")
    s.box(1080, 350, 320, 110, "neutral", "refs + HEAD")
    s.box(1080, 510, 220, 100, "neutral", "index")
    s.box(1080, 680, 320, 110, "neutral", "working tree")

    # Decode and format notes.
    s.note("len(frame) = 4 + len(payload)", 560, 148)
    s.note("H(pack) = trailer", 560, 648)
    s.note("target = applyDelta(base, Δ)", 560, 684)
    s.note("header = 62 B (SHA-1) or 74 B (SHA-256)", 560, 720, NOTE)
    s.note("len(entry) ≡ 0 (mod 8)", 560, 748, NOTE)
    s.note("git status ≡ ∅", 1080, 812, NOTE)

    # The exchange: advertise, want, done, streamed pack.
    s.arrow(360, 230, 560, 230, "advertise refs", label_x=412, label_y=196)
    s.arrow(600, 300, 220, 520, "want oids", label_x=342, label_y=366)
    s.arrow(680, 300, 360, 620, "done", label_x=470, label_y=420)
    s.arrow(360, 585, 560, 555, "streamed pack", label_x=410, label_y=602)

    # Import: write objects and refs, then check out the working tree.
    s.arrow(900, 540, 1080, 235, "write objects", label_x=944, label_y=360)
    s.arrow(900, 600, 1080, 405, "write refs", label_x=944, label_y=490)
    s.arrow(1400, 460, 1400, 680, "checkout", label_x=1290, label_y=638)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 4: modules
# ---------------------------------------------------------------------------

def figure_modules(out_dir):
    s = Scene("modules")
    s.title("Commands share transport, store, and worktree layers")

    s.note("Commands", 60, 104, NOTE, MUTED)
    s.note("Library", 500, 104, NOTE, MUTED)
    s.note("Disk (one store)", 1000, 104, NOTE, MUTED)

    # Command groups.
    s.box(60, 150, 320, 120, "model", "clone, fetch,\npush, pull")
    s.box(60, 330, 320, 120, "model", "checkout, merge,\nrebase, status")
    s.box(60, 510, 320, 120, "model", "inspect, write,\nverify, submodule")

    # Library layers, ordered so that data flows downward without crossing.
    s.box(500, 150, 340, 120, "compiler", "Transport")
    s.box(500, 310, 340, 120, "compiler", "Fetch / Push")
    s.box(500, 470, 340, 110, "analysis", "Store + object,\nindex, ref codecs")
    s.box(500, 630, 340, 120, "runtime", "Worktree +\nMerge / Rebase")

    # One disk store holding everything the tool reads and writes.
    s.box(1000, 150, 440, 600, "neutral",
          "objects and packs\nrefs and HEAD\nindex\nworking tree")

    # Commands to layers.
    s.arrow(380, 210, 500, 210, "URLs", label_x=405, label_y=176)
    s.arrow(380, 390, 500, 690, "trees", label_x=392, label_y=480)
    s.arrow(380, 570, 500, 525, "readObject", label_x=390, label_y=540)

    # Layer to layer.
    s.arrow(670, 270, 670, 310, "pack", label_x=690, label_y=280)
    s.arrow(670, 430, 670, 470, "write", label_x=690, label_y=440)
    s.arrow(840, 630, 840, 580, "index", label_x=858, label_y=600)
    s.arrow(840, 525, 1000, 450, "read/write", label_x=860, label_y=500)

    # Type signatures.
    s.note("Type signatures", 60, 768, NOTE, MUTED)
    s.note("readObject :: Repository → ObjectId → IO GitObject", 60, 794)
    s.note("fetch :: Repository → Remote → IO ()", 60, 828)
    s.note("checkoutTree :: Repository → ObjectId → IO ()", 60, 862)
    return s.save(out_dir)



# ---------------------------------------------------------------------------
# Figure 5: optimization-pipeline
# ---------------------------------------------------------------------------

def figure_optimization_pipeline(out_dir):
    s = Scene("optimization-pipeline")
    s.title("Direct pack retention removes the loose-object round trip")

    s.note("Before: loose round trip (37.1 s)", 60, 104, BODY)
    before = [
        (60, 150, 210, "download pack\n(Transport.hs)"),
        (300, 150, 210, "validate H(pack)\n(Pack/Stream.hs)"),
        (540, 150, 220, "inflate N, write\nN loose objects\n(Pack/Import.hs)"),
        (790, 150, 220, "re-walk pack,\nre-inflate N\n(Pack/Retain.hs)"),
        (1040, 150, 220, "CRC seeks, copy\npack, write idx\n(Pack/Retain.hs)"),
        (1290, 150, 210, "delete N loose,\ncheckout 2x\n(Clone.hs)"),
    ]
    for x, y, w, label in before:
        s.box(x, y, w, 106, "before", label)
    for i in range(len(before) - 1):
        x1 = before[i][0] + before[i][2]
        x2 = before[i + 1][0]
        s.arrow(x1, 203, x2, 203)
    s.churn(556, 268, 6, role="before")
    s.note("N file writes + N reads + N unlinks = O(N) syscalls", 556, 294, NOTE)
    s.note("2N x zlib inflate/deflate, 2 x H(pack)", 60, 334, NOTE)

    s.note("After: direct retention (5.9 s)", 60, 376, BODY)
    after = [
        (60, 430, 210, "download pack\n(Transport.hs)"),
        (300, 430, 210, "validate H(pack)\n(Pack/Stream.hs)"),
        (540, 430, 230, "single walk: inflate,\napply delta, hash,\nCRC, cache"),
        (800, 430, 220, "copy pack,\nwrite v2 index\n(Pack/Retain.hs)"),
        (1050, 430, 220, "parse idx O(N),\ncheckout + index\none blob read"),
    ]
    for x, y, w, label in after:
        s.box(x, y, w, 120, "after", label)
    for i in range(len(after) - 1):
        x1 = after[i][0] + after[i][2]
        x2 = after[i + 1][0]
        s.arrow(x1, 490, x2, 490)
    s.note("0 loose files; one uncompressed cache file U bytes", 540, 560, NOTE)

    s.note("Cost model", 60, 600, BODY)
    s.note("C = |pack| bytes, N = objects, D = delta entries, U = sum of object bytes", 60, 638)
    s.note("T_before = T_net + 6C/BW + 2D/BW_cache + 2N/f + Q", 60, 674)
    s.note("T_after  = T_net + 3C/BW + N/f_read + O(N log N) + U/BW_cache", 60, 710)
    s.note("C reads: before 6, after 4 (validate, walk, CRC, copy); pack write: 1 each", 60, 746)
    s.note("measured: 37.1 s -> 5.9 s local (N = 51988, C = 175 MiB); S = T_old/T_new = 6.3", 60, 782)
    s.note("allocation: about 40 GB -> about 3 GB (strict fields, forced ids, no header copy)", 60, 818)
    s.note("checkout: 2 blob reads per file -> 1; index write joins the same tree walk", 60, 854)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 5b: optimization-cost (stacked phase bars, same scale + zoom)
# ---------------------------------------------------------------------------

def figure_optimization_cost(out_dir):
    s = Scene("optimization-cost")
    s.title("Where 37.1 s went, and where 5.9 s goes")

    scale = 940.0 / 37.63
    origin_x = 210

    def draw_bar(y, phases, bar_scale, height=70):
        x = origin_x
        placed = []
        for name, seconds, role, label_mode in phases:
            width = max(bar_scale * seconds, 3.0)
            s.box(x, y, width, height, role, None)
            placed.append((name, seconds, x, width))
            if label_mode and width >= 44:
                s.note(label_mode, x + 7, y + 10, NOTE, ARROW_INK)
                if width >= 100:
                    s.note(("%.2f s" % seconds), x + 7, y + 36, NOTE, MUTED)
            x += width
        return x, placed

    # Same-scale panel: the point of the first row is that the after bar is tiny.
    s.box(60, 112, 1110, 442, "neutral")
    s.note("same scale (0 to 37.6 s)", 60, 86, NOTE, MUTED)
    axis_y = 132
    s.rule(origin_x, axis_y, origin_x + scale * 37.63, axis_y)
    for tick in [0, 10, 20, 30, 37.63]:
        tx = origin_x + scale * tick
        s.rule(tx, axis_y, tx, axis_y + 8)
        label = ("%.0f s" % tick) if tick < 37.63 else "37.6 s"
        s.note(label, tx - 14, axis_y - 28, NOTE, MUTED)

    s.note("before", 100, 188, BODY, MUTED)
    before_end, _ = draw_bar(
        160,
        [
            ("upload", 0.53, "network", ""),
            ("import", 24.31, "before", "import + 2N loose writes"),
            ("retain", 9.08, "before", "retain re-walk"),
            ("loadStore", 2.28, "measure", ""),
            ("checkout", 1.43, "runtime", ""),
        ],
        scale,
    )
    s.note("37.6 s", before_end + 8, 182, NOTE)
    s.note("thin segments: upload 0.53 | loadStore 2.28 | checkout + idx 1.43", 300, 246, NOTE, MUTED)

    s.note("after", 100, 348, BODY, MUTED)
    after_end, _ = draw_bar(
        320,
        [
            ("upload", 0.50, "network", ""),
            ("retainDirect", 4.40, "after", "retainDirect"),
            ("loadStore", 0.01, "measure", ""),
            ("checkout", 0.72, "runtime", ""),
        ],
        scale,
    )
    s.note("5.63 s", after_end + 8, 342, NOTE)
    s.note("same scale: the whole after bar is 178 px, 15% of the before bar", 300, 406, NOTE, MUTED)
    s.note("upload 0.50 | loadStore 0.01 | checkout + idx 0.72", 300, 436, NOTE, MUTED)

    # Zoomed after bar: composition of the remaining 5.63 s.
    s.box(60, 584, 1110, 238, "neutral")
    s.note("after, zoomed 6.7x (labelled zoom factor)", 60, 558, NOTE, MUTED)
    zoom_scale = 940.0 / 5.63
    zoom_end, _ = draw_bar(
        622,
        [
            ("upload", 0.50, "network", "0.50 s"),
            ("retainDirect", 4.40, "after", "retainDirect: one walk"),
            ("loadStore", 0.01, "measure", ""),
            ("checkout", 0.72, "runtime", "0.72 s"),
        ],
        zoom_scale,
        height=78,
    )
    s.note("5.63 s", zoom_end + 8, 646, NOTE)
    s.note("loadStore 0.01 s is thinner than one pixel at this zoom: bytes live in the index", 300, 716, NOTE, MUTED)
    s.note("retainDirect = 78% of the after bar: inflate + delta + hash + CRC + pack copy", 300, 746, NOTE, MUTED)

    s.note("Evidence: malina, C = 175 MiB pack, N = 51988 objects, U = 254 MiB, local mirror", 60, 838, BODY)
    s.note("H_inflate = U/1.43 s = 178 MiB/s;  H_hash = U/0.78 s = 326 MiB/s", 60, 872, NOTE)
    s.note("C reads: before 6 (1.05 GiB), after 4 (700 MiB); loose file churn before 3C, after 0", 60, 902, NOTE)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 6: thin-pack
# ---------------------------------------------------------------------------

def figure_thin_pack(out_dir):
    s = Scene("thin-pack")
    s.title("Completing a thin pack appends its missing delta bases")

    s.note("Thin pack from the server", 60, 108, BODY)
    s.box(60, 150, 170, 90, "analysis", "header\nN objects")
    s.box(250, 150, 230, 90, "compiler", "full objects\nand offset deltas")
    s.box(500, 150, 250, 90, "risk", "ref delta:\nbase X not in pack")
    s.box(770, 150, 170, 90, "analysis", "trailer\nH(pack)")
    s.arrow(230, 195, 250, 195)
    s.arrow(480, 195, 500, 195)
    s.arrow(750, 195, 770, 195)

    s.box(1010, 140, 320, 130, "neutral", "local store:\nloose objects +\nprevious retained packs")
    s.arrow(750, 240, 1010, 205, "readObject X", label_x=790, label_y=268)

    s.note("B = number of distinct external bases", 60, 286, NOTE)
    s.note("offset_i stays valid: header length = 12 bytes", 60, 314, NOTE)

    s.note("Completed pack: one self-contained pack, same offsets for the original body", 60, 372, BODY)
    s.box(60, 418, 170, 100, "analysis", "header\nN + B objects")
    s.box(250, 418, 300, 100, "compiler", "body copied from\noffset 12")
    s.box(570, 418, 260, 100, "hip", "appended bases\nentry(X_1) ... entry(X_B)")
    s.box(850, 418, 190, 100, "analysis", "new trailer\nH(pack')")
    s.arrow(230, 468, 250, 468)
    s.arrow(550, 468, 570, 468)
    s.arrow(830, 468, 850, 468)
    s.note("pack' = PACK(2, N + B) ‖ P[12 .. C - h] ‖ E(X_1) ‖ ... ‖ E(X_B)", 60, 548)
    s.note("entry(X) = varint(type, |X|) ‖ deflate(X), CRC32 over those bytes", 60, 584)

    s.note("v2 index over the completed pack", 60, 646, BODY)
    s.box(60, 688, 300, 90, "runtime", "fanout + sorted ids\noriginal entries")
    s.box(380, 688, 300, 90, "runtime", "sorted ids + CRC\nappended bases")
    s.box(700, 688, 300, 90, "runtime", "offsets \n32-bit + large table")
    s.box(1020, 688, 300, 90, "analysis", "trailer H(idx.body)")
    s.arrow(360, 733, 380, 733)
    s.arrow(680, 733, 700, 733)
    s.arrow(1000, 733, 1020, 733)

    s.note("before: fetch of a thin pack failed or fell back to loose objects:", 60, 820, NOTE)
    s.note("N loose writes + N unlinks + a second store read; now: one pass, 0 loose objects", 60, 850, NOTE)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 7: pack-math
# ---------------------------------------------------------------------------

def figure_pack_math(out_dir):
    s = Scene("pack-math")
    s.title("Linear index parsing, streaming hashes, and CRC32")

    s.note("Fanout prefix sums", 60, 108, BODY)
    s.note("F[b] = |{ oid : first byte(oid) <= b }|,  F[255] = N", 60, 146)
    s.note("lookup narrows to [F[b-1], F[b]), then binary search: O(log N)", 60, 182)
    for i, label in enumerate(["F[0]", "F[1]", "...", "F[254]", "F[255]"]):
        s.box(60 + i * 138, 226, 118, 60, "runtime", label)
    s.note("old parser: rawOffsets[i] per entry", 60, 314)
    s.note("sum_{i=0}^{N-1} i = N(N-1)/2 = O(N^2);  N = 51988 -> 1.35e9 steps", 60, 350)
    s.note("new parser: BS.index at a computed offset per entry = O(N) = 5.2e4 steps", 60, 386)
    s.note("measured loadStore: 1807 ms -> 10 ms", 60, 422, NOTE)

    s.note("Streaming object id hash", 60, 474, BODY)
    s.note("oid = H(type SP size NUL + payload)", 60, 512)
    s.note("before: header + payload copied to one ByteString: O(|H| + |P|) extra alloc", 60, 548)
    s.note("after: SHA1.init -> update(header) -> update(payload) -> finalize: O(1) extra alloc", 60, 584)
    s.note("saved allocation = sum |P_i| = S_bytes (254 MiB on malina)", 60, 620, NOTE)

    s.note("CRC32 recurrence (reflected, polynomial 0xEDB88320)", 780, 108, BODY)
    s.note("c_(i+1) = (c_i >> 8) xor T[(c_i xor b_i) and 0xFF]", 780, 146)
    s.note("T[k] = eight shifts of k, xor 0xEDB88320 when the low bit is set", 780, 182)
    s.note("CRC = complement(c_n) mod 2^32", 780, 218)
    for i in range(4):
        s.box(780 + i * 170, 262, 150, 64, "analysis", ["T[0]", "T[1]", "...", "T[255]"][i])
    s.note("one 256-entry table, reused for every entry (N x table build saved)", 780, 342, NOTE)
    s.note("v2 index needs CRC32, offset, oid per entry", 780, 386)
    s.note("direct retention captures all three in one walk", 780, 422, NOTE)

    s.note("Object hash", 780, 490, BODY)
    s.note("SHA-1: 512-bit blocks, 80 rounds, 32-bit state;", 780, 528)
    s.note("id = H(header + payload) binds type and size into the name", 780, 564)
    s.note("SHA-256 uses the same streaming update API (object-format=sha256)", 780, 600)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 8: parallel-budget (why the 8-worker experiment was reverted)
# ---------------------------------------------------------------------------

def figure_parallel_budget(out_dir):
    s = Scene("parallel-budget")
    s.title("Why 8 workers bought 7%: Amdahl in the retention walk")

    scale = 640.0 / 0.852  # px per second of retention time
    origin_x = 250

    def retention_bar(y, label, segments, subtitle):
        s.note(label, 60, y + 8, NOTE, MUTED)
        x = origin_x
        for name, seconds, role, text in segments:
            width = max(scale * seconds, 4.0)
            s.box(x, y, width, 58, role, None)
            if width >= 84:
                s.note(name, x + 6, y + 8, NOTE, ARROW_INK)
                s.note(text, x + 6, y + 30, NOTE, MUTED)
            x += width
        s.note(subtitle, 60, y + 76, NOTE, MUTED)
        return x

    s.note("Retention walk on the 24.3 MB synthetic pack (8442 objects)", 60, 104, BODY)
    end = retention_bar(
        140,
        "sequential",
        [
            ("ordered inflate (serial)", 0.49, "before", "0.49 s"),
            ("hash", 0.15, "after", "0.15 s"),
            ("CRC", 0.11, "after", "0.11 s"),
            ("other", 0.10, "neutral", "0.10 s"),
        ],
        "total 0.852 s; the serial 0.49 s inflate is 57.5% of the walk",
    )
    s.note("0.852 s", end + 8, 166, NOTE)

    end2 = retention_bar(
        330,
        "8 workers",
        [
            ("ordered inflate (serial)", 0.49, "before", "0.49 s"),
            ("hash (parallel)", 0.15 / 8, "after", "0.019 s"),
            ("CRC (parallel)", 0.11 / 8, "after", "0.014 s"),
            ("other", 0.10, "neutral", "0.10 s"),
        ],
        "measured 0.771 s; only 0.09 s (9.5%) of the walk was recovered",
    )
    s.note("0.771 s", end2 + 8, 356, NOTE)

    s.note("Amdahl model", 60, 470, BODY)
    s.note("f = T_retain/T_clone = 0.852/1.870 = 45.6%", 60, 508)
    s.note("p = parallelisable/T_retain = (0.15+0.11)/0.852 = 30.5%", 60, 544)
    s.note("S_retain <= 1/(1-p+p/8) = 1.36;   S_clone <= 1/(1-f+f/S_retain) = 1.14", 60, 580)
    s.note("measured: retention 0.852 -> 0.771 s (S=1.10); clone 1.870 -> 1.724 s (S=1.07)", 60, 616)
    s.note("the 10% clone-wall-clock bar needs S >= 1.10; the serial inflate ceiling is 1.15", 60, 652)

    s.note("Why the worker pool could not fix it", 60, 716, BODY)
    s.note("1. inflate is ordered: an offset delta needs its base entry first, so it stays on the main thread;", 60, 754)
    s.note("2. only the object-id hash and the entry CRC are order-independent; together they are 30.5% of the walk;", 60, 786)
    s.note("3. parallel total CPU rose 25% for 0.08 s of wall gain (queue hand-off, payload copies, RTS contention).", 60, 818)
    s.note("Decision: revert the pool, keep the sequential walk, and record the numbers and the ceiling.", 60, 862, NOTE)
    return s.save(out_dir)


FIGURES = [
    figure_object_model,
    figure_formats,
    figure_clone_flow,
    figure_modules,
    figure_optimization_pipeline,
    figure_optimization_cost,
    figure_thin_pack,
    figure_pack_math,
    figure_parallel_budget,
]


def main():
    out_dir = os.path.dirname(os.path.abspath(__file__))
    for figure in FIGURES:
        print("wrote", figure(out_dir))


if __name__ == "__main__":
    main()
