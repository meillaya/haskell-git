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


FIGURES = [
    figure_object_model,
    figure_formats,
    figure_clone_flow,
    figure_modules,
]


def main():
    out_dir = os.path.dirname(os.path.abspath(__file__))
    for figure in FIGURES:
        print("wrote", figure(out_dir))


if __name__ == "__main__":
    main()
