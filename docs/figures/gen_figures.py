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
    s.box(400, 700, 820, 150, "neutral",
          "oid = SHA1(type ‖ size ‖ 0x00 ‖ payload)\nref → oid")
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 2: formats
# ---------------------------------------------------------------------------

def figure_formats(out_dir):
    s = Scene("formats")
    s.title("Four byte layouts define every file the tool reads and writes")

    panels = [
        dict(role="compiler", x=60, y=140, header="Loose object",
             record="type SP size NUL payload",
             formulas=["oid = SHA1(bytes)"],
             caption=".git/objects/xx/yyyy, zlib compressed"),
        dict(role="runtime", x=780, y=140, header="Tree entry",
             record="mode SP name NUL oid[20]",
             formulas=['key = name ‖ ("/" if tree)'],
             caption="payload of the tree object"),
        dict(role="analysis", x=60, y=520, header="Index entry",
             record="62-byte header + name + pad",
             formulas=["len(entry) = (62 + n + 8) & ~7",
                       "len(entry) ≡ 0 (mod 8)"],
             caption=".git/index, one entry per path"),
        dict(role="hip", x=780, y=520, header="Pack object",
             record="header MSB ‖ type[3] ‖ size[4]",
             formulas=["base = offset − decode(bytes)"],
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
        s.note(p["caption"], x + 26, y + 288, NOTE, MUTED)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 3: clone-flow
# ---------------------------------------------------------------------------

def figure_clone_flow(out_dir):
    s = Scene("clone-flow")
    s.title("One Smart HTTP exchange becomes a repository git accepts")

    s.note("Remote", 60, 112, NOTE, MUTED)
    s.note("Decode", 580, 112, NOTE, MUTED)
    s.note("Local", 1100, 112, NOTE, MUTED)

    # Remote column.
    s.box(60, 180, 300, 100, "model", "GET info/refs")
    s.box(60, 520, 300, 120, "model", "POST upload-pack")
    # Decode column.
    s.box(580, 190, 340, 110, "runtime", "pkt-line decoder")
    s.box(580, 500, 340, 150, "runtime", "pack reader +\ndelta resolver")
    # Local column.
    s.box(1100, 180, 320, 110, "neutral", "loose objects")
    s.box(1100, 350, 320, 110, "neutral", "refs + HEAD")
    s.box(1100, 510, 220, 100, "neutral", "index")
    s.box(1100, 680, 320, 110, "neutral", "working tree")

    # Decode formulas.
    s.note("len(frame) = 4 + len(payload)", 580, 148)
    s.note("target = applyDelta(base, Δ)", 580, 670)
    s.note("SHA1(pack) = trailer", 580, 710)
    s.note("len(entry) = (62 + n + 8) & ~7", 580, 750)
    s.note("git status ≡ ∅", 1100, 812, NOTE)

    # The exchange: advertise, want, done, pack.
    s.arrow(360, 230, 580, 230, "advertise refs", label_x=430, label_y=195)
    s.arrow(620, 300, 220, 520, "want oids", label_x=361, label_y=366)
    s.arrow(700, 300, 360, 620, "done", label_x=480, label_y=420)
    s.arrow(360, 585, 580, 570, "raw pack", label_x=430, label_y=600)

    # Import: write objects and refs, then check out the working tree.
    s.arrow(920, 520, 1100, 250, "write", label_x=958, label_y=355)
    s.arrow(920, 610, 1100, 430, "write", label_x=958, label_y=480)
    s.arrow(1380, 460, 1380, 680, "checkout", label_x=1290, label_y=638)
    return s.save(out_dir)


# ---------------------------------------------------------------------------
# Figure 4: modules
# ---------------------------------------------------------------------------

def figure_modules(out_dir):
    s = Scene("modules")
    s.title("Every command goes through one store")

    s.note("Commands", 60, 104, NOTE, MUTED)
    s.note("Library", 500, 104, NOTE, MUTED)
    s.note("Disk (one store)", 980, 104, NOTE, MUTED)

    rows = [140, 305, 470, 635]
    box_h = 115
    commands = ["clone", "inspect commands", "write commands", "verify"]
    library = ["Clone", "Store", "Index and Refs", "Verify"]
    for y, label in zip(rows, commands):
        s.box(60, y, 300, box_h, "model", label)
    for y, label in zip(rows, library):
        if label == "Index and Refs":
            # Narrower, so the Verify to Store arrow can pass on the right.
            s.box(500, y, 240, box_h, "compiler", label)
        else:
            s.box(500, y, 300, box_h, "compiler", label)
    # One disk store holding everything the tool reads and writes.
    s.box(980, 140, 460, 610, "neutral",
          "loose objects and packs\nrefs and HEAD\nindex\nworking tree")

    # Command -> library module.
    s.arrow(360, 197, 500, 197, "clone", label_x=392, label_y=165)
    s.arrow(360, 362, 500, 362, "readObject", label_x=372, label_y=325)
    s.arrow(360, 527, 500, 527, "writeIndex", label_x=372, label_y=492)
    s.arrow(360, 692, 500, 692, "verify", label_x=392, label_y=655)

    # Every module goes through Store.
    s.arrow(700, 255, 700, 305, "stores", label_x=716, label_y=264)
    s.arrow(700, 470, 700, 420, "writes", label_x=716, label_y=431)
    s.arrow(795, 635, 795, 420, "reads", label_x=807, label_y=510)

    # Store -> one repository.
    s.arrow(800, 362, 980, 362, "read/write", label_x=870, label_y=318)

    # Type signatures (body size, as note text).
    s.note("Type signatures", 60, 768, NOTE, MUTED)
    s.note("readObject :: Repository → ObjectId → IO GitObject",
           60, 794)
    s.note("writeIndex :: [Entry] → ByteString", 60, 828)
    s.note("verify :: Repository → IO Report", 60, 862)
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
