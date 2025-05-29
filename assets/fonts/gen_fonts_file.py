import sys
import re
from pathlib import Path
from collections import defaultdict

FONTS_DIR = Path("assets/fonts")
OUTPUT_FILE = sys.argv[1]

def is_numeric(stem: str) -> bool:
    return re.fullmatch(r"\d+", stem) is not None

def flatten_path(path: Path) -> str:
    # Remove extension and flatten path components with underscores
    parts = list(path.parts)
    if parts[0] == "assets":
        parts = parts[2:]  # skip "assets/fonts"
    stem = Path(parts[-1]).stem
    parts[-1] = stem
    parts = [p.replace("-", "_") for p in parts]
    return "_".join(parts)

def get_group_key(path: Path) -> str:
    # group by parent directory relative to sprites
    rel_path = path.relative_to(FONTS_DIR)
    return "_".join(p.replace("-", "_") for p in rel_path.parent.parts)

def build_parent_groups(groups: dict) -> dict:
    # Merge children into parent groups recursively
    merged = defaultdict(list)
    for path_str, idents in groups.items():
        parts = path_str.split("_")
        for i in range(1, len(parts)):
            parent = "_".join(parts[:i])
            merged[parent].extend(idents)
        merged[path_str].extend(idents)
    # Deduplicate
    for key in merged:
        merged[key] = list(dict.fromkeys(merged[key]))
    return merged

def write_sprite_bindings():
    bindings = []
    groups = defaultdict(list)

    for path in sorted(FONTS_DIR.rglob("*.ttf")):
        rel_path = path.as_posix()
        ident = f"{flatten_path(path.relative_to(FONTS_DIR))}_font"

        bindings.append(f'let _{ident} : string * string = "{rel_path}", [%blob "{rel_path}"]')

        group_key = get_group_key(path)
        groups[group_key].append(ident)

    # Create merged parent groups (e.g. ocean = [ocean_regular_001; ...])
    merged_groups = build_parent_groups(groups)

    with open(OUTPUT_FILE, "w") as f:
        f.write("(** Fonts - Auto-generated font blobs *)\n\n")
        for b in sorted(bindings):
            f.write(b + "\n")

if __name__ == "__main__":
    write_sprite_bindings()
