import sys
import re
from pathlib import Path
from collections import defaultdict

SPRITES_DIR = Path("assets/sprites")
OUTPUT_FILE = sys.argv[1]

def is_numeric(stem: str) -> bool:
    return re.fullmatch(r"\d+", stem) is not None

def flatten_path(path: Path) -> str:
    # Remove extension and flatten path components with underscores
    parts = list(path.parts)
    if parts[0] == "assets":
        parts = parts[2:]  # skip "assets/sprites"
    stem = Path(parts[-1]).stem
    parts[-1] = stem
    parts = [p.replace("-", "_") for p in parts]
    return "_".join(parts)

def get_group_key(path: Path) -> str:
    # group by parent directory relative to sprites
    rel_path = path.relative_to(SPRITES_DIR)
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

    for path in sorted(SPRITES_DIR.rglob("*.png")):
        if "_zips" in str(path):
            continue
        rel_path = path.as_posix()
        ident = f"{flatten_path(path.relative_to(SPRITES_DIR))}_sprite"

        bindings.append(f'let {ident} : string * string = "{rel_path}", [%blob "{rel_path}"]')

        group_key = get_group_key(path)
        groups[group_key].append(ident)

    # Create merged parent groups (e.g. ocean = [ocean_regular_001; ...])
    merged_groups = build_parent_groups(groups)

    with open(OUTPUT_FILE, "w") as f:
        f.write("(** Auto-generated sprite blobs *)\n\n")
        for b in sorted(bindings):
            f.write(b + "\n")

        f.write("\n(* Grouped sprite lists *)\n")
        for group, idents in sorted(merged_groups.items()):
            if not group:
                continue
            list_name = f"{group}_sprites"
            list_elems = "; ".join(idents)
            f.write(f"let {list_name} : (string * string) list = [{list_elems}]\n")

if __name__ == "__main__":
    write_sprite_bindings()
