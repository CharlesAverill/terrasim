import os
import zipfile
from pathlib import Path

# Directory containing the zip files (change if needed)
zip_dir = Path(".")
output_dir = Path("..")

# Make output directory if it doesn't exist
output_dir.mkdir(exist_ok=True)

# Get all zip files in the directory
for zip_path in zip_dir.glob("*.zip"):
    filename = zip_path.stem  # e.g., "amoeba-early"
    try:
        _class, stage = filename.rsplit("-", maxsplit=1)
    except ValueError:
        print(f"Skipping malformed filename: {filename}")
        continue

    if stage == "early":
        stage_mul = 0
    elif stage == "middle":
        stage_mul = 1
    elif stage == "late":
        stage_mul = 2
    elif stage == "final":
        stage_mul = 3

    # Create a temporary extract folder
    extract_tmp = output_dir / "__tmp"
    if extract_tmp.exists():
        for f in extract_tmp.iterdir():
            if f.is_file():
                f.unlink()
        extract_tmp.rmdir()
    extract_tmp.mkdir()

    # Extract zip contents to temp directory
    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extractall(extract_tmp)

    # Move files into correct variant directories
    for png_path in sorted(extract_tmp.glob("*.png")):
        idx = int(png_path.stem)  # assumes filenames like 0.png, 1.png, ...
        variant = (idx // 2 + 1) + 4*stage_mul    # variants are grouped in pairs
        dest_dir = output_dir / _class / f"species_{variant}"
        dest_dir.mkdir(parents=True, exist_ok=True)
        png_dest = dest_dir / f"{idx % 2}.png"
        png_path.rename(png_dest)

    # Clean up temp folder
    extract_tmp.rmdir()

