#!/bin/bash

# Find all PNG files with purely numeric base names and rename them with zero-padded names
find . -type f -name "*.png" | while read -r file; do
  dir=$(dirname "$file")
  base=$(basename "$file" .png)

  if [[ "$base" =~ ^[0-9]+$ ]]; then
    newbase=$(printf "%02d" "$base")
    newfile="$dir/$newbase.png"

    if [[ "$file" != "$newfile" ]]; then
      echo "Renaming $file -> $newfile"
      mv "$file" "$newfile"
    fi
  fi
done
