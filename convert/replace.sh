#!/bin/bash

# Set target directory (default to _site)
TARGET_DIR="${1:-_site}"

# Find all .html files inside the directory
find "$TARGET_DIR" -type f -name "*.html" -print0 | while IFS= read -r -d '' file; do
  # Replace ~{super *} with <sup> * </sup> using Perl
  perl -pi -e 's/~\{\s*super\s+(.*?)\s*\}/<sup> \1 <\/sup>/g' "$file"
done
