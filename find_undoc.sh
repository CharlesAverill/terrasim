awk '
  # Function to check if line ends with *)
  function ends_with_star_paren(line) {
    return (line ~ /\*\)\s*$/)
  }

  {
    # Check if previous line ended with a doc comment close *)
    # We want doc=1 if previous line ended with *)
    if (ends_with_star_paren(prev_line)) {
      doc=1
    } else if (/^\s*\(\*\*/) {
      # Or if current line starts with a doc comment (for multiline)
      doc=1
    } else {
      doc=0
    }

    if (/^(let)\s/) {
      if (!doc) {
        print FILENAME ":" FNR ": " $0
      }
      # Reset doc after handling let
      doc=0
    }

    prev_line = $0
  }
' $(find . -name "*.ml" -not -path "./_build/*" -not -path "./lib/assets/*" -not -path "./bin/*" )

