# Create PhD menu in the Praat menu of the Objects window.
Add menu command: "Objects", "Praat", "--",       "", 0,  ""
Add menu command: "Objects", "Praat", "PhD",    "", 0, ""


# Add plugin Functions
Add menu command: "Objects", "Praat",
              ... "Process TextGrids...", "PhD...", 1,
              ... "process_textgrids.praat"
Add menu command: "Objects", "Praat",
              ... "Corpus Audit...", "PhD...", 1,
              ... "corpus_audit.praat"
Add menu command: "Objects", "Praat",
              ... "Get specific data...", "PhD...", 1,
              ... "get_specific_data.praat"

## Play Neutralized Pitch
Add action command:
... "Sound", 0,
... "TextGrid", 0,
... "", 0,
... " ",
... "", 0,
... ""

Add action command:
... "Sound", 0,
... "TextGrid", 0,
... "", 0,
... "PhD",
... "", 0,
... ""

Add action command:
... "Sound", 0,
... "TextGrid", 0,
... "", 0,
... "Compare contours...",
... "PhD", 0,
... "compare_contours.praat"
