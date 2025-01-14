# CONVERT SPEECH FILES TO ANONYMOUS PHONATION-ONLY SOUND FILES
# ============================================================
# Written for Praat 6.0.36

# Antoin Eoin Rodgers
# rodgeran@tcd.ie
# Phonetics and speech Laboratory, Trinity College Dublin
# latest update: 02/09/2022
#
# A script for converting speech recordings into anonymised unidentifiable
# sound files with phonatory components only.
#
# The script uses the same bulk processing structure as process_textgrids.
# It exploits Praat's Klaat Synthesiser functions to creates sound waveforms
# which contain only phonatory component of the original sound files.
# This ensures that the relevant sound data is still analysable while also
# metting the ethics criteria that the speech recordings are not
# publically available.

# Load global variables --------------------------------------------------------
live_version = 2
@globalDictionaries
root$ = root_G$ + analysis_G$[analysis_set]

# GET USER INPUT ---------------------------------------------------------------
form Convert speech recordings to anonymous analysable sound files.
    choice corpus_to_analyse 1
        button alignment
        button focus
        button sentence modes
        #button continuation
    optionmenu Analysis_set: 1
        option Analysis set one (original)
        option Analysis set two (STH hypothesis)
        word Tier_with_boundary_information rhythmic
        sentence Save_to_directory C:\Users\antoi\Github\PhD\recordings\Analysis_1_standard
endform

# Get start time in seconds
@seconds: "started"
writeInfoLine:  "CONVERTING SOUND FILES'newline$'===================='newline$'"

# PROCESS USER INPUT -----------------------------------------------------------
# Define/create output directories.
target_dir$ = replace$(replace$(save_to_directory$, "\", "/", 0) + "/", "//",
... "/", 0)
home$ = replace$(replace$(homeDirectory$, "\", "/", 0) + "/",
... "//", "/", 0)

appendInfoLine: "Source directory: ", replace$(root$, home$, "/", 1)
appendInfoLine: "Target directory: ", replace$(target_dir$, home$, "/", 1)
# Get input directory and output file names.
batchFile$ = corpusRef_G$[corpus_to_analyse]
corpus_to_analyse$ = corpusFolder_G$[corpus_to_analyse]


# Define reference tier
ref_tier$ = tier_with_boundary_information$

# GET/DEFINE META DATA ---------------------------------------------------------
# Get-Generate general f0 stats for target corpus
if not fileReadable("GenStats_'batchFile$'.csv")
      appendInfoLine: "Calculating per-speaker F0 means and SDs."
      @getF0Stats: corpus_to_analyse, analysis_set, 0, 1
endif

# Get min-max F0 for each speaker
gen_stats = Read Table from comma-separated file: "GenStats_'batchFile$'.csv"
Formula (column range): "f0_mean", "f0_max", "2^(self/12)"
num_rows = Get number of rows
for i to num_rows
    min_hz[i] = Get value: i, "f0_min"
    max_hz[i] = Get value: i, "f0_max"
endfor
Remove

# Define directory list by individual folders in root directory.
dir_list = Create Strings as tokens: "", " ,"
Insert string: 0, root$ + "/" + "F5"
Insert string: 0, root$ + "/" + "F6"
Insert string: 0, root$ + "/" + "F12"
Insert string: 0, root$ + "/" + "F15"
Insert string: 0, root$ + "/" + "F16"
Insert string: 0, root$ + "/" + "F17"
Insert string: 0, root$ + "/" + "M4"
Insert string: 0, root$ + "/" + "M5"
Insert string: 0, root$ + "/" + "M8"
Insert string: 0, root$ + "/" + "M9"
Insert string: 0, root$ + "/" + "M10"
num_dirs = Get number of strings
for i to num_dirs
    source_dir$[i] = Get string: i
endfor
Remove

# MAIN LOOP --------------------------------------------------------------------
# Processs each directory.
for dir_i to num_dirs
    appendInfoLine: mid$(date$(), 12, 8), " Reading data from directory ",
    ... dir_i, "/", num_dirs, "."

    # Get speaker directory name
    cur_dir$ = source_dir$[dir_i]

    # create target speaker directory
    createDirectory: replace$(cur_dir$, root$ + "/", target_dir$, 1)

    # Get source directory name
    cur_dir$ = cur_dir$ + "/" + corpus_to_analyse$ + "/"

    # create speaker/corpus and peaker/corpus/pitch directories
    cur_target_dir$ = replace$(cur_dir$, root$ + "/", target_dir$, 1) + "/"
    cur_target_pitch_dir$ = replace$(cur_dir$, root$ + "/", target_dir$, 1) +
    ... "/pitch/"
    createDirectory: cur_target_dir$
    createDirectory: cur_target_pitch_dir$

    cur_fileList =  Create Strings as file list: "fileList" + string$(dir_i),
        ... cur_dir$ + "*.TextGrid"
    num_TextGrids = Get number of strings


    # Processs textgrid and pitch files in each directory.
    for j to num_TextGrids
        # Get current textgrid and pitch objects
        selectObject: cur_fileList
        cur_textgrid$ = Get string: j
        cur_sound_addr$ = cur_dir$ + replace$( cur_textgrid$, "TextGrid", "wav",
        ... 1)
        sound_exists =  fileReadable (cur_sound_addr$)


        if sound_exists
            # Get source files
            cur_sound = Read from file: cur_sound_addr$
            cur_textgrid = Read from file: cur_dir$ + cur_textgrid$
            cur_pitch_name$ = cur_dir$ + "pitch/PF_" + selected$ ("TextGrid")
                ... + ".Pitch"
            cur_orig_pitch$ = cur_dir$ + "pitch/PO_" + selected$ ("TextGrid")
                    ... + ".Pitch"
            cur_pitch = Read from file: cur_pitch_name$
            cur_orig_pitch = Read from file: cur_orig_pitch$

            @find_tier("tier_index", cur_textgrid, ref_tier$, 0)
            if !tier_index
                tier_index = 1
            endif

            @anonymise_speech:
                ... cur_sound, min_hz[dir_i], max_hz[dir_i], cur_orig_pitch
            cur_anon_sound = selected()
            @silentEdge: cur_anon_sound, min_hz[dir_i]

            # Save object to source
            selectObject: cur_pitch
            Save as binary file:
                ... cur_target_pitch_dir$ + selected$("Pitch") + ".Pitch"
            selectObject: cur_textgrid
            Save as binary file:
                ... cur_target_dir$ + selected$("TextGrid") + ".TextGrid"
            selectObject: cur_anon_sound
            Save as WAV file:
                ... cur_target_dir$ + selected$("Sound") + ".wav"

            # Remove objects
            removeObject: cur_sound, cur_textgrid, cur_pitch, cur_anon_sound,
                ... cur_orig_pitch
        endif

        if !live_version and j > 5
            exit
        endif
    endfor
    # remove current file list
    removeObject: cur_fileList
endfor

@seconds: "ended"

# Timing Info
if ended < started
    totSecs = ended + 86400 - started
else
    totSecs = ended - started
endif
appendInfoLine: mid$(date$(), 12, 8), " Finished in ", totSecs, " seconds."

exit

# PROCEDURES ------------------------------------------------------------------
procedure find_tier(.output_var$, .textgrid, .ref_tier$, .is_interval)
    # Returns index of tier with .ref_tier$ and target type of .is_interval.
          # the procedure returns the answer with a variable name which is
          # evaluated from the string .output_var$. 0 = tier index with target
          # label and of tier type not found.
          #
          # This tier is the non-recursive equivalent of a function:
          #     '.output_var$' = find_tier(.textgrid, .tier_name$, .tier_type$)

    .original_selection# = selected#()

    '.output_var$' = 0
    selectObject: .textgrid

    .num_tier = Get number of tiers
    .i = 0

    while .i < .num_tier and '.output_var$' = 0
        .i += 1
        .cur_name$ = Get tier name: .i
        if .cur_name$ == .ref_tier$
            .cur_is_interval = Is interval tier: .i
            '.output_var$' = .i * (.cur_is_interval == .is_interval)
        endif
    endwhile

    selectObject: .original_selection#
endproc

procedure seconds(.varName$)
    '.varName$' = number(mid$(date$(), 12, 2)) * 60 * 60
        ... + number(mid$(date$(), 15, 2)) * 60
        ... + number(mid$(date$(), 18, 2))
endproc

procedure silentEdge: .sound, .f0_min
    # creates silences of pre-phrase and post-phrase duration.
    selectObject: .sound
    .sf = Get sampling frequency
    .name$ = selected$("Sound")

    selectObject: .sound
    sil_textgrid = noprogress To TextGrid (silences): .f0_min, 0, -25, 0.1, 0.1, "U", "V"
    sil_table = noprogress Down to Table: "no", 6, "no", "no"
    num_rows = Get number of rows
    for i to num_rows
        selectObject: sil_table
        cur_text$ = Get value: i, "text"
        if cur_text$ == "U"
            cur_t_min = Get value: i, "tmin"
            cur_t_max = Get value: i, "tmax"
            selectObject: .sound
            noprogress Set part to zero: cur_t_min, cur_t_max, "at nearest zero crossing"
        endif
    endfor
    removeObject: sil_textgrid, sil_table

endproc

procedure anonymise_speech: .sound, .f0_min, .f0_max, .pitch_obj
    .floor = round(.f0_min / 1.25)
    .ceiling = round(.f0_max  * 1.25) + 20
    selectObject: .pitch_obj

    .pitch_tier =  noprogress Down to PitchTier


    selectObject: .sound
    .name$ = selected$("Sound")
    .temp_sound = Copy: "temp_sound"
    Scale intensity: 65
    .is_female = (left$(.name$, 1) == "F")
    .klaatgrid = noprogress To KlattGrid (simple): 0.005, 5, 5000 +
        ... .is_female * 500, 0.025, 50, .floor, .ceiling, .f0_min, "yes"
    plusObject: .pitch_tier
    Replace pitch tier
    selectObject: .klaatgrid
    .phonation_1 =  noprogress To Sound (phonation): 44100, "yes", "yes", "yes",
        ... "yes", "yes", "Powers in tiers", "yes", "yes", "yes"
    #.phonation_2 = noprogress Reduce noise:
    #    ... 0, 0, 0.025, 61, 22050, 40, -20, "spectral-subtraction"

    removeObject: .temp_sound, .klaatgrid, .pitch_tier
    selectObject: .phonation_1
    Rename: .name$
endproc



# Include libraries
include procs/getF0Stats.proc
include procs/globalDictionaries.proc
