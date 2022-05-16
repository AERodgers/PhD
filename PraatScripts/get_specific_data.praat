# OPEN SOUNDS & TEXTGRIDS CONTAINING SPECIFIED PHONOLOGICAL STRUCTURE
# ===================================================================
# Written for Praat 6.0.36

# Antoin Eoin Rodgers
# rodgeran@tcd.ie
# Phonetics and speech Laboratory, Trinity College Dublin
# May 22 2018

# Note this scripts requires the table generated by the "process_textgrids" script in order to work.
# The script will end with an error if the the target text does not exist in any phonological structure.

##############
### USER INPUT
form open textgrids containing given text in phonological analysis
    sentence target_text L*H L%
    optionmenu field 3
        option code
        option sentence
        option phonology
    sentence table_directory C:\Users\antoi\OneDrive\00 Academic\Phonetics and speech\PhD - Derry Intonation\2 Field Recordings\Analysis_1_standard
    sentence table_file a_corpus.Table
#    boolean analyse_one_at_a_time 1
    comment Select tiers to HIDE ("syllable" is necessary)
    boolean ortho 1
    boolean rhythmic 0
    boolean phono 0
    boolean phonetic 1
    boolean vowel 1
    boolean tone 0
    boolean foExtrema 1
    boolean perpLine 1
    boolean maxK 1
    boolean hDur_LDur 1
    boolean comments 0
endform

# create vector of DB field names
db_field$# = {"code","sent","phr_phon"}


# create string of tiers to remove
hide_tiers$ = ""
if ortho = 1
    hide_tiers$ += "ortho "
endif
if rhythmic = 1
    hide_tiers$ += "rhythmic "
endif
if phono = 1
    hide_tiers$ += "phono "
endif
if phonetic = 1
    hide_tiers$ += "phonetic "
endif
if vowel = 1
    hide_tiers$ += "vowel "
endif
if tone = 1
    hide_tiers$ += "tone "
endif
if foExtrema = 1
    hide_tiers$ += "foExtrema "
endif
if perpLine = 1
    hide_tiers$ += "perpLine "
endif
if maxK = 1
    hide_tiers$ += "maxK "
endif
if hDur_LDur = 1
    hide_tiers$ += "HDur LDur "
endif
if comments = 1
    hide_tiers$ += "comments "
endif
last$ = right$(hide_tiers$, 1)
if last$ = " "
    hide_tiers$ = left$(hide_tiers$, length(hide_tiers$) - 1)
endif

#################
### PROCESS FILES

# Get analysis table
Read from file: table_directory$ + "/" + table_file$
batchData = selected()
# get rows matching selection criteria
Extract rows where column (text): db_field$#[field], "contains", target_text$
shortTable = selected()
num_rows = Get number of rows
previousGrid$ = "XKCD_is_a_highly_unlikely_name"
# load sound and textgrid files

writeInfoLine: "Looking for specific text: ", target_text$
duplicates = 0
for i to num_rows
    duplicates += 1
    selectObject: shortTable
    curLocation$ = Get value: i, "location"
    curDrive$ = left$(curLocation$, 3)
    curLocation$ = replace$(
                        ... curLocation$,
                        ... curDrive$,
                        ... left$(table_directory$, 3),
                        ... 1
                        ... )
    curGrid$ = Get value: i, "code"
    # avoid loading duplicate sounds and textgrids
    if curGrid$ != previousGrid$
        Read from file: curLocation$ + curGrid$ + ".TextGrid"
        curGrid = selected ()

        Read from file: curLocation$ + curGrid$ + ".wav"
        Scale intensity: 70
        previousGrid$ = curGrid$
        curSound = selected ()
        appendInfo: newline$, "Looking at: ", curGrid$
        # remove tiers for temporary textgrid, if any have been specified
        if length(hide_tiers$) != 0
            @temp_textgrid: "curGrid", hide_tiers$
            selectObject: temp_textgrid.object
            plusObject: curSound
        else
            selectObject: curGrid
            plusObject: curSound
        endif


		# pause for editting
        Edit
        pauseText$ = "Editting; " + replace$(curGrid$, "_", " ", 0)
        beginPause: pauseText$
            comment: "Go back, forward without saving, or save and forward?"
        edit_choice = endPause: "Back", "Skip", "Save", 3

        # save merged textgrid if any have been specified
        if length(hide_tiers$) != 0
            @merge_textgrids
        endif
        if edit_choice = 3
		    selectObject: curGrid
            Save as text file: curLocation$ + curGrid$ + ".TextGrid"
            appendInfo: " saved."
        elsif edit_choice = 2
            appendInfo: " skipped."
        elsif edit_choice = 1 and i > 1
            i -= (1 + duplicates)
            if i < 0
                i +=1
            endif
            appendInfo: " jumping back."
        else
            appendInfo: " cannot jump back - moving forward"
        endif
	    duplicates = 0
        #Remove Current Objects
        selectObject: curGrid
        plusObject: curSound
        Remove
    endif
endfor

# remove unwanted objects
selectObject: shortTable
plusObject: batchData
Remove


procedure temp_textgrid: .original$, .delete_list$
    # convert  .delete_list$ to array of tiers to be deleted (.delete$[.n] with .n elements)
    .list_length = length(.delete_list$)
    .n = 1
    .prev_start = 1
    for .i to .list_length
        .char$ = mid$(.delete_list$, .i, 1)
        if .char$ = " "
            .delete$[.n] = mid$(.delete_list$, .prev_start, .i - .prev_start)
            .n += 1
            .prev_start = .i + 1
        endif

        if .n = 1
            .delete$[.n] = .delete_list$
        else
            .delete$[.n] = mid$(.delete_list$, .prev_start, .list_length - .prev_start + 1)
        endif
    endfor

    # create a copy of '.original$' and delete target tiers
    selectObject: '.original$'
    .num_tiers = Get number of tiers
    .name$ = selected$("TextGrid")
    .name$ += "_temp"
    Copy: .name$
    .object = selected ()
    for .i to .num_tiers
        .cur_tier = .num_tiers + 1 - .i
        .name_cur$ = Get tier name: .cur_tier
        for .j to .n
            if .delete$[.j] = .name_cur$
                Remove tier: .cur_tier
            endif
        endfor
    endfor
endproc

procedure merge_textgrids
    ### get number of and list of original and temporary tiers
    selectObject: temp_textgrid.object
    .temp_n_tiers = Get number of tiers
    for .i to .temp_n_tiers
        .temp_tier$[.i] = Get tier name: .i
    endfor
    selectObject: 'temp_textgrid.original$'
    .orig_n_tiers = Get number of tiers
    .name$ = selected$("TextGrid")
    for .i to .orig_n_tiers
        .orig_tier$[.i] = Get tier name: .i
    endfor

    ### create 1st tier of merged tier
    selectObject: 'temp_textgrid.original$'
    Extract one tier: 1
    .new = selected()
    if .orig_tier$[1] = .temp_tier$[1]
        selectObject: temp_textgrid.object
        Extract one tier: 1
        .temp_single_tier = selected ()
        plusObject: .new
        Merge
        .newNew =selected()
        Remove tier: 1
        selectObject: .temp_single_tier
        plusObject: .new
        Remove
        .new = .newNew
    endif

    ### merge tiers 2 to .orig_n_tiers
    for .i from 2 to .orig_n_tiers
        .useTemp = 0
        for .j to .temp_n_tiers
            if .orig_tier$[.i] =  .temp_tier$[.j]
                .useTemp = .j
            endif
        endfor
        if .useTemp
            selectObject: temp_textgrid.object
            Extract one tier: .useTemp

        else
            selectObject: 'temp_textgrid.original$'
            Extract one tier: .i
        endif
        .temp_single_tier = selected ()
        plusObject: .new
        Merge
        .newNew =selected()
        selectObject: .temp_single_tier
        plusObject: .new
        Remove
        .new = .newNew
    endfor
    selectObject: 'temp_textgrid.original$'
    plusObject: temp_textgrid.object
    Remove
    'temp_textgrid.original$' = .new
    selectObject: 'temp_textgrid.original$'
    Rename: .name$
endproc
