# Get F0 Stats
# ============
# Gets F0 means and standard deviations for target.
# 20/05/2022
#
# Global dictonaries: global variables suffixes with _G

## UI replacement
form AER PhD: Get F0 Means and Standard Deviation for Sub Corpus
    natural corpus 1
    natural analysis_set 1
    boolean declarative_data_only 1
    boolean save_results 1
    sentence save_directory
endform

# shorten form variables
dec_only = declarative_data_only
save_dir$ = save_directory$

# Load global dictionary.
@globalDictionaries

# if results not to be saved, create temporary file.
if save_results
    reportFile$ = save_dir$ +  "/GenStats_" + corpusRef_G$[corpus] + ".csv"
else
    reportFile$ = "temp.csv"
endif

# Create output file
writeFileLine: reportFile$, "speaker,mean_f0,SD_f0"

# Get mean and SD F0 in ST re 1 Hz for each speaker
for i to speakers_G
    # Set folder address to pitch folder for current speaker.
    folder$ =root_G$ + "/" + analysis_G$[analysis_set] + "/"
        ... + speaker_G$[i] + "/" + corpusFolder_G$[corpus] + "/pitch"

    # Get f0 summary info for current speaker.
    @speakerF0Stats: folder$, dec_only

    # Update output file.
	appendFile: reportFile$,
            ... speaker_G$[i] + "," +
    		... fixed$(speakerF0Stats.meanF0,3) + "," +
    		... fixed$(speakerF0Stats.sdF0,3) + newline$
endfor

# If not saving results, load them into the Praat objects window
if not save_results
    Read Table from comma-separated file: reportFile$
    Rename: "GenStats_" + corpusRef_G$[corpus]
    deleteFile: reportFile$
endif
# PROCEDURES---------------------------------------------------------------------
procedure speakerF0Stats: .folder$, .decOnly
    # decide F0 measurement for analysis

    # Get folder information
    .fileList = Create Strings as file list: "", .folder$
    .files = Get number of strings
    # create output folder
    .tempTable = Create Table with column names: "tempTable", 0, "Frame Time F0"

    # concatenate pitch objects in a table
    for .i to .files
        selectObject: .fileList
        .curFile$ = Get string: .i
        @fileName: .curFile$, "_"
        if left$(fileName.part$[1],1) = "P"
            .mode$ = fileName.part$[3]
        else
            .mode$ = fileName.part$[2]
        endif

        # decide
        if corpus = 3
            if .decOnly
                if cat_G$[.mode$] = "DEC"
                    .include = 1
                else
                    .include = 0
                endif
            endif
        else
            .include = 1
        endif

        if left$(.curFile$, 2) = "PF" and .include
            .curPitch = Read from file: .folder$ + "/" + .curFile$
            @pitch2Table: .curPitch, 0
            .curTable = selected()
            plusObject: .tempTable
            .newTemp = Append
            selectObject: .tempTable
            plusObject: .curPitch
            plusObject: .curTable
            Remove
            .tempTable = .newTemp
        endif
    endfor

    # convert to ST (speaker mean) if specified
    selectObject: .tempTable
    Formula: "F0", "log2(self)*12"


    # get general F0 stats for speaker
    #.minF0 = Get minimum: "F0"
    #.maxF0 = Get maximum: "F0"
    .meanF0 = Get mean: "F0"
    .sdF0 = Get standard deviation: "F0"

    # vvvvv OLD STATS NOT REALLY NEEDED  vvvvvvvvv #

    #.sdMedF0 = Get median absolute deviation: "F0"
    #.q1F0 = Get quantile: "F0", 0.25
    #.medF0 = Get quantile: "F0", 0.5
    #.q3F0 = Get quantile: "F0", 0.75
    #Append column: "DEV_3"
    #Formula: "DEV_3", "(self[""F0""]-.meanF0)^3"
    #.devPWR3  = Get mean: "DEV_3"
    #.dataPoints = Get number of rows
    #.devPWR3 = .devPWR3 * .dataPoints
    #.skew = ((.devPWR3/.dataPoints)/.sdF0^3)*(.dataPoints*(.dataPoints-1))^0.5
    #    ... /(.dataPoints-2)

    # ^^^^^ OLD STATS NOT REALLY NEEDED  ^^^^^ #

    # remove unwanted procedure objects
    selectObject: .tempTable
    plusObject: .fileList
    Remove
endproc

# global.dictionaries.praat ----------------------------------------------------
procedure globalDictionaries
    # NB: this procedure is designed to allow the script to run on different
    #     machines with root directory on different drive.
    #     It is also designed for potential future changes the root directory
    #     identification on different types of machine.

    ### ROOT DIRECTORY
	##################
    root_G$ = "C:\Users\antoi\OneDrive\00 Academic\Phonetics and speech\PhD - Derry Intonation\2 Field Recordings"
    root_G$ = replace$(replace$(root_G$, "\", "/", 0) + "/", "//", "", 0)
    # Get an array of the drives available on the local disk
    .directoryExists = size(fileNames$#(root_G$ + "/")) > 0
    if windows and !.directoryExists
        runSystem: "fsutil fsinfo drives > 'temporarysave_dir$'/f.tmp"
        .drives = Read Strings from raw text file: "'temporarysave_dir$'/f.tmp"
        .drives$ = Get string: 2
        deleteFile: "'temporarysave_dir$'/f.tmp"
        removeObject: .drives
                .drives$ = replace$(.drives$, "Drives: ", "", 1)
        @line2Array: .drives$, " ", "globalDictionaries.drives$"

        original_dir$ = left$(root_G$, index(root_G$, "/"))
        .i = 0
        while .i < .drives_N and !.directoryExists
            .i += 1
            .drives$[.i] = replace$(.drives$[.i], "\", "/", 1)
            root_G$ = replace$(root_G$, original_dir$, .drives$[.i], 1)
            .directoryExists = size(fileNames$#(root_G$ + "/")) > 0
        endwhile

        while !.directoryExists
            root_G$ = chooseFolder$: "Select the root directory."
            root_G$ = replace$(replace$(root_G$, "\", "/", 0) + "/", "//", "", 0)
            .directoryExists = size(fileNames$#(root_G$ + "/")) > 0
        endwhile

    endif

    meanSylDur_M$ = "/M-Corpus_MeanSylDur.Table"
    meanSylDur_A$ = "/A-Corpus_MeanSylDur.Table"
    sylMeanstartT_M$ = "/M-Corpus_sylMeanstartT.Table"
    sylMeanstartT_A$ = "/A-Corpus_sylMeanstartT.Table"
    # analysis folders
    analyses_G = 2
    analysis_G$[1] = "Analysis_1_standard"
    analysis_G$[2] = "Analysis_2_STH"

    ### TEXTGRID TIER NUMBERS
	#########################
    tierName_G$[1] = "ortho"
    tierName_G$[2] = "syllable"
    tierName_G$[3] = "rhythmic"
    tierName_G$[4] = "phono"

    tierName_G["ortho"] = 1
    tierName_G["syllable"] = 2
    tierName_G["rhythmic"] = 3
    tierName_G["phono"] = 4

    ### CORPORA CODES AND DIRECTORY NAMES
	#####################################
    corpora_G = 4
    corpusFolder_G$[1] = "alignment and H_Placement"
    corpusRef_G$[1] = "a_corpus"
    corpusFolder_G$[2] = "focus"
    corpusRef_G$[2] = "F-corpus"
    corpusFolder_G$[3] = "sentence_modes"
    corpusRef_G$[3] = "M-corpus"
    corpusFolder_G$[4] = "continutation"
    corpusRef_G$[4] = "C-corpus"


    #### SPEAKER ARRAYS AND DICTIONARIES
	####################################

    # SPEAKER CODES
    speakers_G = 11
    speaker_G$[1] = "F5"
    speaker_G$[2] = "F6"
    speaker_G$[3] = "F12"
    speaker_G$[4] = "F15"
    speaker_G$[5] = "F16"
    speaker_G$[6] = "F17"
    speaker_G$[7] = "M4"
    speaker_G$[8] = "M5"
    speaker_G$[9] = "M8"
    speaker_G$[10] = "M9"
    speaker_G$[11] = "M10"
    speaker_G$[12] = "Sample"

    # SPEAKER NUMBERS
    speakerNum_G["F5"] = 1
    speakerNum_G["F6"] = 2
    speakerNum_G["F12"] = 3
    speakerNum_G["F15"] = 4
    speakerNum_G["F16"] = 5
    speakerNum_G["F17"] = 6
    speakerNum_G["M4"] = 7
    speakerNum_G["M5"] = 8
    speakerNum_G["M8"] = 9
    speakerNum_G["M9"] = 10
    speakerNum_G["M10"] = 11
    speakerNum_G["Sample"] = 12


    ### M-CORPUS
	############

    # SENTENCE MODE
    cat_G$["MDC1"] = "DEC"
    cat_G$["MDC2"] = "DEC"
    cat_G$["MDC3"] = "DEC"
    cat_G$["MYN1"] = "YNQ"
    cat_G$["MYN2"] = "YNQ"
    cat_G$["MYN3"] = "YNQ"
    cat_G$["MWH1"] = "WHQ"
    cat_G$["MWH2"] = "WHQ"
    cat_G$["MWH3"] = "WHQ"
    cat_G$["MDQ1"] = "DCQ"
    cat_G$["MDQ2"] = "DCQ"
    cat_G$["MDQ3"] = "DCQ"

    # SENTENCE MODE HIERARCHY
    mode_G["DEC"] = 1
    mode_G["WHQ"] = 2
    mode_G["YNQ"] = 3
    mode_G["DCQ"] = 4

    # SENTENCE MODE LONG FORM
    modeLong_G$["DEC"] = "declarative"
    modeLong_G$["WHQ"] = "wh- question"
    modeLong_G$["YNQ"] = "yes/no question"
    modeLong_G$["DCQ"] = "declarative question"

    # TARGET WORD IN NUCLEUS
    nucWord_G$["MDC1"] = "vases"
    nucWord_G$["MDC2"] = "valley"
    nucWord_G$["MDC3"] = "valuables"
    nucWord_G$["MYN1"] = "vases"
    nucWord_G$["MYN2"] = "valley"
    nucWord_G$["MYN3"] = "valuables"
    nucWord_G$["MWH1"] = "vases"
    nucWord_G$["MWH2"] = "valley"
    nucWord_G$["MWH3"] = "valuables"
    nucWord_G$["MDQ1"] = "vases"
    nucWord_G$["MDQ2"] = "valley"
    nucWord_G$["MDQ3"] = "valuables"

    # CODE FOR TARGET WORD IN NUCLEUS
    word_G["vases"] = 1
    word_G["valley"] = 2
    word_G["valuables"] = 3

    word_G$[1] = "vases"
    word_G$[2] = "valley"
    word_G$[3] = "valuables"

    ### A-CORPUS / H-CORPUS
	#######################

    # SYLLABLES OF ANACRUSIS
    ana_G["A01"] = 0
    ana_G["A1422"] = 1
    ana_G["A2422"] = 2
    ana_G["A3422"] = 3
    ana_G["A0131"] = 0
    ana_G["A0221"] = 0
    ana_G["A0321"] = 0
    ana_G["A0423"] = 0
    ana_G["A1111"] = 1
    ana_G["A1211"] = 1
    ana_G["A11"] = 0
    ana_G["A12"] = 0
    ana_G["A13"] = 1
    ana_G["A14"] = 0
    ana_G["A1231"] = 1
    ana_G["A1241"] = 0

    ana_G["H0322"] = 0
    ana_G["H0421"] = 0
    ana_G["H0422"] = 0
    ana_G["H1322"] = 1
    ana_G["H1321"] = 1

    # SYLLABLES IN PRE-NUCLEAR FOOT
    pnSyls_G["A01"] = 4
    pnSyls_G["A1422"] = 4
    pnSyls_G["A2422"] = 4
    pnSyls_G["A3422"] = 1
    pnSyls_G["A0131"] = 2
    pnSyls_G["A0221"] = 3
    pnSyls_G["A0321"] = 3
    pnSyls_G["A0423"] = 4
    pnSyls_G["A1111"] = 1
    pnSyls_G["A1211"] = 2
    pnSyls_G["A11"] = 3
    pnSyls_G["A12"] = 4
    pnSyls_G["A13"] = 2
    pnSyls_G["A14"] = 2
    pnSyls_G["A1231"] = 2
    pnSyls_G["A1241"] = 2

    pnSyls_G["H0322"] = 3
    pnSyls_G["H0421"] = 4
    pnSyls_G["H0422"] = 4
    pnSyls_G["H1322"] = 3
    pnSyls_G["H1321"] = 3

    # PN target stressed Syllable onset
    pnStrOn_G$["A1422"] = "v"
    pnStrOn_G$["A2422"] = "v"
    pnStrOn_G$["A3422"] = "v"
    pnStrOn_G$["A0131"] = "v"
    pnStrOn_G$["A0221"] = "v"
    pnStrOn_G$["A0321"] = "v"
    pnStrOn_G$["A0423"] = "v"
    pnStrOn_G$["A1111"] = "n"
    pnStrOn_G$["A1211"] = "l"
    pnStrOn_G$["A1231"] = "l"
    pnStrOn_G$["A1241"] = "n"

	pnStrOn_G$["H0322"] = "l"
	pnStrOn_G$["H0421"] = "v"
	pnStrOn_G$["H0422"] = "l"
	pnStrOn_G$["H1322"] = "l"
	pnStrOn_G$["H1321"] = "l"

    # PN target stressed Syllable rhyme
    pnStrRhy_G$["A1422"] = "al"
    pnStrRhy_G$["A2422"] = "al"
    pnStrRhy_G$["A3422"] = "al"
    pnStrRhy_G$["A0131"] = "alz"
    pnStrRhy_G$["A0221"] = "alz"
    pnStrRhy_G$["A0321"] = "alz"
    pnStrRhy_G$["A0423"] = "al"
    pnStrRhy_G$["A1111"] = "oU"
    pnStrRhy_G$["A1211"] = "Iv"
    pnStrRhy_G$["A1231"] = "Iv"
    pnStrRhy_G$["A1241"] = "id"

	pnStrRhy_G$["H0322"] = "al"
	pnStrRhy_G$["H0421"] = "al"
	pnStrRhy_G$["H0422"] = "al"
	pnStrRhy_G$["H1322"] = "eIn"
	pnStrRhy_G$["H1321"] = "eIn"

    # NUC target stressed Syllable onset
    nucStrOn_G$["A1422"] = "r"
    nucStrOn_G$["A2422"] = "r"
    nucStrOn_G$["A3422"] = "r"
    nucStrOn_G$["A0131"] = "v"
    nucStrOn_G$["A0221"] = "v"
    nucStrOn_G$["A0321"] = "v"
    nucStrOn_G$["A0423"] = "v"
    nucStrOn_G$["A1111"] = "v"
    nucStrOn_G$["A1211"] = "v"
    nucStrOn_G$["A1231"] = "v"
    nucStrOn_G$["A1241"] = "v"

	nucStrOn_G$["H0322"] = "v"
	nucStrOn_G$["H0421"] = "l"
	nucStrOn_G$["H0422"] = "v"
	nucStrOn_G$["H1322"] = "n"
	nucStrOn_G$["H1321"] = "n"

    # NUC target stressed Syllable rhyme
    nucStrRhy_G$["A1422"] = "Iv"
    nucStrRhy_G$["A2422"] = "Iv"
    nucStrRhy_G$["A3422"] = "Iv"
    nucStrRhy_G$["A0131"] = "al"
    nucStrRhy_G$["A0221"] = "al"
    nucStrRhy_G$["A0321"] = "al"
    nucStrRhy_G$["A0423"] = "al"
    nucStrRhy_G$["A1111"] = "al"
    nucStrRhy_G$["A1211"] = "al"
    nucStrRhy_G$["A1231"] = "al"
    nucStrRhy_G$["A1241"] = "al"

	nucStrRhy_G$["H0322"] = "al"
	nucStrRhy_G$["H0421"] = "al"
	nucStrRhy_G$["H0422"] = "al"
	nucStrRhy_G$["H1322"] = "an"
	nucStrRhy_G$["H1321"] = "an"


    # UNSTRESSED SYLLABLES BEFORE NUCLEUS
    nucPreSyls_G["A01"] = 3
    nucPreSyls_G["A1422"] = 3
    nucPreSyls_G["A2422"] = 3
    nucPreSyls_G["A3422"] = 0
    nucPreSyls_G["A0131"] = 1
    nucPreSyls_G["A0221"] = 2
    nucPreSyls_G["A0321"] = 2
    nucPreSyls_G["A0423"] = 3
    nucPreSyls_G["A1111"] = 0
    nucPreSyls_G["A1211"] = 1
    nucPreSyls_G["A11"] = 2
    nucPreSyls_G["A12"] = 3
    nucPreSyls_G["A13"] = 1
    nucPreSyls_G["A14"] = 1
    nucPreSyls_G["A1231"] = 1
    nucPreSyls_G["A1241"] = 1

    nucPreSyls_G["H0322"] = 2
    nucPreSyls_G["H0422"] = 3
    nucPreSyls_G["H0421"] = 3
    nucPreSyls_G["H1322"] = 2
    nucPreSyls_G["H1321"] = 2

    # SYLLABLES IN NUCLEAR FOOT
    nucSyls_G["A01"] = 2
    nucSyls_G["A1422"] = 2
    nucSyls_G["A2422"] = 2
    nucSyls_G["A3422"] = 2
    nucSyls_G["A0131"] = 3
    nucSyls_G["A0221"] = 2
    nucSyls_G["A0321"] = 2
    nucSyls_G["A0423"] = 2
    nucSyls_G["A1111"] = 1
    nucSyls_G["A1211"] = 1
    nucSyls_G["A11"] = 2
    nucSyls_G["A12"] = 2
    nucSyls_G["A13"] = 1
    nucSyls_G["A14"] = 2
    nucSyls_G["A1231"] = 3
    nucSyls_G["A1241"] = 4

    nucSyls_G["H0322"] = 2
    nucSyls_G["H0421"] = 2
    nucSyls_G["H0422"] = 2
    nucSyls_G["H1322"] = 2
    nucSyls_G["H1321"] = 2

    ### H-CORPUS ONLY
    ##################

    # FINAL SYLLABLE OF WORD WITH LEXICAL STRESS IN PN FOOT
    pnWordEnd_G["A0321"] = 1
    pnWordEnd_G["H0322"] = 2
    pnWordEnd_G["H0421"] = 1
    pnWordEnd_G["H0422"] = 2
    pnWordEnd_G["A0423"] = 3
    pnWordEnd_G["H1322"] = 2
    pnWordEnd_G["H1321"] = 1

    # DOES FIRST SYLLABLE OF WORD WITH LEXICAL STRESS START IN ANACRUSIS
    pnWordStart_G["A0321"] = 0
    pnWordStart_G["H0322"] = 0
    pnWordStart_G["H0421"] = 0
    pnWordStart_G["H0422"] = 0
    pnWordStart_G["A0423"] = 0
    pnWordStart_G["H1322"] = 1
    pnWordStart_G["H1321"] = 1

    ### F-CORPUS
    #############

    # FOCUS FOOT
    focusType["FN3"] = 3
    focusType["FN1"] = 1
    focusType["FN2"] = 2
    focusType["FN0"] = 0

    focusType$["FN3"] = "NF-Val"
    focusType$["FN1"] = "NF-Dad"
    focusType$["FN2"] = "NF-Liv"
    focusType$["FN0"] = "BF"
endproc

procedure fileName: .file_name$, .sep$
    ## get length of filename string and index of <dot> suffix if it exists
    .length = length(.file_name$)
    .period = rindex(.file_name$, ".")
    ## get name of suffix, and fix value of .period if no file suffix
    if .period = 0
        .period = .length + 1
        .suffix$ = ""
    else
        .suffix$ = right$ (.file_name$, .length - .period)
    endif
    .name_end = .period - 1

    ## assume at least one part of file name and set first separator index to 1
    .parts = 1
    .sep[1] = 0
	## find separators in file name
    for .i to .name_end
        .char_cur$ = mid$(.file_name$, .i, 1)
            ## get text for each part before between separators
            if  .char_cur$ = .sep$
                .sep[.parts + 1] = .i
                .part$[.parts ] =
                 ... mid$ (.file_name$, .sep[.parts] + 1,
                 ... .sep[.parts + 1] - .sep[.parts] - 1)
                .parts += 1
            endif
    endfor
	## get text for final part
    .part$[.parts] =
        ... mid$ (.file_name$, .sep[.parts] + 1, .name_end  - .sep[.parts])
    .sep[.parts + 1] = .name_end + 1
endproc

procedure pitch2Table: .pitchObject, .interpolate
    selectObject: .pitchObject

    if .interpolate
        .pitchObject = Interpolate
    endif
    .originalObject = .pitchObject

    # Get key pitch data
    .frameTimeFirst = Get time from frame number: 1
    .timeStep = Get time step

    #create pitch Table (remove temp objects)
    .pitchTier = Down to PitchTier
    .tableofReal = Down to TableOfReal: "Hertz"
    .pitchTable = To Table: "rowLabel"
    selectObject: .pitchTier
    plusObject: .tableofReal
    Remove

    # Get key pitchTable data
    selectObject: .pitchTable
    .rows = Get number of rows
    .rowTimeFirst = Get value: 1, "Time"

    # estimate frame of first row
    Set column label (index): 1, "Frame"
    for .n to .rows
        .rowTimeN = Get value: .n, "Time"
        .tableFrameN = round((.rowTimeN - .frameTimeFirst) / .timeStep + 1)
        Set numeric value: .n, "Frame", .tableFrameN
    endfor

    #removeInterpolated pitch
    if     .originalObject != .pitchObject
        selectObject: .pitchObject
        Remove
    endif
	.table = .pitchTable
endproc

procedure line2Array: .string$, .sep$, .out$
    # correct variable name Strings
    if right$(.out$, 1) != "$"
        .out$ += "$"
    endif
    .size$ = replace$(.out$, "$", "_N", 0)

    # fix input csvLine array
    .string$ = replace$(.string$, "'.sep$' ", .sep$, 0)
    while index(.string$, "  ")
        .string$ = replace$(.string$, "  ", " ", 0)
    endwhile
    .string$ = replace_regex$ (.string$, "^[ \t\r\n]+|[ \t\r\n]+$", "", 0)
    .string$ += .sep$
    # generate output array
    '.size$' = 0
    while length(.string$) > 0
        '.size$' += 1
        .nextElementEnds = index(.string$, .sep$)
        '.out$'['.size$'] = left$(.string$, .nextElementEnds)
        .string$ = replace$(.string$, '.out$'['.size$'], "", 1)
        '.out$'['.size$'] = replace$('.out$'['.size$'], .sep$, "", 1)
        if '.out$'['.size$'] = ""
            '.size$' -= 1
        endif
    endwhile
endproc
