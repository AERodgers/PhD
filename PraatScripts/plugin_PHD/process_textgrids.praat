# ANALYSIS OF TEXTGRIDS AND PITCH CONTOURS V.2.0.9
# ================================================
# Written for Praat 6.0.36

# Antoin Eoin Rodgers
# rodgeran@tcd.ie
# Phonetics and speech Laboratory, Trinity College Dublin
# latest update: 29/08/2022

# Script Purpose
# This script is designed to extract data from pre-annotated textgrids
# (and pitch tier objects) in specified directories. It is intended to be used
# after:
#     1. annotating the textgrids created by the "create_more_tiers" script; or
#     2. running the "acoustic_annotations" script
# It requires a list of all the target directories saved in a .txt file

# Version 2 Notes
# V2 has been written to minimise the amount of number crunching done in praat,
# with the burden of this being done in R instead.
# This is to facilitiate transparency.
#
# main changes:
#     1. Table columns renamed so rightmost text states measurement type.
#     2. Unused columns no longer included in table, but still calculated in
#        script; e.g., rhythm normalised times no longer saved to table.
#     3. All actual times output re utterance start (i.e. from 0)
#     4. V.2.0.1: Updated syl_normT estimations - each of these is now
#                 normalised to a grand mean syllable time in ms for each
#                 stim_metre pairing.
#                 Added v/l/h_syl_ratio column to show target as a proportion
#                 of the syllable.
#     5. V.2.0.2: Removed "include allProcs.praat" line, and replaced it with
#                 relevant procedures from libraries. (Makes script portable.)
#     6. V.2.0.3: Updated @globalDictionaries to work on different machines
#     7. V.2.0.4: Reduced likelihood of undefined pitch values being returned.
#     8. V.2.0.5: Added per-speaker F0 mean and SD re 1 ST to output table.
#                 Added form option for save directory.
#     8. V.2.0.6: Clearer variables.
#                 Now calculates grand mean syllable time for e_t and s_t
#                 removed unneeded columns from output table
#     9. V.2.0.7: Clearer variables.
#                 corrected slope calculation error
#                 now collected utterance and PA slope in ST / sec and
#                 in z-score / sec using speaker f0 mean and SD.
#    10. V.2.0.8: Re-introduced "h_syl_ratio" for word boundary analysis
#    10. V.2.0.9: Re-introduced "f0_min" and "f0_min" speaker stats

## Load global variables

live_version = 2
# 1 = no saves
# 2 = accesses original sound files in private location
# 3 = access github file
@globalDictionaries

appendInfoLine: root_G$
root$ = root_G$ +  analysis_G$[analysis_set]

# GET USER INPUT ---------------------------------------------------------------
form Analysis of TextGrids and Pitch contours
    choice corpus_to_analyse 1
        button alignment
        button focus
        button sentence modes
        #button continuation
    optionmenu Analysis_set: 1
        option Analysis set one (original)
        option Analysis set two (STH hypothesis)
        sentence Save_to_directory C:\Users\antoi\Github\PhD\Ch_6_Form\data
endform

# Get start time in seconds
@seconds: "started"

writeInfoLine:  "PROCESSING TEXTGRIDS'newline$'===================='newline$'"

saveToDir$ = replace$(replace$(save_to_directory$, "\", "/", 0) + "/",
                  ... "//",
                  ... "/",
                  ... 0)

# PROCESS USER INPUT
# Get input directory and output file names.
batchFile$ = corpusRef_G$[corpus_to_analyse]
corpus_to_analyse$ = corpusFolder_G$[corpus_to_analyse]

if not fileReadable("GenStats_'batchFile$'.csv")
    appendInfoLine: "Calculating per-speaker F0 means and SDs."
    @getF0Stats: corpus_to_analyse, analysis_set, 0, 1
endif
gen_stats = Read Table from comma-separated file: "GenStats_'batchFile$'.csv"


# DEFINE KEY VARIABLES ---------------------------------------------------------
corpusArchiveDir$ = "_Corpus archive"

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

# Define tier names (previously part of UI form in V.1).
orthographic$ = "ortho"
syllabic$ = "syllable"
rhythmic$ = "rhythmic"
phonological$ = "phono"
vowel_info$ = "vowel"
tone$ = "tone"
high_tone_duration$ = "HDur"
low_tone_duration$ = "LDur"

# Define/create output directories.
@date
appendInfoLine: root$
createDirectory: root$ + "/" +  corpusArchiveDir$
outputFileAddressArchive$ = root$ + "/" + corpusArchiveDir$ + "/"
                      ... + batchFile$ + "_" + (date.index$) + ".csv"
outputFileAddress$ = root$ + "/" +  batchFile$ + ".Table"

# Create empty output table.
id_data$ = "code speaker gender stim rep sent metre_ID stim_metre "
grid_basics$ = "tot_syls ana_syls "
    ... + "tot_feet cur_foot foot_syls wrd_end_syl "
    ... + "acc_phon phr_phon init_phon fin_phon v_text "
grid_times$ = "phr_start_t phr_end_t ana_end_t "
    ... + "foot_start_t foot_end_t stress_end_t wrd_fin_syl_start_t wrd_end_t "
    ... + "v_onset_t v_offset_t "
alignment_data$ = "s_t e_t l_t h_t "
    ... + "l_syl_start_t l_syl_end_t "
    ... + "h_syl_start_t h_syl_end_t "
    ... + "v_grand_mean_t l_grand_mean_t h_grand_mean_t "
    ... + "s_grand_mean_t e_grand_mean_t "
    ... + "v_syl l_syl h_syl s_syl e_syl "
    ... + "v_syl_ratio l_syl_ratio h_syl_ratio s_syl_ratio e_syl_ratio "
f0_data$ =
    ... "s_f0 e_f0 v_onset_f0 l_f0 h_f0 "
    ... + "lh_slope lh_slope_z intercept_st lh_mean_f0 lh_med_f0 "
    ... + "utt_mean_f0 utt_slope utt_slope_z "
    ... + "spkr_f0_mean spkr_f0_SD spkr_f0_min spkr_f0_med "


output_table = Create Table with column names: "output", 0,
    ... id_data$
    ... + grid_basics$
    ... + grid_times$
    ... + alignment_data$
    ... + f0_data$
    ... + "location"

# PROCESS TEXTGRIDS AND PITCH FILES -------------------------------------------

# Processs each directory.
for dir_i to num_dirs
    appendInfoLine: mid$(date$(), 12, 8), " Reading data from directory ",
    ... dir_i, "/", num_dirs, "."
    selectObject: dir_list
    cur_dir$ = Get string: dir_i
    cur_dir$ = cur_dir$ + "/" + corpus_to_analyse$ + "/"
    cur_fileList =  Create Strings as file list: "fileList" + string$(dir_i),
        ... cur_dir$ + "*.TextGrid"
    num_TextGrids = Get number of strings

    # Processs textgrid and pitch files in each directory.
    for j to num_TextGrids
        # Get current textgrid and pitch objects
        selectObject: cur_fileList
        cur_TextGrid$ = Get string: j
        cur_sound = Read from file: cur_dir$ + replace$(
                                        ... cur_TextGrid$, "TextGrid", "wav", 1
                                        ... )
        cur_textGrid = Read from file: cur_dir$ + cur_TextGrid$

        cur_address$ = replace$(cur_dir$, ".textGrid", "", 0)
        cur_PitchName$ = cur_dir$ + "pitch/PF_" + selected$ ("TextGrid")
            ... + ".Pitch"
        cur_pitch = Read from file: cur_PitchName$

        # Get tier numbers for current grid (non-existent tier = 0)
        @getTierIndices: cur_textGrid

        # Process reference data.
        selectObject: cur_textGrid
        cur_textGrid$ = selected$("TextGrid")
        mrk1 = index (cur_textGrid$, "_")
        mrk2 = rindex (cur_textGrid$, "_")
        speaker$ = left$(cur_textGrid$, mrk1 - 1)
        gender$ = left$(cur_textGrid$, 1)
        stim$ = mid$(cur_textGrid$, mrk1 + 1, mrk2 - mrk1 - 1)
        rep$ = right$(cur_textGrid$, length(cur_textGrid$) - mrk2)

        # Process tiers.
        @processRhythmTier: cur_textGrid
        @processSyllableTier: cur_textGrid
        @processPhonoTier: cur_textGrid
        @processOrthoTier: cur_textGrid
        @processVowelTier: cur_textGrid, cur_sound, cur_pitch
        @processToneTier: cur_textGrid, cur_sound, cur_pitch,
                      ... speaker$, gen_stats
        # Calculate alignment info
        @calculateAlignmentData

        @populateTable


        # remove current pitch, textgrid, tier and table objects
        removeObject: {cur_textGrid, cur_sound, cur_pitch, syl_tier,syl_table,
                   ... rhythm_tier, rhy_table, phono_tier, phono_table,
                   ... ortho_tier, ortho_table, vowel_tier, vowel_table,
                   ... tone_tier, tone_table}

    endfor

    # remove current file list
    selectObject: cur_fileList
    Remove
endfor

# OUTPUT -----------------------------------------------------------------------

# Tidy table
appendInfoLine: mid$(date$(), 12, 8),
    ... " Calculating grand-mean syllable-normalised times."

# Convert syllable-normalised time to grand-mean syllable-normalised time
@grandMeanSylTime: output_table, root$, corpusRef_G$[corpus_to_analyse]

# Convert times to ms using phr_start_t as t=0. Couldn't do this tidily in R!
selectObject: output_table
Formula (column range): "phr_end_t", "h_syl_end_t",
    ... "fixed$((self - self[""phr_start_t""]) * 1000, 0)"
Remove column: "phr_start_t"
Formula (column range): "v_grand_mean_t", "e_grand_mean_t",
                    ... "fixed$(self * 1000, 0)"

# convert F0 to ST re 1 Hz using. Couldn't do this tidily in R!
Formula (column range): "s_f0", "h_f0", "fixed$(12 * log2(self), 2)"
Formula (column range): "lh_mean_f0", "lh_med_f0", "fixed$(self, 2)"

# round syllable ratio values to two decimal places
Formula (column range): "v_syl_ratio", "e_syl_ratio", "fixed$(self, 2)"

# Add per-speaker F0 and SD values.
@addGenF0Stats: output_table, gen_stats
removeObject: gen_stats

# Add info columns indentifying ana_syls_text and nuc_pre_text
@addAnaNucPreText: output_table

# remove unneeded columns
cols_to_remove$# = {"s_syl", "e_syl",
... "v_syl_ratio", "s_syl_ratio", "e_syl_ratio",
... "intercept_st"}

selectObject: output_table
for i to  size(cols_to_remove$#)
    Remove column: cols_to_remove$#[i]
endfor

# Calculate end time before user intervention.
@seconds: "ended"

# Save data
appendInfoLine: mid$(date$(), 12, 8), " Saving batch data."
selectObject: output_table

if live_version
    Save as comma-separated file: outputFileAddressArchive$
    Save as tab-separated file: outputFileAddress$
    Save as comma-separated file: saveToDir$ + batchFile$ + ".csv"

    # Remove remaining objects
    selectObject: dir_list
    plusObject: output_table
    plusObject: dir_list
    Remove
endif
# Timing Info
if ended < started
    totSecs = ended + 86400 - started
else
    totSecs = ended - started
endif
appendInfoLine: mid$(date$(), 12, 8), " Finished in ", totSecs, " seconds."

exit

# PROCEDURES -------------------------------------------------------------------
procedure getTierIndices: .textgrid
    selectObject: .textgrid
    ortho_tier_num = 0
    syl_tier_num  = 0
    rhythm_tier_num = 0
    phono_tier_num = 0
    vowel_tier_num = 0
    tone_tier_num = 0
    h_dur_tier_num = 0
    l_dur_tier_num = 0
    # get information about existing tiers
    num_tiers = Get number of tiers
    for .i to num_tiers
        tier_name$ = Get tier name: .i
        if tier_name$ = syllabic$
            @checkForValidTier: .textgrid, .i, "I"
            syl_tier_num = result
        elsif tier_name$ = orthographic$
            @checkForValidTier: .textgrid, .i, "I"
            ortho_tier_num = result
        elsif tier_name$ = rhythmic$
            @checkForValidTier: .textgrid, .i, "P"
            rhythm_tier_num = result
        elsif tier_name$ = phonological$
            @checkForValidTier: .textgrid, .i, "I"
            phono_tier_num = result
        elsif tier_name$ = vowel_info$
            @checkForValidTier: .textgrid, .i, "I"
            vowel_tier_num = result
        elsif tier_name$ = tone$
            @checkForValidTier: .textgrid, .i, "P"
            tone_tier_num = result
        elsif tier_name$ = high_tone_duration$
            @checkForValidTier: .textgrid, .i, "I"
            h_dur_tier_num = result
        elsif tier_name$ = low_tone_duration$
            @checkForValidTier: .textgrid, .i, "I"
            l_dur_tier_num = result
        endif
    endfor
endproc

procedure checkForValidTier: .textgrid, .inputTierNum, .tierType$
    selectObject: .textgrid
    if left$(.tierType$, 1) = "i" or left$(.tierType$, 1) = "I"
        .emptyTest = Get number of intervals: .inputTierNum
    else
        .emptyTest = Get number of points: .inputTierNum
    endif
    .emptyTest = .emptyTest > 0
    result = .inputTierNum * .emptyTest
endproc

procedure processRhythmTier: .textGrid
    #convert rhythm tier to table
    selectObject: .textGrid
    Extract one tier: rhythm_tier_num
    rhythm_tier = selected()
    Down to Table: "no", 3, "no", "no"
    rhy_table = selected()

    selectObject: rhy_table
    num_rows = Get number of rows

    # get phrase start and end
    phr_start = Get value: 1, "tmin"
    phr_end = Get value: num_rows, "tmin"
    tot_feet = 0
    boundaries = 0

    # get start time of each foot: foot_start[#]
    # get duration of each stressed syllable:  foot_stress_dur[#]
    for .i to num_rows
        rhy_time_cur = Get value: .i, "tmin"
        rhy_text_cur$ = Get value: .i, "text"
        # remove accidental table
        rhy_text_cur$ =
            ... replace$(replace$(rhy_text_cur$, tab$, "", 0), newline$, "", 0)
        for .j to length(rhy_text_cur$)
            char_cur$= mid$ (rhy_text_cur$, .j, 1)
            if char_cur$ = "<"
                tot_feet += 1
                foot_start[tot_feet] = rhy_time_cur
                stress_end[tot_feet] = Get value: .i + 1, "tmin"
                foot_stress_dur[tot_feet] = stress_end[tot_feet] - rhy_time_cur
                foot_start[tot_feet] = rhy_time_cur
            elsif char_cur$ = "%"
                boundaries += 1
                if boundaries = 1
                    init_phono$ = replace$(rhy_text_cur$, "<", "", 0)
                else
                    fin_phono$ =  replace$(rhy_text_cur$, ">", "", 0)
                endif
            endif
        endfor
    endfor

    # get duration of each foot: foot_dur[#]
    for .i to tot_feet - 1
        foot_dur[.i] = foot_start[.i+1] - foot_start[.i]
        foot_end[.i] = foot_start[.i+1]
    endfor
    foot_dur[tot_feet] =  (phr_end) - foot_start[tot_feet]
    foot_end[tot_feet] = phr_end
    # get anacrusis and phrase duration
    ana_end_t = foot_start[1]
    phr_dur =  phr_end - phr_start
endproc

procedure processSyllableTier: .textGrid
    # convert syllable tier to table
    selectObject: .textGrid
    syl_tier = Extract one tier: syl_tier_num
    syl_table = Down to Table: "no", 3, "no", "no"

    selectObject: syl_table
    # get number of syllables
    num_syls = Get number of rows

    # get start time of each syllable: syl_start[#]
    # check number of syllables of anacrusis: ana_syls[#]
    ana_syls = 0
    cur_foot = 0
    foot_one_start = foot_start[1]
    stress_one_end = foot_start[1] + foot_stress_dur[1]
    for i to num_syls
        cur_syl_start = Get value: i, "tmin"
        cur_syl_end_t = Get value: i, "tmax"
        cur_syl_mid = (cur_syl_end_t + cur_syl_start) / 2

        # check if current syllable is part of anacrusis
        if cur_syl_mid < foot_one_start
            ana_syls += 1
        # else check if current syllable is 1st stressed syllable of 1st foot
        elsif cur_syl_mid > foot_one_start and
                ... cur_syl_mid < stress_one_end
            cur_foot = 1
            foot_syls[cur_foot] = 1

        # else check if current syllable is start of a new foot
        elsif cur_syl_mid >
            ... foot_start[cur_foot] + foot_dur[cur_foot]
            cur_foot += 1
            foot_syls[cur_foot] = 1

        # otherwise assume curr syllable is part of current foot
        else
            foot_syls[cur_foot] += 1
        endif
        # get foot identity of each syllable: syl_foot_ID[#]
        syl_foot_ID[i] = cur_foot
        # get duration of each syllable: syl_dur[#]
        syl_dur[i] = cur_syl_end_t - cur_syl_start
        syl_start[i] = cur_syl_start
    endfor

    metrical_ID = ana_syls
    for m_index to cur_foot
        metrical_ID += foot_syls[m_index] * 10^m_index
    endfor
    metrical_ID$ = ""
    m_ID_len = length(string$(metrical_ID))
    for m_index to m_ID_len
           metrical_ID$ += mid$(string$(metrical_ID), m_ID_len - m_index + 1, 1)
    endfor
endproc

procedure processPhonoTier: .textGrid
    # convert syllable tier to table
    selectObject: .textGrid
    Extract one tier: phono_tier_num
    phono_tier = selected()
    Down to Table: "no", 3, "no", "no"
    phono_table = selected()

    # get accent type: accent$[#]
    selectObject: phono_table
    num_rows = Get number of rows
    phr_phono$ = init_phono$
    for i to num_rows
        cur_accent$ = Get value: i, "text"
        accent$[i] =
            ... replace$(replace$(cur_accent$, tab$, "", 0), newline$, "", 0)
        phr_phono$ = phr_phono$ + " " + cur_accent$
    endfor
    phr_phono$ = replace$(replace$(phr_phono$ + " " + fin_phono$, tab$, "", 0),
        ... newline$, "", 0)
endproc

procedure processOrthoTier: .textGrid
    # convert ortho tier to table
    selectObject: .textGrid
    Extract one tier: ortho_tier_num
    ortho_tier = selected()
    Down to Table: "no", 6, "no", "no"
    ortho_table = selected()
    num_words = Get number of rows

    #get sentence
    cur_sent$ = Get value: 1, "text"
    for i from 2 to num_words
        cur_word$ = Get value: i, "text"
        cur_sent$ = replace$(replace$(cur_sent$ + " " + cur_word$, tab$, "", 0),
            ... newline$, "", 0)
    endfor

    #get word end boundaries
    for i to tot_feet
        selectObject: ortho_tier
        mid_lex_stress = foot_stress_dur[i] / 2 + foot_start[i]
        lex_word = Get interval at time: 1, mid_lex_stress
        lex_word$[i] = Get label of interval: 1, lex_word
        word_start_t = Get start point: 1, lex_word
        word_end_t = Get end point: 1, lex_word

       selectObject: syl_tier
       word_fin_syll = Get low interval at time: 1, word_end_t - 0.001
       foot_first_syl = Get low interval at time: 1, mid_lex_stress
       wrd_fin_syl_start_t[i] = Get start time of interval: 1, word_fin_syll
       wrd_end_t[i] = word_end_t
       wrd_end_syl[i] = word_fin_syll - foot_first_syl + 1
    endfor
endproc

procedure processVowelTier: .textGrid, .sound, .pitchObject
    # convert vowel tier to table
    selectObject: .textGrid
    vowel_tier = Extract one tier: vowel_tier_num
    vowel_table = Down to Table: "no", 3, "no", "no"
    vowel_rows = Get number of rows
    for cur_foot to tot_feet
        # add vowel info
        for i to vowel_rows
            selectObject: vowel_table
            vowelText$ = Get value: i, "text"
            # remove accidental tabs and carriage returns
            curVowelText$ = replace$(replace$(vowelText$, tab$, "", 0),
                ... newline$, "", 0)
            curStartTime = Get value: i, "tmin"
            curEndTime = Get value: i, "tmax"
            # add vowel info if foot number is valid

            if curStartTime >= foot_start[cur_foot]
                        ... and curEndTime <=
                        ... foot_start[cur_foot] + foot_stress_dur[cur_foot]
                v_onset[cur_foot] = curStartTime
                v_offset[cur_foot] = curEndTime
                v_text$[cur_foot] = curVowelText$
                @getPitchAtTime: .pitchObject, v_onset[cur_foot]
                v_onset_f0[cur_foot] = result
            endif
        endfor
    endfor
endproc

procedure processToneTier: .textGrid, .sound, .pitchObject,
                       ... .speaker$, .gen_stats
    # Get Global F0 stats: slope and mean
    selectObject: .textGrid
    .num_ints = Get number of intervals: syl_tier_num
    .utt_start_t = Get end point: syl_tier_num, 1
    .utt_end_t = Get start point: syl_tier_num, .num_ints
    @getF0LineRegr: .utt_start_t, .utt_end_t, .pitchObject,
                ... .speaker$, .gen_stats
    utt_slope = getF0LineRegr.slope
    utt_slope_z = getF0LineRegr.slope_z
    utt_mean_f0 = getF0LineRegr.mean_f0


    # convert phono tier to table
    selectObject: .textGrid
    Extract one tier: tone_tier_num
    tone_tier = selected()
    Down to Table: "no", 3, "no", "no"
    tone_table = selected()

    selectObject: tone_table
    num_rows = Get number of rows
    for i to num_rows
        selectObject: tone_table
        cur_text$ = Get value: i, "text"
        cur_time = Get value: i, "tmin"
        foot_ref$ = right$(cur_text$,1)
        if left$(cur_text$, 1) = "L"
            l_t[number(foot_ref$)] = cur_time
            @get_nearest_f0: .pitchObject, cur_time, 0.01
            l_f0[number(foot_ref$)] = get_nearest_f0.f0
        elsif left$(cur_text$, 1) = "H"
            h_t[number(foot_ref$)] = cur_time
            @get_nearest_f0: .pitchObject, cur_time, 0.01
            h_f0[number(foot_ref$)] = get_nearest_f0.f0
        elsif left$(cur_text$, 1) = "S"
            s_t = cur_time
            @get_nearest_f0: .pitchObject, cur_time, 0.01
            s_t = get_nearest_f0.time
            s_f0 = get_nearest_f0.f0
        elsif left$(cur_text$, 1) = "E"
            e_t = cur_time
            @get_nearest_f0: .pitchObject, cur_time, -0.01
            e_t = get_nearest_f0.time
            e_f0 =get_nearest_f0.f0
        endif
    endfor

    # get syllable number and ratio for s_t and e_t

    # Assume extreme times for s_t and e_t
    s_syl = 0
    s_syl_ratio = 0
    e_syl = num_syls + 1
    e_syl_ratio = 0
    temp_str$# = {"s_", "e_"}

    selectObject: syl_table
    .cur_syl = 0
    # get s_t syllable number and ratio time as proportion of syllable
    while (.cur_syl < num_syls) and s_syl = 0
        .cur_syl += 1
        .cur_tmin = Get value: .cur_syl, "tmin"
        .cur_tmax = Get value: .cur_syl, "tmax"
        if  s_t >= .cur_tmin and  s_t <= .cur_tmax
            s_syl = .cur_syl
            s_syl_ratio =  (s_t - .cur_tmin) / (.cur_tmax - .cur_tmin)
        endif
    endwhile

    .cur_syl = num_syls
    # get e_t syllable number and ratio time as proportion of syllable
    while (.cur_syl > 0) and e_syl > num_syls
        .cur_tmin = Get value: .cur_syl, "tmin"
        .cur_tmax = Get value: .cur_syl, "tmax"
        if  e_t >= .cur_tmin and  e_t <= .cur_tmax
            e_syl = .cur_syl
            e_syl_ratio = (e_t - .cur_tmin) / (.cur_tmax - .cur_tmin)
        endif
        .cur_syl -= 1
    endwhile



    # Get intercept & slope of linear regression between L & H in each foot
    for i to tot_feet
        l_t_cur = l_t[i]
        h_t_cur = h_t[i]
        @getF0LineRegr: l_t_cur, h_t_cur, .pitchObject,
                    ... .speaker$, .gen_stats
        lh_slope[i] = getF0LineRegr.slope
        lh_slope_z[i] = getF0LineRegr.slope_z
        intercept_st[i] = getF0LineRegr.intercept_st
        mean_f0[i] = getF0LineRegr.mean_f0
        lh_med_f0[i] = getF0LineRegr.med_f0
    endfor
endproc

procedure calculateAlignmentData
    cur_syl_start = ana_syls
    # get H, L, and V-onset times normalised to rhythm
    for cur_foot to tot_feet
        cur_foot_start = foot_start[cur_foot]
        cur_stress_dur = foot_stress_dur[cur_foot]
        cur_foot_dur = foot_dur[cur_foot]
        cur_foot_syls = foot_syls[cur_foot]
        cur_l_t = l_t[cur_foot]
        cur_h_t = h_t[cur_foot]
        cur_v_onset = v_onset[cur_foot]
        unstressed_denom = (cur_foot_dur - cur_stress_dur)

        # Get times normalised to rhythm
        if cur_l_t < cur_stress_dur
            l_rhy_norm[cur_foot] = cur_l_t / cur_stress_dur
        else
            l_rhy_norm[cur_foot] = 1 + (cur_l_t - cur_stress_dur)
                ... / unstressed_denom
        endif
        if cur_h_t < cur_stress_dur
            h_rhy_norm[cur_foot] = cur_h_t / cur_stress_dur
        else
            h_rhy_norm[cur_foot] = 1 + (cur_h_t - cur_stress_dur)
                ... / unstressed_denom
        endif
        if cur_v_onset < cur_stress_dur
            v_rhy_norm[cur_foot] = cur_v_onset / cur_stress_dur
        else
            v_rhy_norm[cur_foot] = 1 + (cur_v_onset - cur_stress_dur)
                ... / unstressed_denom
        endif

        # Get times normalised to syllables
        for i to foot_syls[cur_foot]
            cur_syl = cur_syl_start + i
            cur_syl_l_edge = syl_start[cur_syl]
            cur_syl_r_edge = syl_start[cur_syl]
                ... + syl_dur[cur_syl]
            if cur_l_t >= cur_syl_l_edge and cur_l_t <= cur_syl_r_edge
                l_syl[cur_foot] = cur_syl
                l_syl_start[cur_foot] = cur_syl_l_edge
                l_syl_end[cur_foot] = cur_syl_r_edge
                l_syl_ratio[cur_foot] = (cur_l_t - cur_syl_l_edge) /
                    ... (cur_syl_r_edge - cur_syl_l_edge)
            endif
            if cur_h_t >= cur_syl_l_edge and cur_h_t <= cur_syl_r_edge
                h_syl[cur_foot] = cur_syl
                h_syl_start[cur_foot] = cur_syl_l_edge
                h_syl_end[cur_foot] = cur_syl_r_edge
                h_syl_ratio[cur_foot] = (cur_h_t - cur_syl_l_edge) /
                    ... (cur_syl_r_edge - cur_syl_l_edge)
            endif
            if cur_v_onset >= cur_syl_l_edge and cur_v_onset <= cur_syl_r_edge
                v_syl[cur_foot] = cur_syl
                v_syl_ratio[cur_foot] = (cur_v_onset - cur_syl_l_edge) /
                    ... (cur_syl_r_edge - cur_syl_l_edge)
            endif
        endfor
        cur_syl_start += foot_syls[cur_foot]
    endfor

    # Get  times of L, H, and V relative to the current foot
    for i to tot_feet
        l_t_ft[i] = l_t[i]
        h_t_ft[i] = h_t[i]
        v_t_ft[i] = v_onset[i]
        v_off_ft[i] = v_offset[i]
    endfor
endproc

procedure getF0LineRegr: .s_t, .e_t, .pitchObj, .speaker$, .gen_stats
    # Calculates linear regression of pitch curve between T*  +T

    #convert target pitch object to table
    selectObject: .pitchObj
    @pitch2Table: .pitchObj, 0
    .pitchTable = pitch2Table.table

    #keep only rows between l_t and h_t
    if .s_t > .e_t
        .temp = .s_t
        .s_t = .e_t
        .e_t = .temp
    endif

    .num_rows = Get number of rows
    for .i to .num_rows
        .curRow = .num_rows - .i + 1
        .curT = Get value: .curRow, "Time"
        .cur_num_rows = Get number of rows
        # remove line ONLY if outside time range and there are at least
        # three rows in table.
        if (.curT < .s_t or .curT > .e_t) and (.cur_num_rows > 2)
            Remove row: .curRow
        endif
    endfor

    #convert Hz to semitones re 1 Hz
    Formula: "F0", "12 * log2(self)"

    @tableStats: .pitchTable, "Time", "F0"
    .slope = tableStats.slope
    .intercept_st = tableStats.intercept
    .mean_f0 = tableStats.yMean
    .med_f0 = tableStats.yMed

    # Get speaker mean F0 and SD.
    selectObject: .gen_stats
    .speaker_row = Search column: "speaker", .speaker$
    .speaker_mean_f0 = Get value: .speaker_row, "f0_mean"
    .speaker_f0_SD = Get value: .speaker_row, "f0_SD"

    # Get z_scored_f0 Slope
    selectObject: .pitchTable
    Formula: "F0", "(self - .speaker_mean_f0) / .speaker_f0_SD"
    @tableStats: .pitchTable, "Time", "F0"
    .slope_z = tableStats.slope

    selectObject: .pitchTable

    Remove
endproc

procedure populateTable
    for i to tot_feet
        selectObject: output_table
        Append row
        bottomRow = Get number of rows
        # add general info
        Set string value: bottomRow, "code", cur_textGrid$
        Set string value: bottomRow, "speaker", speaker$
        Set string value: bottomRow, "gender", gender$
        Set string value: bottomRow, "stim", stim$
        Set string value: bottomRow, "rep", rep$
        Set string value: bottomRow, "location", cur_address$

        # add data requiring RHYTHM tier only
        Set string value: bottomRow, "init_phon", init_phono$
        Set string value: bottomRow, "fin_phon", fin_phono$
        Set numeric value: bottomRow, "phr_start_t", phr_start
        Set numeric value: bottomRow, "phr_end_t", phr_end
        Set numeric value: bottomRow, "tot_feet", tot_feet
        Set numeric value: bottomRow, "stress_end_t", stress_end[i]
        Set numeric value: bottomRow, "foot_end_t", foot_end[i]
        Set numeric value: bottomRow, "foot_start_t", foot_start[i]
        Set numeric value: bottomRow, "cur_foot", i

        # add data requiring SYLLABLE and/or rhythm tiers
        Set string value: bottomRow, "metre_ID", metrical_ID$
        Set string value: bottomRow, "stim_metre", stim$ + "_" + metrical_ID$
        Set numeric value: bottomRow, "ana_syls", ana_syls
        Set numeric value: bottomRow, "ana_end_t", ana_end_t
        Set numeric value: bottomRow, "foot_syls", foot_syls[i]
        Set numeric value: bottomRow, "tot_syls", num_syls

        # add data requiring PHONO and/or rhythm tiers
        Set string value: bottomRow, "acc_phon", accent$[i]
        Set string value: bottomRow, "phr_phon", phr_phono$

        # add data requiring ORTHO and/or rhythm / syllable tiers
        Set string value: bottomRow, "sent", cur_sent$
        Set numeric value: bottomRow, "wrd_fin_syl_start_t",
                                  ... wrd_fin_syl_start_t[i]
        Set numeric value: bottomRow, "wrd_end_t", wrd_end_t[i]
        Set numeric value: bottomRow, "wrd_end_syl", wrd_end_syl[i]

        # add data requiring TONE and rhythm, syllable tiers
        Set numeric value: bottomRow, "l_f0", l_f0[i]
        Set numeric value: bottomRow, "h_f0", h_f0[i]
        Set numeric value: bottomRow, "s_f0", s_f0
        Set numeric value: bottomRow, "e_f0", e_f0
        Set numeric value: bottomRow, "v_onset_f0", v_onset_f0[i]
        Set numeric value: bottomRow, "lh_slope", lh_slope[i]
        Set numeric value: bottomRow, "lh_slope_z", lh_slope_z[i]
        Set numeric value: bottomRow, "intercept_st", intercept_st[i]
        Set numeric value: bottomRow, "lh_mean_f0", mean_f0[i]
        Set numeric value: bottomRow, "lh_med_f0", lh_med_f0[i]
        Set numeric value: bottomRow, "utt_slope", utt_slope
        Set numeric value: bottomRow, "utt_slope_z", utt_slope_z
        Set string value: bottomRow, "utt_mean_f0", fixed$(utt_mean_f0, 2)

        # add ALIGNMENT data requiring VOWEL Tiers
        Set numeric value: bottomRow, "l_t", l_t[i]
        Set numeric value: bottomRow, "h_t", h_t[i]
        Set numeric value: bottomRow, "s_t", s_t
        Set numeric value: bottomRow, "e_t", e_t
        Set numeric value: bottomRow, "v_syl", v_syl[i]
        Set numeric value: bottomRow, "v_syl_ratio", v_syl_ratio[i]

        # add other ALIGNMENT data
        Set numeric value: bottomRow, "l_syl_start_t", l_syl_start[i]
        Set numeric value: bottomRow, "l_syl_end_t", l_syl_end[i]
        Set numeric value: bottomRow, "h_syl_start_t", h_syl_start[i]
        Set numeric value: bottomRow, "h_syl_end_t", h_syl_end[i]
        Set numeric value: bottomRow, "l_syl", l_syl[i]
        Set numeric value: bottomRow, "h_syl", h_syl[i]
        Set numeric value: bottomRow, "s_syl", s_syl
        Set numeric value: bottomRow, "e_syl", e_syl
        Set numeric value: bottomRow, "l_syl_ratio", l_syl_ratio[i]
        Set numeric value: bottomRow, "h_syl_ratio", h_syl_ratio[i]
        Set numeric value: bottomRow, "s_syl_ratio", s_syl_ratio
        Set numeric value: bottomRow, "e_syl_ratio", e_syl_ratio

        # add data requiring VOWEL and RHYTHM tiers
        Set numeric value: bottomRow, "v_onset_t", v_t_ft[i]
        Set numeric value: bottomRow, "v_offset_t", v_off_ft[i]
        Set string value: bottomRow, "v_text", v_text$[i]
        endif
    endfor
endproc

procedure getPitchAtTime: .pitchObject, .time
    selectObject: .pitchObject
    result = Get value at time: .time, "Hertz", "Linear"
    # Get value manually if the result is undefined.
    if result = undefined
        @get_nearest_f0: .pitchObject, .time, 0.01
        result = get_nearest_f0.f0
    endif

endproc

procedure get_nearest_f0: .object, .time, .time_step
    # Finds nearestdefined f0 to .time (prefers right over left)
    selectObject: .object
    .f0 = Get value at time: .time, "Hertz", "Linear"
    while .f0 = undefined
        .time = .time + .time_step/10
        .f0 = Get value at time: .time, "Hertz", "Linear"
    endwhile
endproc

procedure grandMeanSylTime: .corpusTable, .root$, .corpus$
    # Converts syllable-normalised time to grand-mean syllable-normalised time
    # across all utterances). i.e.:
    # INPUT: syllable normalised time, where integer part = syllable number,
    #        decimal part = proportion of syllable
    # OUPUT: grand mean syllable-nomalised time, where output value is the
    #        time based on the average syllable duration across all utterences
    #        with the same stimulus and metre ID (i.e. the same stim_metre
    #        parameter)

    # Read or create Grand mean Source table
    .gmSourceCSV$ = .root$ + "/" + .corpus$ + "_MeanSylDur.csv"
    if fileReadable(.gmSourceCSV$)
        .gmTable = Read Table from comma-separated file: .gmSourceCSV$
    else
        @meanSylDurs: .corpusTable, 0
        .gmTable =  meanSylDurs.table
        selectObject: .gmTable
        Save as comma-separated file: .gmSourceCSV$
    endif

    # Convert gmTable to set of arrays for convenience and speed of processing.
    # NOTE: .gm_dur[.curStim, .curSyl] = grand mean duration of current syllable
    #           	   	                 in current stimulus (actual metre not
    #                                    target meter.)
    #     .gm_start[.curStim, .curSyl] = grand mean start time of current syll-
    #                                    able for current stimulus.
    selectObject: .gmTable
    .numStims = Get number of rows
    for .curStim to .numStims
        .stimMetre$[.curStim] = Get value: .curStim, "stimMetre"
        .numSyls[.curStim] = Get value: .curStim, "numSyls"
        .gm_start[.curStim, 1] = 0
        for .curSyl to .numSyls[.curStim]
            .gm_dur[.curStim, .curSyl] =
                ... Get value: .curStim, "s" + string$(.curSyl)
            if .curSyl > 1
                .gm_start[.curStim, .curSyl] =
                ... .gm_start[.curStim, .curSyl - 1] +
                ... .gm_dur[.curStim, .curSyl - 1]
            endif
        endfor
    endfor

    # use syl_NormT prefixes to use in loop
    .affix$[1] = "v_"
    .affix$[2] = "l_"
    .affix$[3] = "h_"
    .affix$[4] = "s_"
    .affix$[5] = "e_"

    selectObject: .corpusTable
    # add temporary columns for grand mean calculation
    for .i to 5
        Append column: .affix$[.i] + "gmstart"
        Append column: .affix$[.i] + "gm_dur"
    endfor

    # convert syl number and syl duration grandmean start time and duration
    # using grand mean array values
    for .curStim to .numStims
        for .i to 5
            .durCol$ =  .affix$[.i] + "gm_dur"
            .startCol$ =  .affix$[.i] + "gmstart"
            .tgtRatio$ = .affix$[.i] + "syl_ratio"
            .tgtSylNum$ = .affix$[.i] + "syl"
            Formula: .durCol$,
                ... "if self$[""stim_metre""] = .stimMetre$[.curStim] then " +
                ... "self[.tgtRatio$] * .gm_dur[.curStim, self[.tgtSylNum$]] " +
                ... "else self endif"

            Formula: .startCol$,
                ... "if self$[""stim_metre""] = .stimMetre$[.curStim] then " +
                ... ".gm_start[.curStim, self[.tgtSylNum$]] else self endif"
        endfor
    endfor
    # convert grand_mean_t columns to grand mean syllable times
    for .i to 5
        Formula: .affix$[.i] + "grand_mean_t",
            ... "self[.affix$[.i] + ""gm_dur""] + self[.affix$[.i] + ""gmstart""]"
        # Remove columns as no longer necessary
        Remove column: .affix$[.i] + "gm_dur"
        Remove column: .affix$[.i] + "gmstart"
    endfor

    # remove remaining object
    selectObject: .gmTable
    Remove
endproc

# ==============================================================================
# PROCEDURES FROM OWN LIBRARIES=================================================


# object.management.praat ------------------------------------------------------
# Time and Date procedures
procedure seconds: .varName$
    '.varName$' = number(mid$(date$(), 12, 2))*60*60
        ... + number(mid$(date$(), 15, 2))*60
        ... + number(mid$(date$(), 18, 2))
endproc

procedure date
    .zeros$ = "00"
    @month

    .day$ = left$(date$(),3)
    .day = number(mid$(date$(),9,2))
    .day0$ = mid$(date$(),9,2)

    .month$ = mid$(date$(),5, 3)
    .month = month.num[.month$]
    .month0$ = left$(.zeros$, 2-length(string$(.month))) +  string$(.month)

    .year$ = right$(date$(),4)
    .year = number(.year$)
    .time$ = mid$(date$(), 12, 5)
    .hour = number(mid$(date$(), 12, 2))
    .min = number(mid$(date$(), 15, 2))
    .sec = number(mid$(date$(), 18, 2))

    .index = .sec
        ... + .min           *60
        ... + .hour          *60*60
        ... + (.day -1)      *60*60*24
        ... + (.month - 1)   *60*60*24*31
        ... + (.year - 2019) *60*60*24*31*12

    .index$ = .year$
        ... + "_" + .month0$
        ... + "_" + .day0$
        ... + "_" + mid$(date$(), 12, 2)
        ... + "_" + mid$(date$(), 15, 2)
        ... + "_" + mid$(date$(), 18, 2)
endproc

procedure month
    .num["Jan"] = 1
    .num["Feb"] = 2
    .num["Mar"] = 3
    .num["Apr"] = 4
    .num["May"] = 5
    .num["Jun"] = 6
    .num["Jul"] = 7
    .num["Aug"] = 8
    .num["Sep"] = 9
    .num["Oct"] = 10
    .num["Nov"] = 11
    .num["Dec"] = 12
endproc

# Table, array, and variable management procedures
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

procedure tableStats: .table, .colX$, .colY$
    @keepCols: .table, "'.colX$' '.colY$'", "tableStats.shortTable"

	.numRows = Get number of rows
	.factor$ = Get column label: 1
	if .colX$ != .factor$
		@table2array: .shortTable, .colY$, "tableStats.colTemp$"
		Remove column: .colY$
		Append column: .colY$
		for .i to table2array.n
		    Set string value: .i, .colY$, .colTemp$[.i]
		endfor
	endif

    if .numRows > 1
		.stDevY = Get standard deviation: .colY$
		.stDevY = number(fixed$(.stDevY, 3))
		.stDevX = Get standard deviation: .colX$
		.linear_regression = To linear regression
		.linear_regression$ = Info
		.slope = extractNumber (.linear_regression$, "Coefficient of factor '.colX$': ")
		.slope = number(fixed$(.slope, 3))
		.intercept = extractNumber (.linear_regression$, "Intercept: ")
		.intercept = number(fixed$(.intercept, 3))
		.r = number(fixed$(.slope * .stDevX / .stDevY, 3))
		selectObject: .linear_regression
		.info$ = Info
		Remove
	else
		.stDevY = undefined
		.stDevX = undefined
		.linear_regression = undefined
		.linear_regression$ = "N/A"
		.slope = undefined
		.intercept = Get value: 1, .colY$
		.r = undefined
		.info$ = "N/A"
	endif

	selectObject: .shortTable
	.xMean = Get mean: .colX$
	.xMed = Get quantile: .colX$, 0.5
	.yMean = Get mean: .colY$
	.yMed = Get quantile: .colY$, 0.5
	Remove
endproc

procedure table2array: .table, .col$, .array$
    .string = right$(.array$, 1) = "$"
    selectObject: .table
    .n = Get number of rows
    for .i to .n
        if .string
            .cur_val$ = Get value: .i, .col$
            '.array$'[.i] = .cur_val$
        else
            .cur_val = Get value: .i, .col$
            '.array$'[.i] = .cur_val
        endif
    endfor
endproc

procedure keepCols: .table, .keep_cols$, .new_table$
    @list2array: .keep_cols$, ".keep$"
    selectObject: .table
    '.new_table$' = Copy: .new_table$
    .num_cols = Get number of columns
    for .i to .num_cols
        .col_cur = .num_cols + 1 - .i
        .label_cur$ = Get column label: .col_cur
        .keep_me = 0
        for .j to list2array.n
            if .label_cur$ = list2array.keep$[.j]
                .keep_me = 1
            endif
        endfor
        if .keep_me = 0
            Remove column: .label_cur$
        endif
    endfor
endproc

procedure list2array: .list$, .array$
    .list_length = length(.list$)
    .n = 1
    .prev_start = 1
    for .i to .list_length
        .char$ = mid$(.list$, .i, 1)
        if .char$ = " "
            '.array$'[.n] = mid$(.list$, .prev_start, .i - .prev_start)
            .origIndex[.n] = .prev_start
            .n += 1
            .prev_start = .i + 1
        endif
    endfor
    if .n = 1
        '.array$'[.n] = .list$
    else
        '.array$'[.n] = mid$(.list$, .prev_start, .list_length - .prev_start + 1)
    endif
    .origIndex[.n] = .prev_start
endproc

procedure unique_strings: .table, .column$, .output_array$
    #create temp copy to sort
    selectObject: .table
    .tempTable = Copy: "Temp"
    Sort rows: .column$
    # Check column exists
    .column_exists = Get column index: .column$
    if not .column_exists
    appendInfoLine: "ERROR:", tab$, "@unique_strings: ", .table, ", """, .column$,
                ... """, """, .output_array$, """"
    appendInfoLine: tab$, "Col. """, .column$, """ not found in table #", .table
    appendInfoLine: "EXITING SCRIPT"
        exit
    endif

    #correct name of output array
    if right$(.output_array$, 1) != "$"
        #create variable name for unique count
        .unique_count$ = .output_array$
        .output_array$ += "$"
    else
        #create variable name for unique count
        .unique_count$ = replace$(.output_array$, "$", "", 1)
    endif
    .unique_num$ = replace$(.output_array$, "$", "Num", 1)
    .num_rows = Get number of rows
    '.unique_count$' = 1

    '.output_array$'[1] = Get value: 1, .column$

    for .i to .num_rows
        .cur_string$ = Get value: .i, .column$
        .string_is_new = 1
        for j to '.unique_count$'
            if .cur_string$ = '.output_array$'[j]
                .string_is_new = 0
            endif
        endfor
        if .string_is_new
            '.unique_count$' += 1
            '.output_array$'['.unique_count$'] = .cur_string$
        endif
    endfor

    # look for number of entries for each unique entry value
    for .i to '.unique_count$'
        #find first entry for current unique entry value
        .curRow = Search column: .column$, '.output_array$'[.i]

        # populate first element in each array
        '.unique_num$'[.i] = 1
        .curStimMetre$ = '.output_array$'[.i]

        # create "done" flag to end array (i.e. end if the last entry was not
        # one of the unique entries or if there are no more table rows)
        .done = (.curRow >= .num_rows) or (.curStimMetre$ != '.output_array$'[.i])

        # search the table until there done
        while not .done
            .curRow += 1
            if .curRow < .num_rows
                .curStimMetre$ = Get value: .curRow, .column$
                if .curStimMetre$ = '.output_array$'[.i]
                    '.unique_num$'[.i] += 1
                endif
            endif
        .done = (.curRow >= .num_rows) or (.curStimMetre$ != '.output_array$'[.i])
        endwhile
    endfor
    #remove the temp table
    Remove
endproc

procedure removeRowsWhere: .table, .col$, .criteria$
    selectObject: .table
    .num_rows = Get number of rows
    for .i to .num_rows
        .cur_row = .num_rows + 1 - .i
        .cur_value$ = Get value: .cur_row, .col$
        if .cur_value$ '.criteria$'
            Remove row: .cur_row
        endif
    endfor
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

# Corpus management / processing procedures -------------------------------------
procedure meanSylDurs: .corpus_choice, .analysis_set
    # .analysis_set = 0 --> corpus_choice object already in object window!
    if .analysis_set = 0
        selectObject: .corpus_choice
        .corpus = Copy: selected$("Table")
        .baseName$ = selected$("Table")
    else
        .corpus = Read from file: root_G$ + "/" + analysis_G$[.analysis_set] + "/"
            ... + corpusRef_G$[.corpus_choice] + ".Table"
        .baseName$ = corpusRef_G$[.corpus_choice]
    endif

    # TRIM CORPUS
    @trimCorpus: "meanSylDurs.corpus"

    # GET ARRAY OF UNIQUE STIM_METRE CODES
    selectObject: .corpus
    @unique_strings: .corpus, "stim_metre", "meanSylDurs.stim_metres"

    # CREATE TABLES LISTING EACH LOCATION OF EACH UNIQUE-STIM METRE
    for .curStimMetre to .stim_metres
        selectObject: .corpus
        .curName$ = .stim_metres$[.curStimMetre]
        .stimMetreTable[.curStimMetre] = Copy: .curName$
        @removeRowsWhere: .stimMetreTable[.curStimMetre], "stim_metre",
            ... "!= meanSylDurs.stim_metres$[meanSylDurs.curStimMetre]"
        .stimMetreReps[.curStimMetre] = Get number of rows
    endfor

    # Remove redundant corpus object
    selectObject: .corpus
    Remove
    .maxSyls = 0
    for .i to .stim_metres
        selectObject: .stimMetreTable[.i]
        .numRows = Get number of rows
        for .curGrid to .numRows
            selectObject: .stimMetreTable[.i]
            .curFile$ = Get value: .curGrid, "code"
            .dir$ = Get value: .curGrid, "location"
            .curFile = Read from file: .dir$ + .curFile$ + ".TextGrid"
            .tIndex = (.curGrid * (.i - 1)) + .curGrid
            @rhythm: .curFile, 3, 0
            @syllable: .curFile, 2, 0
            selectObject: rhythm.table
            Remove
            selectObject: syllable.table
            Append column: "dur"
            Formula: "dur", "self[""tmax""]-self[""tmin""]"
            Remove column: "tmin"
            Remove column: "tmax"
            Remove column: "text"
            Remove column: "metre_ID"
            Remove column: "syl"
            .durTable[.curGrid] = Transpose
            selectObject: syllable.table
            plusObject: .curFile
            Remove
        endfor

        ## create table of all syl durs for cur STIM_METRE
        selectObject: .durTable[1]

        for .curGrid from 2 to .numRows
            plusObject: .durTable[.curGrid]
        endfor

        .combined = Append
        Rename: .stim_metres$[.i]
        Set column label (index): 1, "text"
        Remove column: "text"
        .numSyls[.i] = Get number of columns

        ## update maxSyls
        if .numSyls[.i] > .maxSyls
            .maxSyls = .numSyls[.i]
        endif

        for .col to .numSyls[.i]
            Set column label (index): .col, "syl_" + string$(.col)
            .mean[.i,.col] = Get mean: "syl_" + string$(.col)
            .sd[.i,.col] = Get standard deviation: "syl_" + string$(.col)
        endfor

        selectObject: .stimMetreTable[.i]
        plusObject: .combined
        for .curGrid to .numRows
            plusObject: .durTable[.curGrid]
        endfor
        Remove
    endfor

    ##CREATE OUTPUT TABLE
    .sylCols$ = ""
    for .i to .maxSyls
        .sylCols$ += " s" + string$(.i)
    endfor

    .table = Create Table with column names:
        ... .baseName$ + "_MeanSylDur",
        ... .stim_metres, "stimMetre numReps numSyls phrDur" + .sylCols$

    ##POPULATE OUTPUT TABLE
    for .i to .stim_metres
        .curPhrDur = 0
        Set string value: .i, "stimMetre", .stim_metres$[.i]
        Set numeric value: .i, "numSyls", .numSyls[.i]
        for .j to .numSyls[.i]
             Set numeric value: .i, "s" + string$(.j),
                 ... number(fixed$(.mean[.i,.j],3))
             .curPhrDur += .mean[.i,.j]
        endfor
        Set numeric value: .i, "phrDur", number(fixed$(.curPhrDur,3))
        Set numeric value: .i, "numReps", .stimMetreReps[.i]
    endfor

    Formula (column range): "s1", "s" +string$(.maxSyls),
        ... "if self$ = """" then self$ = ""0"" else self endif"
endproc

procedure trimCorpus: .originalCorpusVar$
    .corpus = '.originalCorpusVar$'
    @keepCols: .corpus,
           ... "code speaker rep stim metre_ID cur_foot stim_metre location",
           ... "trimCorpus.trim"
    selectObject: .corpus
    .name$ = selected$("Table")
    selectObject: .trim
    Rename: .name$ + "_trimmed"
    @removeRowsWhere: .trim, "cur_foot", "!= ""1"""
    Remove column: "cur_foot"
    selectObject: .corpus
    Remove
    '.originalCorpusVar$' = .trim
endproc

procedure rhythm: .textGrid, .tierNum, .startAtZero

    #convert rhythm tier to table
    selectObject: .textGrid
    .code$ = selected$("TextGrid")
    .name$ = "Rhy_"  + .code$

    .tierGrid = Extract one tier: .tierNum
    .tierTable = Down to Table: "no", 3, "no", "no"
    selectObject: .tierTable
    .numRows = Get number of rows

    # get phrase start and end
    .phrstart = Get value: 1, "tmin"
    .phrEnd = Get value: .numRows, "tmin"
    .feet = 0
    .boundaries = 0

    # get start time of each foot: foot_start[#]
    # get duration of each stressed syllable:  .ftStrDur[#]
    for .i to .numRows
        .timeCur = Get value: .i, "tmin"
        .textCur$ = Get value: .i, "text"
        # remove accidental table
        .textCur$ = replace$(replace$(.textCur$, tab$, "", 0), newline$, "", 0)
        for .j to length(.textCur$)
            .charCur$= mid$ (.textCur$, .j, 1)
            if .charCur$ = "<"
                .feet += 1
                .ftstart[.feet] = .timeCur
                .strEnd[.i] = Get value: .i+1, "tmin"
                .ftStrDur[.feet] = .strEnd[.i] - .timeCur
                .ftstart[.feet] = .timeCur - .phrstart
            elsif .charCur$ = "%"
                .boundaries += 1
                if .boundaries = 1
                    init_phono$ = replace$(.textCur$, "<", "", 0)
                else
                    .finPhono$ =  replace$(.textCur$, ">", "", 0)
                endif
            endif
        endfor
    endfor

    # get duration of each foot: .ftDur[#]
    for .i to .feet - 1
        .ftDur[.i] = .ftstart[.i+1] - .ftstart[.i]
    endfor
    .ftDur[.feet] =  (.phrEnd - .phrstart) - .ftstart[.feet]
    # get anacrusis and phrase duration
    .anaDur = .ftstart[1]
    .phrDur =  .phrEnd - .phrstart

    # create RhythmTable mimicking interval table
    .table = Create Table with column names: .name$, 0, "num_foot tmin foot_start_t"
        ... + " tmax ft_dur text"
    # add anacrusis
    if .anaDur > 0
        Append row
        Set numeric value: 1, "tmin", 0
        Set numeric value: 1, "tmax", .ftstart[1]
        Set string value: 1, "text", "%"
        Set numeric value: 1, "num_foot", 0
        Set numeric value: 1, "foot_start_t", 0
        Set numeric value: 1, "ft_dur", .ftstart[1]
    endif
    # add stressed syllables plus unstressed syllables (except final tail)
    for .i to .feet
        Append row
        .curRow = Get number of rows
        Set numeric value: .curRow, "tmin", .ftstart[.i]
        Set numeric value: .curRow, "tmax", .ftstart[.i] + .ftStrDur[.i]
        Set numeric value: .curRow, "num_foot", .i
        Set numeric value: .curRow, "foot_start_t", .ftstart[.i]
        Set numeric value: .curRow, "ft_dur", .ftDur[.i]

        if .anaDur = 0 and .i = 1
            .text$ = "%<STR>"
        elsif .ftStrDur[.i] = .ftDur[.i] and .i = .feet
            .text$ = "<STR>%"
        else
            .text$ = "<STR>"
        endif
        Set string value: .curRow, "text", .text$

        if .i != .feet and .ftDur[.i] != .ftStrDur[.i]
            Append row
            .curRow = Get number of rows
            Set numeric value: .curRow, "tmin", .ftstart[.i] + .ftStrDur[.i]
            Set numeric value: .curRow, "tmax", .ftstart[.i + 1]
            Set string value: .curRow, "text", "..."
            Set numeric value: .curRow, "num_foot", .i
            Set numeric value: .curRow, "foot_start_t", .ftstart[.i]
            Set numeric value: .curRow, "ft_dur", .ftDur[.i]
        endif
    endfor
    # add final tail if it exists
    if .ftStrDur[.feet] < .ftDur[.feet]
        Append row
        .curRow = Get number of rows
        Set numeric value: .curRow, "tmin", .ftstart[.feet] + .ftStrDur[.feet]
        Set numeric value: .curRow, "tmax", .phrDur
        Set string value: .curRow, "text", "%"
        Set numeric value: .curRow, "num_foot", .i-1
        Set numeric value: .curRow, "foot_start_t", .ftstart[.i-1]
        Set numeric value: .curRow, "ft_dur", .ftDur[.i-1]
    endif
    if .startAtZero
        .offset = 0
    else
        .offset = .phrstart
    endif
    Formula: "tmin", "number(fixed$(self + .offset, 3))"
    Formula: "tmax", "number(fixed$(self + .offset, 3))"
    Formula: "ft_dur", "number(fixed$(self + .offset, 3))"
    Formula: "foot_start_t", "number(fixed$(self + .offset, 3))"

    selectObject: .tierGrid
    plusObject: .tierTable
    Remove
endproc

procedure syllable: .textGrid, .tierNum, .startAtZero
    # convert syllable tier to table
    selectObject: .textGrid
    .code$ = selected$("TextGrid")
    .name$ = "Syl_"  + .code$
    .syl_tier = Extract one tier: .tierNum
    .table = Down to Table: "yes", 3, "no", "no"
    Rename: .name$
    Set column label (index): 1, "syl"
    .phr_start = rhythm.phrstart

    selectObject: .table
    # get number of syllables
    .num_syls = Get number of rows

    # get start time of each syllable: syl_start[#]
    # check number of syllables of anacrusis: ana_syls[#]
    .ana_syls = 0
    .cur_foot = 0
    .foot_one_start = .phr_start + rhythm.ftstart[1]
    .stress_one_end = .phr_start + rhythm.ftstart[1] + rhythm.ftStrDur[1]
    for .i to .num_syls
        .cur_syl_start = Get value: .i, "tmin"
        .cur_syl_end = Get value: .i, "tmax"
        .cur_syl_mid = (.cur_syl_end + .cur_syl_start) / 2

        # check if current syllable is part of anacrusis
        if .cur_syl_mid < .foot_one_start
            .ana_syls += 1
        # else check if current syllable is 1st stressed syllable of 1st foot
        elsif .cur_syl_mid > .foot_one_start and
                ... .cur_syl_mid < .stress_one_end
            .cur_foot = 1
            .foot_syls[.cur_foot] = 1

        # else check if current syllable is start of a new foot
        elsif .cur_syl_mid > .phr_start + rhythm.ftstart[.cur_foot] +
            ... rhythm.ftDur[.cur_foot]
            .cur_foot += 1
            .foot_syls[.cur_foot] = 1

        # otherwise assume curr syllable is part of current foot
        else
            .foot_syls[.cur_foot] += 1
        endif
        # get foot identity of each syllable: syl_foot_ID[#]
        .syl_foot_ID[.i] = .cur_foot
        # get duration of each syllable: syl_dur[#]
        .syl_dur[.i] = .cur_syl_end - .cur_syl_start
        .syl_start[.i] = .cur_syl_start - .phr_start
    endfor

    .metrical_ID = .ana_syls
    for .m_index to .cur_foot
        .metrical_ID += .foot_syls[.m_index] * 10^.m_index
    endfor
    .metrical_ID$ = ""
    .m_ID_len = length(string$(.metrical_ID))
    for .m_index to .m_ID_len
   	    .metrical_ID$ +=
            ... mid$(string$(.metrical_ID), .m_ID_len - .m_index + 1, 1)
    endfor

    if .startAtZero
        .offset = .phr_start
    else
        .offset = 0
    endif
    Formula: "tmin", "number(fixed$(self - .offset, 3))"
    Formula: "tmax", "number(fixed$(self - .offset, 3))"

    # add utterence identifer column
    selectObject: .table
    .numRows = Get number of rows
    Insert column: 1, "metre_ID"
    Formula: "metre_ID", ".metrical_ID$"

    selectObject: .syl_tier
    Remove
endproc

procedure addGenF0Stats: .output_table, .gen_stats
    selectObject: .gen_stats
    .num_speakers = Get number of rows
    for .i to .num_speakers
        selectObject: .gen_stats
        .speaker$ = Get value: .i, "speaker"
        .f0_mean = Get value: .i, "f0_mean"
        .f0_SD = Get value: .i, "f0_SD"
        .f0_min = Get value: .i, "f0_min"
        .f0_med = Get value: .i, "f0_med"
        selectObject: .output_table
        Formula: "spkr_f0_mean",
        ... "if self$[""speaker""] = .speaker$ then .f0_mean else self endif"
        Formula: "spkr_f0_SD",
        ... "if self$[""speaker""] = .speaker$ then .f0_SD else self endif"
        Formula: "spkr_f0_min",
        ... "if self$[""speaker""] = .speaker$ then .f0_min else self endif"
        Formula: "spkr_f0_med",
        ... "if self$[""speaker""] = .speaker$ then .f0_med else self endif"
    endfor
endproc

procedure addAnaNucPreText: .table
    # Adds info about text in anacrusis and unstressed text before nuclear PA.

    # Get look-up table.
    ana_nuc_pre_table =
    ...Read Table from comma-separated file: "AH_corpus_ana_pre_text.csv"
    num_stims = Get number of rows

    # Add info columns to main table
    selectObject: .table
    Insert column: 9, "ana_text"
    Insert column: 10, "nuc_pre_text"
    Insert column: 11, "ana_has_word_end"
    Insert column: 12, "nuc_is_new_word"
    num_rows = Get number of rows

    for i to num_stims
        # Get info for current target stim.
        selectObject: ana_nuc_pre_table
        cur_stim$ = Get value: i, "stim"
        cur_ana_txt$ = Get value: i, "ana_text"
        cur_pre_txt$ = Get value: i, "nuc_pre_text"
        cur_ana_end$ = Get value: i, "ana_has_word_end"
        cur_nuc_new$ = Get value: i, "nuc_is_new_word"

        # add anacrusis and pre-nucleus text into to main table.
        selectObject: .table
        Formula: "ana_text",
        ... "if self$[""stim""] == cur_stim$ then cur_ana_txt$ else self$ endif"
        Formula: "nuc_pre_text",
        ... "if self$[""stim""] == cur_stim$ then cur_pre_txt$ else self$ endif"
        Formula: "ana_has_word_end",
        ... "if self$[""stim""] == cur_stim$ then cur_ana_end$ else self$ endif"
        Formula: "nuc_is_new_word",
        ... "if self$[""stim""] == cur_stim$ then cur_nuc_new$ else self$ endif"

    endfor
    removeObject: ana_nuc_pre_table

endproc


include procs/getF0Stats.proc
include procs/globalDictionaries.proc
