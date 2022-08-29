# CORPUS AUDIT
# ============
# Checks for and removes invalid utterances in PhD Derry City English Corpora.
#
# Written for Praat 6.0.36
#
# Antoin Eoin Rodgers
# rodgeran@tcd.ie
# Phonetics and speech Laboratory, Trinity College Dublin
#
# latest update: 20/04/2022

# UI Form
form Choose Corpus File to Audit
    sentence data_file D:\Users\antoi\GitHub\PhD\Ch_6_Form\data\a_corpus.csv
    natural target_feet 2
endform

# GET DATA
data = Read Table from comma-separated file: data_file$
file_root$ = left$(data_file$, rindex(data_file$, ".") - 1)

# create speaker_stimulus field
Insert column: 1, "speaker_stim"
Formula: "speaker_stim", "self$[""speaker""] + ""_"" + self$[""stim""]"

# create temporary copy of data table
temp_data = Copy: "Temp"
Sort rows: "speaker_stim"

# Remove excess columns and rows
delete_col = Get column index: "acc_phon"
num_cols = Get number of columns
while num_cols >= delete_col
    cur_col$ = Get column label: delete_col
    Remove column: cur_col$
    num_cols = Get number of columns
endwhile

num_rows = Get number of rows
for i to num_rows
    cur_row = num_rows - i + 1
    cur_foot = Get value: cur_row, "cur_foot"
    if cur_foot !=1
        Remove row: cur_row
    endif
endfor

# REMOVE INVALID UTTERANCES
writeFileLine: "'file_root$'_invalid.csv", "code,problem"

# Check foot count
num_rows = Get number of rows
for i to num_rows
    cur_row = num_rows - i + 1
    # check number of feet
    cur_tot_feet = Get value: cur_row, "tot_feet"
    if cur_tot_feet != target_feet
        cur_code$ = Get value: cur_row, "code"
        appendFileLine: "'file_root$'_invalid.csv", "'cur_code$',foot count"
        Remove row: cur_row
    endif
endfor

# check meter
num_rows = Get number of rows
for i to num_rows
    cur_row = num_rows - i + 1
    cur_stim_meter$ = Get value: cur_row, "stim_metre"
    target_stim$ = mid$(cur_stim_meter$, 2, 3)
    #act_stim$ = right$(cur_stim_meter$, length(cur_stim_meter$) - index(cur_stim_meter$, "_"))
    act_stim$ = right$(cur_stim_meter$, 3)
    if act_stim$ != target_stim$
        cur_code$ = Get value: cur_row, "code"
        appendFileLine: "'file_root$'_invalid.csv", "'cur_code$',meter"
        Remove row: cur_row
    endif
endfor

# AUDIT DATA
# Get arrays of speakers and stimuli
num_rows = Get number of rows
Sort rows: "speaker"
speaker_count = 1
speaker$[1] = Get value: 1, "speaker"
for i from 2 to num_rows
    cur_speaker$ = Get value: i, "speaker"
    if cur_speaker$ != speaker$[speaker_count]
        speaker_count += 1
        speaker$[speaker_count] = cur_speaker$
    endif
endfor

Sort rows: "stim"
stim_count = 1
stim$[1] = Get value: 1, "stim"
for i from 2 to num_rows
    cur_stim$ = Get value: i, "stim"
    if cur_stim$ != stim$[stim_count]
        stim_count += 1
        stim$[stim_count] = cur_stim$
    endif
endfor

total_valid = Get number of rows
# Get number of utterances per speaker
for cur_speaker to speaker_count
    selectObject: temp_data
    nowarn Extract rows where: "self$[""speaker""] == speaker$[cur_speaker]"
    speaker_tot[cur_speaker] = Get number of rows
    Remove
endfor

# Get number of utterances per stimulus
for cur_stim to stim_count
    selectObject: temp_data
    nowarn Extract rows where: "self$[""stim""] == stim$[cur_stim]"
    stim_tot[cur_stim] = Get number of rows
    Remove
endfor

# Get number of utterances per speaker AND stimulus
for cur_speaker to speaker_count
    for cur_stim to stim_count
        selectObject: temp_data
        nowarn Extract rows where:
        ... "self$[""stim""] == stim$[cur_stim] and " +
        ... "self$[""speaker""] == speaker$[cur_speaker]"
        speaker_stim[cur_speaker, cur_stim] = Get number of rows
        Remove
    endfor
endfor

# CREATE AUDIT FILE
writeFile: "'file_root$'_audit_summary.csv", "speaker,"
for cur_stim to stim_count - 1
    appendFile: "'file_root$'_audit_summary.csv", stim$[cur_stim], ","
endfor
appendFile: "'file_root$'_audit_summary.csv",
    ... stim$[stim_count], ",TOTAL", newline$

# add rows
for cur_speaker to speaker_count
    appendFile: "'file_root$'_audit_summary.csv", speaker$[cur_speaker], ","
    for cur_stim to stim_count - 1
        appendFile: "'file_root$'_audit_summary.csv",
            ... speaker_stim[cur_speaker, cur_stim], ","
    endfor
    appendFile: "'file_root$'_audit_summary.csv",
        ... speaker_stim[cur_speaker, stim_count], ","
    appendFile: "'file_root$'_audit_summary.csv",
        ... speaker_tot[cur_speaker], newline$
endfor

# add summation row
appendFile: "'file_root$'_audit_summary.csv", "TOTAL,"
for cur_stim to stim_count - 1
    appendFile: "'file_root$'_audit_summary.csv", stim_tot[cur_stim], ","
endfor
appendFile: "'file_root$'_audit_summary.csv", stim_tot[stim_count], ","
appendFile: "'file_root$'_audit_summary.csv", total_valid, newline$


# CREATE AUDITED DATA TABLE
invalid = Read Table from comma-separated file: "'file_root$'_invalid.csv"
num_invalid = Get number of rows
for i to num_invalid
    selectObject: invalid
    cur_invalid$ = Get value: i, "code"
    selectObject: data
    num_items = Get number of rows
    for j to num_items
        cur_item = num_items - j + 1
        cur_code$ = Get value: cur_item, "code"
        if cur_code$ = cur_invalid$
            Remove row: cur_item
        endif
    endfor
endfor

selectObject: data
Remove column: "speaker_stim"
Save as comma-separated file: "'file_root$'_audited.csv"

# TIDY OBJECTS WINDOW
removeObject: {invalid, temp_data, data}
