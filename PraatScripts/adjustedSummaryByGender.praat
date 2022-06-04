# convert table with data count into table of tokens with gender column.

# UI
form
    word treatment foot_syls
    word response init_contour
    word count value
    sentence table_m C:/Users/antoi/Documents/GitHub/PhD/Ch_6_Form/output/M.csv
    sentence table_f C:/Users/antoi/Documents/GitHub/PhD/Ch_6_Form/output/F.csv
    sentence table_g C:/Users/antoi/Documents/GitHub/PhD/Ch_6_Form/output/G.csv
endform

# Create table of tokens for female speakers
table_f = Read Table from comma-separated file: table_f$
@count2rows(table_f, treatment$, response$, count$)
f_adj = selected()

# Create table of tokens for male speakers
table_m = Read Table from comma-separated file: table_m$
@count2rows(table_m, treatment$, response$, count$)
m_adj = selected()

# Generate combined table with additional gender column.
@addGenderCol(m_adj, f_adj)
g_adj = selected()
Save as comma-separated file: table_g$

# Remove objects from objects window.
removeObject: { table_m, table_f, m_adj, f_adj, g_adj }

procedure count2rows(.table, .treatment$, .response$, .count$)
    # Convert table with counts of responses per treatment to table of tokens.
    selectObject: .table
    .name$ = selected$("Table")
    .num_rows = Get number of rows
    .mean = Get mean: .count$
    .total = round(.mean * .num_rows)

    .new_table = Create Table with column names:
             ... .name$  + "_adj", .total, { .treatment$, .response$ }

    .item = 0
    for .cur_row to .num_rows
        selectObject: .table
        .cur_treatment$ = Get value: .cur_row, .treatment$
        .cur_response$ = Get value: .cur_row, .response$
        .cur_value = Get value: .cur_row, .count$
        selectObject: .new_table
        for .i to .cur_value
            .item += 1
            Set string value: .item, .treatment$, .cur_treatment$
            Set string value: .item, .response$, .cur_response$
        endfor
    endfor
endproc

procedure addGenderCol(m, f)
    # Create a table with a gender column from two separate tables.
    #
    # Each table must have the same number of columns with the same names.
    selectObject: m
    plusObject: f
    Append column: "gender"
    selectObject: m
    Formula: "gender", """M"""
    selectObject: f
    Formula: "gender", """F"""
    new_name$ = replace$(selected$("Table"), "_F", "_ALL", 1)
    plusObject: m
    Append
    Rename: new_name$
endproc
