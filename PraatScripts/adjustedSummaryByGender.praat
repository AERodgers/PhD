# convert table with data count into table of individual entries with gender column

# UI
form
    word treatment foot_syls
    word response acc_phon
    word count value
    sentence table_m ..\Ch_6_Form\output\pn_foot_adj_M.csv
    sentence table_f ..\Ch_6_Form\output\pn_foot_adj_F.csv
    sentence table_g ..\Ch_6_Form\output\pn_foot_adj_all_gender.csv
endform

table_m = Read Table from comma-separated file: table_m$
table_f = Read Table from comma-separated file: table_f$
@count2rows(table_m, treatment$, response$, count$)
m_adj = selected()
@count2rows(table_f, treatment$, response$, count$)
f_adj = selected()
@addGenderCol(m_adj, f_adj)
g_adj = selected()
Save as comma-separated file: table_g$

removeObject: { table_m, table_f, m_adj, f_adj, g_adj }


procedure count2rows(.table, .treatment$, .response$, .count$)
    selectObject: .table
    .name$ = selected$("Table")
    .num_rows = Get number of rows
    .mean = Get mean: .count$
    .total = .mean * .num_rows
 
    .new_table = Create Table with column names: .name$  + "_adj", .total, { .treatment$, .response$ }

    .item = 0
    for .cur_row to .num_rows
        selectObject: .table
        .cur_foot_syls = Get value: .cur_row, .treatment$
        .cur_acc_phon$ = Get value: .cur_row, .response$
        .cur_value = Get value: .cur_row, .count$
        selectObject: .new_table 
        for .i to .cur_value
            .item += 1
            Set numeric value: .item, .treatment$, .cur_foot_syls
            Set string value: .item, .response$, .cur_acc_phon$ 
        endfor
    endfor
endproc

procedure addGenderCol(m, f)
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




