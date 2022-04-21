script <- paste(getwd( ), "/PraatScripts/table_of_count_to_table_of_items.praat", sep = "")
my_treatment <- "foot_syls"
my_response <- "acc_phon"
my_count <- "value"
my_table_m <- paste(getwd(), "/Ch_6_Form/output/pn_foot_adj_M.csv", sep = "")
my_table_f <- paste(getwd(), "/Ch_6_Form/output/pn_foot_adj_F.csv", sep = "")
my_table_g <- paste(getwd(), "/Ch_6_Form/output/pn_foot_adj_all_gender.csv", sep = "")
options("speakr.praat.path" = "C:/Program Files/Praat/Praat.exe")
praat_run(
  script, my_treatment, my_response, my_count, my_table_m, my_table_f, my_table_g
  )



