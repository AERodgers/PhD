# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
###  Bulk Adjust p Value   ####################################################
adjustP_posthoc <-
  function(my_folder,            # source folder with .csv files to be updated.
           p_column="p,value",   # name of p.column
           method = "BH",        # p. adjustment method
           marginal = TRUE,      # include marginal significance flag.
           write = TRUE,         # write results to file flag.
           report = FALSE,       # flag to report total number of tests and
                                 # p.values<0.05 before and after adjustment.
           suffix_id="b0b1"      # suffix ID for files for analysis
                                 # (b0b1 = all files ending in "_b0" and "_b1")
  )
  {
    # Load required packages
    require("dplyr")
    require("readr")
    require("mefa4")

    # Abbreviate method where necessary.
    if(method %in% c("hochberg", "hommel", "bonferroni")) {
      my_meth <- switch(method,
                        "hochberg" = "hoch",
                        "hommel" = "homm",
                        "bonferroni" = "bonf")
    }
    else{my_meth <- method}

    # Enquote variables which whose values will be evaluated as variables.
    p_column = enquo(p_column)
    new_adj_col = paste("p.adj (", method, ")", sep="")
    new_adj_col = enquo(new_adj_col)

    if(suffix_id=="b0b1"){
      # Get tibble of all b0 and b1 files to be adjusted.
      file_tibble <-
        list.files(my_folder, "*_b0.csv", full.names = TRUE) %>%
        read_csv(id = "file_name",
                 col_names = TRUE,
                 show_col_types = FALSE) %>%
        # Create dummy slope column for b0 files.
        mutate(slope = NA, .before = intercept) %>%
        rbind(
          list.files(my_folder, "*_b1.csv", full.names = TRUE) %>%
            read_csv(id = "file_name", col_names = TRUE, show_col_types = FALSE)
        ) %>%
        # avoid reduplication of current method column
        select(-any_of(!!new_adj_col)) %>%
        # Add p.adjusted column using method.
        mutate(p.adj = p.adjust(!!p_column,
                                method = method),
               .after = !!p_column) %>%
        relocate(intercept)}
    else {
      # Get tibble of files to be adjusted
      file_tibble <-
        list.files(my_folder,
                   paste("*", suffix_id, ".csv", sep=""),
                   full.names = TRUE) %>%
        read_csv(id = "file_name",
                 col_names = TRUE,
                 show_col_types = FALSE) %>%

        # avoid reduplication of current method column
        select(-any_of(c(!!new_adj_col, signif.))) %>%
        # Add p.adjusted column using method.
        mutate(p.adj = p.adjust(!!p_column,
                                method = method),
               .after = !!p_column) %>%
        relocate(intercept)

    }

    # Get summary info about p values.
    p_values <- file_tibble %>% nrow()
    sig_p_values <- file_tibble %>% filter(!!p_column < 0.05) %>% nrow()
    sig_p_values_adj  <- file_tibble %>% filter(p.adj < 0.05) %>% nrow()
    p_counts <- tibble(p_values, sig_p_values, sig_p_values_adj)

    file_tibble <- file_tibble %>%
      mutate(
        # Add significance column.
        signif. = if_else(
          p.adj < 0.0001, "p<0.0001", if_else(
            p.adj < 0.001, "p<0.001", if_else(
              p.adj < 0.01, "p<0.01", if_else(
                p.adj < 0.05, "p<0.05", if_else(
                  p.adj < 0.1 & marginal, "(p<0.1)",""))))),
        # Change p.adj and p_column to more readable format.
        p.adj = if_else(p.adj < 0.001,
                        as.character(formatC(p.adj, format="e", digits = 2)),
                        as.character(round(p.adj, 4), digits = 2)),
        !!p_column := if_else(
          !!p_column < 0.001,
          as.character(formatC(!!p_column, format="e", digits = 2)),
          as.character(round(!!p_column, 4), digits = 2))
      ) %>%
      # Change name of p.adj to indicate adjustment method.
      rename(!!new_adj_col := p.adj)

    # Re-save updated tables as original file name.
    if (write) {
      for (cur_file in unique(file_tibble$file_name))
      {
        cur_set <- file_tibble %>%
          filter(file_name == cur_file) %>%
          select(-file_name)
        if (is.na(cur_set$slope[1])) {
          cur_set <- cur_set %>% select(-slope)
        }
        write_csv(cur_set, cur_file)
      }
    }
    if(report){return(p_counts)}
  }
