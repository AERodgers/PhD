### OLD FUNCTIONS
###
### ###  Bonferonni Adjustment  ####################################################
bonferroniAdjust <- function(myTibble,
                             bonferroniMultiplier=0,
                             exclude_terms = NULL,
                             p.adj="p.adj")
{
    if (bonferroniMultiplier)
    {
        p.adj = enquo(p.adj)

        myTibble <- mutate(myTibble,
                           !!p.adj := if_else(
                               !is_null(exclude_terms) & term %in% exclude_terms,
                               NA_real_,
                               if_else(
                                   p.value * bonferroniMultiplier > 1,
                                   1,
                                   p.value * bonferroniMultiplier
                               )
                           )
        )
    }
    return(myTibble)
}

###  Parameter Summary ##########################################################
param_summary <-
    function(df, treatment, phonology, is_nucleus = FALSE)
    {
        # Summarise dataset by speaker, treatment variable, and phonology
        treatment <- enquo(treatment)
        phonology  <- enquo(phonology)

        if (is_nucleus == TRUE)
        {
            ans <- df %>%
                group_by(speaker, !!treatment, !!phonology) %>%
                summarise(
                    l_t = round(mean(l_t), 2),
                    h_t = round(mean(h_t), 2),
                    lh_dur = round(mean(lh_dur), 2),
                    l_f0 = round(mean(l_f0), 2),
                    l_f0_z = round(mean(l_f0_z), 3),
                    h_f0 = round(mean(h_f0), 2),
                    h_f0_z = round(mean(h_f0_z), 3),
                    e_f0 = round(mean(e_f0, 2)),
                    e_f0_z = round(mean(e_f0_z, 3)),
                    e_f0_exc_z = round(mean(e_f0_exc_z, 3)),
                    e_t = round(mean(e_t, 2)),
                    he_dur = round(mean(he_dur, 2)),
                    f0_exc = round(mean(f0_exc), 2),
                    f0_exc_z = round(mean(f0_exc_z), 3),
                    lh_slope = round(mean(lh_slope), 2),
                    .groups = "keep"
                )
        } else {
            ans <- df %>%
                group_by(speaker, !!treatment, !!phonology) %>%
                summarise(
                    s_t = round(mean(s_t, 2)),
                    s_f0_z = round(mean(s_f0_z, 3)),
                    l_t = round(mean(l_t), 2),
                    h_t = round(mean(h_t), 2),
                    lh_dur = round(mean(lh_dur), 2),
                    l_f0 = round(mean(l_f0), 2),
                    l_f0_z = round(mean(l_f0_z), 3),
                    h_f0 = round(mean(h_f0), 2),
                    h_f0_z = round(mean(h_f0_z), 3),
                    f0_exc = round(mean(f0_exc), 2),
                    f0_exc_z = round(mean(f0_exc_z), 3),
                    lh_slope = round(mean(lh_slope), 2),
                    .groups = "keep"
                )
        }

        return(ans)

    }


###  Get Parameter Means  ######################################################
param_means <-
    function(df,
             phonet_param,
             hor_param,
             vert_param,
             rounding = 2)
    {
        # Return a horizontal x vertical tibble summarising mean parameter phonetic.

        phonet_param <- enquo(phonet_param)
        hor_param  <- enquo(hor_param)
        vert_param  <- enquo(vert_param)

        return(
            df %>%
                group_by(!!hor_param, !!vert_param) %>%
                summarise(new_means = round(mean(!!phonet_param), rounding),
                          .groups = "keep") %>%
                pivot_wider(names_from = !!hor_param, values_from = new_means)
        )
    }

###  Get Data object name as String from LME/GLM model #########################
getModelDataName <-function(my_model) {
    return(my_model@call[["data"]])
}



tidyPrediction <- function(model, write = NULL) {
    require("knitr")
    require("kableExtra")
    require("ggeffects")

    pred_list <- ggpredict(model)
    obj_names <- names(pred_list)
    obj_i = 0

    for (cur_obj in pred_list)
    {
        obj_i = obj_i + 1
        cur_obj_name <- obj_names[obj_i]
        cur_caption <- paste(cur_obj %>% get_title, "re", cur_obj_name)
        save_address <- paste(write, "_", cur_obj_name, ".csv", sep = "")
        cur_obj_name = enquo(cur_obj_name)
        cur_table <- as_tibble(cur_obj) %>%
            select(-group) %>%
            relocate(std.error, .after = conf.high) %>%
            mutate(
                x = str_replace_all(x,
                                    "(\\_|\\[|\\]|\\$|\\^|\\>)",
                                    "\\\\\\1"),
                across(
                    2:last_col(),
                    ~ as.numeric(.)
                ),
                across(
                    2:last_col(),
                    ~ if_else(
                        abs(.) < 0.001,
                        as.character(formatC(
                            ., format = "e", digits = 1
                        )),
                        as.character(round(., 2))
                    )
                )
            ) %>%
            rename(!!cur_obj_name := x)

        cur_table %>%
            knitr::kable(caption = cur_caption) %>%
            kable_styling(full_width = FALSE, position = "left") %>%
            print(cur_table)

        if(!is.null(write)){write_csv(cur_table, save_address)}


    }
}

