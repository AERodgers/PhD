




###  Tidy Intercepts of multiple analyses  #####################################
tidyIntercepts <- function(all_models_tidy)
{
    return(
        filter(all_models_tidy, pairwise == "intercept") %>%
            select(-pairwise) %>%
            rename(intercept = term)
    )
}


###  Tidy Pairwise Tables of multiple analyses  ################################
tidyPairwise <- function(all_models_tidy, is_GLM = FALSE)
{

    my_stat <- ifelse(is_GLM, "z.value", "t.value")
    my_headers <- c(
        "pairwise",
        "term",
        "estimate",
        "conf.low",
        "conf.high",
        "std.error",
        eval(my_stat),
        "df",
        "p.value"
    )

    if (is_GLM){my_headers <- my_headers [my_headers != "df"]}
    my_headers = enquos(my_headers)
    return(
        filter(all_models_tidy, pairwise %notin% c("intercept", "N/A")) %>%
            select(!!!my_headers) %>%
            rename(intercept = pairwise, slope = term)
    )
}







#OLD VERSION


getModelFixedFX <- function(my_equation,
                            my_data,
                            is_separation = FALSE,
                            write = NULL,
                            is_GLM = FALSE,
                            is_Bayesian = T,
                            exponentiate = TRUE,
                            optimizer = "optimx",
                            extra_text = "",
                            report = c("slopes", "intercepts"),
                            b1_only = NULL,
                            ignore_list = "",
                            post_hoc_method = "BH")
{
    require("formattable")
    require("tidyverse")
    require("mefa4")
    require("lme4")
    require("blme")

    my_stat <- ifelse(is_GLM, "z.value", "t.value")

    my_headers <- c(
        "term",
        "estimate",
        "conf.low",
        "conf.high",
        "std.error",
        eval(my_stat),
        "df",
        "p.value"
    )

    my_stat = enquo(my_stat)

    post_hoc_method <- paste("p.adj (",
                             shortPAsjMeth(post_hoc_method),
                             ")",
                             sep="")
    post_hoc_method <- enquo(post_hoc_method)

    if (is_GLM) {
        my_headers <- my_headers[my_headers != "df"]
    }
    my_headers = enquos(my_headers)


    # run base model.
    if (is_GLM) {

        # RUN BLGMER
        if (!is_separation & is_Bayesian) {
            # don't use prior
            base_model <- bglmer(
                my_equation,
                data = my_data,
                family = binomial(link = "logit"),
                # Change optimizer to avoid convergence errors/
                control = glmerControl(
                    optimizer = optimizer,
                    calc.derivs = FALSE,
                    optCtrl = list(
                        method = "nlminb",
                        starttests = FALSE,
                        kkt = FALSE
                    )
                )
            )
        }
        else if (is_separation & is_Bayesian)
        {
            # Use prior
            base_model <- bglmer(
                my_equation,
                data = my_data,
                family = binomial(link = "logit"),
                fixef.prior = normal(),
                # Change optimizer to avoid convergence errors/
                control = glmerControl(
                    optimizer = optimizer,
                    calc.derivs = FALSE,
                    optCtrl = list(
                        method = "nlminb",
                        starttests = FALSE,
                        kkt = FALSE
                    )
                )
            )
        }
        # USE GLMER (not bayesian)
        else  {
            base_model <- glmer(
                my_equation,
                data = my_data,
                family = binomial(link = "logit"),
                # Change optimizer to avoid convergence errors/
                control = glmerControl(
                    optimizer = optimizer,
                    calc.derivs = FALSE,
                    optCtrl = list(
                        method = "nlminb",
                        starttests = FALSE,
                        kkt = FALSE
                    )
                )
            )
        }
    }
    # RUN LMER
    else {
        base_model <- lmer(
            my_equation,
            data = my_data,
            control = lmerControl(
                optimizer = optimizer,
                calc.derivs = FALSE,
                optCtrl = list(
                    method = "nlminb",
                    starttests = FALSE,
                    kkt = FALSE
                )
            )
        )
    }


    # create empty tidy model output
    all_models_tidy = tibble()
    # create list of fixed factors
    all_fixed_factors <-
        (str_replace_all(deparse(formula(
            base_model, fixed.only = TRUE
        )[3]),
        "[\\(|\\)|+ ]",
        " ") %>%
            str_squish() %>%
            str_split(" "))
    fixed_factors <- NULL
    for (cur_list in all_fixed_factors)
    {fixed_factors <- c(fixed_factors, cur_list)}

    if (!is.null(b1_only))
    {
        fixed_factors <- fixed_factors[fixed_factors %notin% b1_only]
        two_level_factors <- b1_only
        two_level_terms <- b1_only
        # two_level_terms is the name of the factor, e.g., "gender"
        # two_level_factors is the factor + the non-intercept level, e.g, "genderM",
        # or the factor if it is continuous.
    }
    else
    {
        two_level_factors <- character()
        two_level_terms <- character()
    }
    # Get list of non-interaction factors
    multilevel_factors <-
        fixed_factors[fixed_factors %in% colnames(my_data)]
    multilevel_factors <-
        multilevel_factors[multilevel_factors %notin% ignore_list]
    # Get list of 2-level fixed factors.



    for (cur_factor in multilevel_factors) {
        if (levels(my_data[[cur_factor]]) %>% length() == 2) {
            two_level_factors <- c(two_level_factors, cur_factor)
            two_level_terms <- c(two_level_terms,
                                 paste(cur_factor,
                                       levels(my_data[[cur_factor]])[2],
                                       sep = ""))
        }
    }
    # remove 2-level factors from list of multilevel_factors
    multilevel_factors <-
        multilevel_factors[multilevel_factors %notin% two_level_factors]

    # loop through each multilevel fixed factor of interest (multilevel_factors)
    for (cur_factor in multilevel_factors)
    {
        # Get levels for current factor
        cur_levels = levels(my_data[[cur_factor]])
        num_levels = length(cur_levels)
        # Make list of terms to keep
        keep_terms = character()
        for (level_name in cur_levels) {
            keep_terms = c(keep_terms, paste(cur_factor, level_name, sep = ""))
        }
        # loop through dataframe, reordering levels of current factor each time.
        for (cur_level in 1:(num_levels))
        {
            # Run current model.
            if (is_GLM) {
                if (!is_separation & is_Bayesian)
                    # RUN BGLMER W/O SEPARATION ISSUE
                {
                    # don't use prior
                    cur_model <- bglmer(
                        my_equation,
                        data = my_data,
                        family = binomial(link = "logit"),
                        # Change optimizer to avoid convergence errors/
                        control = glmerControl(
                            optimizer = optimizer,
                            calc.derivs = FALSE,
                            optCtrl = list(
                                method = "nlminb",
                                starttests = FALSE,
                                kkt = FALSE
                            )
                        )
                    )
                }
                else if (is_separation & is_Bayesian)
                    # RUN BGLMER WITH SEPARATION ISSUE
                {
                    # Use prior
                    cur_model <- bglmer(
                        my_equation,
                        data = my_data,
                        family = binomial(link = "logit"),
                        fixef.prior = normal(),
                        # Change optimizer to avoid convergence errors/
                        control = glmerControl(
                            optimizer = optimizer,
                            calc.derivs = FALSE,
                            optCtrl = list(
                                method = "nlminb",
                                starttests = FALSE,
                                kkt = FALSE
                            )
                        )
                    )
                }
                else
                    # RUN GLMER (NON-BAYESIAN)
                {
                    cur_model <- glmer(
                        my_equation,
                        data = my_data,
                        family = binomial(link = "logit"),
                        # Change optimizer to avoid convergence errors/
                        control = glmerControl(
                            optimizer = optimizer,
                            calc.derivs = FALSE,
                            optCtrl = list(
                                method = "nlminb",
                                starttests = FALSE,
                                kkt = FALSE
                            )
                        )
                    )
                }
            }
            # RUN LME
            else {
                cur_model <- lmer(
                    my_equation,
                    data = my_data,
                    control = lmerControl(
                        optimizer = optimizer,
                        calc.derivs = FALSE,
                        optCtrl = list(
                            method = "nlminb",
                            starttests = FALSE,
                            kkt = FALSE
                        )
                    )
                )
            }

            # Tidy the model.
            # Decide whether or not to exponentiate GLM data
            if (is_GLM) {
                cur_model_tidy <-
                    tidy(cur_model,
                         exponentiate = exponentiate,
                         conf.int = T)
            }
            else{
                cur_model_tidy <- tidy(cur_model, conf.int = T)
            }

            cur_model_tidy <- cur_model_tidy %>%
                # Retain fixed factors only
                filter(effect == "fixed") %>%
                # remove unnecessary columns
                select(-c(effect, group)) %>%
                # Tidy up numbers.
                mutate(
                    estimate = round(estimate, 3),
                    std.error = round(std.error, 3),
                    statistic = round(statistic, 3)
                ) %>%
                rename(!!my_stat := statistic)



            if (!is_GLM) {
                cur_model_tidy <- cur_model_tidy %>%
                    mutate(df = round(df, 2))
            }
            cur_model_tidy <- cur_model_tidy %>%
                # re-order columns
                select(!!!my_headers) %>%
                filter((term %in% c(keep_terms, b1_only, "(Intercept)"))) %>%
                # Prepare current model for pasting to all models output.
                # Make 'pairwise' column = intercept.
                mutate(
                    pairwise =
                        if_else(
                            term == "(Intercept)",
                            "intercept",
                            if_else(term %notin% c(keep_terms, b1_only),
                                    "N/A",
                                    keep_terms[cur_level])
                        ),
                    # change 'term' so "intercept" states the target condition name.
                    term =
                        if_else(term == "(Intercept)",
                                keep_terms[cur_level],
                                term)
                )

            keep_comparisons = b1_only
            # make list of pairwise comparisons to keeps
            for (j in cur_level:(num_levels))
            {
                keep_comparisons <- c(keep_comparisons, keep_terms[j])
            }

            # remove pairwise comparisons which have already been done
            cur_model_tidy <-
                filter(cur_model_tidy, term %in% keep_comparisons)

            # add remaining pairwise comparisons to main tibble.
            all_models_tidy <- bind_rows(all_models_tidy, cur_model_tidy)

            # restructure the order of levels for next LME cur_model.
            cur_levels <- c(cur_levels[2:num_levels], cur_levels[1])

            factor_var <- sym(cur_factor)
            factor_var_name <- quo_name(cur_factor)

            my_data <- my_data %>%
                mutate(!!factor_var := factor(!!factor_var,
                                              levels = cur_levels))
        }
    }

    # Get intercepts and pairwise comparisons tables
    all_models_tidy <- all_models_tidy %>%
        relocate(pairwise) %>%
        # remove continuous variables which have crept into the model output.
        filter(term %notin% b1_only)

    my_intercepts <- tidyIntercepts(all_models_tidy)
    my_pairwise <- tidyPairwise(all_models_tidy, is_GLM = is_GLM)

    if (is_GLM) {
        two_level_factor_slopes <- tidy(base_model,
                                        conf.int = T,
                                        exponentiate = exponentiate) %>%
            filter(effect %notin% "ran_pars") %>%
            mutate(
                estimate = round(estimate, 3),
                std.error = round(std.error, 3),
                statistic = round(statistic, 3)
            )
    }
    else
    {
        two_level_factor_slopes <- tidy(base_model, conf.int = T) %>%
            filter(effect %notin% "ran_pars") %>%
            mutate(
                estimate = round(estimate, 3),
                std.error = round(std.error, 3),
                statistic = round(statistic, 3)
            )
    }

    if (!is_GLM) {
        two_level_factor_slopes <- two_level_factor_slopes %>%
            mutate(df = round(df, 2))
    }

    two_level_factor_slopes <- two_level_factor_slopes %>%
        filter(term %in% two_level_terms) %>%
        select(-c(group, effect)) %>%
        mutate(
            estimate = round(estimate, 3),
            std.error = round(std.error, 3),
            statistic = round(statistic, 3),
        ) %>%
        mutate(intercept = "intercept", .before = term)  %>%
        relocate(conf.high, .after = "std.error") %>%
        relocate(conf.low, .after = "std.error") %>%
        rename(!!my_stat := statistic,
               slope = term)
    # put B1 only parameters at bottom of tibble
    my_pairwise.temp <- my_pairwise %>%
        filter(slope %in% (b1_only)) %>%
        arrange(slope)
    my_pairwise <- my_pairwise %>%
        filter(slope %notin% (b1_only)) %>%
        rbind(my_pairwise.temp)


    # bind two-level factor stats to my_pairwise
    my_pairwise <- rbind(my_pairwise, two_level_factor_slopes)

    # Write tables to file
    my_formula <- getModelFormula(base_model)
    if (!is.null(write))
    {
        write(
            paste(str_replace_all(my_formula, "\\`", ""), extra_text, sep = ""),
            paste(write, "_formula.txt", sep = "")
        )
        if ("intercepts" %in% report)
        {
            write_csv(my_intercepts %>% mutate( !!post_hoc_method := NA,signif. = NA),
                      paste(write, "_b0.csv", sep = ""))
        }
        if ("slopes" %in% report)
        {
            write_csv(my_pairwise %>% mutate( !!post_hoc_method := NA,signif. = NA),
                      paste(write, "_b1.csv", sep = ""))
        }
    }
    # Output formatted tables
    my_intercepts <- my_intercepts %>%
        mutate(intercept = str_replace_all(intercept, "([\\*\\[\\^\\>])", "\\\\\\1")) %>%
        formattable(
            caption = paste(
                "b0 for",
                str_replace_all(my_formula, "\\`", ""),
                extra_text,
                sep = " "
            ),
            title = ""
        ) %>%
        mutate(across(
            c(p.value, estimate, conf.low, conf.high),
            ~ if_else(
                abs(.) < 0.001 | abs(.) > 100000,
                as.character(formatC(
                    ., format = "e", digits = 1
                )),
                if_else(
                    abs(.) < 10,
                    as.character(round(., 3), digits = 3),
                    as.character(round(., 1), digits = 1),
                )
            )
        ))

    my_pairwise <-  my_pairwise %>%
        mutate(
            intercept = str_replace_all(intercept, "([\\*\\[\\^\\>])", "\\\\\\1"),
            slope = str_replace_all(slope, "([\\*\\[\\^\\>])", "\\\\\\1")
        ) %>%
        formattable(
            caption = paste(
                "b1 for",
                str_replace_all(my_formula, "\\`", ""),
                extra_text,
                sep = " "
            ),
            title = ""
        ) %>%
        mutate(across(
            c(p.value, estimate, conf.low, conf.high),
            ~ if_else(
                abs(.) < 0.001 | abs(.) > 100000,
                as.character(formatC(
                    ., format = "e", digits = 1
                )),
                if_else(
                    abs(.) < 10,
                    as.character(round(., 3), digits = 3),
                    as.character(round(., 1), digits = 1),
                )
            )
        ))

    return(list(
        "intercepts" = my_intercepts,
        "slopes" = my_pairwise,
        "model" = base_model
    ))
}


