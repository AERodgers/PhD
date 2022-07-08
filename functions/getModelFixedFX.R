# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Includes three functions
#     - getModelFixedFX()
#     - tidyIntercepts()
#     - tidyPairwise()
#
###  Get Fixed Effects of LME/GLMM Model #######################################
getModelFixedFX <- function(my_equation,
                       my_data,
                       exclude_terms = NULL,
                       bf_adj = 0,
                       write="",
                       is_GLM=FALSE,
                       optimizer = "optimx")
{
  require("formattable", "performance", "tidyverse", "mefa4")

  my_stat <- ifelse(is_GLM, "z.value", "z.value")

  my_headers <- c(
    "term",
    "estimate",
    "std.error",
    "2.5% CI",
    "97.5% CI",
    eval(my_stat),
    "df",
    "p.value"
  )

  my_stat = enquo(my_stat)

  if (is_GLM){my_headers <- my_headers[my_headers != "df"]}
  my_headers = enquos(my_headers)


  # run base model.
  if (is_GLM) {
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

  my_formula <- str_c(formula(base_model))
  my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])

  # create list of fixed factors
  fixed_factors <-
    (str_replace_all(deparse(formula(base_model,fixed.only = TRUE)[3]),
                     "[\\(|\\)|+ ]",
                     " ") %>%
       str_squish() %>%
       str_split(" "))[[1]]
  # make list of factors for intercepts and pairwise comparison tables.
  if (!is_null(exclude_terms)) {
    keep_factors <- fixed_factors[fixed_factors %notin% exclude_terms]
  # Ensure only factors representing my_data columns are included.

    keep_factors <- keep_factors[keep_factors %in% colnames(my_data)]
  }
  else{
    keep_factors <- fixed_factors[fixed_factors %in% colnames(my_data)]
  }

  # create empty tibble for fixed factor output
  all_models_tidy = tibble()

  # loop through each fixed factor of interest (keep_factors)
  for (cur_factor in keep_factors)
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
      cur_model_tidy <- tidy(cur_model) %>%
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

      if (!is_GLM){
        cur_model_tidy <- cur_model_tidy %>%
          mutate(df = round(df, 2))
      }

      # Get CIs of model using Wald method (fast, fixed effects only)
      ci_Wald <- confint(cur_model, method = "Wald") %>%
        as_tibble() %>%
        filter(`2.5 %` != "NA") %>%
        mutate(`2.5 %` = round(`2.5 %`, 3),
               `97.5 %` = round(`97.5 %`, 3)) %>%
        rename("2.5% CI" = "2.5 %",
               "97.5% CI" = "97.5 %")

      # Bind tidy model to CI intervals
      cur_model_tidy <- cbind(cur_model_tidy, ci_Wald) %>%
        # re-order columns
        select(!!!my_headers)

      cur_model_tidy <- cur_model_tidy %>%
        filter((term %in% c(keep_terms, "(Intercept)"))) %>%
        # Prepare current model for pasting to all models output.
        # Make 'pairwise' column = intercept.
        mutate(
          pairwise =
            if_else(
              term == "(Intercept)",
              "intercept",
              if_else(term %notin% keep_terms,
                      "N/A",
                      keep_terms[cur_level])
            ),
          # change 'term' so "intercept" states the target condition name.
          term =
            if_else(term == "(Intercept)",
                    keep_terms[cur_level],
                    term)

        )

      keep_comparisons = NULL
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
    relocate(pairwise)

  my_intercepts <- tidyIntercepts(all_models_tidy)
  my_pairwise <- tidyPairwise(all_models_tidy, is_GLM=is_GLM)

  # Write tables to file
  if (write != "")
  {
    write_csv(my_intercepts,
              paste(write, "_b0.csv", sep = ""))
    write_csv(my_pairwise,
              paste(write, "_b1.csv", sep = ""))

  }

  # Output formatted tables
  my_intercepts <- my_intercepts %>%
    mutate(
      intercept = str_replace_all(intercept, "([\\*\\[\\^\\>])", "\\\\\\1")
      ) %>%
    formattable(caption = paste("b0 for", my_formula, sep = " "))

  my_pairwise <-  my_pairwise %>%
    mutate(
      intercept = str_replace_all(intercept, "([\\*\\[\\^\\>])", "\\\\\\1"),
      slope = str_replace_all(slope, "([\\*\\[\\^\\>])", "\\\\\\1")
    ) %>%
    formattable(caption = paste("b1 for", my_formula, sep = " "))

  return(list("intercepts" = my_intercepts, "pairwise" = my_pairwise))
}

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

  my_stat <- ifelse(is_GLM, "z.value", "z.value")

  my_headers <- c(
    "pairwise",
    "term",
    "estimate",
    "std.error",
    "2.5% CI",
    "97.5% CI",
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


