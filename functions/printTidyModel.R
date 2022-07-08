# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
###  Print Tidy Model  #########################################################
printTidyModel <-
  function(my_model,
           bf_adj = 1,
           write_r2 = "",
           is_GLM = FALSE)
  {
    require("formattable")
    require("tidyverse")
    require("mefa4")
    require("performance")
    require("kableExtra")

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


  my_formula <- str_c(formula(my_model))
  my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])

  tidy_model <- tidy(my_model) %>%
    filter(effect %notin% "ran_pars") %>%
    select(-c(group, effect)) %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3)
    ) %>%
    rename(!!my_stat := statistic)

  if (!is_GLM){
    tidy_model <- tidy_model %>%
    mutate(df = round(df, 2))
    }

  # Get CIs of model using Wald method (fast, fixed effects only)
  ci_Wald <- confint(my_model, method = "Wald") %>%
    as_tibble() %>%
    filter(`2.5 %` != "NA") %>%
    mutate(`2.5 %` = round(`2.5 %`, 3),
           `97.5 %` = round(`97.5 %`, 3)) %>%
    rename("2.5% CI" = "2.5 %",
           "97.5% CI" = "97.5 %")

  # Bind tidy model to CIs
  tidy_model <-
  cbind(tidy_model, ci_Wald) %>%
    # re-order columns
    select(!!!my_headers) %>%
    formattable(
      caption = paste(my_formula)
    ) %>%
    mutate(term = str_replace_all(term, "([\\*\\[\\^\\>])", "\\\\\\1"))

  r2_nakagawa <-  knitr::kable(
    r2_nakagawa(my_model),
    caption = "Conditional and marginal R^2^ of model",
    digits = 2,
    align = "l"
  )  %>% kable_styling(full_width = FALSE, position="left")


  if (write_r2 != "") {
    do.call(rbind, r2_nakagawa(my_model))[, 1] %>%
      write.csv(paste(write_r2, "_r2.csv", sep = ""))
  }

  my_plot <- print(
    plot_model(
      my_model,
      title = my_formula,
      show.intercept = FALSE,
      show.values = TRUE,
      vline.color = "red",
      colors = "Black"
    )
  )
return(list("r2" = r2_nakagawa, "table" = tidy_model, "plot" = my_plot))
}
