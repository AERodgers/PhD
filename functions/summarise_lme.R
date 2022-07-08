# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
###  Summarise LME  ############################################################
summarise_lme <-
  function(my_model, run_step = FALSE, my_tolerance = 1e-05, write=NULL)
    # short function to remove need for repetition of optimized used throughout.
  {
    require("lme4")
    require("lmerTest")
    require("optimx")
    require("performance")

    my_formula <- str_c(formula(my_model))
    my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])

    # output results
    drawResiduals(my_model)
    print(summary(my_model))

    anova <- anova(my_model) %>%
      tidy() %>%
      formattable(caption=paste("Anova of model:", my_formula)) %>%
      sigCodesTidy(p.value, FALSE) %>%
      rename(`F value` = statistic)
    if(!is_null(write)){
      write_csv(anova, write)
    }


    cat("\nCheck_singularity(my_model, tolerance =",
        my_tolerance,
        "-->",
        check_singularity(my_model, tolerance=my_tolerance),
        "\n"
    )


    if (run_step)
    {
      cat("\n")
      cat("\nResults of step().\n")
      step(my_model) %>% print()
    }

  return(anova)
  }
