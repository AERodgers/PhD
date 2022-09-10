#123456789#123456789#123456789#123456789#123456789#123456789#123456789#123456789
# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Bank of personal functions

require("tidyverse")
require("parallel")
ncores <- detectCores()


###  Set themes, colours schemes, and formatters ###############################

# Set themes and colour schemes.
theme_set(theme_minimal(base_size = 10))

# Change this as required
options("speakr.praat.path" = "C:/Program Files/Praat/Praat.exe")


###
###  Install Missing Packages ##################################################
installMissingPackages <- function (package_list)
{
  # installs packages from  package_list which are not already installed in.
  installed_packages <-
    package_list %in% rownames(installed.packages())

  if (any(installed_packages == F))
  {
    install.packages(package_list[!installed_packages])
  }

  # Packages loading
  invisible(lapply(
    package_list,
    suppressPackageStartupMessages(library),
    character.only = T
  ))

}
###
###  Balance data  #############################################################
balancedData <- function(data_set,
                         treatment_col,
                         response_col,
                         gender_filter,
                         num_speakers,
                         num_reps,
                         use_pa_hierarchy = T)
{
  # Returns an data matrix of phonological data of a projected balanced dataset.
  #    This function takes a subset of the data, calculates each token count
  #     per speaker per condition as a ratio of the total tokens for that
  #     condition and multiplies it by a constant reflecting the target token
  #     for that utterance. It then calculates the total number of tokens per
  #     condition as a ratio of the total number of speakers represented for
  #     that condition. Again, this is multiplied by a constant to reflect the
  #     intended number of speakers. The values are then rounded to an integer
  #     to approximate the ideal total tokens for the dataset.
  #
  #     data_set ....... target corpus data set
  #     treatment_col .. treatment column / independent variable)
  #     response_col ... response column / dependent variable (phonology)
  #     gender_filter .. "F" for female only, "M" for male only, "" for all
  #     num_speakers ... number of speakers in ideal corpus
  #     num_reps ....... number of repetitions per speaker in ideal corpus.
  if (gender_filter == "M") {
    data_set <- data_set %>% filter(gender == "M")
  }
  else {
    if (gender_filter == "F") {
      data_set <- data_set %>% filter(gender == "F")
    }
  }

  treatment_col = enquo(arg = treatment_col)
  response_col = enquo(arg = response_col)

  # Get number of speakers per target.
  speakers_per_target = data_set %>%
    select(speaker, !!treatment_col) %>%
    group_by(!!treatment_col) %>%
    summarise(speakers = n_distinct(speaker), .groups = "keep")

  # Get number of reps per speaker per target.
  pn_foot_reps <- data_set %>%
    group_by(speaker, !!treatment_col) %>%
    summarise(acc_count = n(), .groups = "keep")
  kable(pn_foot_reps)

  # Get number of PA tokens per speaker per target
  pn_foot_summary <- data_set %>%
    group_by(speaker, !!treatment_col, !!response_col) %>%
    summarise(acc_count = n(), .groups = "keep") %>%
    spread(!!response_col, acc_count, is.na <- 0)

  balanced <- left_join(pn_foot_summary, pn_foot_reps)

  pa_columns <-
    colnames(balanced)[3:(length(colnames(balanced)) - 1)]

  # Convert tokens to ratios of tokens per speaker per target
  balanced <- balanced %>%

    mutate(across(pa_columns,  ~  (.x / acc_count * num_reps))) %>%
    group_by(!!treatment_col) %>%
    select(-speaker, -acc_count)

  num_cols <- length(colnames(balanced))

  balanced <- balanced %>%
    gather(!!response_col, mod_count, 2:num_cols)

  # Arrange PA levels according to hypothesized hierarchy.
  if (use_pa_hierarchy) {
    balanced <- balanced %>%
      mutate(acc_phon = factor(!!response_col,
                               levels = c("(*)", "L*", "H*", ">H*", "L*H")))
  }

  balanced <- balanced %>%
    group_by(!!treatment_col, !!response_col) %>%
    summarise(mod_sum = sum(mod_count), .groups = "keep") %>%
    spread(!!response_col, mod_sum) %>%
    # Adjust token count re number of speakers per target condition.
    left_join(speakers_per_target) %>%
    mutate(across(pa_columns,  ~  round(.x / speakers * num_speakers))) %>%
    select(-speakers)

  return(balanced)
}


###
###  Significance Code Tidy  ###################################################
sigCodesTidy <-
  function(my_tibble,
           p_value = "p.adj",
           incl_marginal_sig = T)
    # Create significance column in tibble using p_value
  {
    p_value <- enquo(p_value)
    my_tibble <- mutate(my_tibble,
                        `signif.` =
                          if_else(
                            !!p_value < 0.001,
                            'p<0.001',
                            if_else(
                              !!p_value < 0.01,
                              'p<0.01',
                              if_else(
                                !!p_value < 0.05,
                                'p<0.05',
                                if_else(!!p_value < 0.1 &
                                          incl_marginal_sig,
                                        '(p<0.1)',
                                        '')
                              )
                            )
                          )) %>%
      mutate(`signif.` =
               if_else(
                 !is.na(!!p_value),
                 `signif.`,
                 if_else(
                   p.value < 0.001,
                   "p<0.001",
                   if_else(
                     p.value < 0.01,
                     "p<0.01",
                     if_else(
                       p.value < 0.05,
                       "p<0.05",
                       if_else(p.value < 0.1 &
                                 incl_marginal_sig,
                               "(p<0.1)",
                               "")
                     )
                   )
                 )
               ))



    return(my_tibble)
  }


###
###  Get m_corpus #############################################################
get_m_corpus <- function(file_address)

  # Include package for %in% / %notin% syntactic notation
  {
  installMissingPackages(c("mefa4"))
  return(
    as_tibble(read.csv(file_address)) %>%
      # Only keep pertinent columns!
      select(
        speaker,
        gender,
        stim,
        cur_foot,
        tot_feet,
        init_phon,
        phr_phon,
        fin_phon,
        foot_syls,
        tot_syls,
        acc_phon,
        foot_start_t,
        foot_end_t,
        v_onset_t,
        l_t,
        h_t,
        s_t,
        e_t,
        v_grand_mean_t,
        l_grand_mean_t,
        h_grand_mean_t,
        s_grand_mean_t,
        e_grand_mean_t,
        l_f0,
        h_f0,
        s_f0,
        e_f0,
        phr_end_t,
        lh_mean_f0,
        lh_slope,
        lh_slope_z,
        utt_slope,
        utt_mean_f0,
        utt_slope_z,
        spkr_f0_mean,
        spkr_f0_SD
      ) %>%
      mutate(
        # create composite parameters for continuous data.
        foot_dur = foot_end_t - foot_start_t,
        speech_rate = round(tot_syls / phr_end_t * 1000, 3),
        f0_exc = h_f0 - l_f0,
        e_f0_exc = e_f0 - h_f0,
        lh_dur = h_t - l_t,
        he_dur = e_t - h_t,
        l_f0_z = (l_f0 - spkr_f0_mean) / spkr_f0_SD,
        h_f0_z = (h_f0 - spkr_f0_mean) / spkr_f0_SD,
        s_f0_z = (s_f0 - spkr_f0_mean) / spkr_f0_SD,
        e_f0_z = (e_f0 - spkr_f0_mean) / spkr_f0_SD,
        utt_mean_f0_z = (utt_mean_f0 - spkr_f0_mean) / spkr_f0_SD,
        lh_mean_f0_z = ( lh_mean_f0 - spkr_f0_mean) / spkr_f0_SD,
        # redo excursion based on z-scores
        f0_exc_z = h_f0_z - l_f0_z,
        e_f0_exc_z = e_f0_z - h_f0_z,
        # Make L and H times relative to vowel onset (TBU).
        s_t = s_t - v_onset_t,
        l_t = l_t - v_onset_t,
        h_t = h_t - v_onset_t,
        e_t = e_t - v_onset_t,
        s_grand_mean_t = s_grand_mean_t - v_grand_mean_t,
        l_grand_mean_t = l_grand_mean_t - v_grand_mean_t,
        h_grand_mean_t = h_grand_mean_t - v_grand_mean_t,
        e_grand_mean_t = e_grand_mean_t - v_grand_mean_t,

        # Ensure factor variables are interpreted as factors
        foot_syls = factor(foot_syls, levels = unique(foot_syls)),
        gender = factor(gender, levels = unique(gender)),

        # create mode and prompt columns
        mode = factor(str_sub(stim, 1, 3), levels = c("MDC", "MWH", "MYN", "MDQ")),
        prompt = str_sub(stim, 4, 4),
        prompt = str_replace(prompt, "1", "vases"),
        prompt = str_replace(prompt, "2", "valley"),
        prompt = str_replace(prompt, "3", "valuables"),
        prompt = factor(prompt, levels=c("vases", "valley", "valuables")),
        # Ignore downstep.
        #acc_phon = str_replace(acc_phon, "!", ""),

        # Arrange speaker factors in more intuitive order.
        speaker = factor(
          speaker,
          levels = c(
            "F5",
            "F6",
            "F12",
            "F15",
            "F16",
            "F17",
            "M4",
            "M5",
            "M8",
            "M9",
            "M10"
          )
        )
      ) %>%
      # Remove columns which have outlived their use!
      select(
        -c(
          v_onset_t,
          foot_end_t,
          foot_start_t,
          tot_syls,
          phr_end_t,
          spkr_f0_mean,
          spkr_f0_SD
        )
      ) %>%
      filter(tot_feet == cur_foot) %>%
      unite(
        nuc_contour,
        acc_phon,
        fin_phon,
        sep = " ",
        remove = F
      ) %>%
      mutate(
        # correct nuc_contour
        nuc_contour = if_else(
          str_detect(nuc_contour, "\\]") & !str_detect(nuc_contour, "\\["),
          paste("^[", nuc_contour, sep = ""),
          nuc_contour),
        nuc_contour = str_replace(nuc_contour,
                                  "\\^\\[L\\*H\\s\\%]",
                                  "^[L*H] %"),
        nuc_contour = factor(
          nuc_contour,
          levels = c(
            "H* L%",
            ">H* L%",
            "^[L*]H L%",
            "L*H %",
            "L*H L%",
            "L*^[H] %",
            "L*^[H L%]",
            "L*^[H] L%",
            "^[L*H] %",
            "^[L*H L%]",
            "^[L*H] L%"
          )
        ),
        # Correct acc_phon
        acc_phon = str_replace_all(acc_phon, "\\s%|\\sL%", ""),
        acc_phon = str_replace_all(acc_phon,"^(L\\*H\\]|\\^\\[L\\*H)$",
                               "^[L*H]"),
        acc_phon = str_replace_all(acc_phon, "^L\\*\\^\\[H$", "L*^[H]"),
        # Correct fin_phon
        fin_phon = str_replace(fin_phon, "\\%\\]|L\\%\\]", "^[L%]"),
       # fin_phon = str_replace(fin_phon, "^L\\%\\]", "\\^\\[L\\%\\]"),
        across(c("phr_phon", "acc_phon", "fin_phon"),
               ~ factor(., levels = unique(.)))
        )
  )

}


###
###  Get Formula as String from LME/(B)GLM model ##################################
getModelFormula <-function(my_model) {
  installMissingPackages("stringr")
  my_formula <- str_c(formula(my_model))
  my_formula <- paste(my_formula[2], my_formula[1], my_formula[3])
  return(my_formula)
}
###
###  Summarise LME  ############################################################
summariseLME <-
  function(my_model,
           run_step = F,
           my_tolerance = 1e-05,
           write=NULL,
           extra_text="",
           post_hoc_method = "BH",
           print_summary = T,
           plot_resids = T)
    # short function to remove need for repetition of optimized used throughout.
  {
    installMissingPackages(c("lme4",
                             "lmerTest",
                             "optimx",
                             "stringr",
                             "broomExtra",
                             "ggplot2"))



    ### inner function
    ##################
    drawResiduals <- function(myModel)
    {
      myResiduals <- residuals(myModel)
      par(mfrow = c(1, 3))
      hist(myResiduals,
           xlab = "Residuals",
           main = "(a) Histogram of residuals")
      qqnorm(myResiduals,
             main = "(b) Q-Q Plot of residuals")
      qqline(myResiduals,
             xlab = "Fitted values",
             ylab = "Residuals")
      plot(fitted(myModel),
           myResiduals,
           main = "(c) Residual plot")
    }
    ##################

    ### Outer function
    ##################
    post_hoc_method <- paste("p.adj (",
                             shortPAdjMeth(post_hoc_method),
                             ")",
                             sep="")
    post_hoc_method <- enquo(post_hoc_method)

    my_formula <- getModelFormula(my_model)
    # output results


    if(plot_resids) {
      drawResiduals(my_model)
      }

    if(print_summary) {
      cat("\nFormula:\n", my_formula, "\n\n", sep = "")
      print(summary(my_model))
    }

    anova <- anova(my_model) %>%
      tidy() %>%
      mutate(across(`sumsq`:`statistic`, ~ round(., 3))) %>%
      formattable(
        caption=paste(
          "Anova of model", my_formula, sep=": ")
      ) %>%
      sigCodesTidy(p.value, F) %>%
      rename(`F value` = statistic)
    if(!is_null(write)){
      anova %>% mutate(!!post_hoc_method := NA, signif. = NA) %>%
      relocate(signif., .after=!!post_hoc_method) %>%
      write_csv(write)
      formula_save <- str_replace(write, "_anova.csv", "_formula.txt") %>%
        str_replace(".csv", ".txt")
      write(
        paste(str_replace_all(my_formula, "\\`", ""), extra_text),
        formula_save)

    }

    if (run_step)
    {
      cat("\nRunning step() ...\n", sep = "")
      step_result <- step(my_model)
      cat("Model found:",
          getModelFormula(get_model(step_result)), "\n\n")
      }


    cat("\nisSingular(my_model, tol =",
        my_tolerance,
        ") -->",
        isSingular(my_model, tol=my_tolerance),
        "\n\n", sep = ""
    )

  return(anova)
    ##################
  }


###
###  Analyse Model and extract key info ########################################
analyseModel <-
  function(my_model,
           write = NULL,
           is_GLM = F,
           exponentiate = T,
           type = "est",
           factor_matrix = F,
           ci.lvl = 0.95,
           y_lab = NULL,
           y_lim = NULL,
           plot_rounding = 1,
           panel_prefix = NULL,
           breaks = waiver(),
           hjust = "inward",
           per_row = 2
           )
  {
    require("formattable")
    require("tidyverse")
    require("mefa4")
    require("kableExtra")
    require("performance")
    require("broom")
    require("broom.mixed")
    require("sjPlot")
    require("lme4")
    require("lmerTest")
    require("ggeffects")
    require("sjlabelled")

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

    if (is_GLM) {
      my_headers <- my_headers[my_headers != "df"]
    }

    my_headers = enquos(my_headers)
    my_formula <- getModelFormula(my_model)

    if (is_GLM) {
      tidy_model <-
        tidy(my_model, exponentiate = exponentiate, conf.int = T)
    }
    else{
      tidy_model <-
        tidy(my_model, conf.int = T) %>% mutate(df = round(df, 2))
    }

    tidy_model <- tidy_model %>%
      filter(effect %notin% "ran_pars") %>%
      select(-c(group, effect)) %>%
      mutate(
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        statistic = round(statistic, 3)
      ) %>%
      rename(!!my_stat := statistic) %>%
      # re-order columns
      select(!!!my_headers) %>%
      formattable(caption = paste("summary of model:",
                                  str_replace_all(my_formula, "\\`", "")),
                  title = "") %>%
      mutate(
        # avoid escape character errors
        term = str_replace_all(term, "([\\*\\[\\^\\>])", "\\\\\\1"),
        # make p.value readable
        p.value = if_else(
          p.value < 0.0001,
          as.character(formatC(
            p.value, format = "e", digits = 1
          )),
          as.character(round(p.value, 4), digits = 2)
        )
      )

    if (is_GLM) {
      tidy_model <- tidy_model %>%
        mutate(# report log odds
          # across(c(estimate, conf.low, conf.high), ~ exp(.)),
          across(
            c(estimate, conf.low, conf.high),
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
    }

    r2 <- knitr::kable(
      r2(my_model),
      caption = "Conditional and marginal R^2^ of model",
      digits = 2,
      align = "l"
    )  %>% kable_styling(full_width = F, position = "left") %>%
      remove_column(3)
    print(r2)

    if (!is.null(write)) {
      do.call(rbind, r2(my_model))[, 1] %>%
        write.csv(paste(write, "_r2.csv", sep = ""))
    }

    if (is_GLM)
    {
      dependent_var <- deparse(formula(my_model)[[2]])
      fixed_factors <-
        formula(my_model, fixed.only = T)[3] %>%
        deparse() %>%
        str_replace_all("[\\(|\\)|+ ]", " ") %>%
        str_squish() %>%
        str_split(" ") %>%  unlist()
      fixed_factors = fixed_factors[fixed_factors != "*"]

      if (factor_matrix) {
        my_plot <- ggpredict(my_model,
                             terms = fixed_factors,
                             ci.lvl = ci.lvl) %>%
          plot() +
          ylim(0, 1) +
          geom_text(aes(
            label = round(predicted, 2),
            hjust = hjust,
            position = "dodge"),
            check_overlap = T,
            size=3) +
          theme(axis.title.x=element_blank())
        if (!is.null(write))
        {
          png(
            filename =
              paste0(write, "_re_", cur_factor, "_pred.png"),
            width =  15.25 / per_row,
            height = 6.5,
            units = "cm",
            res = 300
          )
          print(my_plot)
          dev.off()
        }

        print(my_plot)
      }
      else{
        letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
        cur_letter = 0
        for (cur_factor in fixed_factors) {
          if (typeof(my_model@frame[[cur_factor]]) == "double"){
            all = "[all]"
          }
          else{
            all = NULL
          }


          if (is.null(y_lab)){y_lab = cur_factor}
          cur_letter = cur_letter + (1 * cur_letter < length(letters))

            if(panel_prefix == "letters") {
              lettering = paste0((letters[cur_letter]), ".")
            }
          else{
            lettering = ""
          }


          my_plot <- ggpredict(my_model,
                               terms = paste(cur_factor, all),
                               ci.lvl = ci.lvl) %>%
            plot() +
            xlab(cur_factor) +
            ylim(0,1) +
            ylab("predicted probability") +
            labs(caption = paste (lettering, "Predicted probability of",
                                  dependent_var,
                                  "re",
                                  cur_factor)) +
            scale_y_continuous(breaks=breaks, limits = c(0, 1)) +
            theme(plot.title=element_blank(),
                  axis.title.x=element_blank(),
                  plot.caption=element_text(hjust = 0, size = 10),
                  plot.caption.position= "plot")
          if(is.null(all)) {
            my_plot <- my_plot +
              geom_label(aes(label = round(predicted, plot_rounding)),
                         label.padding = unit(0.5, "mm"),
                         label.r = unit(0.0, "mm"),
                         check_overlap = T,
                         size=3)
          }
          if (!is.null(write))
          {
            png(
              filename =
                paste0(write, "_re_", cur_factor, "_pred.png"),
              width =  15.25 / per_row,
              height = 6.5,
              units = "cm",
              res = 300
            )
            print(my_plot)
            dev.off()
          }

          my_plot %>% print()

        }
        }
    }
    else
    {
      dependent_var <- deparse(formula(my_model)[[2]])
      fixed_factors <-
        formula(my_model, fixed.only = T)[3] %>%
        deparse() %>%
        str_replace_all("[\\(|\\)|+ ]", " ") %>%
        str_squish() %>%
        str_split(" ") %>%  unlist()
      fixed_factors = fixed_factors[fixed_factors != "*"]
      letters = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")

      cur_letter = 0
      for (cur_factor in fixed_factors) {
        if (typeof(my_model@frame[[cur_factor]]) == "double"){
          all = "[all]"
        }
        else{
          all = NULL
        }

        if (is.null(y_lab)){y_lab = cur_factor}
        cur_letter = cur_letter + (1 * cur_letter < length(letters))

        if(is.null(panel_prefix)){
          lettering = ""
        }
        else
          if(panel_prefix == "letters") {
            lettering = paste0((letters[cur_letter]), ". ")
          }

        else {
          lettering = panel_prefix
        }


         my_plot <- ggpredict(my_model,
                             terms = paste(cur_factor, all),
                             ci.lvl = ci.lvl) %>%
          plot() +
          ylab(y_lab) +
          labs(caption = paste0 (lettering, "Predicted ",
                                 dependent_var,
                                 " re ",
                                 cur_factor, ".")) +
          geom_label(aes(label = round(predicted, plot_rounding)),
                     label.padding = unit(0.5, "mm"),
                     label.r = unit(0.0, "mm"),
                     check_overlap = T,
                     size=3) +
          theme(plot.title=element_blank(),
                legend.position = NULL,
                plot.caption=element_text(hjust = 0, size = 10),
                plot.caption.position= "plot")

         if (!is.null(y_lim)){
           my_plot <- my_plot +
             scale_y_continuous(breaks=breaks, limits = y_lim)
         }
         if (!is.null(write))
         {
           png(
             filename =
               paste0(write, "_re_", cur_factor, "_pred.png"),
             width =  15.25 / per_row,
             height = 6.5,
             units = "cm",
             res = 300
           )
           print(my_plot)
           dev.off()
         }
           print(my_plot)
      }
    }

    return(list(
      "r2" = r2,
      "table" = tidy_model,
      "plot" = my_plot
    ))
  }



###
###  Get intercepts and slopes of Fixed Effects of LME/GLMM Model ##############
getModelFixedFX <- function(model,
                            write = NULL,
                            exponentiate = T,
                            extra_text = "",
                            report = c("slopes", "intercepts"),
                            ignore_list = "",
                            post_hoc_method = "BH")
{
  require("formattable")
  require("tidyverse")
  require("mefa4")
  require("lme4")
  require("blme")

  # Get information from model
  formula <- formula(model)
  data = model@frame
  factor_info <- tibble(factors = colnames(data),
                        categorical = sapply(data, is.factor)
                        )


  fixed_factors <-
    (str_replace_all(deparse(formula(model, fixed.only = T)[3]),
                     "[\\(|\\)|+ ]",
                     " ") %>%
       str_squish() %>%
       str_split(" ")) %>%
    unlist ()

  is_GLM <- isGLMM(model)
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

  if (is_GLM) {
    my_headers <- my_headers[my_headers != "df"]
  }

  my_stat = enquo(my_stat)
  post_hoc_method <- paste0("p.adj (", shortPAdjMeth(post_hoc_method),  ")")
  post_hoc_method <- enquo(post_hoc_method)
  my_headers = enquos(my_headers)


  # include continuous fixed factors as two_level_factors.
  cont_fixed_factors <-
    fixed_factors[fixed_factors %in% (factor_info %>%
                                        filter(!categorical))$factors]
  two_level_factors <- cont_fixed_factors
  two_level_terms <- cont_fixed_factors



  # Get list of multi-level factors and exclude continuous factors.
  multilevel_factors <-
    fixed_factors[fixed_factors %in% (factor_info %>%
                                        filter(categorical))$factors]
  # Exclude factors on ignore list.
  multilevel_factors <-
    multilevel_factors[multilevel_factors %notin% ignore_list]

  # Add two-level fixed factors to two_level_factors & two_level_terms.
  for (cur_factor in multilevel_factors) {

    if (levels(data[[cur_factor]]) %>% length() == 2) {
      two_level_factors <- c(two_level_factors, cur_factor)
      two_level_terms <- c(two_level_terms,
                           paste0(cur_factor,
                                  levels(data[[cur_factor]])[2]))
    }
  }

  # remove 2-level factors from list of multilevel_factors
  multilevel_factors <-
    multilevel_factors[multilevel_factors %notin% two_level_factors]

  # set first keep_terms list to include 2-level terms and continuous factors.
  initial_keep_terms <- c(two_level_terms)
  all_models_tidy <- tibble()
  # loop through each multilevel fixed factor of interest (multilevel_factors)
  for (cur_factor in multilevel_factors)
  {
    # Get levels for current factor
    cur_levels = levels(data[[cur_factor]])
    num_levels = length(cur_levels)

    keep_terms = NULL
    # Make list of terms to keep
    for (level_name in cur_levels) {
      keep_terms = c(keep_terms, paste(cur_factor, level_name, sep = ""))
    }
    keep_terms <- c(keep_terms, initial_keep_terms)
    # loop through dataframe, reordering levels of current factor each time.
    for (cur_level in 1:(num_levels)) {
      model <- update(model, data = data)

      # Get tidy model.
      if (is_GLM) {
        model_tidy <- tidy(model,
                           exponentiate = exponentiate,
                           conf.int = T)
      }
      else {
        model_tidy <- tidy(model, conf.int = T)
      }

      model_tidy <- model_tidy %>%
        # Retain fixed factors only
        filter(effect == "fixed") %>%
        # remove unnecessary columns
        select(-c(effect, group)) %>%
        # Tidy up numbers.
        mutate(across(
          any_of(c("df", "estimate", "std.error", "statistic")),
          ~ round(., 2))) %>%
        rename(!!my_stat := statistic)

      model_tidy <- model_tidy %>%
        # re-order columns
        select(!!!my_headers) %>%
        filter((term %in% c(keep_terms, "(Intercept)"))) %>%
        # Prepare current model for pasting to all models output.
        # Make 'pairwise' column = intercept.
        mutate(
          pairwise =
            if_else(term == "(Intercept)",
                    "intercept",
                    if_else(term %notin% c(keep_terms),
                            "N/A",
                            keep_terms[cur_level])),
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
      model_tidy <-
        filter(model_tidy, term %in% keep_comparisons)

      # add remaining pairwise comparisons to main tibble.
      all_models_tidy <- bind_rows(all_models_tidy, model_tidy)

      # restructure the order of levels for next LME model.
      cur_levels <- c(cur_levels[2:num_levels], cur_levels[1])

      factor_var <- sym(cur_factor)
      factor_var_name <- quo_name(cur_factor)


      data <- data %>%
        mutate(!!factor_var := factor(!!factor_var,
                                      levels = cur_levels))

    }

    # Reset initial_keep_terms to it doesn't contain 2-level or continuous factors.
    initial_keep_terms <- NULL
  }

  # Get intercepts and pairwise comparisons tables
  all_models_tidy <- all_models_tidy %>% relocate(pairwise)

  my_intercepts <- filter(all_models_tidy, pairwise == "intercept") %>%
    select(-pairwise) %>%
    rename(intercept = term)

  my_pairwise <-
    filter(all_models_tidy, pairwise %notin% c("intercept", "N/A")) %>%
    rename(intercept = pairwise, slope = term)

  if (is_GLM) {
    two_level_factor_slopes <- tidy(model,
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
    two_level_factor_slopes <- tidy(model, conf.int = T) %>%
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
    filter(slope %in% c(two_level_terms, cont_fixed_factors)) %>%
    arrange(slope)
  my_pairwise <- my_pairwise %>%
    filter(slope %notin% c(two_level_terms, cont_fixed_factors)) %>%
    rbind(my_pairwise.temp)


  # bind two-level factor stats to my_pairwise
  my_pairwise <- rbind(my_pairwise, two_level_factor_slopes)

  # Write tables to file
  my_formula <- getModelFormula(model)
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
    "model" = model
  ))
}



###
###  Post-hoc bulk adjust p Value   ####################################################
adjustP_posthoc <-
  function(my_folder,            # source folder with .csv files to be updated.
           p_column = "p.value", # name of p.column
           method = "BH",        # p. adjustment method
           significance = T,     # flag for including significance column,
           marginal = F,         # include marginal significance flag.
           write = T,         # write results to file flag.
           report = F,       # flag to report total number of tests and
                                 # p.values < 0.05 before and after adjustment.
           print = F,        # Print output or not
           suffix_id=""          # suffix ID for files for analysis,

  )
  {
    # Load required packages
    require("dplyr")
    require("formattable")
    require("readr")
    require("mefa4")
    require("knitr")
    require("kableExtra")
    require("performance")
    require("tidyverse")
    require("weights")

    # Abbreviate method where necessary.
    my_meth <- shortPAdjMeth(method)


    # Enquote variables which whose values will be evaluated as variables.
    p_column = enquo(p_column)
    new_adj_col = paste("p.adj (", my_meth, ")", sep="")
    new_adj_col = enquo(new_adj_col)

      # Get tibble of files to be adjusted
      file_tibble <-
        list.files(my_folder,
                   paste("*", suffix_id, ".csv", sep=""),
                   full.names = T) %>%
        read_csv(id = "file_name",
                 col_names = T,
                 show_col_types = F) %>%

        # avoid reduplication of columns
        select(-any_of(c("p.adj.",
                         "p.adj (holm)",
                         "p.adj (hoch)",
                         "p.adj (homm)",
                         "p.adj (bonf)",
                         "p.adj (BH)",
                         "p.adj (BY)",
                         "p.adj (FDR)",
                         "p.adj (none)",
                         "signif."))) %>%
        # Add p.adjusted column using method.
        mutate(p.adj = p.adjust(!!p_column,
                                method = method),
               .after = !!p_column)

    # Get summary info about p values.
    p_values <- file_tibble %>% nrow()
    sig_p_values <- file_tibble %>% filter(!!p_column < 0.05) %>% nrow()
    sig_p_values_adj  <- file_tibble %>% filter(p.adj < 0.05) %>% nrow()
    p_counts <- tibble(p_values, sig_p_values, sig_p_values_adj)



    if (significance) {
      file_tibble <- file_tibble %>%
        mutate(# Add significance column.
          signif. = if_else(
            p.adj < 0.001,
            "p<.001",
            if_else(p.adj < 0.01, "p<.01", if_else(
              p.adj < 0.05,
              "p<.05",
              if_else(p.adj < 0.1 & marginal, "(p<.1)", "")
            ))
          ))
    }

    file_tibble <- file_tibble %>%
      mutate(
        # Change p.adj and p_column to more readable format.
        p.adj = if_else(p.adj < 0.001,
                        as.character(formatC(p.adj, format="e", digits = 1)),
                        as.character(round(p.adj, 3), digits = 2)),
        !!p_column := if_else(
          !!p_column < 0.001,
          as.character(formatC(!!p_column, format="e", digits = 1)),
          as.character(round(!!p_column, 3), digits = 2))
      )

    # remove p,adj from file with "none" method, i.e., no p.adjustment.
    if (method == "none") {
      file_tibble <- select(file_tibble,-p.adj)
    }


      i = 0
      for (cur_file in unique(file_tibble$file_name))
      {
        cur_set <- file_tibble %>%
          filter(file_name == cur_file) %>%
          select(-file_name)

        # Re-save updated tables as original file name (depending of whether or
        # not method is "none".
        if (write) {
          if (method == "none") {
            write_csv(cur_set , cur_file)
          }
          else{
            write_csv(cur_set %>% rename(!!new_adj_col := p.adj), cur_file)
          }

        }


        # print table.
        if (print) {
          i = i + 1
          formula_file <- cur_file %>%
            str_replace("_b0.csv|_b1.csv|_anova.csv", "_formula.txt")
          if (file.exists(formula_file)) {
            my_caption <-
              paste(i, "\\. ", read_lines(formula_file), sep = "")
          }
          else{
            my_caption  <-
              paste(i, ". ", str_replace(cur_file, ".csv", ""), sep = "")
          }

          print_this <- cur_set %>%
            tidyStatNumbers() %>%
            mutate(across(everything(),
                          ~ str_replace_all(., "([\\*\\[\\^\\>])", "\\\\\\1")))

          if (method != "none"){
          print_this <- print_this %>% rename(!!new_adj_col := p.adj)
          }

          print_this <- print_this %>%
            knitr::kable(caption = my_caption) %>%
            kable_styling(full_width = F, position = "left")

          print(print_this)
        }
      }


    if (report) {
      return(p_counts)
    }
  }
###
###  Tidy Predictions ####################################################
printTidyPredictions <-
  function(model, caption_suffix, factor_matrix = F) {
    require("ggeffects")
    require("tidyverse")
    require("knitr")
    require("kableExtra")


      pred_list <- ggpredict(model)
      obj_names <- names(pred_list)
      obj_i = 0
      if (factor_matrix) {
        ggpredict(model, terms = obj_names) %>%
          as_tibble() %>%
          relocate(std.error, .after = conf.high) %>%
          relocate(group , .before = x) %>%
          tidyNumbers() %>%
          rename(estimate = x) %>%
          arrange(group) %>%
          mutate(across(c(group, estimate),
                        ~ str_replace_all(.,
                                          "(\\_|\\[|\\]|\\$|\\^|\\>)",
                                          "\\\\\\1"))) %>%
          knitr::kable(caption = paste("predicted probability of",
                                       response_labels(model))) %>%
          kable_styling(full_width = F, position = "left") %>%
          print()
      }
      else
      {
        for (cur_obj in pred_list)
        {
          obj_i = obj_i + 1
          cur_obj_name <- obj_names[obj_i]
          cur_obj_name = enquo(cur_obj_name)
          cur_caption <- cur_obj %>% get_title

          as_tibble(cur_obj) %>%
            select(-group) %>%
            relocate(std.error, .after = conf.high) %>%
            mutate(
              x = str_replace_all(x,
                                  "(\\_|\\[|\\]|\\$|\\^|\\>)",
                                  "\\\\\\1")
            ) %>%
            tidyNumbers() %>%
            rename(!!cur_obj_name := x) %>%
            knitr::kable(caption = cur_caption) %>%
            kable_styling(full_width = F, position = "left") %>%
            print()

        }
      }
  }
###
###  Shorten P Adjustment method name #########################################
shortPAdjMeth <- function(method){
  require("mefa4")
  if(method %in% c("hochberg", "hommel", "bonferroni")) {
    short_meth <- switch(method,
                         "hochberg" = "hoch",
                         "hommel" = "homm",
                         "bonferroni" = "bonf")
  }
  else{short_meth <- method}
  return(short_meth)}

###
###  Output Chi Squared Results ################################################
outputChiSqResults <- function(anova,
                               model,
                               extra_text = "",
                               write = "test",
                               post_hoc_method = "BH")

{
  # rename and enquote relevant arguments.
  my_formula <- getModelFormula(model)

  post_hoc_method <- paste("p.adj (",
                           shortPAdjMeth(post_hoc_method),
                           ")",
                           sep = "")
  post_hoc_method <- enquo(post_hoc_method)

  # convert anova to formattable object
  anova <- anova %>%
    formattable(caption = paste("ANOVA:", my_formula, extra_text)) %>%
    # tidy up decimal places
    mutate(
      across(2:last_col(),
             ~ if_else(abs(.) < 0.001 | abs(.) > 100000,
                       as.character(formatC(., format = "e", digits = 1)),
                       if_else(round(.) == .,
                               as.character(.),
                               as.character(round(., 3), digits = 3)))))

    # Save anova
    anova %>%
    # add blank p.adj and significance columns.
    mutate(!!post_hoc_method := NA, signif. = NA) %>%
    relocate(signif., .after = !!post_hoc_method) %>%
    write_csv(paste(write, "_anova.csv", sep = ""))

    # Save formula
    write(paste(str_replace_all(my_formula, "\\`", ""), extra_text, sep = ""),
          paste(write, "_formula.txt", sep = ""))

    # Return tidy anova.
    return(anova)
}


###
###  Tidy numbers in table #####################################################
tidyNumbers <- function(data,
                        p.value = "p.value",
                        p.decimals = 3,
                        abs.max = 10^4,
                        abs.min = 0.01,
                        digits = 2){

  data %>%
    mutate(
      across(
        any_of(p.value) & where(is.numeric),
        ~ if_else(
          . < 10 ^(-p.decimals),
          paste("<", format(10^-p.decimals, scientific = F), sep="") %>%
            str_replace("0\\.", "."),
          as.character(round(., p.decimals))
        )
      ),
      across(
        where(is.numeric),
        ~ if_else(abs(.) < abs.min | abs(.) >= abs.max,
                  as.character(formatC(., format = "e", (digits - 1))),
                  as.character(round(., digits))
        )
      )
    ) %>%
    return()

}



###
### Tidy Stat Table Numbers ####################################################
tidyStatNumbers <- function(stats) {
  installMissingPackages(c("tidyverse", "weights"))
  original_nrow <- nrow(stats)
  lost_factors <- NULL
  original_factors <- as.vector(select(stats, 1))$term
  stats <- filter(stats, !is.na(stats[[2]]))
  if(nrow(stats) != original_nrow){
    lost_factors <- original_factors[original_factors %notin% as.vector(select(stats, 1))$term]
    cat(paste(lost_factors, sep = ", "), " couldn't be calculated.\n")
        }


    stats <- mutate(stats,
           across(contains("signif."),
                  ~ if_else(is.na(.), "", as.character(.))),
           across(where(is_double) & !(contains("p.") | contains("Pr")),
                  ~ if_else(abs(.) < 0.01 | abs(.) > 100000,
                            as.character(formatC(., format = "e", digits = 1)),
                            if_else(
                              abs(.) > 1000,
                              as.character(round(., 0)),
                              as.character(round(., 2), digits = 2)
                              )
                            )
                  ),
           across(contains("p.") | contains("Pr"),
                  ~ if_else(as.numeric(.) < 0.001,
                            "<.001",
                            as.character(rd(as.numeric(.), digits = 3))
                            )
                  ),
           across(where(is_character),
                  ~ if_else(. == "0.0e+00" | . == "-0.0e+00", "0", .)),
           across(where(is_character), ~ str_replace(., "TRUE", "T")),
           across(where(is_character), ~ str_replace(., "FALSE", "F"))
           )


    for(i in lost_factors){
      stats <- add_row(stats, term = i)
    }

    # stats <- mutate(stats,
    #                 across(.cols = everything(), ~ if_else(is.na(.), "", .)))
    return(stats)

}

###
### Optimize Models based on lme4 package ######################################
optimizeModel <- function(model,
                       checks = c("allFit", "optimx", "nloptwrap"),
                       verbose = T,
                       reject_nl = T) {

  ###
  # Returns lme4-based model which converges, if possible.
  # If no better model found, original model is returned with a text Warning.
  #
  # Notes:
  #   1. For large or complex models this can be exceedingly slow!
  #   2. Nelder-mead is not advised for high-dimensional models,
  #      reject_nl is set to T as default. If you want to include Nelder-Mead
  #      optimization, set: reject_nl = F.
  #
  # The functions are largely adapted from:
  #     Nugent, Joshua.
  #     Using allFit() with (g)lmer.
  #     https://joshua-nugent.github.io/allFit/.
  #     15 June, 2022
  #     last accessed: 30 August, 2022.
  ###

  library(tidyverse)
  library(parallel)
  library(lme4)
  library(optimx)
  library(dfoptim)
  library(blme) # my preferred bayesian lme package.

  solution <- "original model"
  original_model <- model

  if(verbose){
  cat("Searching for good optimization settings with optimizeModel().\n", sep = "")
  cat("  Formula:   ", getModelFormula(model), "\n", sep = "")
  cat("  Optimizer: ", model@optinfo$optimizer, "\n", sep = "")
  }

  ### inner functions
  ##################
  tryAllFit <- function(model, verbose = F) {
    # Tries to return a model which converges in allFit()
    original_model <- model
    ncores <- detectCores()

    # Run allFit on multiple cores.
    if (verbose){
    cat("\nchecking allFit()\n", sep = "")
      }
    diff_optims <-
      allFit(model,
             maxfun = 1e5,
             verbose = verbose,
             parallel = "multicore",
             ncpus = ncores)

    # Get list of allFit() model messages
    diff_optims_OK <- diff_optims[sapply(diff_optims, is, "merMod")]
    lapply(diff_optims_OK, function(x)
      x@optinfo$conv$lme4$messages)

    # Get logical list of "well" optimized models (i.e. no messages).
    convergence_results <-lapply(diff_optims_OK,
                                 function(x)
                                   x@optinfo$conv$lme4$messages)

    working_indices <- sapply(convergence_results, is.null)

    if (sum(working_indices) == 0) {
      model <- original_model
    } else {
      model <- diff_optims[working_indices][[1]]
    }

    return(model)
  }

  tryOptimx <- function(model, reject_nl = T, verbose = F) {
    # Tries to return a model which converges by varying optimx() methods.
    original_model <- model
    optimx_options <-
      c("L-BFGS-B", "nlminb", "nlm", "bobyqa", "hjkb")
    if (!reject_nl){optimx_options <- c(optimx_options,  "nmkb")}

    num_options <- length(optimx_options)
    info <- "No option"
    if(verbose){
    cat("\nChecking optimx optimization settings", sep = "")
      }

    i <- 0
    while (i < num_options & !modelIsOK(model, reject_nl)) {
      i  <-  i + 1
      if(verbose){
      cat(",", optimx_options[i])

        # Either use glmerControl...
        if(model@resp$family[1] == "binomial" |
           model@resp$family[1] == "multinomial") {
          try(model <- update(model,
                              control = glmerControl(
                                optimizer = "optimx",
                                optCtrl = list(method = optimx_options[i],
                                               maxit = 1e9))))
        }
        # ... or use lmerControl
        else {
          try(model <- update(model,
                              control = lmerControl(
                                optimizer = "optimx",
                                optCtrl = list(method = optimx_options[i],
                                               maxit = 1e9))))
        }
      }
    }

    if(verbose){cat("\n")}

    if (modelIsOK(model, reject_nl)) {
      info <- optimx_options[i]
    }
    else {
      model <- original_model
    }

    return(model)
  }

  trynlopt <- function(model, reject_nl = T, verbose = F) {
    # Tries to return a model which converges by through lnloptwrap algorithms.
    original_model <- model
    opts <- c("NLOPT_LN_PRAXIS",
              "NLOPT_GN_CRS2_LM",
              "NLOPT_LN_COBYLA",
              "NLOPT_LN_NEWUOA",
              "NLOPT_LN_NEWUOA_BOUND",
              "NLOPT_LN_SBPLX",
              "NLOPT_LN_BOBYQA")
    if (!reject_nl) {
      opts = c(opts, "NLOPT_LN_NELDERMEAD")
    }

    num_options <- length(opts)
    info <- "No option"
    if(verbose){cat("\nChecking nloptwrap options", sep = "")}

    i <- 0
    while (i < num_options & !modelIsOK(model, reject_nl)) {
      i  <- i + 1
      if(verbose){cat(",", opts[i])}
      cur_option <- opts[i]

      # Either use glmerControl...
      if(model@resp$family[1] == "binomial" |
         model@resp$family[1] == "multinomial") {
        try(model <- update(model, control = glmerControl(
          optimizer = "nloptwrap", optCtrl = list(algorithm = opts[i],
                                                  maxfun = 1e9,
                                                  maxeval = 1e7,
                                                  xtol_abs = 1e-9,
                                                  ftol_abs = 1e-9)
        )))
      }
      # ... or use lmerControl
      else {
        try(model <- update(model, control = lmerControl(
          optimizer = "nloptwrap", optCtrl = list(algorithm = opts[i],
                                                  maxfun = 1e9,
                                                  maxeval = 1e7,
                                                  xtol_abs = 1e-9,
                                                  ftol_abs = 1e-9)
          )))
      }


    }
    if(verbose){cat("\n")}

    if (modelIsOK(model, reject_nl)) {
      info <- opts[i]
      cat("\n\n")
      cat("***** WARNING ****\n")
      cat("NB: manually update lmerControl:\n\n")
            cat("control = lmerControl(\n")
            cat("  optimizer = \"nloptwrap\",\n")
      cat(paste("  optCtrl = list(algorithm = \"",  opts[i], "\",\n", sep = ""))
            cat("                 maxfun = 1e9,\n")
            cat("                 maxeval = 1e7,\n")
            cat("                 xtol_abs = 1e-9,\n")
            cat("                 ftol_abs = 1e-9))\n")
    }
    else{
      model <- original_model
    }

    return(model)
  }

  modelIsOK <- function(model, reject_nl = T) {
    # Returns T is a model converges and is not singular.

    ans <- as.logical(
      # Check for convergence
      is.null(model@optinfo$conv$lme4$messages) &
        # check for singularity
        !isSingular(model) &
        # check for other warnings
        is.null(model@optinfo$conv$lme4$warnings))

    if (model@optinfo$optimizer %in% c("nmkbw","Nelder_Mead") & reject_nl){
      ans <- F}

    return(ans)
  }

  getModelElements <- function(model) {
    # Get elements of a model used for functions associated with optimizeModel
    ans <- list(
      "formula" =  formula <- formula(model),
      "optimizer" =  model@optinfo$optimizer,
      "frame" =  model@frame,
      "maxfun" = model@optinfo$control$maxfun,
      "package" = model@resp$.objectPackage
    )
    return(ans)
  }

  ### Outer function
  if (!modelIsOK(model, reject_nl)) {
    if(verbose){
    cat("\nRunning basic model with less strict control settings.\n", sep = "")
    }

    # Either use glmerControl...
    if(model@resp$family[1] == "binomial" |
       model@resp$family[1] == "multinomial"){
      try(model <- update(lh_pn_model, control = glmerControl(
        optCtrl = list(maxfun = 10e9,
                       xtol_abs = 1e-9,
                       ftol_abs = 1e-9))))
    }

    # ... or lmerControl
    else {
    try(model <- update(model, control = lmerControl(
      optCtrl = list(maxit = 1e9,
                     maxfun = 1e9,
                     xtol_abs = 1e-9,
                     ftol_abs = 1e-9))))
    }

    if (modelIsOK(model, reject_nl)){
      solution  <-  "less strict control settings"
      }
  }

  if (!modelIsOK(model, reject_nl) &
      "allFit" %in% checks) {
    try(model <- tryAllFit(model, verbose))
    if (modelIsOK(model, reject_nl)) {
      solution <- "after trying allFit()"
    }
  }

  if (!modelIsOK(model, reject_nl) & "optimx" %in% checks) {
    model <- tryOptimx(model, reject_nl, verbose)
    if (modelIsOK(model, reject_nl)) {
      solution <- "after varying optimx settings"
    }
  }

  if (!modelIsOK(model, reject_nl) & "nloptwrap" %in% checks) {
    model <- trynlopt(model, reject_nl, verbose)
    if (modelIsOK(model, reject_nl)) {
      solution <- "after varying nloptwrap options"
    }
  }

  if (!modelIsOK(model, reject_nl)) {
    model <- original_model
    cat("\nNo alternatives converged. Reverting to original model.\n\n",
        sep = "")
  }
  else {
    found <- getModelElements(model)
    cat("\nModel found using ",
        found$optimizer,
        " after trying ",
        solution,
        ".\n\n",
        sep = "")
  }

  return(model)

}

modelIsOK <- function(model, reject_nl = T) {
  # Returns T is a model converges and is not singular.

  ans <- as.logical(
    # Check for convergence
    is.null(model@optinfo$conv$lme4$messages) &
      # check for singularity
      !isSingular(model) &
      # check for other warnings
      is.null(model@optinfo$conv$lme4$warnings))

  if (model@optinfo$optimizer %in% c("nmkbw","Nelder_Mead") & reject_nl){
    ans <- F}

  return(ans)

}


###
