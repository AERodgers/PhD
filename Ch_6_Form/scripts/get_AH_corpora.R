# LOAD AND PROCESS AH CORPUS
# --------------------------

corpus <- as_tibble(read.csv("data/a_corpus_audited.csv")) %>%
  # remove reference columns not needed for statistical analysis.
  select(
    -c(
      sent,
      v_text,
      l_syl_end_t,
      h_syl_end_t,
      v_sylNormT,
      l_sylNormT,
      h_sylNormT,
      v_syl,
      v_syl_ratio,
      l_syl_ratio,
      intercept_st,
      mean_st,
      med_st,
      phr_phon,
      location,
      rep,
      metre_ID,
      stim_metre
    )
  ) %>%
  mutate(
    # get foot_duration.
    foot_dur = foot_end_t - foot_strt_t,
    # include speech_rate
    speech_rate = round(tot_syls / phr_end_t * 1000, 3),
    # treat foot_syls and ana_syls as factor
    ana_syls = factor(ana_syls, levels = unique(ana_syls)),
    foot_syls = factor(foot_syls, levels = unique(foot_syls)),
    # Ignore downstep.
    acc_phon = str_replace(acc_phon, "!", ""),
    # Arrange PA levels according to hypothesized hierarchy.
    acc_phon = factor(acc_phon, levels = c("(*)", "L*", "H*", ">H*", "L*H")),
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
  )


# CREATE PN SUBSETS
# -----------------

# Extract PN data.
pn <- filter(corpus, cur_foot == 1) %>%
  unite(init_contour,
        init_phon,
        acc_phon,
        sep = " ",
        remove = FALSE)
# Get subset of all data for PN analysis

# Extract PN anacrusis data.
pn_ana <- pn %>%
  filter(stim %in% c("A0423", "A1422", "A2422", "A3422")) %>%
  # Remove columns which are no longer needed for pn_ana analysis.
  select(-(cur_foot:wrd_end_syl),-fin_phon)

# Make PN foot-size dataset.
pn_foot <- pn %>%
  filter(stim %in% c("A0131", "A0221", "A0321", "A0423")) %>%
  # remove columns no longer needed for pn_foot analysis.
  select(-(ana_syls:cur_foot), -wrd_end_syl,-fin_phon) %>% 
  mutate(foot_syls = factor(foot_syls, levels = c(1, 2, 3, 4)))


# Make PN word-boundary dataset.
pn_lex <- pn %>% filter(stim %in% c(
  "A0321", "H0322", "H0433", "A0423",  "H1321", "H1322"
))


# CREATE NUCLEAR PA SUBSETS
# -------------------------

nuc <- filter(corpus, cur_foot == 2) %>%
  # remove columns which are no needed for nuclear PA analysis.
  select(-init_phon,-ana_syls) %>%
  # Create nuclear contour column.
  unite(nuc_contour,
        acc_phon,
        fin_phon,
        sep = " ",
        remove = FALSE)

# Make nuclear PA preceding syllable dataset.
nuc_pre <- nuc %>%
  filter(stim %in% c("A1111", "A0221", "A0321", "A0423")) %>%
  # Get number of preceding syllables from stim code
  mutate(pre_syls = str_sub(stim, 3, 3))  %>%
  mutate(pre_syls = as.integer(pre_syls))

# Make nuclear PA foot size dataset.
nuc_foot <- nuc %>%
  # Get dataset for syllables preceding nuclear PA.
  filter(stim %in% c("A1211", "A0221", "A1231", "A1241"))

