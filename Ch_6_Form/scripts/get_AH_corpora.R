
# LOAD AH CORPUS
corpus <- as_tibble(read.csv("data/a_corpus_audited.csv")) %>%
  # Retain reference columns and phonological data.
  # include speech_rate
  mutate(speech_rate = round(phr_end_t / tot_syls) / 1000) %>% 
  # treat foot_syls and ana_syls as factor
  mutate(ana_syls = factor(ana_syls, levels = unique(ana_syls))) %>% 
  mutate(foot_syls = factor(foot_syls, levels = unique(foot_syls))) %>% 
  # Ignore downstep.
  mutate(acc_phon = str_replace(acc_phon, "!", "")) %>%
  # Arrange PA levels according to hypothesized hierarchy.
  mutate(
    acc_phon = factor(acc_phon,levels = c("(*)", "L*", "H*", ">H*", "L*H"))
  ) %>%
  # Arrange PA levels according to hypothesized hierarchy.
  mutate(speaker = factor(
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
  ))


# CREATE PN SUBSETS
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
  select(-(cur_foot:wrd_end_syl),-fin_phon)

# Extract PN foot-size data.
pn_foot <- pn %>%
  filter(stim %in% c("A0131", "A0221", "A0321", "A0423")) %>%
  select(-(ana_syls:cur_foot), -wrd_end_syl,-fin_phon) %>% 
  mutate(foot_syls = factor(foot_syls, levels = c(1, 2, 3, 4)))


# Extract PN word-boundary data.
pn_lex <- pn %>% filter(stim %in% c(
  "A0321", "H0322", "H0433", "A0423",  "H1321", "H1322"
))


# CREATE NUCLEAR PA SUBSETS
# Extract nuclear PA data.
nuc <- filter(corpus, cur_foot == 2) %>%
  select(-init_phon,-ana_syls) %>%
  # Create nuclear contour column.
  unite(nuc_contour,
        acc_phon,
        fin_phon,
        sep = " ",
        remove = FALSE)

# Extract nuclear PN foot-size data.
nuc_pre <- nuc %>%
  filter(stim %in% c("A1111", "A0221", "A0321", "A0423")) %>%
  # Get number of preceding syllables from stim code
  mutate(pre_syls = str_sub(stim, 3, 3))  %>%
  mutate(pre_syls = as.integer(pre_syls))

nuc_foot <- nuc %>%
  # Get dataset for syllables preceding nuclear PA.
  filter(stim %in% c("A1211", "A0221", "A1231", "A1241"))

# Remove unneeded variables from R Environment.
rm(corpus, nuc, pn)

