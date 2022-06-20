


# Create data set.
outcome <-
    c(1, 1, 1, 1, 1, 2, 1, 2, 3, 2, 3, 3,
      1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3,
      1, 1, 1, 2, 2, 1, 2, 3, 2, 3, 3)
outcome <- c(outcome,
    c(1, 1, 1, 1, 1, 2, 1, 2, 3, 2, 3, 3,
      1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3,
      1, 1, 1, 2, 2, 1, 2, 3, 2, 3),
    c(1, 1, 1, 1, 1, 2, 1, 2, 3, 2, 3, 3,
      1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3,
      1, 1, 1, 2, 2, 1, 2, 3, 3, 3, 3)
    )

treatment <-
    c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4,
      1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4,
      1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4)

treatment <- c(
    treatment,
    c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4,
      1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4,
      1, 1, 1, 2, 2, 2, 3, 3, 3, 4),
    treatment
    )

patient <-
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
      4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
      5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9
      )

my_tibble_1 <- as_tibble(data.frame(outcome, treatment, patient))
my_tibble_1 <- my_tibble %>%
    mutate(outcome = factor(outcome),
           treatment = factor(treatment),
           patient = factor(patient))


my_model_1 <- glmer(as.formula(outcome ~ treatment + (treatment | patient)),
                  data = my_tibble_1,
                  family = binomial(link = "logit"),
                  # change optimizer to avoid convergence errors
                  control = glmerControl(
                      optimizer = "optimx",
                      calc.derivs = FALSE,
                      optCtrl = list(
                          method = "nlminb",
                          starttests = FALSE,
                          kkt = FALSE
                      )
                  )
)


my_model_1 %>% tidy() %>% filter(effect == "fixed") %>% select(-group) %>%
    formattable(
        caption = "Fixed effects when treatment 1 always leads to outcome 1") %>%
    print()

my_tibble_2 <- my_tibble_1
my_tibble_2[1,"outcome"] = "2"

my_model_2 <- glmer(as.formula(outcome ~ treatment + (treatment | patient)),
                  data = my_tibble_2,
                  family = binomial(link = "logit"),
                  # change optimizer to avoid convergence errors
                  control = glmerControl(
                      optimizer = "optimx",
                      calc.derivs = FALSE,
                      optCtrl = list(
                          method = "nlminb",
                          starttests = FALSE,
                          kkt = FALSE
                      )
                  )
)

my_model_2 %>%
    tidy() %>%
    filter(effect == "fixed") %>%
    select(-group) %>%
    rename(z_score = statistic) %>%
    formattable(caption = "Fixed effects when treatment 1 does not always lead to outcome 1") %>%
    print()
