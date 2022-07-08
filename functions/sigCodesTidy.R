# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
###  Significance Code Tidy  ###################################################
sigCodesTidy <-
  function(my_tibble,
           p_value = "p.adj",
           incl_marginal_sig = TRUE)
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
