# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
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
