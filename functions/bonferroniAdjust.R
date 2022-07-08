# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
###  Bonferonni Adjustment  ####################################################
bonferroniAdjust <- function(myTibble,
                             bonferroniMultiplier=0,
                             exclude_terms = NULL,
                             p.adj="p.adj")
  {
  if (bonferroniMultiplier)
    {
    p.adj = enquo(p.adj)

    myTibble <- mutate(myTibble,
                       !!p.adj := if_else(
                         !is_null(exclude_terms) & term %in% exclude_terms,
                         NA_real_,
                         if_else(
                           p.value * bonferroniMultiplier >= 1,
                           0.9999,
                           p.value * bonferroniMultiplier
                           )
                         )
                       )
    }
    return(myTibble)
  }
