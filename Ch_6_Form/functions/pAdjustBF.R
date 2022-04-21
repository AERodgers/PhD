pAdjustBF <- function(myTibble, excludeTerms, bonferroniMultiplier){
    myTibble <- mutate(myTibble,
                       p.adjusted = if_else(
                           term %in% excludeTerms,
                           p.value,
                           if_else(p.value * bonferroniMultiplier > 1,
                                   0.9999,
                                   p.value * bonferroniMultiplier)

                       ),
                       p.adjusted = round(p.adjusted, 4)
                       )
    return(myTibble)
}


