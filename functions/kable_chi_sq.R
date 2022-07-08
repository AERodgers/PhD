# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
###  Kable Chi Squared  ########################################################
kable_chi_sq <- function(chi_sq_test)
  {
    # returns a kable() object of the chi_sq_test input.
    require("knitr", "janitor")

    x2d <- round(chi_sq_test$statistic[1],10)
    names(x2d) <- NULL

    df <- chi_sq_test$parameter[1]
    names(df) <- NULL

    p <-chi_sq_test$p.value[1]


    df <- data.frame(term=c("Chi-squared", "df", "p.value"),
                    value=c(x2d, df, p))

    names(df) <- NULL
    return(kable(df, caption="Pearson's Chi-squared test"))


}
