optimizeLme <- function(model, optimizer = "bobyqa")
    {
    library(parallel)
    library(lme4)
    library(optimx)
    library(minqa)
    library(dfoptim)
    library(blme) # my prefered bayesian lme package.



    if (!modelIsOK(model))
    # Try to find better model via allFit().
    {checkAllFit(model)}

    if (!modelIsOK(model) & model@resp$family["family"] == "numeric")
    # Try to find better LME model via optimx variations().
    {tryOptimx(model)}

    if (!modelIsOK(model))
        {cat("\nNo alternatives converged. Proceed with extreme caution.\n")}

    return(model)
    }

modelIsOK <- function(model)
    {
    return(
        as.logical(
            # Check for convergence
            is.null(model@optinfo$conv$lme4$messages) *
                # check for singularity
                !isSingular(model) *
                # check for other warnings
                is.null(model@optinfo$conv$lme4$warnings)
            )
        )
    }

checkAllFit <- function(model)
    {
    library(parallel)
    library(lme4)
    library(optimx)
    library(minqa)
    library(dfoptim)
    library(blme)

    ncores <- detectCores()

    # Run allFit on multiple cores.
    diff_optims <-
        allFit(model,
               maxfun = 1e5,
               verbose = F,
               parallel = 'multicore',
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
        first_fit <- model
    } else {
        model <- diff_optims[working_indices][[1]]
    }


    return(model)

}

tryOptimx <- function(model)
    {
    library(lme4)
    library(optimx)
    library(dfoptim)
    library(blme)

    optimx_options <-
        c("L-BFGS-B", "nlminb", "nlm", "bobyqa", "nmkb", "hjkb")
    num_options <- length(optimx_options)

    formula <- formula(model)
    frame <- model@frame
    i <- 0
    while (i <= num_options & !modelIsOK(model))
        {
        i = i + 1

            model <- lmer(
                formula = formula,
                data = frame,
                control = lmerControl(
                    optimizer = "optimx",
                    optCtrl = list(method = optimx_options[i],
                                   maxit = 1e9)
                )
            )
        print(paste0("Testing optimx: ", optimx_options[i]))

        }

    return(model)
    }

optimizeLme(lh_pn_model)



