# Search for "good" opimization of Model based on lme4 package
#
# Returns lme4-based model which converges, if possible.
# If no better model is found, original model is returned with a text warning.
#
# email: antoinrodgers at tcd dot ie
# twitter: at phonetic_antoin
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
#
# Antoin Eoin Rodgers
# Phonetics and Speech Laboratory
# Trinity College. Dublin

optLme4Mdl <- function(model,
                       checks = c("allFit", "optimx", "nloptwrap"),
                       verbose = T,
                       reject_nl = T) {
    # Search for "good" opimization of Model based on lme4 package.
    #
    #    Returns lme4-based model which converges, if possible.
    #    If no better model is found, the original is returned with a warning.
    #
    #    Arguments:
    #        model     = model to check
    #        checks    = string (vector) of optimizer checks to run
    #                      - "allFit" = run allFit()
    #                      - "optimx" = try optimx using different methods.
    #                      - "nloptwrap" = try nloptwrap with different methods.
    #        verbose   = logical argument to control text output.
    #        reject_nl = logical argument for rejecting Nelder-Mead optimizers.

    library(tidyverse)
    library(parallel)
    library(lme4)
    library(optimx)
    library(dfoptim)
    library(blme) # my preferred Bayesian lme package.

    solution <- "original model"
    original_model <- model

    ### inner functions, (Outer function code is at the end.)
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
                model <- update(model,
                                control = lmerControl(
                                    optimizer = "optimx",
                                    optCtrl = list(method = optimx_options[i],
                                                   maxit = 1e9)
                                )
                )
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
        # Tries to return a model which converges using lnloptwrap algorithms.
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
            try(
                model <- update(model,
                                control = lmerControl(
                                    optimizer = "nloptwrap",
                                    optCtrl = list(algorithm = opts[i],
                                                   maxfun = 1e9,
                                                   maxeval = 1e7,
                                                   xtol_abs = 1e-9,
                                                   ftol_abs = 1e-9)))
            )

        }
        if(verbose){cat("\n")}

        if (modelIsOK(model, reject_nl)) {
            info <- opts[i]
            cat("\n\n")
            cat("***** WARNING ****\n")
            cat("NB: manually update lmerControl:\n\n")
            cat("control = lmerControl(\n")
            cat("  optimizer = \"nloptwrap\",\n")
            cat(paste("  optCtrl = list(algorithm = \"",
                      opts[i], "\",\n", sep = ""))
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
        # Returns TRUE is a model converges and is not singular.

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
        # Get elements of a model used for functions associated with optLme4Mdl
        ans <- list(
            "formula" =  formula <- formula(model),
            "optimizer" =  model@optinfo$optimizer,
            "frame" =  model@frame,
            "maxfun" = model@optinfo$control$maxfun,
            "package" = model@resp$.objectPackage
        )
        return(ans)
    }

    # Outer function

    if(verbose){
        cat("Searching for good optimization settings with optLme4Mdl().\n", sep = "")
        cat("  Formula:   ", getModelFormula(model), "\n", sep = "")
        cat("  Optimizer: ", model@optinfo$optimizer, "\n", sep = "")
    }

    if (!modelIsOK(model, reject_nl)) {
        if(verbose){
            cat("\nRunning basic model with less strict control settings.\n", sep = "")
        }
        try(model <- update(model, control = lmerControl(
            optCtrl = list(
                maxit = 1e9,
                maxfun = 1e9,
                xtol_abs = 1e-9,
                ftol_abs = 1e-9
            )
        )))
        if (!modelIsOK(model, reject_nl)){
            solution = " less strict control settings."
        }
    }

    if (!modelIsOK(model, reject_nl) & "allFit" %in% checks) {
        try(model <- tryAllFit(model, verbose))
        if (modelIsOK(model, reject_nl)) {
            solution <- "after trying allFit()"
        }
    }

    if (!modelIsOK(model, reject_nl) & "optimx" %in% checks) {
        model <- tryOptimx(model, reject_nl, verbose)
        if (modelIsOK(model, reject_nl)) {
            solution <- "after varying optimx settings."
        }
    }

    if (!modelIsOK(model, reject_nl) & "nloptwrap" %in% checks) {
        model <- trynlopt(model, reject_nl, verbose)
        if (modelIsOK(model, reject_nl)) {
            solution <- "after varying nloptwrap options."
        }
    }

    if (!modelIsOK(model, reject_nl)) {
        model <- original_model
        cat("\nNo alternatives converged. Reverting to original model.\n",
            sep = "")
    }
    else {
        found <- getModelElements(model)
        cat("\nModel found using ",
            found$optimizer,
            " after trying ",
            solution,
            ".\n",
            sep = "")
    }

    return(model)

}
