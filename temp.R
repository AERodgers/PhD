optLme4Mdl <- function(model,
                       checks = c("allFit", "optimx", "nloptwrap"),
                       reject_nl = T) {
    # Returns lme4-mased model which converges, if possible.
    # If no better model found, original model is returned with a text Warning.
    # Note: for large or complex models this can be exceedingly slow!

    library(tidyverse)
    library(parallel)
    library(lme4)
    library(optimx)
    library(dfoptim)
    library(blme) # my preferred bayesian lme package.

    ### inner functions
    ##################
    modelIsOK <- function(model, reject_nl = F) {
        # Returns TRUE is a model converges and is not singular.

        return(as.logical(
            # Check for convergence
            is.null(model@optinfo$conv$lme4$messages) &
                # check for singularity
                !isSingular(model) &
                # check for other warnings
                is.null(model@optinfo$conv$lme4$warnings) &
                (!reject_nl | !(model@optinfo$optimizer %in% c("nmkbw",
                                                               "Nelder_Mead")))
        ))

    }

    tryAllFit <- function(model) {
        # Tries to return a model which converges in allFit()

        library(parallel)
        library(lme4)
        library(optimx)
        library(minqa)
        library(dfoptim)
        library(blme)

        ncores <- detectCores()

        # Run allFit on multiple cores.
        cat("\nchecking allFit()\n")
        diff_optims <-
            allFit(model,
                   maxfun = 1e5,
                   verbose = T,
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
            first_fit <- model
        } else {
            model <- diff_optims[working_indices][[1]]
        }


        return(model)

    }

    tryOptimx <- function(model, reject_nl = F) {
        # Tries to return a model which converges by varying optimx() methods.

        optimx_options <-
            c("L-BFGS-B", "nlminb", "nlm", "bobyqa", "hjkb")
        if (!reject_nl){optimx_options <- c(optimx_options,  "nmkb")}

        num_options <- length(optimx_options)
        model_elements <- getModelElements(model)
        info <- "No option"

        cat("\nRunning basic model with different settings\n" )
        model <- lmer(model_elements$formula,
                      data = model_elements$frame,
                      control = lmerControl(
                          optCtrl = list(maxit = 1e9, maxfun = 1e9)
                      ))

        cat("checking optimx optimization settings:\n")


        i <- 0
        while (i < num_options & !modelIsOK(model)) {
            i  <-  i + 1
            cat(" -", optimx_options[i], "\n" )
            model <- lmer(
                formula = model_elements$formula,
                data = model_elements$frame,
                control = lmerControl(
                    optimizer = "optimx",
                    optCtrl = list(method = optimx_options[i],
                                   maxit = 1e9)
                )
            )
        }

        if (modelIsOK(model)) {
            info <- optimx_options[i]
        }
        cat("\n\n", info, "works from settings options.")

        return(model)
    }

    trynlopt <- function(model, reject_nl = F) {
        # Tries to return a model which converges by through lnloptwrap algorithms.

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
        model_elements <- getModelElements(model)
        info <- "No option"

        cat("\nChecking nloptwrap options:\n")

        i <- 0
        while (i < num_options & !modelIsOK(model)) {
            i  <- i + 1
            cat(" -", opts[i], "\n" )
            model <- lmer(formula = model_elements$formula,
                          data = model_elements$frame,
                          control = lmerControl(optimizer = "nloptwrap",
                                                optCtrl = list(algorithm = opts[i],
                                                               maxfun = 1e9,
                                                               maxeval = 1e7,
                                                               xtol_abs = 1e-9,
                                                               ftol_abs = 1e-9)))

        }

        if (modelIsOK(model)) {
            info <- opts[i]
        }
        cat("\n\n", info, "works from nloptwrap options.\n")

        return(model)
    }

    getModelElements <- function(model) {
        # Get elements of a model used for functions associated with optLme4Mdl
        ans <- list("formula" =  formula <- formula(model),
                    "optimizer" =  model@optinfo$optimizer,
                    "frame" =  model@frame,
                    "maxfun" = model@optinfo$control$maxfun,
                    "package" = model@resp$.objectPackage)
        return(ans)
    }


    ### outer function
    ##################
    if (!modelIsOK(model) & "allFit" %in% checks) {
        model <- tryAllFit(model)
        }

    if (!modelIsOK(model) & "optimx" %in% checks){
        model <- tryOptimx(model, reject_nl)
        }

    if (!modelIsOK(model) & "nloptwrap" %in% checks) {
        model <- trynlopt(model, reject_nl)
        }


    if (!modelIsOK(model)) {
        cat("\nNo alternatives converged. Proceed with extreme caution.\n")
        }
    else {cat("\nModel found.\n")
        }

    return(model)

    }




