# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "fireSense_IgnitionFit",
  description = "Fit statistical models that can be used to parameterize (calibrate) 
                 the fire ignition component of landscape fire models (e.g. fireSense).",
  keywords = c("fire frequency", "optimization", "additive property", "poisson", "negative binomial", "fireSense"),
  authors = c(person("Jean", "Marchal", email = "jean.d.marchal@gmail.com", role = c("aut", "cre"))),
  childModules = character(),
  version = list(SpaDES.core = "0.1.0", fireSense_IgnitionFit = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSense_IgnitionFit.Rmd"),
  reqdPkgs = list("DEoptim", "dplyr", "MASS", "magrittr", "numDeriv", "parallel"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", default, min, max, "parameter description")),
    defineParameter(name = "formula", class = "formula", default = NA,
                    desc = "a formula describing the model to be fitted. 
                            Piece-wised terms can be specifed using 
                            `pw(variableName, knotName)`."),
    defineParameter(name = "family", class = "function, character", default = "negative.binomial",
                    desc = "a family function (must be wrapped with `quote()`) 
                            or a character string naming a family function. For 
                            additional details see `?family`"),
    defineParameter(name = "data", class = "character", default = "dataFireSense_IgnitionFit",
                    desc = "a character vector indicating the names of objects 
                            in the `simList` environment in which to look for 
                            variables present in the model formula. `data` 
                            objects should be data.frames."),
    defineParameter(name = "plot", class = "logical", default = TRUE,
                    desc = "logical. Plot model fit against the data. Prediction interval"),
    defineParameter(name = "start", class = "numeric, list", default = NULL, 
                    desc = "optional starting values for the parameters to be 
                            estimated. Those are passed to `nlminb` and can be a
                            single vector, or a list of vectors. In the latter 
                            case, only the best solution, that is, the one which
                            minimizes the most the objective function, is kept."),
    defineParameter(name = "lb", class = "list", default = NULL, 
                    desc = "optional named list with up to three elements, 
                            'coef', 'theta' and 'knots', specifying lower bounds
                            for coefficients to be estimated. These must be
                            finite and will be recycled if necessary to match 
                            `length(coefficients)`."),
    defineParameter(name = "ub", class = "numeric", default = NULL, 
                    desc = "optional named list with up to three elements, 
                            'coef', 'theta' and 'knots', specifying upper bounds
                            for coefficients to be estimated. These must be
                            finite and will be recycled if necessary to match 
                            `length(coefficients)`."),
    defineParameter(name = "iterDEoptim", class = "integer", default = 2000,
                    desc = "integer defining the maximum number of iterations 
                            allowed (DEoptim optimizer). Default is 2000."),
    defineParameter(name = "iterNlminb", class = "integer", default = 500, 
                    desc = "if start is not supplied, iterNlminb defines 
                            the number of trials, or searches, to be performed
                            by the nlminb optimizer in order to find the best
                            solution. Default is 500."),
    defineParameter(name = "cores", class = "integer", default = 1, 
                    desc = "non-negative integer. Defines the number of logical
                            cores to be used for parallel computation. The
                            default value is 1, which disables parallel 
                            computing."),
    defineParameter(name = "trace", class = "numeric", default = 0,
                    desc = "non-negative integer. If > 0, tracing information on
                            the progress of the optimization are printed every
                            `trace` iteration. If parallel computing is enable, 
                            nlminb trace logs are written into the working directory. 
                            Log files are prefixed with 'fireSense_IgnitionFit_trace'
                            followed by the nodename (see ?Sys.info) and the
                            subprocess pid. Default is 0, which turns off tracing."),
    defineParameter(name = "nlminb.control", class = "numeric",
                    default = list(iter.max = 5e3L, eval.max=5e3L),
                    desc = "optional list of control parameters to be passed to 
                            the `nlminb` optimizer. See `?nlminb`."),
    defineParameter(name = ".runInitialTime", class = "numeric", default = start(sim),
                    desc = "when to start this module? By default, the start 
                            time of the simulation."),
    defineParameter(name = ".runInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between two runs of this module,
                            expressed in units of simulation time. By default, NA, which means that this module only runs once per simulation."),
    defineParameter(name = ".saveInitialTime", class = "numeric", default = NA, 
                    desc = "optional. When to start saving output to a file."),
    defineParameter(name = ".saveInterval", class = "numeric", default = NA, 
                    desc = "optional. Interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = expectsInput(
    objectName = "dataFireSense_IgnitionFit",
    objectClass = "data.frame",
    desc = "One or more objects of class data.frame in which to look for variables present in the model formula.",
    sourceURL = NA_character_
  ),
  outputObjects = createsOutput(
    objectName = "fireSense_IgnitionFitted",
    objectClass = "fireSense_IgnitionFit",
    desc = "A fitted model object of class fireSense_IgnitionFit."
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.fireSense_IgnitionFit = function(sim, eventTime, eventType, debug = FALSE) 
{
  moduleName <- current(sim)$moduleName
  
  switch(
    eventType,
    init = {
      sim <- frequencyFitInit(sim)
      
      sim <- scheduleEvent(sim, eventTime = P(sim)$.runInitialTime, moduleName, "run")
      
      if (!is.na(P(sim)$.saveInitialTime))
        sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, moduleName, "save", .last())
    },
    run = { 
      sim <- frequencyFitRun(sim)
      
      if (!is.na(P(sim)$.runInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.runInterval, moduleName, "run")
    },
    save = { 
      sim <- frequencyFitSave(sim)
      
      if (!is.na(P(sim)$.saveInterval))
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, moduleName, "save", .last())
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  
  invisible(sim)
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
frequencyFitInit <- function(sim) 
{
  moduleName <- current(sim)$moduleName

  # Checking parameters
  stopifnot(P(sim)$trace >= 0)
  stopifnot(P(sim)$cores >= 1)
  stopifnot(P(sim)$iterDEoptim >= 1)
  stopifnot(P(sim)$iterNlminb >= 1)
  if (!is(P(sim)$formula, "formula")) stop(moduleName, "> The supplied object for the 'formula' parameter is not of class formula.")
  
  invisible(sim)
}

frequencyFitRun <- function(sim)
{
  moduleName <- current(sim)$moduleName

  ## Toolbox: set of functions used internally by frequencyFitRun
    ## Handling piecewise terms in a formula
    pw <- function(variable, knot) pmax(variable - knot, 0)
      
    ## Compute the order of magnitude
    oom <- function(x) 10^(ceiling(log10(abs(x))))
    
    ## Extract the elements of the special terms, i.e. the variable and the knot value
    extractSpecial <- function(v, k)
    {
      cl <- match.call()

      if(missing(k)) stop(moduleName, "> Argument 'knotName' is missing (variable '", as.character(cl$v), "')")
      else list(variable = if (is(cl$v, "AsIs")) cl$v else as.character(cl$v), knot = as.character(cl$k))
    }
    
    ## Function to pass to the optimizer
    obj <- function(params, linkinv, nll, sm, nx, mm, mod_env, offset)
    {
      ## Parameters scaling
      params <- drop(params %*% sm)
      
      mu <- drop(mm %*% params[1L:nx]) + offset
      
      ## link implementation
      mu <- linkinv(mu)
      
      if(any(mu <= 0) || anyNA(mu) || any(is.infinite(mu)) || length(mu) == 0) return(1e20)
      else return(eval(nll, envir = as.list(environment()), enclos = mod_env))
    }
      
    ## Function to pass to the optimizer (PW version)
    objPW <- function(params, formula, linkinv, nll, sm, updateKnotExpr, nx, mod_env, offset)
    {
      ## Parameters scaling
      params <- drop(params %*% sm)
      
      eval(updateKnotExpr, envir = environment(), enclos = mod_env) ## update knot's values
  
      mu <- drop(model.matrix(formula, mod_env) %*% params[1:nx]) + offset
      
      ## link implementation
      mu <- linkinv(mu)
       
      if(any(mu <= 0) || anyNA(mu) || any(is.infinite(mu)) || length(mu) == 0) return(1e20)
      else return(eval(nll, envir = as.list(environment()), enclos = mod_env))
    }
      
    ## Nlminb wrapper
    objNlminb <- function(start, objective, lower, upper, control) 
    {
      nlminb.call <- quote(nlminb(start = start, objective = objective, lower = lower, upper = upper, control = control))
      nlminb.call[names(formals(objective)[-1L])] <- parse(text = formalArgs(objective)[-1L])

      o <- eval(nlminb.call)
      
      i <- 1L
      
      ## When there is no convergence and restart is possible, run nlminb() again
      while(as.integer(gsub("[\\(\\)]", "", regmatches(o$message, gregexpr("\\(.*?\\)", o$message))[[1L]])) %in% 7:14 & i < 3L)
      {
        i <- i + 1L
        o <- eval(nlminb.call)
      }
      o
    }
    
  # Load inputs in the data container
  # list2env(as.list(envir(sim)), envir = mod)
    
  mod_env <- new.env()

  for (x in P(sim)$data)
  {
    if (!is.null(sim[[x]])) 
    {
      if (is.data.frame(sim[[x]])) 
      {
        list2env(sim[[x]], envir = mod_env)
      } 
      else stop(moduleName, "> '", x, "' is not a data.frame.")
    }
  }
  
  # Define pw() within the data container
  mod_env$pw <- pw
        
  if (is.empty.model(P(sim)$formula))
    stop(moduleName, "> The formula describes an empty model.")
  
  terms <- terms.formula(formula <- P(sim)$formula, specials = "pw")
  
  if (attr(terms, "response")) y <- as.character(formula[[2L]])
  else stop(moduleName, "> Incomplete formula, the LHS is missing.")

  nx <- length(labels(terms)) + attr(terms, "intercept") ## Number of variables (covariates)
  allxy <- all.vars(terms)
  
  # Check the presence of at least one piecewise term
  hvPW <- !is.null(attr(terms, "specials")$pw)
  
  kLB <- kUB <- NULL

  if (hvPW) 
  {
    objfun <- objPW
    
    specialsInd <- which(unlist(lapply(attr(terms,"variables"), is.call)))
    specialsCalls <- attr(terms,"variables")[specialsInd]
    
    ## Extract the names of the knots (breakpoints)
    ## Alternative way: all.vars(terms)[!all.vars(terms) %in% rownames(attr(terms,"factors"))]
    specialsTerms <- lapply(
      specialsCalls, 
      function(specialsCall)
      {
        if (specialsCall[[1L]] == "pw")
        {
          specialsCall[[1L]] <- quote(extractSpecial)
          eval(specialsCall)
        }
      }
    )
    
    specialsTerms <- specialsTerms[!unlist(lapply(specialsTerms, is.null))]
    
    kNames <- sapply(specialsTerms, "[[", "knot")
    
    if (anyDuplicated(kNames)) 
      stop(moduleName, "> Knot's names are not unique.")
    
    nk <- length(kNames)
    allx <- allxy[!allxy %in% c(y, kNames)]
    
    missing <- !allxy[!allxy %in% kNames] %in% ls(mod_env, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[!allxy %in% kNames][missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects nor in the simList environment.")
    
    ## Covariates that have a breakpoint
    pwVarNames <- sapply(specialsTerms, "[[", "variable", simplify = FALSE)
    
    kUB <- if (is.null(P(sim)$ub$k)) lapply(pwVarNames, function(x) max(if(is(x, "AsIs")) x else mod_env[[x]])) %>% unlist
           else rep_len(P(sim)$ub$k, nk) ## User-defined bounds (recycled if necessary)
    
    kLB <- if (is.null(P(sim)$lb$k)) lapply(pwVarNames, function(x) min(if(is(x, "AsIs")) x else mod_env[[x]])) %>% unlist
           else rep_len(P(sim)$lb$k, nk) ## User-defined bounds (recycled if necessary)
    
    invisible(
      mapply(
        kNames, 
        z = pwVarNames, 
        FUN = function(w, z) 
          mod_env[[w]] <- mean(if(is(z, "AsIs")) z else mod_env[[z]]),
        SIMPLIFY = FALSE
      )
    )
    
    updateKnotExpr <- parse(text = paste0("mod_env[[\"", kNames, "\"]] = params[", (nx + 1L):(nx + nk), "]", collapse="; "))
  } 
  else
  {
    missing <- !allxy %in% ls(mod_env, all.names = TRUE)
    
    if (s <- sum(missing))
      stop(moduleName, "> '", allxy[missing][1L], "'",
           if (s > 1) paste0(" (and ", s-1L, " other", if (s>2) "s", ")"),
           " not found in data objects nor in the simList environment.")
    
    allx <- allxy[allxy != y] 
    objfun <- obj
    nk <- 0L
  }
  
  family <- P(sim)$family
  
  if (is.language(family)) family <- eval(family)
  
  if (is.character(family))
  {
    family <- get(family, mode = "function", envir = parent.frame())
    family <- tryCatch(
      family(),
      error = function(e) family(
        theta = suppressWarnings(
          glm.nb(formula = formula,
                 y = FALSE,
                 model = FALSE,
                 data = mod_env)[["theta"]]
        )
      )
    )
  } 
  else if (is.function(family))
  {
    family <- family()
  }
  
  # Check ff family is negative.binomial
  isFamilyNB <- grepl(x = family$family, pattern = "Negative Binomial")
  
  # Extract starting value for theta
  if (isFamilyNB)
    theta <- as.numeric(gsub(x = regmatches(family$family, gregexpr("\\(.*?\\)", family$family))[[1L]], pattern = "[\\(\\)]", replacement = ""))
  
  linkinv <- family$linkinv
  
  mm <- model.matrix(object = terms, data = mod_env)
  
  # Does the model formula contain an offset?
  model_offset <- model.offset(model.frame(formula, mod_env))
  offset <- if(is.null(model_offset)) 0 else model_offset
  
  ## Define the scaling matrix. This is used later in the optimization process
  ## to rescale parameter values between 0 and 1, i.e. put all variables on the same scale.
    n <- nx + nk
    if (isFamilyNB) n <- n + 1L
    
    sm <- matrix(0, n, n)
    diag(sm) <- 1

  ## Define parameter bounds automatically if they are not supplied by user
  ## First defined the bounds for DEoptim, the first optimizer    

    ## Upper bounds
      DEoptimUB <- c(
        if (is.null(P(sim)$ub$c))
        {
          ## Automatically estimate an upper boundary for each parameter       
          (suppressWarnings(
            tryCatch(
              glm(
                formula = formula,
                y = FALSE,
                model = FALSE,
                data = mod_env,
                family = poisson(link = family$link)
              ),
              error = function(e) stop(
                moduleName, "> Automated estimation of upper bounds", 
                " (coefs) failed, please set the 'coef' element of ",
                "the 'ub' parameter."
              )
            )
          ) %>% coef %>% oom(.)) * 10L -> ub
          
          if (anyNA(ub))
            stop(
              moduleName, "> Automated estimation of upper bounds (coefs) failed, ",
              "please set the 'coef' element of the 'ub' parameter."
            )
          else ub
        } 
        else
          rep_len(P(sim)$ub$c, nx), ## User-defined bounds (recycled if necessary)
        kUB
      )

    ## Lower bounds
      DEoptimLB <- c({
        switch(family$link,
               log = {
                 
                 if (is.null(P(sim)$lb$c)) -DEoptimUB[1L:nx] ## Automatically estimate a lower boundary for each parameter
                 else rep_len(P(sim)$lb$c, nx) ## User-defined bounds (recycled if necessary)
                 
               }, identity = {
                 
                 if (is.null(P(sim)$lb$c)) rep_len(1e-16, nx) ## Ensure non-negativity
                 else rep_len(P(sim)$lb$c, nx) ## User-defined bounds (recycled if necessary)
                 
               }, stop(moduleName, "> Link function ", family$link, " is not supported."))
      }, kLB)
    
      ## If negative.binomial family needs to add bounds for theta parameter
      if (isFamilyNB) 
      {
        DEoptimUB <- c(DEoptimUB, if (is.null(P(sim)$ub$t)) 2L * theta else P(sim)$ub$t)
        DEoptimLB <- c(DEoptimLB, if (is.null(P(sim)$lb$t)) 1e-16 else P(sim)$lb$t) ## Enfore non-negativity
      }

  ## Then, define lower and upper bounds for the second optimizer (nlminb)
    ## Upper bounds
      nlminbUB <- DEoptimUB
      if (is.null(P(sim)$ub$c))
        nlminbUB[1:nx] <- rep_len(Inf, nx)

    ## Lower bounds  
      nlminbLB <- if (is.null(P(sim)$lb$c))
      {
        c(switch(family$link,
                 log = rep_len(-Inf, nx),       ## log-link, default: -Inf for terms and 0 for breakpoints/knots
                 identity = rep_len(1e-16, nx)) ## identity link, default: enforce non-negativity
          , kLB)
        
      } else DEoptimLB ## User-defined lower bounds for parameters to be estimated
  
      ## If negative.binomial family add bounds for the theta parameter
      if (isFamilyNB && is.null(P(sim)$lb$t)) 
      {
        nlminbLB <- c(nlminbLB, 1e-16) ## Enforce non-negativity
      } 
      else if (isFamilyNB)
      {
        nlminbLB <- c(nlminbLB, P(sim)$lb$t)
      }

  ## Define the log-likelihood function (objective function)
  nll <- switch(family$family,
                poisson = parse(text=paste0("-sum(dpois(x=", y,", lambda = mu, log = TRUE))")),
                parse(text=paste0("-sum(dnbinom(x=", y,", mu = mu, size = params[length(params)], log = TRUE))")))
  
  trace <- P(sim)$trace
  
  if (P(sim)$cores > 1) 
  {
    if (.Platform$OS.type == "unix")
      mkCluster <- parallel::makeForkCluster
    else
      mkCluster <- parallel::makePSOCKcluster
    
    cl <- mkCluster(P(sim)$cores)
    on.exit(stopCluster(cl))
    clusterEvalQ(cl, library("MASS"))
  }
  
  ## If starting values are not supplied
    if (is.null(P(sim)$start)) 
    {
      ## First optimizer, get rough estimates of the parameter values
      ## Use these estimates to compute the order of magnitude of these parameters
  
      control <- list(itermax = P(sim)$iterDEoptim, trace = P(sim)$trace)
      if(P(sim)$cores > 1) control$cluster <- cl

      DEoptimCall <- quote(DEoptim(fn = objfun, lower = DEoptimLB, upper = DEoptimUB, control = do.call("DEoptim.control", control)))
      DEoptimCall[names(formals(objfun)[-1])] <- parse(text = formalArgs(objfun)[-1])
      DEoptimBestMem <- eval(DEoptimCall) %>% `[[` ("optim") %>% `[[` ("bestmem")
      
      ## Update scaling matrix
      diag(sm) <- oom(DEoptimBestMem)
      
      ## Update of the lower and upper bounds of the coefficients based on the scaling matrix
      nlminbLB[c(1:nx, length(nlminbLB))] <- nlminbLB[c(1:nx, length(nlminbLB))] / diag(sm)[c(1:nx, length(nlminbLB))]
      nlminbUB[c(1:nx, length(nlminbUB))] <- nlminbUB[c(1:nx, length(nlminbUB))] / diag(sm)[c(1:nx, length(nlminbUB))]
      
      ## Update of the lower and upper bounds for the knots based on the scaling matrix
        if (hvPW) 
        {
          kLB <- DEoptimLB[(nx + 1L):(nx + nk)] / diag(sm)[(nx + 1L):(nx + nk)]
          nlminbLB[(nx + 1L):(nx + nk)] <- if (is.null(P(sim)$lb$k)) kLB else pmax(kLB, P(sim)$lb$k)
          
          kUB <- DEoptimUB[(nx + 1L):(nx + nk)] / diag(sm)[(nx + 1L):(nx + nk)]
          nlminbUB[(nx + 1L):(nx + nk)] <- if(is.null(P(sim)$ub$k)) kUB else pmin(kUB, P(sim)$ub$k)
        }

      getRandomStarts <- function(.) pmin(pmax(rnorm(length(DEoptimBestMem),0L,2L)/10 + unname(DEoptimBestMem/oom(DEoptimBestMem)), nlminbLB), nlminbUB)
      start <- c(lapply(1:P(sim)$iterNlminb, getRandomStarts), list(unname(DEoptimBestMem/oom(DEoptimBestMem))))
    } 
    else 
    {
      start <- if (is.list(P(sim)$start))
      {
        diag(sm) <- lapply(P(sim)$start, oom) %>%
          do.call("rbind", .) %>%
          apply(2, function(x) as.numeric(names(base::which.max(table(x)))))
        
         lapply(P(sim)$start, function(x) x / diag(sm))
      } 
      else 
      {
        diag(sm) <- oom(P(sim)$start)
        P(sim)$start / diag(sm)
      }
    }

  out <- if (is.list(start)) 
  {
    if (P(sim)$cores > 1) 
    {
      outputPath <- outputPath(sim)
      basePattern <- paste(moduleName, Sys.info()[["nodename"]], format(Sys.time(), "%Y%m%d"), "trace", sep = "_")

      if (trace) 
      {
        clusterExport(cl, c("outputPath", "basePattern"), envir = environment())
        
        clusterEvalQ(
          cl, 
          sink(file.path(outputPath, paste0(basePattern, ".", Sys.getpid())))
        )
      }  
      
      out <- clusterApplyLB(cl = cl, x = start, fun = objNlminb, objective = objfun, lower = nlminbLB, upper = nlminbUB, control = c(P(sim)$nlminb.control, list(trace = trace)))
      
      if (trace) clusterEvalQ(cl, sink())
    } 
    else out <- lapply(start, objNlminb, objective = objfun, lower = nlminbLB, upper = nlminbUB, control = c(P(sim)$nlminb.control, list(trace = trace)))
    
    ## Select best minimum amongst all trials
    out[[which.min(sapply(out, "[[", "objective"))]]
    
  } 
  else objNlminb(start, objfun, nlminbLB, nlminbUB, c(P(sim)$nlminb.control, list(trace = trace)))

  ## Compute the standard errors around the estimates
    hess.call <- quote(numDeriv::hessian(func = objfun, x = out$par))
    hess.call[names(formals(objfun)[-1L])] <- parse(text = formalArgs(objfun)[-1L])
    hess <- eval(hess.call)
    se <- suppressWarnings(tryCatch(drop(sqrt(diag(solve(hess))) %*% sm), error = function(e) NA))
    
  convergence <- TRUE

  if (out$convergence)
  {
    convergence <- FALSE
    convergDiagnostic <- paste0("nlminb optimizer did not converge (", out$message, ")")
    warning(moduleName, "> ", convergDiagnostic, immediate. = TRUE)
  } 
  else if(anyNA(se)) 
  {
    ## Negative values in the Hessian matrix suggest that the algorithm did not converge
    convergence <- FALSE
    convergDiagnostic <- "nlminb optimizer reached relative convergence, saddle point?"
    warning(moduleName, "> ", convergDiagnostic, immediate. = TRUE)
  }
  else
  {
    convergDiagnostic <- out$message
  }
  
  ## Parameters scaling: Revert back estimated coefficients to their original scale
  out$par <- drop(out$par %*% sm)

  l <- list(formula = formula,
            family = family,
            coef = setNames(out$par[1:nx], colnames(mm)),
            coef.se = setNames(se[1:nx], colnames(mm)),
            LL = -out$objective,
            AIC = 2 * length(out$par) + 2 * out$objective,
            convergence = convergence,
            convergenceDiagnostic = convergDiagnostic)
  
  if (hvPW) 
  {
    l$knots <- setNames(out$par[(nx + 1L):(nx + nk)], kNames)
    l$knots.se <- setNames(se[(nx + 1L):(nx + nk)], kNames)
  }
  
  if(isFamilyNB)
  {
    ## Update the NB family template with the estimated theta
    l$family <- MASS::negative.binomial(theta = out$par[length(out$par)], link = family$link)
    l$theta <- out$par[length(out$par)]
    l$theta.se <- se[length(se)]
  }
  
  sim$fireSense_IgnitionFitted <- l
  class(sim$fireSense_IgnitionFitted) <- "fireSense_IgnitionFit"
  
  invisible(sim)
}


frequencyFitSave <- function(sim)
{
  timeUnit <- timeunit(sim)
  currentTime <- time(sim, timeUnit)
  
  saveRDS(
    sim$fireSense_IgnitionFitted, 
    file = file.path(paths(sim)$out, paste0("fireSense_IgnitionFitted_", timeUnit, currentTime, ".rds"))
  )
  
  invisible(sim)
}
