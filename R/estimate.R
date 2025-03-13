#' estimate
#'
#' Main estimation funciton
##'  @param dat data set
##' @param family family should be one of binomial, continuous
##' @param model model should be one of sampler or clm (default is clm)
#' @importFrom stats coef lm pnorm qnorm runif
#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr pull
#' @export
estimate <- function(dat, family, model = "clm") {
  if(missing(family)){
    stop("Must specify family to be logistic or continuous")
  }
  dat <- as.data.frame(dat)
  T <- dat["T"]
  O <- dat[["O"]]
  Y <- dat['Y']
  N <- dim(dat)[1]
  C = length(unique(O))

  if(family == "binomial"){
    # warn logic here, won't work well unless have proxy
    Tstar = dat[["Tstar"]]
    Ystar = dat[["Ystar"]]
  } else if (family == "continuous"){
    Tstar = dat[["T"]]
    Ystar = dat[["T"]]
  }
  if(model == "stan"){
    WZ <- model_output_stan(N, C, O, T, Y)
  }else if(model == "clm"){
    T <- dat[["T"]]
    Y <- dat[["Y"]]
    WZ <- model_output_clm(N, C, O, Tstar, Ystar)

  }
  dat["W"] <- WZ[["W"]]
  dat["Z"] <- WZ[["Z"]]

  if(family == "binomial"){
    mod1 <- stats::lm(W ~ Z + T + O + Y, dat)
    what <- cbind(1,dat$Z, dat$T, dat$O, 1) %*% coef(mod1)
    bTYhat <- stats::glm(Y ~ T + what, dat, family = binomial())
  }else if(family == "continuous"){
    what <- stats::lm(W ~ Z + T + O, dat)$fitted.values
    bTYhat <- lm(Y ~ T + what, dat)
  }



  return(list("bootstrapMod" = bTYhat))


}

