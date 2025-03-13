#' estimate
#'
#' Main estimation funciton
#'  @param dat data set
#' @param family family should be one of binomial, continuous
#' @param model model should be one of sampler or clm (default is clm)
#' @param return_what return the fitted proximal control variable or the full model
#' @importFrom stats coef lm pnorm qnorm runif
#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr pull
#' @export
estimate <- function(dat, family, model = "clm", return_what = TRUE) {
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
    # mm for more general instead of just cbinding all together
    mm <- cbind(model.matrix(~Z + T+ O, dat), 1)
    what <- mm %*% coef(mod1)
    if(return_what){
      return(list("what" = what))
    }
    bTYhat <- stats::glm(Y ~ T + what, dat, family = binomial())
  }else if(family == "continuous"){
    what <- stats::lm(W ~ Z + T + O, dat)$fitted.values
    if(return_what){
      return(list("what" = what))
    }
    bTYhat <- lm(Y ~ T + what, dat)
  }



  return(list("bootstrapMod" = bTYhat,
             "what" = what))


}

