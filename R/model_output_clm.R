#' model_output_clm
#'
#' Main estimation funciton
##'  @param dat data set
##' @param family family should be one of logistic, continuous (can be inferred)
##' @param model model should be one of sampler or clm (default is clm)
#' @importFrom stats coef lm pnorm qnorm runif
#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr pull
#' @importFrom ordinal clm
#' @export
model_output_clm <- function(N, C, O, Tstar, Ystar) {

  mod <- summary(ordinal::clm(as.factor(O) ~ Tstar))
  pars <- mod$coefficients
  beta <- pars[nrow(pars),1]
  g <- pars[1:(nrow(pars)-1),1]


  ez <- Tstar * beta
  z <- rep(0, length(ez))
  y <- O;
  y <- as.numeric(as.factor(y))
  for (j in seq_along(ez)) {
    a <- max(-Inf, g[y[j] - 1], na.rm = TRUE)
    b <- min(Inf, g[y[j]], na.rm = TRUE)
    u <- runif(1, pnorm(a - ez[j]), pnorm(b - ez[j]))
    if(u == 1)
    {z[j] <- max(z)}
    else if(u ==0)
    {z[j] <- min(z)}
    else
    {z[j] <- ez[j] + qnorm(u)}
  }




  mod <- summary(ordinal::clm(as.factor(O) ~ Ystar))
  pars <- mod$coefficients
  beta <- pars[nrow(pars),1]
  g <- pars[1:(nrow(pars)-1),1]


  ew <- Ystar * beta

  w <- rep(0, length(ew))
  for (j in seq_along(ew)) {
    a <- max(-Inf, g[y[j] - 1], na.rm = TRUE)
    b <- min(Inf, g[y[j]], na.rm = TRUE)
    u <- runif(1, pnorm(a - ew[j]), pnorm(b - ew[j]))
    if(u == 1)
    {w[j] <- max(w)}
    else if(u ==0)
    {w[j] <- min(w)}
    else
    {w[j] <- ez[j] + qnorm(u)}
  }

  return(list("Z" = z, "W" = w))
}
