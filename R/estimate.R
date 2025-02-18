#' estimate
#'
#' This is a description of my function.
#' @importFrom stats coef lm pnorm qnorm runif
#' @importFrom cmdstanr cmdstan_model
#' @importFrom dplyr pull
#' @export
estimate <- function(dat) {
  dat <- as.data.frame(dat)
  T <- dat["T"]
  O <- dat[["O"]]
  Y <- dat['Y']
  N <- dim(dat)[1]
  C = length(unique(O))

  center <- rep(0, 4)
  stan_data_T <- list(N = N, C = C,
                      y = O, X = T, center = center)
  file_path <- system.file("extdata", "ordinal.stan", package = "ordinalconfounder")
  compiled_model <- cmdstanr::cmdstan_model("../tools/ordinal.stan")

  fitT <- compiled_model$sample(data = stan_data_T,
                                 seed = 123,
                                 init = 1,
                                 chains = 4,
                                 parallel_chains = 4,
                                 refresh = 0)

  pars <- fitT$summary() |> dplyr::pull(mean)
  g <- pars[3:6]
  beta <- pars[2]

  ez <- dat$T * beta
  z <- rep(0, length(ez))

  for (j in seq_along(ez)) {
    a <- max(-Inf, g[O[j] - 1], na.rm = TRUE)
    b <- min(Inf, g[O[j]], na.rm = TRUE)
    u <- runif(1, pnorm(a - ez[j]), pnorm(b - ez[j]))
    if(u == 1)
    {z[j] <- max(z)}
    else if(u ==0)
    {z[j] <- min(z)}
    else
    {z[j] <- ez[j] + qnorm(u)}
  }


  stan_data_Y <- list(N = N, C = C,
                      y = dat$O, X = matrix(dat$Y), center = center)
  fitY <- compiled_model$sample(data = stan_data_Y,
                                 seed = 123,
                                 init = 1,
                                 chains = 4,
                                 parallel_chains = 4,
                                 refresh = 0)

  pars <- fitY$summary() |> dplyr::pull(mean)
  g <- pars[3:6]
  beta <- pars[2]

  ew <- dat$Y * beta
  w <- rep(0, length(ew))
  for (j in seq_along(ew)) {
    a <- max(-Inf, g[O[j] - 1], na.rm = TRUE)
    b <- min(Inf, g[O[j]], na.rm = TRUE)
    u <- runif(1, pnorm(a - ew[j]), pnorm(b - ew[j]))
    if(u == 1)
    {w[j] <- max(w)}
    else if(u ==0)
    {w[j] <- min(w)}
    else
    {w[j] <- ez[j] + qnorm(u)}
  }

  dat[['z']] <- z
  dat[['w']] <- w

  what <- stats::lm(w ~ z + T + O, dat)$fitted.values

  ATE <- stats::coef(lm(Y ~ T + what, dat))['T']
  return(ATE)



}

