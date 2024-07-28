# estimate!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

estimate <- function(data) {
  T <- data[["T"]]
  O <- data[["O"]]
  Y <- data[["Y"]]
  N <- dim(data)[1]
  C = length(unique(O))

  stan_data_T <- list(N = N, C = C,
                      y = O, X = T)
  fitT <- compiled_model$sample(data = stan_data_T,
                                 seed = 123,
                                 init = 1,
                                 chains = 4,
                                 parallel_chains = 4,
                                 refresh = 0)
}

