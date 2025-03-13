devtools::load_all("./")
n <- 1e2
U <- rnorm(n)
T <- 4 * U + rnorm(n)
Y <- 8 * T -5 * U + rnorm(n)
O <- as.numeric(cut(U, 5)) # other ways to generate ordinal variables

dat <- data.frame(cbind(U, T, Y, O))


lm(Y ~ T + O, dat)


print(estimate(dat, family = "continuous"))

