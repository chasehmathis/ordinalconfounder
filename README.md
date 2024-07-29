# ordinalconfounder

With normal multivariate data $(U, T, Y)$ where $U$ is hidden, we sometimes have ordinal, binned data that is a proxy for $U$.

Proximal Causal Learning requires *two* proxies. Here, we consider using only one proxy.

```
{r}
n <- 1e2
U <- rnorm(n)
T <- 2 * U + rnorm(n)
Y <- 4 * T -6 * U + rnorm(n)
O <- as.numeric(cut(U, 5)) # some binned data

dat <- data.frame(cbind(U, T, Y, O))
```

Consider our estimator, now:

```
{r}
library(ordinalconfounder)

# naive estimate
lm(Y ~ T + O, dat)

# bayesian boot
estimate(dat)
```
