modelstring <- "
  model {
    for (i in 1:length(x)) {
      x[i] ~ dnorm(mu, tau)
      pred[i] ~ dnorm(mu, tau)
    }
    mu ~ dnorm(10, 1 / 100)
    tau ~ dgamma(3, 12)

    # Example of matrix parameter
    pred_matrix[1, 1:length(x)] = pred
    pred_matrix[2, 1:length(x)] = abs(mu - pred)
}
"

model <-
  rjags::jags.model(
    file = textConnection(modelstring),
    data = list(x = rnorm(n = 10, mean = 15, sd = 5)),
    n.chains = 2,
    n.adapt = 1000,
    quiet = TRUE
  )

variable.names <- c("mu", "tau", "pred", "pred_matrix")
n.iter <- 40
thin <- 1
