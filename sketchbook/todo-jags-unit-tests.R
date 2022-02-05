# assign-iterations-mcarray -----------------------------------------------

# Multidimensional test ---
x <- array(seq(180), dim = c(2, 2, 15, 3))
value <- 1000 * x[, , 6:10, ]
at <- 6:10

assign_iterations_mcarray(x, value, at)


# Single dimensional test ---
x <- array(seq(180), dim = c(4, 15, 3))
value <- 1000 * x[, 6:10, ]
at <- 6:10

assign_iterations_mcarray(x, value, at)


# Scalar test ----
x <- array(seq(45), dim = c(1, 15, 3))
value <- 1000 * x[, 6:10, ]
at <- 6:10

assign_iterations_mcarray(x, value, at)


# jags_samples_with_knitrpb -----------------------------------------------

jags_data <-
  local({
    n <- 1000
    x <- stats::rnorm(n, 0, 5)
    list(n = n, x = x)
  })

jags_string <- "
    model {
      for (i in 1:n) {
        x[i] ~ dnorm(mu, tau)
      }

      matr[1, 1:2] = c(mu, 0)
      matr[2, 1:2] = c(0, tau)

      mu ~ dnorm(0, 1 / 1000)
      tau ~ dgamma(10, 1)
    }
  "

jags_model <-
  rjags::jags.model(
    file = textConnection(jags_string),
    data = jags_data,
    n.chains = 4,
    n.adapt = 100,
    quiet = TRUE
  )

z <-
  jags_samples(
    model = jags_model,
    variable.names = c("mu", "tau", "matr"),
    n.iter = 100000,
    n.update = 1000,
    thin = 1,
    steps = 100
  )
