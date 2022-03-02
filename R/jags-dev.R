jags_in_knitr1 <- function(model, variable.names, n.iter, n.update, n.thin) {
  # Initialise progress bar to work in knitr
  steps <- min(100, floor(n.iter / n.thin))
  pb <- knitrProgressBar::progress_estimated(steps + 1)
  knitrProgressBar::update_progress(pb)

  # Define trace start and end points for each step
  n_iter_per_step <- floor(n.iter / steps)

  ends <- cumsum(rep(n_iter_per_step, steps))
  ends[length(ends)] <- n.iter
  starts <- 1 + c(0, utils::head(ends, -1))

  # Run warm-up samples and extract attributes before discarding
  warmup_samples <-
    rjags::jags.samples(
      model,
      variable.names,
      thin = n.thin,
      n.iter = n.update,
      quiet = TRUE,
      progress.bar = "none"
    )

  out <-
    purrr::imap(warmup_samples, function(.x, .y) {
      new_dims <- dim(.x)
      new_dims["iteration"] <- floor(n.iter / n.thin)

      structure(
        array(NA, dim = new_dims),
        class = "mcarray",
        varname = .y,
        type = "trace",
        iterations = c(
          start = model$iter() + starts[1],
          end = model$iter() + ends[length(ends)],
          thin = n.thin
        )
      )
    })

  rm(warmup_samples)

  # One JAGS call per step to be placed into `out` at the correct index,
  #  (keep pointer to next blank space in `out`)
  iters_added <- 0

  for (i in seq_len(steps)) {
    knitrProgressBar::update_progress(pb)

    cur_samples <-
      rjags::jags.samples(
        model,
        variable.names,
        n.iter = ends[i] - starts[i] + 1,
        thin  = 1,
        type = "trace",
        quiet = TRUE,
        progress.bar = "none"
      )

    # Subset samples according to OVERALL 'thin'
    cur_iters_to_keep <- (seq(starts[i], ends[i]) %% n.thin) == 0
    cur_thinned_samples <-
      purrr::map(cur_samples, function(.x) {
        abind::asub(
          x = .x,
          idx = cur_iters_to_keep,
          dims = which(names(dim(.x)) == "iteration"),
          drop = FALSE
        )
      })

    # Place samples in `out` and move pointer to next blank space
    cur_replacement_seq <- iters_added + seq(from = 1, to = sum(cur_iters_to_keep))

    for (varnm in names(out)) {
      out[[varnm]] <-
        assign_iterations_mcarray(
          out[[varnm]],
          cur_thinned_samples[[varnm]],
          at = cur_replacement_seq
        )
    }

    iters_added <- iters_added + sum(cur_iters_to_keep)
  }

  return(out)
}
