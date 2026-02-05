# S3 wrapper utilities for cholera_fit objects

#' Coerce an object into class `cholera_fit`
#'
#' @param x A fit object (typically from `cholera_fit_pmcmc()`).
#'
#' @return The same object with class `cholera_fit` prepended.
#' @export
cholera_as_fit <- function(x) {
  if (inherits(x, "cholera_fit")) return(x)
  class(x) <- unique(c("cholera_fit", class(x)))
  x
}

#' Extract fit metadata (packer, prior, start parameters, data)
#'
#' @param fit A `cholera_fit` object.
#'
#' @return A named list.
#' @export
cholera_fit_metadata <- function(fit) {
  fit <- cholera_as_fit(fit)
  list(
    packer = attr(fit, "packer", exact = TRUE),
    prior = attr(fit, "prior", exact = TRUE),
    start_pars = attr(fit, "start_pars", exact = TRUE),
    data = attr(fit, "data", exact = TRUE)
  )
}

#' Posterior summary table for a fit
#'
#' @param fit A `cholera_fit` object.
#' @param burnin Burn-in proportion (0-1) or integer iterations.
#' @param thin Thinning interval.
#' @param probs Quantiles to compute.
#'
#' @return A data.frame with columns: parameter, mean, sd, and quantiles.
#' @export
cholera_posterior_summary <- function(fit, burnin = 0.5, thin = 1, probs = c(0.025, 0.5, 0.975)) {
  fit <- cholera_as_fit(fit)
  draws <- cholera_fit_select_iterations(cholera_fit_draws(fit), burnin = burnin, thin = thin)

  q <- t(apply(draws, 2, stats::quantile, probs = probs, names = TRUE))
  mu <- colMeans(draws)
  sd <- apply(draws, 2, stats::sd)

  out <- data.frame(
    parameter = colnames(draws),
    mean = as.numeric(mu),
    sd = as.numeric(sd),
    stringsAsFactors = FALSE
  )

  qdf <- as.data.frame(q, stringsAsFactors = FALSE)
  colnames(qdf) <- paste0("q", gsub("\\.", "p", colnames(qdf)))
  out <- cbind(out, qdf)

  if (requireNamespace("tibble", quietly = TRUE)) out <- tibble::as_tibble(out)
  out
}

#' @export
print.cholera_fit <- function(x, ...) {
  x <- cholera_as_fit(x)

  cat("<cholera_fit>\n")
  md <- cholera_fit_metadata(x)

  n_iter <- NA_integer_
  n_par <- NA_integer_
  dr <- try(cholera_fit_draws(x), silent = TRUE)
  if (!inherits(dr, "try-error")) {
    n_iter <- nrow(dr)
    n_par <- ncol(dr)
  }
  cat("Posterior draws: ", n_iter, " iterations; ", n_par, " parameters\n", sep = "")

  if (is.data.frame(md$data) && all(c("time", "cases") %in% names(md$data))) {
    cat("Data: ", nrow(md$data), " observations; time range [",
        min(md$data$time), ", ", max(md$data$time), "]\n", sep = "")
  } else {
    cat("Data: <not attached>\n")
  }

  invisible(x)
}

#' @export
summary.cholera_fit <- function(object, burnin = 0.5, thin = 1, probs = c(0.025, 0.5, 0.975), ...) {
  cholera_posterior_summary(object, burnin = burnin, thin = thin, probs = probs)
}
