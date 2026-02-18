# Extra diagnostics and plotting helpers for fit objects

.chlaa_fit_density_vector <- function(fit) {
  fit <- chlaa_as_fit(fit)
  d <- fit$density
  if (is.null(d)) stop("fit does not contain density values", call. = FALSE)

  if (is.vector(d)) return(as.numeric(d))
  if (is.matrix(d)) return(as.numeric(d))

  stop("Unsupported density structure in fit$density", call. = FALSE)
}

#' Extract Likelihood (Log-Density) Trace From A Fit
#'
#' @param fit Fit object returned by `chlaa_fit_pmcmc()`.
#' @param burnin Burn-in proportion in (0,1) or iteration count.
#' @param thin Thinning interval.
#'
#' @return A data.frame with columns `iteration` and `log_density`.
#' @export
chlaa_fit_density_trace <- function(fit, burnin = 0, thin = 1) {
  dens <- .chlaa_fit_density_vector(fit)
  idx <- chlaa_fit_select_iterations(matrix(seq_along(dens), ncol = 1), burnin = burnin, thin = thin)[, 1]

  data.frame(
    iteration = idx,
    log_density = dens[idx],
    stringsAsFactors = FALSE
  )
}

#' Plot The Likelihood Trace
#'
#' @param fit Fit object returned by `chlaa_fit_pmcmc()`.
#' @param burnin Burn-in proportion in (0,1) or iteration count.
#' @param thin Thinning interval.
#'
#' @return A ggplot object.
#' @export
chlaa_plot_likelihood_trace <- function(fit, burnin = 0, thin = 1) {
  .require_suggested("ggplot2")
  df <- chlaa_fit_density_trace(fit, burnin = burnin, thin = thin)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$iteration, y = .data$log_density)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Iteration",
      y = "Log posterior density",
      title = "Likelihood / posterior density trace"
    ) +
    ggplot2::theme_minimal()
}

#' Plot Distribution Of Likelihood Values
#'
#' @param fit Fit object returned by `chlaa_fit_pmcmc()`.
#' @param burnin Burn-in proportion in (0,1) or iteration count.
#' @param thin Thinning interval.
#'
#' @return A ggplot object.
#' @export
chlaa_plot_likelihood_density <- function(fit, burnin = 0, thin = 1) {
  .require_suggested("ggplot2")
  df <- chlaa_fit_density_trace(fit, burnin = burnin, thin = thin)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$log_density)) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.5) +
    ggplot2::geom_density() +
    ggplot2::labs(
      x = "Log posterior density",
      y = "Count / density",
      title = "Distribution of sampled likelihood values"
    ) +
    ggplot2::theme_minimal()
}

#' Plot Pairwise Posterior Parameter Densities
#'
#' @param fit Fit object returned by `chlaa_fit_pmcmc()`.
#' @param parameters Optional parameter subset. Defaults to the first five.
#' @param burnin Burn-in proportion in (0,1) or iteration count.
#' @param thin Thinning interval.
#' @param max_points Maximum sampled posterior rows to plot.
#'
#' @return A ggplot object.
#' @export
chlaa_plot_parameter_pairs <- function(fit,
                                       parameters = NULL,
                                       burnin = 0.5,
                                       thin = 1,
                                       max_points = 2000) {
  .require_suggested("ggplot2")

  draws <- chlaa_fit_select_iterations(chlaa_fit_draws(chlaa_as_fit(fit)), burnin = burnin, thin = thin)

  if (is.null(parameters)) {
    parameters <- colnames(draws)[seq_len(min(5, ncol(draws)))]
  } else {
    parameters <- intersect(parameters, colnames(draws))
  }
  if (length(parameters) < 2) {
    stop("Need at least two parameters for pairwise plotting", call. = FALSE)
  }

  d <- as.data.frame(draws[, parameters, drop = FALSE], stringsAsFactors = FALSE)
  if (nrow(d) > max_points) {
    set.seed(1)
    d <- d[sample.int(nrow(d), max_points), , drop = FALSE]
  }

  pairs <- utils::combn(parameters, 2, simplify = FALSE)
  long <- do.call(rbind, lapply(pairs, function(pp) {
    data.frame(
      param_x = pp[1],
      param_y = pp[2],
      x = d[[pp[1]]],
      y = d[[pp[2]]],
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(alpha = 0.2, size = 0.5) +
    ggplot2::geom_density_2d(alpha = 0.6) +
    ggplot2::facet_grid(.data$param_y ~ .data$param_x, scales = "free") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Pairwise posterior parameter densities"
    ) +
    ggplot2::theme_minimal()
}

#' Plot Parameters Against Sampled Likelihood
#'
#' @param fit Fit object returned by `chlaa_fit_pmcmc()`.
#' @param parameters Optional parameter subset. Defaults to the first six.
#' @param burnin Burn-in proportion in (0,1) or iteration count.
#' @param thin Thinning interval.
#' @param max_points Maximum sampled posterior rows to plot.
#'
#' @return A ggplot object.
#' @export
chlaa_plot_parameter_vs_likelihood <- function(fit,
                                               parameters = NULL,
                                               burnin = 0.5,
                                               thin = 1,
                                               max_points = 2000) {
  .require_suggested("ggplot2")

  fit <- chlaa_as_fit(fit)
  draws_all <- chlaa_fit_draws(fit)
  dens_all <- .chlaa_fit_density_vector(fit)
  if (length(dens_all) != nrow(draws_all)) {
    stop("length(fit$density) does not match number of posterior draws", call. = FALSE)
  }

  idx <- chlaa_fit_select_iterations(matrix(seq_len(nrow(draws_all)), ncol = 1), burnin = burnin, thin = thin)[, 1]
  draws <- draws_all[idx, , drop = FALSE]
  dens <- dens_all[idx]

  if (is.null(parameters)) {
    parameters <- colnames(draws)[seq_len(min(6, ncol(draws)))]
  } else {
    parameters <- intersect(parameters, colnames(draws))
  }
  if (length(parameters) < 1) stop("No requested parameters found in draws", call. = FALSE)

  d <- as.data.frame(draws[, parameters, drop = FALSE], stringsAsFactors = FALSE)
  d$log_density <- dens
  if (nrow(d) > max_points) {
    set.seed(1)
    d <- d[sample.int(nrow(d), max_points), , drop = FALSE]
  }

  long <- do.call(rbind, lapply(parameters, function(p) {
    data.frame(parameter = p, value = d[[p]], log_density = d$log_density, stringsAsFactors = FALSE)
  }))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$value, y = .data$log_density)) +
    ggplot2::geom_point(alpha = 0.2, size = 0.4) +
    ggplot2::geom_smooth(se = FALSE, linewidth = 0.6) +
    ggplot2::facet_wrap(~ .data$parameter, scales = "free_x") +
    ggplot2::labs(
      x = "Parameter value",
      y = "Log posterior density",
      title = "Parameter-likelihood relationship"
    ) +
    ggplot2::theme_minimal()
}
