#' Estimate the mediascores model
#' 
#' @examples
#' sim_data <- simulate_data(200, 500)
#' posterior <- mediascores(sim_data$Y, sim_data$group, sim_data$anchors, 
#'                           algorithm = "meanfield")
mediascores <- function(Y, group = NULL, anchors,
                        algorithm = c("sampling", "meanfield"),
                        adapt_delta = 0.8,
                        warmup = 1000, iter = 2000,
                        chains = 4, refresh = 50) {

  algorithm <- match.arg(algorithm)
  n_row <- nrow(Y)
  n_col <- ncol(Y)
  n_groups <- 1
  if (!is.null(group)) {
    n_groups <- length(unique(group))
  } else {
    group <- rep(1, n_row)
    n_groups <- 1
  }

  # N = number of users; M = number of domains; G = number of groups;
  # group = group each user belongs to; Y = user-domain count matrix
  model_data <- list(N = n_row, M = n_col, G = n_groups,
                     group = group, Y = Y, anchors = anchors)

  if (algorithm == "sampling") {
    posterior <- rstan::sampling(stanmodels$mediascores, 
                                 data = model_data,
                                 warmup = warmup, iter = iter,
                                 refresh = 50, chains = chains,
                                 open_progress = TRUE)
  } else if (algorithm == "meanfield") {
    posterior <- rstan::vb(stanmodels$mediascores, data = model_data,
                           output_samples = 5000, tol_rel_obj = 0.001,
                           algorithm = "meanfield")
  }

  return(posterior)

}
