#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sims_combined
sim_summarize <- function(sims_combined) {

 sims_combined |>
  select(-run_seed) |>
  group_by(strategy, corr_gre_pass, base_prob) |>
  summarize(across(where(is.numeric), median))

}
