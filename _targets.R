## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

inputs_sim <- expand_grid(n_apply = 5000,
                          run_seed = 1:100,
                          corr_gre_pass = c(0, 0.20),
                          corr_gpa_pass = 0.20,
                          corr_gre_gpa = 0.20,
                          base_prob = c(0.7, 0.85))

tar_plan(

 sims <- tar_map(
  values = inputs_sim,
  tar_target(sim, sim_run(n_apply = n_apply,
                          run_seed = run_seed,
                          corr_gre_pass = corr_gre_pass,
                          corr_gpa_pass = corr_gpa_pass,
                          corr_gre_gpa = corr_gre_gpa,
                          base_prob = base_prob))

 ),

 tar_combine(
  sims_combined,
  sims[[1]],
  command = bind_rows(!!!.x)
 ),

 tar_target(
  sims_smry,
  sim_summarize(sims_combined)
 ),

 tar_render(sims_report, "docs/sims_report.Rmd")


)
