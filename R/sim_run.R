#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#'
#' @param run_seed
#' @param beta_intercept baseline chance of passing qualification exam
#' @param corr_gre_pass correlation b/t GRE score and passing exam
#'
sim_run <- function(n_apply,
                    corr_gre_pass,
                    corr_gpa_pass,
                    corr_gre_gpa,
                    run_seed,
                    base_prob = 0.80) {

 # initialize correlation matrix
 mat_corr <- matrix(data = 0, nrow = 3, ncol = 3)

 # set up correlation matrix
 diag(mat_corr) <- 1
 colnames(mat_corr) <- c('gre','gpa','pass_linpred')
 rownames(mat_corr) <- colnames(mat_corr)

 mat_corr['gre', 'gpa']          <- corr_gre_gpa
 mat_corr['gpa', 'pass_linpred'] <- corr_gpa_pass
 mat_corr['gre', 'pass_linpred'] <- corr_gre_pass

 mat_corr[lower.tri(mat_corr)] <- mat_corr[upper.tri(mat_corr)]

 mat_cov <- MBESS::cor2cov(cor.mat = mat_corr, sd = c(50, 0.5, 2))

 set.seed(run_seed)

 mat_application <- rmvnorm(n = n_apply,
                            mean = c(300, 3, logit(base_prob)),
                            sigma = mat_cov)

 colnames(mat_application) <- colnames(mat_corr)

 data_application <- as_tibble(mat_application) |>
  mutate(pass_prob = logit_inv(pass_linpred))

 gre_gt_75pct <- data_application |>
  filter(gre > as.numeric(quantile(gre, 0.75))) |>
  summarize(across(everything(), median),
            n_admit = n(),
            n_pass = sum(rbinom(n=n(), size=1, prob=pass_prob)))

 gpa_gt_3 <- data_application |>
  filter(gpa > 3.0) |>
  summarize(across(everything(), median),
            n_admit = n(),
            n_pass = sum(rbinom(n=n(), size=1, prob=pass_prob)))

 all <- data_application |>
  summarize(across(everything(), median),
            n_admit = n(),
            n_pass = sum(rbinom(n=n(), size=1, prob=pass_prob)))

 bind_rows(all = all,
           gpa_gt_3 = gpa_gt_3,
           gre_gt_75pct = gre_gt_75pct,
           .id = 'strategy') |>
  mutate(n_apply = n_apply,
         corr_gre_pass = corr_gre_pass,
         corr_gpa_pass = corr_gpa_pass,
         corr_gre_gpa = corr_gre_gpa,
         run_seed = run_seed,
         base_prob = base_prob,
         .before = pass_prob) |>
  select(-pass_linpred)

}
