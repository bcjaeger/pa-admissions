

logit <- function(x) log(x / (1-x))

logit_inv <- function(x) exp(x) / (1 + exp(x))

n_obs <- 1000

# assumption:
# baseline chance of passing qualification exam
beta_intercept <- logit(0.90)

linpred_eta <- seq(-6, 6, length.out = 100)

library(mvtnorm)

# initialize correlation matrix
mat_corr <- matrix(data = 0, nrow = 3, ncol = 3)

# set up correlation matrix
diag(mat_corr) <- 1
colnames(mat_corr) <- c('gre','gpa','pass_linpred')
rownames(mat_corr) <- colnames(mat_corr)

mat_corr['gre', 'gpa'] <- 0.20
mat_corr['gpa', 'pass_linpred'] <- 0.20
mat_corr['gre', 'pass_linpred'] <- 0.20

mat_corr[lower.tri(mat_corr)] <- mat_corr[upper.tri(mat_corr)]

mat_corr

mat_cov <- MBESS::cor2cov(cor.mat = mat_corr, sd = c(50, 0.5, 0.3))

mat_application <- rmvnorm(n = n_obs,
                           mean = c(300, 3, beta_intercept),
                           sigma = mat_cov)

colnames(mat_application) <- colnames(mat_corr)

library(tidyverse)

data_application <- as_tibble(mat_application) |>
 mutate(pass_prob = logit_inv(pass_linpred))

gre_gt_75pct <- data_application |>
 filter(gre > as.numeric(quantile(gre, 0.75))) |>
 summarize(across(everything(), mean))

gre_all <- data_application |>
 summarize(across(everything(), mean))




