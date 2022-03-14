

logit <- function(x){
 log(x / (1-x))
}

logit_inv <- function(x){
 exp(x) / (1 + exp(x))
}
