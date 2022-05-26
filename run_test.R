library(parallel)
library(tidyverse)
library(rstatix)

setwd("~/Documents/Research/DataSimulation")
#case1df <- read.csv("data/case1_icc0.7_10000.csv")
# case1df <- read.csv("data/case1_icc0.4_1000.csv")
#case2df <- read.csv("data/case2_icc0.7_10000.csv")
case1df <- read.csv("ICC-0.4/case1_icc0.4_1000.csv")


# define a function that output only the first value in a vector
getFirst <- function(x){
  x[1]
}


# loop over all models
modnum <- 1:12


# case 1 p-values from ind t-test
lsproprej_case1 <- mclapply(modnum, function(i){
  
  mod <- paste0(paste("model", i, sep = "_"), "a")
  grp <- paste0(paste("group_model", i, sep = "_"), "a")
  clst <- paste0(paste("clst_model", i, sep = "_"), "a")
  
  tsform <- as.formula(paste(mod, "~", grp))
  
  case1dftest <- case1df %>% 
    mutate_at({{clst}}, factor) %>% 
    select({{mod}}, {{grp}}, iter)
  
  case1indt_2side <- case1dftest %>% 
    group_by(iter) %>% 
    t_test(tsform) %>% 
    pull(p)
  
  case1indt_1side <- case1dftest %>% 
    group_by(iter) %>% 
    t_test(tsform, alternative = "less") %>% 
    pull(p)
  
  case1wilcox_2side <- case1dftest %>% 
    group_by(iter) %>% 
    wilcox_test(tsform) %>% 
    pull(p)
  
  case1wilcox_1side <- case1dftest %>% 
    group_by(iter) %>% 
    wilcox_test(tsform, alternative = "less") %>% 
    pull(p)
  
  pvals <- cbind(case1indt_2side, case1indt_1side, 
                 case1wilcox_2side, case1wilcox_1side)
  tfres <- apply(pvals, 2, function(x){x < 0.05})
  proprej <- colMeans(tfres)
  
  proprej
}, mc.cores = 5)

indtest_case1 <- t(do.call("cbind", lapply(lsproprej_case1, as.data.frame)))
# write.csv(indtest_case1, "output/case1_10000_twil.csv", row.names = F)



# case 2 p-values from ind t-test
lsproprej_case2 <- mclapply(modnum, function(i){
  
  mod <- paste0(paste("model", i, sep = "_"), "b")
  grp <- paste0(paste("group_model", i, sep = "_"), "b")
  clst <- paste0(paste("clst_model", i, sep = "_"), "b")

  tsform <- as.formula(paste(mod, "~", grp))

  
  case2dftest <- case2df %>% 
    mutate_at({{clst}}, factor) %>% 
    select({{mod}}, {{grp}}, iter)
  
  case2indt_2side <- case2dftest %>% 
    group_by(iter) %>% 
    t_test(tsform) %>% 
    pull(p)
  
  case2indt_1side <- case2dftest %>% 
    group_by(iter) %>% 
    t_test(tsform, alternative = "less") %>% 
    pull(p)
  
  case2wilcox_2side <- case2dftest %>% 
    group_by(iter) %>% 
    wilcox_test(tsform) %>% 
    pull(p)
  
  case2wilcox_1side <- case2dftest %>% 
    group_by(iter) %>% 
    wilcox_test(tsform, alternative = "less") %>% 
    pull(p)
  
  pvals <- cbind(case2indt_2side, case2indt_1side, 
                 case2wilcox_2side, case2wilcox_1side)
  tfres <- apply(pvals, 2, function(x){x < 0.05})
  proprej <- colMeans(tfres)
  
  proprej

}, mc.cores = 5)

indtest_case2 <- t(do.call("cbind", lapply(lspval_case2, as.data.frame)))
# write.csv(indtest_case2, "output/case2_10000_twil.csv", row.names = F)



# case 1 rejection rates from paired tests
lsproprej_case1_paired <- mclapply(modnum, function(i){
  
  mod <- paste0(paste("mean_model", i, sep = "_"), "a")
  .mod <- sym(paste0(paste("mean_model", i, sep = "_"), "a"))
  grp <- paste0(paste("group_model", i, sep = "_"), "a")
  clst <- paste0(paste("clst_model", i, sep = "_"), "a")
  
  tsform <- as.formula(paste("clstmean", "~", grp))
  
## original code where distinct values were taken, couldn't deal with 
## certain cases where there are replicated cluster means.
  # case1dftest <- case1df %>% 
  #   select({{mod}}, {{grp}}, {{clst}}, iter) %>% 
  #   mutate_at(c({{clst}}, {{grp}}, "iter"), as.factor) %>% 
  #   group_by_at(vars(iter, {{clst}}, {{grp}})) %>% 
  #   distinct_at(vars({{mod}})) %>% 
  #   group_by(iter)

## Modify by taking the 1st value
  case1dftest <- case1df %>% 
    select({{mod}}, {{grp}}, {{clst}}, iter) %>% 
    mutate_at(c({{clst}}, {{grp}}, "iter"), as.factor) %>% 
    group_by_at(vars(iter, {{clst}}, {{grp}})) %>% 
    summarise(clstmean = getFirst(!!.mod), .groups = "drop") %>% 
    group_by(iter)
  
  case1pdt_2side <- case1dftest %>% 
    pairwise_t_test(tsform, paired = T) %>% 
    pull(p)
  
  case1pdt_1side <- case1dftest %>% 
    pairwise_t_test(tsform, paired = T, alternative = "less") %>% 
    pull(p)
  
  case1pdwilcox_2side <- case1dftest %>% 
    pairwise_wilcox_test(tsform, paired = T) %>% 
    pull(p)
  
  case1pdwilcox_1side <- case1dftest %>% 
    pairwise_wilcox_test(tsform, paired = T, alternative = "less") %>% 
    pull(p)
  
  pvals <- cbind(case1pdt_2side, case1pdt_1side, 
                 case1pdwilcox_2side, case1pdwilcox_1side)
  tfres <- apply(pvals, 2, function(x){x < 0.05})
  proprej <- colMeans(tfres)
  
  proprej
  
}, mc.cores = 5)

pdtest_case1 <- t(do.call("cbind", lapply(lsproprej_case1_paired, as.data.frame)))
# write.csv(pdtest_case1, "output/case1_10000_paired.csv", row.names = F)


# case 2 rejection rates from paired tests
lsproprej_case2_paired <- mclapply(modnum, function(i){
  
  mod <- paste0(paste("mean_model", i, sep = "_"), "b")
  .mod <- sym(paste0(paste("mean_model", i, sep = "_"), "b"))
  grp <- paste0(paste("group_model", i, sep = "_"), "b")
  clst <- paste0(paste("clst_model", i, sep = "_"), "b")
  
  tsform <- as.formula(paste("clstmean", "~", grp))

## original code where distinct values were taken, couldn't deal with 
## certain cases where there are replicated cluster means.
  # case2dftest <- case2df %>% 
  #   select({{mod}}, {{grp}}, {{clst}}, iter) %>% 
  #   mutate_at(c({{clst}}, {{grp}}, "iter"), as.factor) %>% 
  #   group_by_at(vars(iter, {{clst}}, {{grp}})) %>% 
  #   distinct_at(vars({{mod}})) %>% 
  #   group_by(iter)

  case2dftest <- case2df %>% 
    select({{mod}}, {{grp}}, {{clst}}, iter) %>% 
    mutate_at(c({{clst}}, {{grp}}, "iter"), as.factor) %>% 
    group_by_at(vars(iter, {{clst}}, {{grp}})) %>% 
    summarise(clstmean = getFirst(!!.mod), .groups = "drop") %>% 
    group_by(iter)
  
  case2pdt_2side <- case2dftest %>% 
    pairwise_t_test(tsform, paired = T) %>% 
    pull(p)
  
  case2pdt_1side <- case2dftest %>% 
    pairwise_t_test(tsform, paired = T, alternative = "less") %>% 
    pull(p)
  
  case2pdwilcox_2side <- case2dftest %>% 
    pairwise_wilcox_test(tsform, paired = T) %>% 
    pull(p)
  
  case2pdwilcox_1side <- case2dftest %>% 
    pairwise_wilcox_test(tsform, paired = T, alternative = "less") %>% 
    pull(p)
  
  pvals <- cbind(case2pdt_2side, case2pdt_1side, 
                 case2pdwilcox_2side, case2pdwilcox_1side)
  tfres <- apply(pvals, 2, function(x){x < 0.05})
  proprej <- colMeans(tfres)
  
  proprej
  
}, mc.cores = 5)

pdtest_case2 <- t(do.call("cbind", lapply(lsproprej_case2_paired, as.data.frame)))
# write.csv(pdtest_case2, "output/case2_10000_paired.csv", row.names = F)