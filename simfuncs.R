library(tidyverse)
library(doParallel)

# functions
getCluster <- function(num_obs, num_cluster, num_sample) {
  cluster_probs <- rep(1/num_cluster, num_cluster)
  return(rmultinom(n = num_sample, size = num_obs, prob = cluster_probs))
}


getNormal_case2 <- function(Mu, Beta, size, sig1, sig2) {
  cluster_k <- data.frame(
    bk = rnorm(1, mean = 0, sd = sig1),
    epik = rnorm(size, mean = 0, sd = sig2),
    group = rep(c(0, 1), times = c(ceiling(size/2), size - ceiling(size/2)))) %>%
    mutate(norm_val = Mu + group*Beta + bk + epik) %>% 
    select(group, norm_val)
  return(cluster_k)
}

getNormal_case1 <- function(Mu, Beta, size, sig1, sig2, group) {
  cluster_k <- data.frame(
    bk = rnorm(1, mean = 0, sd = sig1),
    epik = rnorm(size, mean = 0, sd = sig2),
    group = group) %>%
    mutate(norm_val = Mu + group*Beta + bk + epik) %>% 
    select(group, norm_val)
  return(cluster_k)
}

getSkewed_case2 <- function(Mu, Beta, size, sig1, sig2) {
  cluster_k <- data.frame(
    bk = rnorm(1, mean = 0, sd = sig1),
    epik = rnorm(size, mean = 0, sd = sig2),
    group = rep(c(0, 1), times = c(ceiling(size/2), size - ceiling(size/2)))) %>%
    mutate(skew_val = exp(Mu + bk+ epik) + group*Beta) %>% 
    select(group, skew_val)
  return(cluster_k)
}

getSkewed_case1 <- function(Mu, Beta, size, sig1, sig2, group) {
  cluster_k <- data.frame(
    bk = rnorm(1, mean = 0, sd = sig1),
    epik = rnorm(size, mean = 0, sd = sig2),
    group = group) %>% 
    mutate(skew_val = exp(Mu + bk+ epik) + group*Beta) %>% 
    select(group, skew_val)
  return(cluster_k)
}

iterClusterNorm_case2 <- function(Mu, Beta){
 df <- foreach (i = 1:20, .combine = rbind) %dopar% {
    getNormal_case2(Mu, Beta, cltsizes[i], sig1, sig2) %>% 
      group_by(group) %>% 
      mutate(mean = mean(norm_val)) %>% 
      mutate(clst = i)
  }
  return(df)
}

iterClusterNorm_case1 <- function(Mu, Beta){
  df1 <- foreach (i = 1:10, .combine = rbind) %dopar% {
    getNormal_case1(Mu, Beta, cltsizes[i], sig1, sig2, 0) %>%
      mutate(mean = mean(norm_val), clst = i)
  }
  df2 <- foreach (i = 11:20, .combine = rbind) %dopar% {
    getNormal_case1(Mu, Beta, cltsizes[i], sig1, sig2, 1) %>%
      mutate(mean = mean(norm_val), clst = i)
  }
  return(rbind(df1, df2))
}

iterClusterSkew_case2 <- function(Mu, Beta){
  df <- foreach (i = 1:20, .combine = rbind) %dopar% {
    getSkewed_case2(Mu, Beta, cltsizes[i], sig1, sig2) %>% 
      group_by(group) %>% 
      mutate(mean = mean(skew_val)) %>% 
      mutate(clst = i)
  }
  return(df)
}

iterClusterSkew_case1 <- function(Mu, Beta){
  df1 <- foreach (i = 1:10, .combine = rbind) %dopar% {
    getSkewed_case1(Mu, Beta, cltsizes[i], sig1, sig2, 0) %>%
      mutate(mean = mean(skew_val), clst = i)
  }
  df2 <- foreach (i = 11:20, .combine = rbind) %dopar% {
    getSkewed_case1(Mu, Beta, cltsizes[i], sig1, sig2, 1) %>%
      mutate(mean = mean(skew_val),  clst = i)
  }
  return(rbind(df1, df2))
}
