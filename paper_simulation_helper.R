library("ggplot2")
require("grid")
library("ggthemes")
library("truncnorm")

#generate distribution from normal
simulate_normal <- function(n, mean, sd, total.sites){
  return(round(rtruncnorm(n, a=-total.sites/2, b=total.sites/2, mean, sd)))
}

#generate distribution from uniform
simulate_uniform <- function(n, total.sites){
  return(round(runif(n, -total.sites/2, total.sites/2)))
}

#calculate Cliff's delta
cliffs.delta <- function(x, total.sites){
  l = total.sites/2
  n = length(x)
  denom = 1/(n*l)
  
  countif <- function(val){
    sum(abs(x) == val) * (l - 2 * val)
  }
  num <- sapply(0:(total.sites/2), countif)

  return( sum(num/(n*l)) )
}

#get window size
get.window <- function(x, total.sites){
  b.test <- function(w){
    s = sum(abs(x) < w)
    p = binom.test(s, length(x), p = w*2/total.sites, "greater")$p.value
  }
  ps <- sapply(1:(total.sites/2 - 1), b.test)
  
  return( which.min(ps) )
}

#centrimo pvalue
centrimo.p <- function(x, total.sites, w){
  
  s = sum(abs(x) < w)
  p = binom.test(s, length(x), p = w*2/total.sites, "greater")$p.value
  return (p)
}

lower.zeta.score <- function(x, total.sites, w, m=2, delta=0.05){

  theta.hat=sum(abs(x) < w)/length(x)
  theta.null=w*2/total.sites
  #zeta = max(0, abs(theta.hat - theta.null) - sqrt(log(2*m/delta) / (2*total.sites) ))
  zeta = abs(theta.hat - theta.null) - sqrt(log(2*m/delta) / (2*total.sites) )
  
  return(zeta)
}

lower.credible.region <- function(x, total.sites, w, alpha=0.05){
  
  s=sum(abs(x) < w)
  n=length(x)
  theta.null=w*2/total.sites
  
  lower.conf <- qbeta(alpha/n, 0.5 + s, 0.5 + n - s)
  lower.conf<- lower.conf - theta.null
  
  return(lower.conf)
}

simulate <- function(n, mean, sd, total.sites, replicates){
  
  cliffs.delta.null <- c()
  cliffs.delta.alt <- c()
  centrimo.null <- c()
  centrimo.alt <- c()
  zeta.null <- c()
  zeta.alt <- c()
  credible.null <- c()
  credible.alt <- c()
  
  for(i in 1:replicates){
    
    alt = simulate_normal(n, mean, sd, total.sites)
    null = simulate_uniform(n, total.sites)
    
    w.alt <- get.window(alt, total.sites)
    w.null <- get.window(null, total.sites)
    
    centrimo.null <- c(centrimo.null, centrimo.p(null, total.sites, w.null))
    centrimo.alt <- c(centrimo.alt, centrimo.p(alt, total.sites, w.alt))
    
    cliffs.delta.null = c(cliffs.delta.null, cliffs.delta(null, total.sites))
    cliffs.delta.alt = c(cliffs.delta.alt, cliffs.delta(alt, total.sites))
    
    zeta.null <- c(zeta.null, lower.zeta.score(null, total.sites, w.null, m=2))
    zeta.alt <- c(zeta.alt, lower.zeta.score(alt, total.sites, w.alt, m=2))
    
    credible.null <- c(credible.null, lower.credible.region(null, total.sites, w.null))
    credible.alt <- c(credible.alt, lower.credible.region(alt, total.sites, w.alt))
  }
  
  df <- data.frame(c(1:replicates))
  colnames(df)[1] = "index"
  df$centrimo.null <- centrimo.null
  df$centrimo.alt <- centrimo.alt
  df$cliffs.delta.null <- cliffs.delta.null
  df$cliffs.delta.alt <- cliffs.delta.alt
  df$zeta.null <- zeta.null
  df$zeta.alt <- zeta.alt
  df$credible.null <- credible.null
  df$credible.alt <- credible.alt
  
  return(df)
}

