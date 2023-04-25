#calculate cliffs delta
cliffs.delta <- function(bins, counts){
  l = length(bins)/2
  n = sum(counts)
  denom = 1/(n*l)

  num <- counts*(l - 2*(abs(bins)))
  return( sum(num)/(n*l) )
}

#centrimo pvalue
centrimo.p <- function(s, n, total, window){
  
  p = binom.test(s, n, p = window/total, "greater")$p.value
  return (p)
}

lower.zeta.score <- function(s, n, total, window, m=2, delta=0.05){

  theta.hat=s/n
  theta.null=window/total
  zeta = max(0, abs(theta.hat - theta.null) - sqrt(log(2*m/delta) / total/2 ))
  
  return(zeta)
}

lower.credible.region <- function(s, n, total, window, alpha=0.05){

  theta.null=window/total
  
  lower.conf <- qbeta(alpha/n, 0.5 + s, 0.5 + n - s)
  lower.conf<-  lower.conf - theta.null
  
  return(lower.conf)
}
