source("paper_simulation_helper.R")
#source("helper.R")
library("reshape2")
library("ggplot2")
library("plyr")

run <- function(n, mean, sd, total.sites, replicates){

  max.i = max(length(mean), length(sd))
  if(length(mean) == 1)
    mean = rep(mean, max.i)
  if(length(sd) == 1)
    sd = rep(sd, max.i)
  
  datas <- vector("list", length(n))

  for(j in 1:length(n)){
    size = n[j]
    print(paste("----",j, "-----"))
    
    centrimo.error <- c()
    cliffs.delta.error <- c()
    zeta.error <- c()
    credible.error <- c()
    
    for(i in 1:max.i){
      print(i)
      df <- simulate(size, mean[i], sd[i], total.sites, replicates)
      
      cte <- (sum(df$centrimo.null < df$centrimo.alt) + 0.5 * sum(df$centrimo.null == df$centrimo.alt) )/ nrow(df)
      centrimo.error <- c(centrimo.error, cte)
      
      sde <- (sum(df$cliffs.delta.null > df$cliffs.delta.alt) + 0.5 * sum(df$cliffs.delta.null == df$cliffs.delta.alt) )/ nrow(df)
      cliffs.delta.error <- c(cliffs.delta.error, sde)
      
      ze <- (sum(df$zeta.null > df$zeta.alt) + 0.5 * sum(df$zeta.null == df$zeta.alt) )/ nrow(df)
      zeta.error <- c(zeta.error, ze)
      
      cre <- (sum(df$credible.null > df$credible.alt) + 0.5 * sum(df$credible.null == df$credible.alt) )/ nrow(df)
      credible.error <- c(credible.error, cre)
      
    }
    
    data <- data.frame(sd)
    data$mean <- mean
    data$n <- size
    data$centrimo.error <- centrimo.error
    data$cliffs.delta.error <- cliffs.delta.error
    data$zeta.error <- zeta.error
    data$credible.error <- credible.error
    data <- melt(data, id=c("sd", "n", "mean")) 
    datas[[j]] <- data
  }
  return(do.call("rbind", datas))
}

#meanplot
n = c(25, 100, 500, 2000)
mean = seq(0, 200, 10)
sd = 100
total.sites = 500
replicates = 2000

#sdplot
n = c(25, 100, 500, 2000)
mean = 0
sd = seq(0, 1000, 50)
total.sites = 500
replicates = 2000
write.csv(data, "sd_change2.txt")

#run
data <- run(n, mean, sd, total.sites, replicates)

p <- ggplot(data, aes(sd, value, group=variable, colour=variable))
p <- p + geom_line(size=1.5, alpha=0.75) + geom_point(size=2)
p <- p + geom_hline(yintercept = 0.5, colour="red", size=1, alpha=0.5)
p <- p + facet_wrap(~n)
p <- p + annotate("text", x = 0, y = 0.5, hjust =0,  vjust=0, 
                  label = 'bold("RANDOM GUESS")', parse=T)
p <- p + scale_colour_manual(values = c("#0a1128", "#E8679E", "#6EA248", "#f7b267"),
                             labels=c("Centrimo", "Cliff's Delta", "Lower Zeta", "Lower Credible"))
p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.1))
p <- p + labs(title="", colour="Method",
              x="SD", y="Error Rate")
#p <- scottTheme1(p)
p

write.csv(data, "sd_change2.txt")


#print(paste("Centrimo error:", centrimo.error))
#print(paste("Cliff's delta error:", cliffs.delta.error))
#print(paste("Zeta error:", zeta.error))
#print(paste("Credible region error:", credible.error))

add_layer <- function(p, m, s){
  p <- p + geom_area(stat="function", fun=dnorm, fill="red", alpha=0.05, args=list(mean=m, sd=s))
  return(p)
}


p1 <- ggplot(data = data.frame(x = c(-total.sites/2, total.sites/2)), aes(x))
for(i in 1:length(means)){
  p1 <- add_layer(p1, means[i], sds[i])
}
  p1 <- p1 + scale_y_continuous(breaks = NULL)
  p1 <- p1 + geom_vline(xintercept = 0, colour="red", size=1, alpha=0.5)
p1


means = seq(0, 150, 10)
sds = rep(100, 10)

distrib <- function(mean, sd, sites){
  plots=length(mean)
  nper=1e5
  dd <- data.frame(
    predicted = rnorm(plots*nper, mean = 2, sd = 2),
    state = rep(1:plots, each = nper)
  ) 
  
  grid <- with(dd, seq(min(predicted), max(predicted), length = 100))
  normaldens <- ddply(dd, "state", function(df) {
    data.frame( 
      predicted = grid,
      density = dnorm(grid, mean(df$predicted), sd(df$predicted))
    )
  })
  
  ggplot(dd, aes(predicted))  + 
    #geom_density() + 
    geom_line(aes(y = density), data = normaldens, colour = "red") +
    geom_vline(xintercept = 0, colour="black", size=1, alpha=0.5)+
    facet_wrap(~ state, nrow=1) 
}
distrib(means, sds, 500)

