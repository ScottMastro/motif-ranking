setwd("C:/Users/scott/Desktop/Report")
source("paper_real_helper.R")
source("helper.R")
library(reshape2)
library(ggplot2)
library("plyr")

txt <- "usf1.2"
setwd(paste("C:/Users/Scott/Desktop/Report/", txt, "/out", sep=""))

#import files
meta <- read.csv("meta.txt", sep="\t")
centrimo <- read.csv("centrimo.txt", sep="\t")
sites <- read.csv("sites.txt", sep="\t")
#neg.sites <- read.csv("neg_sites.txt", sep="\t")

get.obs <- function(sites, uid){
  s <- na.omit(sites[[uid]])
  start <- -length(s)/2 + 0.5
  pos <- seq(start, start+length(s)-0.5)
  return(list(pos, s))
  #list(count[rep(seq(1, nrow(count)), count$sites), "position"], nrow(count))
}

calculate_scott <- function(uid){
  #print(paste("doing", uid, "..."))
  counts = get.obs(sites, uid)
  scotts.delta(counts[[1]], counts[[2]])
}

calculate_centrimo <- function(id){
  row<-centrimo[centrimo$id==id,]
  centrimo.p(row$sites_in_bin, row$total_sites, row$total_width, row$bin_width)
}

calculate_zeta <- function(id){
  row<-centrimo[centrimo$id==id,]
  lower.zeta.score(row$sites_in_bin, row$total_sites, row$total_width, row$bin_width)
}

calculate_credible <- function(id){
  row<-centrimo[centrimo$id==id,]
  lower.credible.region(row$sites_in_bin, row$total_sites, row$total_width, row$bin_width)
}

uids <- as.character(meta$uid)
effect.size <- as.data.frame(sapply(uids, calculate_scott))
colnames(effect.size) <- c("scott.delta")
effect.size$uid <- rownames(effect.size)
result <- merge(effect.size, meta, by="uid")[,c("id", "alt", "scott.delta")]

result$centrimo <- sapply(result$id, calculate_centrimo)
result$zeta <- sapply(result$id, calculate_zeta)
result$credible <- sapply(result$id, calculate_credible)

result <- result[order(-result$scott.delta), ]
result$scott.delta.rank <- 1:nrow(result) 
result <- result[order(result$centrimo), ]
result$centrimo.rank <- 1:nrow(result) 
result <- result[order(-result$zeta), ]
result$zeta.rank <- 1:nrow(result) 
result <- result[order(-result$credible), ]
result$credible.rank <- 1:nrow(result) 


ggplot(result, aes(x=centrimo.rank,y=scott.delta.rank)) +
  geom_point()

ggplot(result, aes(x=scott.delta.rank,y=zeta.rank)) +
  geom_point()

ggplot(result, aes(x=centrimo.rank,y=credible.rank)) +
  geom_point()
