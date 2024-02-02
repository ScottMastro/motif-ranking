source("helper.R")

library(dplyr)
library(reshape2)
library(ggplot2)

options('stringsAsFactors'=FALSE)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Usage: Rscript calculate_rankings.R <centrimo.txt or centrimo.tsv> <meta.txt> <sites.txt> [output, default=ranking.txt]")
}

#parse files
centrimo <- read.csv(args[1], sep="\t")

# rename needed columns in new centrimo.tsv format, back to prior centrimo.txt
centrimo <- rename_with(centrimo, recode, motif_id="id", motif_alt_id="alt")

meta <- read.csv(args[2], sep="\t")
sites <- read.csv(args[3], sep="\t")

outfile <- "ranking.txt"
if (length(args) > 3) {
   outfile <- args[4]
}

get.obs <- function(sites, uid){
  s <- na.omit(sites[[uid]])
  start <- -length(s)/2 + 0.5
  pos <- seq(start, start+length(s)-0.5)

  return(list(pos, s))
}

calculate_cliffs <- function(uid){
  counts <- get.obs(sites, uid)

  cliffs.delta(counts[[1]], counts[[2]])
}

calculate_centrimo <- function(id){
  row <- centrimo[centrimo$id==id,]

  centrimo.p(row$sites_in_bin, row$total_sites, row$total_width, row$bin_width)
}

calculate_zeta <- function(id){
  row <- centrimo[centrimo$id==id,]

  lower.zeta.score(row$sites_in_bin, row$total_sites, row$total_width, row$bin_width)
}

calculate_credible <- function(id){
  row <- centrimo[centrimo$id==id,]

  lower.credible.region(row$sites_in_bin, row$total_sites, row$total_width, row$bin_width)
}

uids <- as.character(meta$uid)

effect.size <- as.data.frame(sapply(uids, calculate_cliffs))
colnames(effect.size) <- c("cliffs.delta")
effect.size$uid <- rownames(effect.size)

result <- merge(effect.size, meta, by="uid")[,c("id", "alt", "cliffs.delta")]

result$centrimo <- sapply(result$id, calculate_centrimo)
result$zeta <- sapply(result$id, calculate_zeta)
result$credible <- sapply(result$id, calculate_credible)

result <- result[order(-result$cliffs.delta), ]
result$cliffs.delta.rank <- 1:nrow(result) 

result <- result[order(result$centrimo), ]
result$centrimo.rank <- 1:nrow(result) 

result <- result[order(-result$zeta), ]
result$zeta.rank <- 1:nrow(result) 

result <- result[order(-result$credible), ]
result$credible.rank <- 1:nrow(result) 

write.table(result, outfile, sep="\t", row.names=FALSE, quote=FALSE)
