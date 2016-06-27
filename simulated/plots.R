# plots.R - DESC
# plots.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(ggplot2)
library(gridExtra)
library(FLCore)

load('out/0.6/simsDET201306071347.RData')

# sims 
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(x$catch)))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(rec(x$stock))))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(ssb(x$stock))))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(fbar(x$stock))))

val <- do.call('rbind', val)

#
pdf(file="DET.pdf")
print(ggplot(val) + geom_line(aes(x=year, y=data)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y'))
dev.off()

# inputE
val <- lapply(inputE02, function(x) cbind(x$val, as.data.frame(x$catch)))





# SELECT one run per LH, first year

# SP: 1
sp <- as.data.frame(FLQuants(
  n=sims[[1]]$stock@stock.n[,1],
  wt=sims[[1]]$stock@stock.wt[,1],
  mat=sims[[1]]$stock@mat[,1],
  sel=sims[[1]]$stock@harvest[,1] / max(sims[[1]]$stock@harvest[,1]),
  ))
sp$facet <- sp$qname
levels(sp$facet) <- c("Abundance", "Weight", "Maturity, Selectivity", "Maturity, Selectivity")

# LP: 97
lp <- as.data.frame(FLQuants(
  n=sims[[97]]$stock@stock.n[,1],
  wt=sims[[97]]$stock@stock.wt[,1],
  mat=sims[[97]]$stock@mat[,1],
  sel=sims[[97]]$stock@harvest[,1] / max(sims[[97]]$stock@harvest[,1]),
  ))
lp$facet <- lp$qname
levels(lp$facet) <- c("Abundance", "Weight", "Maturity, Selectivity", "Maturity, Selectivity")


# DE: 49
de <- as.data.frame(FLQuants(
  n=sims[[49]]$stock@stock.n[,1],
  wt=sims[[49]]$stock@stock.wt[,1],
  mat=sims[[49]]$stock@mat[,1],
  sel=sims[[49]]$stock@harvest[,1] / max(sims[[49]]$stock@harvest[,1]),
  ))
de$facet <- de$qname
levels(de$facet) <- c("Abundance", "Weight", "Maturity, Selectivity", "Maturity, Selectivity")

# PLOT

psp <- ggplot(cbind(sp, lh='SP'), aes(x=age, y=data)) + facet_grid(facet~lh, scales='free') +
  geom_bar(data=subset(sp, qname == "n"), stat='identity') +
  geom_line(data=subset(sp, qname %in% "wt")) +
  geom_line(data=subset(sp, qname %in% c("sel", "mat")), aes(group=qname, linetype=qname)) +
  theme_bw() + ylab("") +
  theme(legend.position="none") +
  theme(strip.text.y = element_blank(), strip.background = element_blank())

plp <- ggplot(cbind(lp, lh='LP'), aes(x=age, y=data)) + facet_grid(facet~lh, scales='free') +
  geom_bar(data=subset(lp, qname == "n"), stat='identity') +
  geom_line(data=subset(lp, qname %in% "wt")) +
  geom_line(data=subset(lp, qname %in% c("sel", "mat")), aes(group=qname, linetype=qname)) +
  theme_bw() + ylab("") +
  theme(legend.position="none") +
  theme(strip.text.y = element_blank(), strip.background = element_blank())

pde <- ggplot(cbind(de, lh='DE'), aes(x=age, y=data)) + facet_grid(facet~lh, scales='free') +
  geom_bar(data=subset(de, qname == "n"), stat='identity') +
  geom_line(data=subset(de, qname %in% "wt")) +
  geom_line(data=subset(de, qname %in% c("sel", "mat")), aes(group=qname, linetype=qname)) +
  theme_bw() + ylab("") +
  theme(legend.justification=c(1,0), legend.position=c(0.99,0), legend.title=element_blank()) +
  theme(strip.background = element_blank())

lay <- rbind(c(1,1,2,2,2,3,3,3))
grid.arrange(grobs=list(psp, plp, pde), layout_matrix = lay)

pdf(file='~/Dropbox/Print/LHs_0.pdf')
dev.off()

