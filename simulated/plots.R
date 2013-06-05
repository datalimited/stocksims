# plots.R - DESC
# plots.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(ggplot2)
library(FLCore)

load('out/0.6/sims06201306051031.RData')

# sims 
val <- lapply(sims06, function(x) cbind(x$val, as.data.frame(x$catch)))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(rec(x$stock))))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(ssb(x$stock)[,,,,,1:10])))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(fbar(x$stock))))

val <- do.call('rbind', val)

#
pdf(file="catchE.pdf")
print(ggplot(val) + geom_point(aes(x=year, y=data), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y'))
dev.off()

# inputE
val <- lapply(inputE02, function(x) cbind(x$val, as.data.frame(x$catch)))
