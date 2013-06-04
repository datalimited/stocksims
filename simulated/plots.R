# plots.R - DESC
# plots.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(ggplot2)

# 
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(x$catch)))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(rec(x$stock))))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(ssb(x$stock)[,,,,,1:10])))
val <- lapply(sims, function(x) cbind(x$val, as.data.frame(fbar(x$stock))))
val <- do.call('rbind', val)

#
ggplot(val) + geom_line(aes(x=year, y=data)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y')

