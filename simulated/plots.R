# plots.R - DESC
# plots.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(ggplot2)

# 
val <- lapply(res$sims, function(x) cbind(x$val, as.data.frame(FLQuantPoint(x$catch))))
val <- do.call('rbind', val)

#
ggplot(val) + geom_line(aes(x=year, y=data, group=iter, lty=iter)) + stat_smooth(se=TRUE) + facet_grid(TS+ED+AR~LH+UR+ID)



head(as.data.frame(FLQuantPoint(m(ple4)))) 
ggplot(res) + geom_line(aes(x=year,y=data,group=iter,size=iter,lty=iter))

