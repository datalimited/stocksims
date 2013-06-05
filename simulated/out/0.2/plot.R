# plot.R - DESC
# plot.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(ggplot2)

files <- system('ls lh/*.RData', intern=TRUE)

# SP {{{

# SP sims
idx <- files[grep('simsSP', files)]
load(idx)

# wide data.frame w/ ssb, fbar, catch + val
out <- lapply(sims, function(x)
	cbind(model.frame(FLQuants(ssb=ssb(x$stock), recdev=rec(x$stock)-iterMeans(rec(x$stock)), harvest=fbar(x$stock), catch=x$catchE)),
		x$val))
out <- do.call(rbind, out)

# plot {{{
pdf(file="outSP.pdf")

print(ggplot(out) + geom_point(aes(x=year, y=ssb), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('SSB'))

print(ggplot(out) + geom_point(aes(x=year, y=harvest), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Fbar'))

print(ggplot(out) + geom_point(aes(x=year, y=catch), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Catch'))

dev.off() # }}}

outS <- subset(out, iter %in% 1:10)

# plot {{{
pdf(file="outSPspag.pdf")

print(ggplot(outS) + geom_line(aes(x=year, y=ssb, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('SSB') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

print(ggplot(outS) + geom_line(aes(x=year, y=harvest, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Fbar') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

print(ggplot(outS) + geom_line(aes(x=year, y=catch, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Catch') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

print(ggplot(outS) + geom_line(aes(x=year, y=recdev, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Rec dev') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

dev.off() # }}}

# }}}

# DE {{{

# DE sims
idx <- files[grep('simsDE', files)]
load(idx)

# wide data.frame w/ ssb, fbar, catch + val
out <- lapply(sims, function(x)
	cbind(model.frame(FLQuants(ssb=ssb(x$stock), rec=rec(x$stock), harvest=fbar(x$stock), catch=x$catchE)),
		x$val))
out <- do.call(rbind, out)

# plot {{{
pdf(file="outDE.pdf")

print(ggplot(out) + geom_point(aes(x=year, y=ssb), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('SSB'))

print(ggplot(out) + geom_point(aes(x=year, y=harvest), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Fbar'))

print(ggplot(out) + geom_point(aes(x=year, y=catch), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Catch'))

dev.off() # }}}

outS <- subset(out, iter %in% 1:10)

# plot {{{
pdf(file="outDEDEag.pdf")

print(ggplot(outS) + geom_line(aes(x=year, y=ssb, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('SSB') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

print(ggplot(outS) + geom_line(aes(x=year, y=harvest, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Fbar') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

print(ggplot(outS) + geom_line(aes(x=year, y=catch, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Catch') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

print(ggplot(outS) + geom_line(aes(x=year, y=rec, color=iter)) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Rec') + theme(legend.position="none") + scale_color_manual(values=rep("red", 10)))

dev.off() # }}}

# }}}
