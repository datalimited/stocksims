# plot.R - DESC
# plot.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(ggplot2)
library(FLCore)

files <- system('ls lh/*.RData', intern=TRUE)

# SP {{{

# SP sims
idx <- files[grep('simsSP', files)]
load(idx)

# wide data.frame w/ ssb, fbar, catch + val
out <- lapply(sims, function(x)
	cbind(model.frame(FLQuants(ssb=ssb(x$stock), rec=rec(x$stock),
		recdev=log(rec(x$stock)/eval(as.list(model(x$brp))[[3]], c(as(params(x$brp), 'list'),
		list(ssb=ssb(x$stock))))), harvest=fbar(x$stock), catch=x$catchE)), x$val))
out <- do.call(rbind, out)
out <- out[out$TS == 60,]

# plot {{{
pdf(file="outSP.pdf")

print(ggplot(out) + geom_point(aes(x=year, y=ssb), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('SSB'))

print(ggplot(out) + geom_point(aes(x=year, y=harvest), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Fbar'))

print(ggplot(out) + geom_point(aes(x=year, y=catch), alpha=0.01) + facet_grid(TS+ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Catch'))

dev.off() # }}}

outS <- subset(out, iter %in% 1:5)

names <- c('LH', 'ID', 'AR', 'ED', 'UR', 'TS')
label <- character(0)

for (i in names)
	label <- paste(label, paste(unique(outS[,i]),  collapse="/"), sep="_")


# plot {{{
pdf(file="outSPspag.pdf")

print(ggplot(outS) + geom_line(aes(x=year, y=ssb, color=iter)) + facet_grid(ED+AR~LH+UR+ID, scale='free_y') + ggtitle(paste('SSB', label)) + theme(legend.position="none"))

print(ggplot(outS) + geom_line(aes(x=year, y=harvest, color=iter)) + facet_grid(ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Fbar') + theme(legend.position="none"))

print(ggplot(outS) + geom_line(aes(x=year, y=catch, color=iter)) + facet_grid(ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Catch') + theme(legend.position="none"))

print(ggplot(outS) + geom_line(aes(x=year, y=rec, color=iter)) + facet_grid(ED+AR~LH+UR+ID, scale='free_y') + ggtitle('Rec') + theme(legend.position="none"))

print(ggplot(outS) + geom_point(aes(x=ssb, y=rec, color=iter)) + facet_grid(ED+AR~LH+UR+ID, scale='free') + ggtitle('S/R') + theme(legend.position="none"))

print(ggplot(outS) + geom_point(aes(x=year, y=recdev, color=iter, alpha=1/300)) + facet_grid(ED+AR~LH+UR+ID, scale='free_y') + ggtitle('RecDev') + theme(legend.position="none") + scale_color_manual(values=rep("gray", 10)) + geom_hline(aes(yintercept=0), linetype="dashed"))

dev.off() # }}}

# }}}
