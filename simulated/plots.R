# plots.R - DESC
# plots.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

# Overall plot of SSB
fqs <- FLQuants(lapply(out, function(x) ssb(x$stock)))

pdf(file="out/overallPlot.pdf")
print(xyplot(data~year|qname, fqs, strip=FALSE, type='l', ylab='SSB'))
dev.off()

# One page report per dataset

# stock

# df
# ssb, tb, catch, refpts, ssb/ssbMSY
