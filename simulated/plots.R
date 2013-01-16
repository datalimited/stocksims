# plots.R - DESC
# plots.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

# Overall plot of SSB
fqs <- FLQuants(lapply(out, function(x) ssb(x$stock)))
xyplot(data~year|qname, fqs,
	lattice.options=list(par.strip.text=list(cex=0.3)), type='l')

# One page report per dataset

# stock

# df
# ssb, tb, catch, refpts, ssb/ssbMSY
