# plots.R - DESC
# plots.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# XX {{{
# }}}

xyplot(data~age, catch.sel(brp), type='b')

xyplot(data~age, harvest(brp)[,5], type='b', pch=19)

xyplot(data~age, harvest(stk)[,30])
xyplot(data~age|as.factor(year), harvest(stk), scales=list(relation='free'))

xyplot(data~year, iterMeans(ssb(stk) / ssb(stk1)))

# One page report per dataset

# stock

# df
# ssb, tb, catch, refpts, ssb/ssbMSY
