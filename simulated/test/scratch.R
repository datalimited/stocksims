# scratch.R - DESC
# scratch.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

fctl <- fwdControl(data.frame(year=2:30, quantity='f', val=c(rep(1.10, 14), rep(0.80, 15)), rel.year=1:29))

xyplot(data~age, groups=year, harvest(stk))
xyplot(data~age, groups=year, harvest(stk)[,3])

fctl@trgtArray <- array(NA, dim=c(29, 3, 100), dimnames=list(1:29, c('min','val','max'), iter=1:100))
fctl@trgtArray[,2,] <- rnorm(29*100, 80, 15)

# IDEA: foo to be passed to fwd via Rcpp


# fwd.control, F levels
fctl <- fwdControl(data.frame(year=2:years, quantity='f', val=0.1))


# fwd
stk <- fwd(stk, fctl, sr=list(model='geomean', params=FLPar(a=rnorm(100, 1000, 200), iter=100)))


