# sim.R - DESC
# sim.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(FLAdvice)
library(FLBioDym)

args <- list()


# (1) Contrast

# (2) LH

# (3) Initial depletion levels

# (4) Error levels

# (5) Selection patterns change

# (6) Length of time series

# (7) Un-reported catch


## TEST 1

# foo(lhpar, iniF, selectivity, target, error.level

# (2) LH
# par
par <- gislasim(FLPar(linf=150))
brp <- lh(par)

# (3) Initial depletion levels
# stk at F level
stk <- as(brp, 'FLStock')[, 10]
dimnames(stk) <- list(year=1)
# range(stk)[c('minfbar', 'maxfbar')] <- c(5, 40)

## TODO Re-do w/window et al
stk <- stf(stk, 29, 1)

## TODO Add to stf?
stock(stk) <- computeStock(stk)

# (1) Contrast
# fwd.control, F levels
fctl <- fwdControl(data.frame(year=2:30, quantity='f', val=0.6))

fctl <- fwdControl(data.frame(year=2:30, quantity='f', val=c(rep(1.10, 14), rep(0.80, 15)), rel.year=1:29))

# (4) Error levels
# Observation error in catch
fctl@trgtArray <- array(NA, dim=c(29, 3, 100), dimnames=list(1:29, c('min','val','max'), iter=1:100))
fctl@trgtArray[,2,] <- rnorm(29*100, 80, 15)
stk <- propagate(stk, 100)

# fwd
stk <- fwd(stk, fctl, sr=list(model='geomean', params=FLPar(a=1000)))

# (5) Selection patterns change
# FROM dome-shaped TO flat top half way

# (6) Length of time series

# (7) Un-reported catch

# BD

