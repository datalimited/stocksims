# sim.R - DESC
# sim.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(FLAdvice)
# library(FLBioDym)

## Vars

years <- 60
iters <- 100
xp <- 0.109


## TEST 1

# foo(lhpar, iniF, selectivity, target, error.level

# (1) LH & selectivity
# par
par <- gislasim(FLPar(linf=60, sl=20, sr=2, a1=4, s=0.6))
brp <- lh(par)

# (2) Initial depletion levels
# stk at F level, SELECTIVITY
stk <- as(brp, 'FLStock')[, 5]
dimnames(stk) <- list(year=1)
range(stk, c('minfbar', 'maxfbar')) <- c(c(par['a1',]), 30)

## TODO Re-do w/window et al
stk <- stf(stk, years-1, 1)

## TODO Add to stf?
stock(stk) <- computeStock(stk)

# Recruitment variability
srres <- rlnorm(100, stock(stk)[,2], 0.1)

stk <- propagate(stk, iters)

# (3) Contrast (values of xp?)

# Target F dynamics
# F_{y} = F_{y-1} * (B_{y-1} / BMSY) ^ x
bmsy <- c(refpts(brp)['msy', 'ssb'])
for (year in 2:years) {
	har <- fbar(stk)[,year-1]
	bio <- ssb(stk)[,year-1]
	eff <- har * (bio / bmsy) ^ xp
	fctl <- fwdControl(data.frame(year=year, quantity='f', val=c(eff)[1]))
	stk <- fwd(stk, fctl, sr=list(model='bevholt', params=params(brp)), sr.residuals=srres)
}

# (4) Selection patterns change
# FROM dome-shaped TO flat top half way

# (5) Length of time series

# (6) Un-reported catch


# ----

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


