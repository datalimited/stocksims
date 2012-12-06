# sim.R - DESC
# sim.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(FLAdvice)

set.seed(1973)

## VARS
years <- 60
iters <- 10
xp <- 0.200

## PARAMS

# foo(lhpar, iniF, selectivity, target, error.level

# (1) LH & selectivity
# par
par <- gislasim(FLPar(linf=60, sl=4, sr=120, a1=4, s=0.6))
brp <- lh(par, range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20))

# par <- gislasim(FLPar(linf=60, sl=4, sr=30, a1=2, s=0.6))
# brp <- lh(par, range=c(min=1, max=20, minfbar=2, maxfbar=20, plusgroup=20))

# (2) Initial depletion levels
# stk at F level, SELECTIVITY
stk <- as(brp, 'FLStock')[, 5]
dimnames(stk) <- list(year=1)
range(stk, c('minfbar', 'maxfbar')) <- c(c(par['a1',]), 20)

## TODO Re-do w/window et al
stk <- stf(stk, years-1, 1)

## TODO Add to stf?
stock(stk) <- computeStock(stk)

# Recruitment variability
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
	fctl@trgtArray <- array(NA, dim=c(1,3,iters), dimnames=list(year, c('min','val','max'), iter=1:iters))
	fctl@trgtArray[,2,] <- c(eff)
	srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=year)), 0.2)
	stk <- fwd(stk, fctl, sr=list(model='bevholt', params=params(brp)), sr.residuals=srres)
}

stk1 <- stk

harvest(stk)[,31:60] <- harvest(stk)[,31] * FLQuant(c(1,1.05, 1.1, 1.05,
	seq(1.05, 0.85, length=16)), dim=c(20,1,1,1,1,iters), quant='age')

# (4) Selection patterns change
# FROM dome-shaped TO flat top half way
for (year in 31:years) {
	har <- fbar(stk)[,year-1]
	bio <- ssb(stk)[,year-1]
	eff <- har * (bio / bmsy) ^ xp
	fctl <- fwdControl(data.frame(year=year, quantity='f', val=c(eff)[1]))
	fctl@trgtArray <- array(NA, dim=c(1,3,iters), dimnames=list(year, c('min','val','max'), iter=1:iters))
	fctl@trgtArray[,2,] <- c(eff)
	srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=year)), 0.2)
	stk <- fwd(stk, fctl, sr=list(model='bevholt', params=params(brp)), sr.residuals=srres)
}

# (5) Length of time series

# (6) Un-reported catch

