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

# (1) LH & selectivity

# Linf=60cm, ages=1:20, fbar=4:20, steep=0.6
par <- gislasim(FLPar(linf=60, sl=4, sr=120, a1=4, s=0.6))
brp <- lh(par, range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20))

# SP_ID50_ED0.2_SELFD_TS60_UR0

# Life history: SP, DE, LP
# Initial depletion: ID0, ID40, ID60
# Effort/F dynamics, x value: ED0.1, ED0.3, ED0.6, FD
# Selectivity: SELFD, SELF, SELD, SELDF
# Length of time series: TS20, TS40, TS60
# Under-reporting catch %: UR0, UR10, UR25, UR50

# Small Pelagic: Linf=30cm, ages=1:8, fbar=2:8, steep=0.7

# Demersal: Linf=100cm, ages=1:20, fbar=4:20, steep=0.6

# Large Pelagic: Linf=250cm, ages=1:30, fbar=6:30, steep=0.85

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

