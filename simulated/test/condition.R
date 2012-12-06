# condition.R - DESC
# condition.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(FLAdvice)

source('functions.R')

## VARS
set.seed(1973)
years <- 60
iters <- 10
xp <- 0.200
vBiomass <- 1000

# Demersal: Linf=100cm, ages=1:20, fbar=4:20, steep=0.6
# Large Pelagic: Linf=250cm, ages=1:30, fbar=6:30, steep=0.85

# Life history: SP, DE, LP
# Initial depletion: ID0, ID40, ID60
# Effort/F dynamics, x value: ED0.1, ED0.3, ED0.6, FD
# Selectivity: SELFD, SELF, SELD, SELDF
# Length of time series: TS20, TS40, TS60
# Under-reporting catch %: UR0, UR10, UR25, UR50

# Small Pelagic: Linf=30cm, ages=1:8, fbar=2:8, steep=0.7

# SP_ID50_ED0.2_SELFD_TS60_UR0
par <- gislasim(FLPar(linf=30, sl=2, sr=120, a1=2, s=0.7, v=vBiomass))
brp <- lh(par, range=c(min=1, max=8, minfbar=2, maxfbar=8, plusgroup=8))

stk <- setupStock(brp, iniBiomass=vBiomass)

stk <- effortDynamics(stk, bmsy=c(refpts(brp)['msy', 'ssb']),
	sr=list(model='bevholt', params=params(brp)))


