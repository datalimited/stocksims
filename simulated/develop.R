# develop.R - DESC
# develop.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# Scenarios list {{{
sce <- list(
	# LH
	LH=list(
# 	DE Demersal: Linf=70cm, ages=1:20, fbar=2:20, steep=0.8
		DE=list(
			par=FLPar(linf=70, sl=2, sr=120, a1=2, s=0.80, v=vBiomass),
			range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20))),
# Initial depletion: ID0, ID30, ID60
	ID=list(ID0=1),
# Effort/F dynamics, x value: ED0, ED0.1, ED0.3, ED0.6
	ED=list(ED0.1=0.1),
# TODO Selectivity: SELFD, SELF, SELD, SELDF
	SEL=list(SELFD=NA),
# Length of time series (years): TS20, TS40, TS60
	TS=list(TS60=60)
) # }}}

lh <- names(sce$LH)[1]
id <- names(sce$ID)[1]
ed <- names(sce$ED)[1]
sel <- names(sce$SEL)[1]
ts <- names(sce$TS)[1]


par <- gislasim(sce$LH[[lh]]$par)
brp <- lh(par, range=sce$LH[[lh]]$range)

stk <- setupStock(brp, iniBiomass=vBiomass * sce$ID[[id]], nyears)
sr <- list(model='bevholt', params=params(brp))
years <- 2:nyears
