# condition.R - DESC
# condition.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

library(FLAdvice)

source('functions.R')

# VARS
set.seed(1973)
nyears <- 60
iters <- 1
vBiomass <- 1000

# OUT
out <- list()

# Scenarios list {{{
sce <- list(
# LH
	LH=list(
# 	SP Small Pelagic: Linf=30cm, ages=1:8, fbar=2:8, steep=0.7
		SP=list(
			par=FLPar(linf=30, sl=2, sr=120, a1=2, s=0.7, v=vBiomass),
			range=c(min=1, max=8, minfbar=2, maxfbar=8, plusgroup=8)),
# 	DE Demersal: Linf=100cm, ages=1:20, fbar=4:20, steep=0.6
		DE=list(
			par=FLPar(linf=100, sl=2, sr=120, a1=2, s=0.6, v=vBiomass),
			range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20)),
# 	LP Large Pelagic: Linf=250cm, ages=1:30, fbar=6:30, steep=0.85
		LP=list(
			par=FLPar(linf=250, sl=2, sr=120, a1=2, s=0.85, v=vBiomass),
			range=c(min=1, max=30, minfbar=6, maxfbar=30, plusgroup=30))),
# Initial depletion: ID10, ID40, ID60
	ID=list(ID10=0.90, ID40=0.60, ID60=0.40),
# Effort/F dynamics, x value: ED0.1, ED0.3, ED0.6, FD
	ED=list(ED0.1=0.1, ED0.3=0.3, ED0.6=0.6),
# TODO Selectivity: SELFD, SELF, SELD, SELDF
	SEL=list(SELFD=NA, SELD=NA, SELDF=NA),
# Length of time series (years): TS20, TS40, TS60
	TS=list(TS20=20, TS40=40, TS60=60),
# Under-reporting catch %: UR0, UR10, UR25, UR50
	UR=list(UR0=0, UR10=10, UR25=25, UR50=50)
) # }}}

# Input list {{{
# LH
for(lh in names(sce$LH)) {
	par <- gislasim(sce$LH[[lh]]$par)
	brp <- lh(par, range=sce$LH[[lh]]$range)
	# ID
	for(id in names(sce$ID)) {
		stk <- setupStock(brp, iniBiomass=vBiomass * sce$ID[[id]], nyears)
		# ED
		for(ed in names(sce$ED)) {
			stk <- effortDynamics(stk, bmsy=c(refpts(brp)['msy', 'ssb']),
				sr=list(model='bevholt', params=params(brp)), years=2:nyears, xp=sce$ED[[ed]])
			# SEL
				# TS
				for(ts in names(sce$TS)) {
					stock <- stk[,seq(nyears-sce$TS[[ts]]+1, nyears)]
					# UR
					for(ur in names(sce$UR)) {
						# NOTE: harvest, stock and catch in stk will not match anymore
						catch(stock) <- catch(stock) * (100 - sce$UR[[ur]] / 100)
					# SAVE
					name <- paste(lh, id, ed, ts, ur, sep="_")
					out[[name]] <- list(lh=par, code=name, stock=stock)
					print(name)
					}
				}
		}
	}
} # }}}

# save RData
save(out, file="out/out.RData")

# JAN 16 2013 18:34
