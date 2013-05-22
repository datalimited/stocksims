# condition.R - DESC
# condition.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

library(FLBRP)
library(FLash)
library(FLAssess)

source('functions.R')

# VARS
set.seed(1973)
nyears <- 60
iters <- 1
vBiomass <- 1000

# SIMS & INPUT
sims <- list()
input <- list()

# sce: Scenarios list {{{
sce <- list(
	# LH
	LH=list(
	# SP Small Pelagic: Linf=30cm, ages=1:8, fbar=2:8, steep=0.70
		SP=list(
			par=FLPar(linf=30, sl=2, sr=120, a1=2, s=0.70, v=vBiomass),
			range=c(min=1, max=8, minfbar=2, maxfbar=8, plusgroup=8)),
# 	DE Demersal: Linf=70cm, ages=1:20, fbar=2:20, steep=0.80
		DE=list(
			par=FLPar(linf=70, sl=2, sr=120, a1=2, s=0.80, v=vBiomass),
			range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20)),
# 	LP Large Pelagic: Linf=150cm, ages=1:20, fbar=6:30, steep=0.80
		LP=list(
			par=FLPar(linf=150, sl=2, sr=120, a1=2, s=0.80, v=vBiomass),
			range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20))),
# Initial depletion: ID0, ID30, ID60
	ID=list(ID0=1, ID30=0.70, ID60=0.40),
# Autocorrelation in SR residuals
	AR=list(AR=0.8, NR=0),
# Effort/F dynamics, x value: RC, ED0, ED0.3, OW
	ED=list(RC=0.80, ED0.1=0.1, ED0.6=0.6, OW=0.80),
# TODO Selectivity: SELFD, SELF, SELD, SELDF
	SEL=list(SELFD=NA, SELD=NA, SELDF=NA, SELF=NA),
# Underreporting: UR0, UR50
	UR=list(UR0=0, UR25=0.50),
# Length of time series (years): TS20, TS40, TS60
	TS=list(TS20=20, TS60=60)
) # }}}

# val: VAL {{{
val <- cbind(
	LH=factor("SP", levels=names(sce$LH)),
	SEL=factor("SELF", levels=names(sce$SEL)),
	AR=factor("NR", levels=names(sce$AR)),
	ED=factor("RC", levels=names(sce$ED)),
	as.data.frame(lapply(sce[-c(1, 3, 4, 5)],
		function(x) factor(NA, levels=unlist(x))))
)[,names(sce)] # }}}

# Input list {{{
# LH
for(lh in names(sce$LH)) {
	par <- gislasim(sce$LH[[lh]]$par)
	brp <- lh(par, range=sce$LH[[lh]]$range)
	# ID
	for(id in names(sce$ID)) {
		stk <- setupStock(brp, iniBiomass=vBiomass * sce$ID[[id]], nyears)
		# AR
		for(ar in names(sce$AR)) {
		srres <- switch(ar,
			#
			"NR"=rlnorm(iters, FLQuant(0, dimnames=list(year=2:nyears)), 0),
			"AR"=ar1lnorm(years=2:nyears, sd=0.2, rho=0.8))
		# ED
		for(ed in names(sce$ED)) {
			stk <- switch(ed, 
			# one way trip
			"OW"=oneWayTrip(stk, fmax=refpts(brp)['crash', 'harvest']*sce$ED[[ed]], 
				sr=list(model='bevholt', params=params(brp)), years=2:nyears, srres=srres),
			# roller coaster
			"RC"=rollerCoaster(stk, fmax=refpts(brp)['crash', 'harvest']*sce$ED[[ed]], 
			fmsy=refpts(brp)['msy', 'harvest'], years=2:nyears, up=0.1, down=0.05,
				sr=list(model='bevholt', params=params(brp)), srres=srres),
			# effort dynamics
			"ED0.1"=effortDynamics(stk, bmsy=c(refpts(brp)['msy', 'ssb']),
				sr=list(model='bevholt', params=params(brp)), years=2:nyears, xp=sce$ED[[ed]],
				srres=srres),
			"ED0.6"=effortDynamics(stk, bmsy=c(refpts(brp)['msy', 'ssb']),
				sr=list(model='bevholt', params=params(brp)), years=2:nyears, xp=sce$ED[[ed]],
				srres=srres)
			)
			# SEL
			sel <- "SELF"
				# TS
				for(ts in names(sce$TS)) {
					stock <- stk[,seq(nyears-sce$TS[[ts]]+1, nyears)]
						# UR
						for (ur in names(sce$UR)) {
							##
							# VAL
							val[1,] <- c(lh, sce$ID[[id]],  sce$AR[[ar]], ed, sel, sce$UR[[ur]],
								sce$TS[[ts]])
							# NAME
							name <- paste(lh, id, ar, ed, sel, ts, ur, sep="_")
							name(stock) <- name
							desc(stock) <- paste(name, Sys.time())
							# SIMS
							sims[[name]] <- list(lh=par, code=name, stock=stock,
								refpts=refpts(brp), val=val, catch=catch(stock)*(1-sce$UR[[ur]]))
							# NOISE in C
							# INPUT
							input[[name]] <- list(catch=as.data.frame(catch(stock)*(1-sce$UR[[ur]]))[,
								c("year", "data")], linf=c(par['linf']), tmax=dims(stock)$max,
								tmat=which(c(mat(brp)) > 0.5)[1]-1)
							print(name)
						}
				}
				}
		}
	}
} # }}}

# ED 1.20BMSY

# save RData
save(sims, file=paste("out/sims", format(Sys.time(), "%Y%m%d%H%M"), ".RData", sep=""))
save(input, file=paste("out/input", format(Sys.time(), "%Y%m%d%H%M"), ".RData", sep=""))

# Sensitivity runs:
# effortDynamics to 1.2 BMSY

# AR1 in rec
# Error in C
