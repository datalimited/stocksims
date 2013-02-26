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

# SIMS & INPUT
sims <- list()
input <- list()

# Scenarios list {{{
sce <- list(
	# LH
	LH=list(
	# SP Small Pelagic: Linf=30cm, ages=1:8, fbar=2:8, steep=0.7
		SP=list(
			par=FLPar(linf=30, sl=2, sr=120, a1=2, s=0.70, v=vBiomass),
			range=c(min=1, max=8, minfbar=2, maxfbar=8, plusgroup=8)),
# 	DE Demersal: Linf=70cm, ages=1:20, fbar=2:20, steep=0.8
		DE=list(
			par=FLPar(linf=70, sl=2, sr=120, a1=2, s=0.80, v=vBiomass),
			range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20)),
# 	LP Large Pelagic: Linf=150cm, ages=1:20, fbar=6:30, steep=0.85
		LP=list(
			par=FLPar(linf=150, sl=2, sr=120, a1=2, s=0.80, v=vBiomass),
			range=c(min=1, max=20, minfbar=4, maxfbar=20, plusgroup=20))),
# Initial depletion: ID0, ID30, ID60
	ID=list(ID0=1, ID40=0.70, ID60=0.40),
# Effort/F dynamics, x value: ED0, ED0.1, ED0.3, ED0.6
	ED=list(ED0=0, ED0.1=0.1, ED0.3=0.3, ED0.6=0.6),
# TODO Selectivity: SELFD, SELF, SELD, SELDF
	SEL=list(SELFD=NA, SELD=NA, SELDF=NA, SELF=NA),
# Length of time series (years): TS20, TS40, TS60
	TS=list(TS20=20, TS60=60)
) # }}}

# VAL {{{
val <- cbind(
	LH=factor("SP", levels=names(sce$LH)),
	SEL=factor("SELF", levels=names(sce$SEL)),
	as.data.frame(lapply(sce[-c(1, 4)],
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
		# ED
		for(ed in names(sce$ED)) {
			stk <- effortDynamics(stk, bmsy=c(refpts(brp)['msy', 'ssb']),
				sr=list(model='bevholt', params=params(brp)), years=2:nyears, xp=sce$ED[[ed]])
			# SEL
				sel <- "SELF"
				# TS
				for(ts in names(sce$TS)) {
					stock <- stk[,seq(nyears-sce$TS[[ts]]+1, nyears)]
						# VAL
						val[1,] <- c(lh, sce$ID[[id]], sce$ED[[ed]], sel, sce$TS[[ts]])
						# NAME
						name <- paste(lh, id, ed, sel, ts, sep="_")
						name(stock) <- name
						desc(stock) <- paste(name, Sys.time())
						# BD
#						bd <- FLBioDym(catch=catch(stock), index=stock(stock))
#						bd@bounds[,1:4] <- matrix(unlist(
#							list(r=c(1, 0.05, 5, 0.5),
#								 k=c(1, c(max(catch(bd))), vBiomass*10, vBiomass),
#								 p=c(-1, 1, 10, 1),
#								 b0=c(1, c(max(catch(bd))), vBiomass*10, vBiomass),
#								 q=c(1, 0.1, 10, 1),
#								 sigma=c(1, 0.01, 10, 0.1))), ncol=4, byrow=T)
#						bd <- admbBD(bd)
						# SIMS
						sims[[name]] <- list(lh=par, code=name, stock=stock,
							refpts=refpts(brp), val=val)
						input[[name]] <- list(catch=as.data.frame(catch(stock))[, c("year", "data")],
							linf=par['linf'])
						print(name)
				}
		}
	}
} # }}}

# save RData
save(sims, file=paste("out/sims", format(Sys.time(), "%Y%m%d%H%M"), ".RData", sep=""))
save(input, file=paste("out/input", format(Sys.time(), "%Y%m%d%H%M"), ".RData", sep=""))

# Sensitivity runs
# Under-reporting catch %: UR0, UR10, UR25, UR50
	UR=list(UR0=0, UR10=10, UR25=25, UR50=50)
