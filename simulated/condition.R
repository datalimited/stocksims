# condition.R - DESC
# condition.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

library(plyr)
library(FLBRP)
library(FLAssess)
library(FLash)

source('functions.R')

# VARS
set.seed(666)
nyears <- 60 # Max. number of years
iters <- 250 # No. of replicates for SR residuals
vBiomass <- 1000 # Initial VBiomass
margSD <- 0.6 # Marginal SD of AR1 process
rsd <- 0.2 # Log SD of SR residuals

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
	ED=list(ED0=0, ED0.6=0.6, OW=0.80, RC=0.80, RC2=0.80),
# TODO Selectivity: SELFD, SELF, SELD, SELDF
	SEL=list(SELFD=NA, SELD=NA, SELDF=NA, SELF=NA),
# Underreporting: UR0, UR50
	UR=list(UR0=0, UR50=0.50),
# Length of time series (years): TS20, TS60
	TS=list(TS60=60, TS20=20)
) # }}}

# val: VAL {{{
val <- data.frame(
	LH=NA,
	ID=NA,
	AR=NA,
	ED=NA,
	SEL=NA,
	UR=NA,
	TS=NA) # }}}

# RUN for sims and input {{{
# LH
for(lh in names(sce$LH)) {
	par <- gislasim(sce$LH[[lh]]$par)
	brp <- lh(par, range=sce$LH[[lh]]$range)

# AR
for(ar in names(sce$AR)) {
	srres <- switch(ar,
	"NR"=rlnorm(iters, FLQuant(0, dimnames=list(year=2:nyears)), sd=rsd),
	"AR"=ar1rnorm(rho=0.8, iters=iters, years=2:nyears, margSD=margSD))
# ID
for(id in names(sce$ID)) {
	stk <- setupStock(brp, iniBiomass=vBiomass * sce$ID[[id]], nyears)

# ED
for(ed in names(sce$ED)) {
	stock <- switch(ed, 
	# one way trip
	"OW"=oneWayTrip(stk, fmax=refpts(brp)['crash', 'harvest']*sce$ED[[ed]], 
		sr=list(model='bevholt', params=params(brp)), years=2:nyears, srres=srres),
	# roller coaster
	"RC"=rollerCoaster(stk, fmax=refpts(brp)['crash', 'harvest']*sce$ED[[ed]], 
		fmsy=refpts(brp)['msy', 'harvest'], years=2:nyears, up=0.1, down=0.05,
		sr=list(model='bevholt', params=params(brp)), srres=srres),
	# RC2
	"RC2"=rollerCoaster2(stk, fmax=refpts(brp)['crash', 'harvest']*sce$ED[[ed]], 
		fmsy=refpts(brp)['msy', 'harvest'], years=2:nyears, upy=25, top=5, downy=30,
		sr=list(model='bevholt', params=params(brp)), srres=srres),
	# effort dynamics
	"ED0"=effortDynamics(stk, bmsy=c(refpts(brp)['msy', 'ssb']),
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
	stock <- stock[,seq(nyears-sce$TS[[ts]]+1, nyears)]

# UR
for (ur in names(sce$UR)) {
	
# VAL
val[1,] <- c(lh, sce$ID[[id]],  sce$AR[[ar]], ed, sel, sce$UR[[ur]],
	sce$TS[[ts]])

# NAME
name <- paste(lh, id, ar, ed, sel, ur, ts, sep="_")
name(stock) <- name
desc(stock) <- paste(name, Sys.time())

# SIMS
sims[[name]] <- list(lh=par, code=name, stock=stock,
refpts=refpts(brp), val=val, catch=catch(stock)*(1-sce$UR[[ur]]))

print(name)
gc()

}}}}} #
save(sims, input, file="out/dumpRUN.RData") 
} # }}}

# Error in C: 30% CV {{{

# Add catchE
sims <- lapply(sims, function(x) {
	# Normal error with CV=20%
	x$catchE <- FLQuant(rnorm(iters, c(x$catch), c(mean(x$catch)) * 0.20),
		dimnames=dimnames(x$catch))
	return(x)
	})

# input
input <- lapply(sims, function(x) {
	y <- list()
	# catch
	y$catch <- as.data.frame(x$catch)[,c('year', 'data')]
	# linf
	y$linf <- c(x$lh['linf'])
	# tmax
	y$tmax <- dims(x$stock)$max
	# tmat
	y$tmat <- which(c(mat(x$stock)) > 0.5)[1]-1
	return(y)
	})

# inputE (list of 500 input lists)
inputTMP <- lapply(sims, function(x)
	dlply(as.data.frame(x$catchE)[,c('year', 'iter', 'data')], 'iter', subset))

inputE <- vector('list', length=iters)
names(inputE) <- paste('iter', 1:iters, sep="")
for (i in 1:iters) {
	inputE[[i]] <- vector('list', length=length(inputTMP))
	names(inputE[[i]]) <- names(sims)
	for (j in 1:length(inputTMP)) {
		inputE[[i]][[j]]$catch <- inputTMP[[j]][[i]]
		inputE[[i]][[j]][c('linf', 'tmax', 'tmat')] <- input[[j]][c('linf', 'tmax', 'tmat')]
	}
} # }}}

# ED 1.20BMSY

# save RData
save(sims, file=paste("out/sims", format(Sys.time(), "%Y%m%d%H%M"), ".RData", sep=""))
save(input, file=paste("out/input", format(Sys.time(), "%Y%m%d%H%M"), ".RData", sep=""))
save(inputE, file=paste("out/inputE", format(Sys.time(), "%Y%m%d%H%M"),
	".RData", sep=""))
