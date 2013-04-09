# functions.R - DESC
# functions.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

# setupStock {{{
setupStock <- function(brp, iniBiomass, nyears) {
	
	# find corresponding F
	idx <- max(which(ssb(brp) <= iniBiomass)[1], 2)

	# create from FLBRP
	stk <- as(brp, 'FLStock')[, idx]

	# change 'year' dimnames
	dimnames(stk) <- list(year=1)

	# expand
	stk <- stf(stk, nyears-1, 1)

	# get total biomass recomputed
	stock(stk) <- computeStock(stk)

	# add iters
	stk <- propagate(stk, iters)

	return(stk)
} # }}}

# effortDynamics {{{
effortDynamics <- function(stk, bmsy, sr, years=2:dims(stk)$maxyear, xp) {

	for (year in years) {
		har <- fbar(stk)[,year-1]
		bio <- ssb(stk)[,year-1]
		
		# Target F dynamics
		# F_{y} = F_{y-1} * (B_{y-1} / BMSY) ^ x
		eff <- har * (bio / bmsy) ^ xp
	
		# fwdControl
		fctl <- fwdControl(data.frame(year=year, quantity='f', val=c(eff)[1]))
		fctl@trgtArray <- array(NA, dim=c(1,3,iters),
			dimnames=list(year, c('min','val','max'), iter=1:iters))
		fctl@trgtArray[,2,] <- c(eff)

		# SR residuals
		srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=year)), 0)

		# fwd
		stk <- fwd(stk, fctl, sr=sr, sr.residuals=srres)
    cat ("\r", round(100*year/nyears, digits=0), "% ", sep="")
	}
	cat("\n")
	return(stk)
} # }}}

# oneWayTrip {{{
oneWayTrip <- function(stk, sr, fmax=refpts(brp)['crash', 'harvest']*0.80,
	years=2:dims(stk)$maxyear) {


	# limits
	f0 <- c(fbar(stk)[,1])
	fmax <- c(fmax)
	rate <- exp((log(fmax) - log(f0)) / (length(years)))

	# linear trend
	f <- rate^(0:59)*f0
	
	# fwdControl
	fctl <- fwdControl(data.frame(year=years, quantity='f', val=f[-1]))

	# SR residuals
	srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=years)), 0)

	# fwd
	stk <- fwd(stk, fctl, sr=sr, sr.residuals=srres)
	
	return(stk)
} # }}}

# rollerCoaster {{{
rollerCoaster <- function(stk, sr, fmax=refpts(brp)['crash', 'harvest']*0.80,
	fmsy=refpts(brp)['msy', 'harvest'], years=2:dims(stk)$maxyear, up=0.05, down=0.04) {

	# F
	f <- rep(NA, length(years))

	# limits
	f0 <- c(fbar(stk)[,1])
	fmax <- c(fmax)

	# linear trend
	rateup <- log(fmax/f0) / log(1 + up)
	fup <- f0 * ((1 + up) ^ (0:ceiling(rateup)))
	lfup <- length(fup)
	f[1:lfup] <- fup

	# at the top
	f[lfup:(lfup+5)] <- fup[lfup]

	# coming down!
	ratedo <- log(fmsy/f[length(fup)+5]) / log(1 + down)
	fdo <- f[lfup+5] * ((1 + down) ^ (0:ceiling(ratedo)))
	lfdo <- length(f) - (lfup +6) + 1
	f[(lfup+6):length(f)] <- fdo[1:lfdo]
	
	# fwdControl
	fctl <- fwdControl(data.frame(year=years, quantity='f', val=f))

	# SR residuals
	srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=years)), 0)

	# fwd
	stk <- fwd(stk, fctl, sr=sr, sr.residuals=srres)
	
	return(stk)
} # }}}

# oneWayQuickTrip {{{
oneWayQuickTrip <- function(stk, sr, fmax=refpts(brp)['crash', 'harvest']*0.80,
	rate=1.20, years=2:dims(stk)$maxyear) {

	# limits
	f0 <- c(fbar(stk)[,1])
	fmax <- c(fmax)

	# linear trend
	f <- rate^(0:59)*f0
	f[f > fmax] <- fmax
	

	# fwdControl
	fctl <- fwdControl(data.frame(year=years, quantity='f', val=f[-1]))

	# SR residuals
	srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=years)), 0)

	# fwd
	stk <- fwd(stk, fctl, sr=sr, sr.residuals=srres)
	
	return(stk)
} # }}}
