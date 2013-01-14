# functions.R - DESC
# functions.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# setupStock {{{
setupStock <- function(brp, iniBiomass) {
	
	# find corresponding F
	idx <- which(ssb(brp) <= iniBiomass)[1]

	# create from FLBRP
	stk <- as(brp, 'FLStock')[, idx]

	# change 'year' dimnames
	dimnames(stk) <- list(year=1)

	# make range fbar match par$a1 and last age
	range(stk, c('minfbar', 'maxfbar')) <- c(c(par['a1',]), 20)

	# expand
	stk <- stf(stk, years-1, 1)

	# get total biomass recomputed
	stock(stk) <- computeStock(stk)

	# add iters
	stk <- propagate(stk, iters)

	return(stk)
} # }}}

# effortDynamics {{{
effortDynamics <- function(stk, bmsy, sr, years=2:dims(stk)$maxyear) {

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
		srres <- rlnorm(iters, FLQuant(0, dimnames=list(year=year)), 0.2)

		# fwd
		stk <- fwd(stk, fctl, sr=sr, sr.residuals=srres)
	}
	return(stk)
} # }}}
