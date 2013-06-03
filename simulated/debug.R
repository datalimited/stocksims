# debug.R - DESC
# debug.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# Strange catch trends in LP_ID0_NR_OW_SELF_UR0_TS60
bug <- sims[["LP_ID0_NR_OW_SELF_UR0_TS60"]]
bug <- simsOW[["LP_ID0_NR_OW_SELF_UR0_TS60"]]

plot(catch(bug$stock))

sce$LH <- sce$LH['LP']
sce$ID <- sce$ID['ID0']
sce$AR <- sce$AR['NR']

sce$ED <- sce$ED['OW']
sce$ED <- sce$ED['RC']

sce$ED<-sce$ED[c(1,2,3)]

sce$UR <- sce$UR['UR0']
sce$TS <- sce$TS['TS60']

debug(oneWayTrip)

# Adding stochastic recruitment
srres<-ar1lnorm(sd=0.2, rho=0.8, years=2:nyears)

srres <- rlnorm(5, FLQuant(0, dimnames=list(year=2:nyears)), 0)
