# debug.R - DESC
# debug.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# Strange catch trends in LP_ID0_NR_OW_SELF_UR0_TS60
bug <- sims[["LP_ID0_NR_OW_SELF_UR0_TS60"]]
bug <- sims[["LP_ID0_AR_OW_SELF_UR0_TS60"]]
bug <- simsOW[["LP_ID0_NR_OW_SELF_UR0_TS60"]]

plot(catch(bug$stock))

sce$LH <- sce$LH['LP']
sce$ID <- sce$ID['ID0']
sce$UR <- sce$UR['UR0']
sce$TS <- sce$TS['TS60']

#
sce$ED <- sce$ED['OW']
sce$ED <- sce$ED['RC']
sce$ED <- sce$ED['RC2']
sce$ED <- sce$ED['ED0']
sce$ED <- sce$ED['ED0.6']

sce$AR <- sce$AR['NR']

debug(oneWayTrip)

# Adding stochastic recruitment

lh <- names(sce$LH)[1]
id <- names(sce$ID)[1]
ar <- names(sce$AR)[1]
ed <- names(sce$ED)[1]
ts <- names(sce$TS)[1]

srres<-ar1lnorm(sd=0.2, rho=0.8, years=2:nyears)

# 1. ADD iters to ar1lnorm
# 2. TEST ED w/ iters
