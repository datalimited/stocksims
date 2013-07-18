# debug.R - DESC
# debug.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# TODO sd=0.6, rho=0.6

# Strange catch trends in LP_ID0_NR_OW_SELF_UR0_TS60
bug <- sims[["LP_ID0_NR_OW_SELF_UR0_TS60"]]
bug <- sims[["LP_ID0_AR_OW_SELF_UR0_TS60"]]
bug <- simsOW[["LP_ID0_NR_OW_SELF_UR0_TS60"]]

plot(catch(bug$stock))

sce$LH <- sce$LH['SP']

sce$ID <- sce$ID['ID0']

sce$UR <- sce$UR['UR0']

sce$TS <- sce$TS['TS60']

#
sce$ED <- sce$ED[c('OW', 'RC', 'RC2')]
sce$ED <- sce$ED['OW']
sce$ED <- sce$ED['RC']
sce$ED <- sce$ED['RC2']
sce$ED <- sce$ED['ED0']
sce$ED <- sce$ED['ED0.6']

sce$AR <- sce$AR['NR']
sce$AR <- sce$AR['NE']

debug(oneWayTrip)

# Look at AR1 residuals
sce$LH <- sce$LH['SP']
sce$ED <- sce$ED[c('OW', 'RC', 'RC2')]
iters <- 25


#
i <- 57
x<-sims[[i]]

plot(catch(x$stock), main=c("C", names(sims[i])))


xyplot(data~year|iter, catch(x$stock), type='l', main=c("C", names(sims[i])))
xyplot(data~year|iter, fbar(x$stock), type='l', main=c("F", names(sims[i])))

plot(ssb(x$stock), main=c("SSB", names(sims[i])))
plot(rec(x$stock), main=c("Rec", names(sims[i])))



# Adding stochastic recruitment

lh <- names(sce$LH)[1]
id <- names(sce$ID)[1]
ar <- names(sce$AR)[1]
ed <- names(sce$ED)[1]
ts <- names(sce$TS)[1]

srres<-ar1lnorm(sd=0.2, rho=0.8, years=2:nyears)

# 1. ADD iters to ar1lnorm
# 2. TEST ED w/ iters

# doMC

library(doMC)
registerDoMC(3)

foreach(i=1:3) %dopar% sqrt(i)


#

combine <- function(x){
	res <- model.frame(FLQuants(catch=x$catch, stock=stock(x$stock), harvest=harvest(x$stock)))
	res <- cbind(res, bmsy=c(x$brp@refpts[2,5]))
	res <- cbind(res, id=x$code)
	res <- cbind(res, val=x$val)
	return(res)
}

simout<-	ldply(sims[1:100], combine)

for (i in seq(length(sims))[1:10]) {
	x <- sims[[i]]
	res <- model.frame(FLQuants(catch=x$catch, stock=stock(x$stock), harvest=harvest(x$stock)))
	res <- cbind(res, bmsy=c(x$brp@refpts[2,5]))
	res <- cbind(res, id=x$code)
	res <- cbind(res, val=x$val)
	write.table(res, file="TEST.csv", append=TRUE)
}


for (i in seq(length(sims))[1:10]) {
	x <- sims[[i]]
	res <- model.frame(FLQuants(catch=x$catch[,,,,,1:100], stock=stock(x$stock)[,,,,,1:100], harvest=harvest(x$stock)[,,,,,1:100]))
	res <- cbind(res, bmsy=c(x$brp@refpts[2,5]))
	res <- cbind(res, id=x$code)
	res <- cbind(res, val=x$val)
	if(i == 1)
		write.table(res, sep=",", file="TEST.csv",
			row.names=FALSE, col.names=TRUE)
	else
		write.table(res, sep=",", file="TEST.csv", append=TRUE,
			row.names=FALSE, col.names=FALSE)
}



create table test (age text,year integer,unit text,season text,area text,iter integer,catch double,stock double,harvest double,bmsy double,id text,valLH text,valID text,valAR text,valED text,valSEL text,valUR text,valTS text);

.separator ","
.headers on
.import TEST.csv test

library(RSQLite)
con <- dbConnect(SQLite(), dbname = "testR.db")
dbWriteTable(con, "TEST", res, row.names = FALSE)
dbDisconnect(con)
