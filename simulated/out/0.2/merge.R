# merge.R - DESC
# merge.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

library(FLCore)

files <- system('ls lh/*.RData', intern=TRUE)

# inputE
idx <- files[grep("inputE", files)]

load(idx[1])
inputE02 <- inputE

load(idx[2])
for(i in 1:length(inputE02))
	inputE02[[i]] <- c(inputE02[[i]], inputE[[i]])

load(idx[3])
for(i in 1:length(inputE02))
	inputE02[[i]] <- c(inputE02[[i]], inputE[[i]])

inputE <- inputE02

save(inputE, file=paste("inputE02", format(Sys.time(), "%Y%m%d%H%M"),
	".RData", sep=""))

# sims
idx <- files[grep("sims", files)]

load(idx[1])
sims02 <- sims

load(idx[2])
sims02 <- c(sims02, sims)

load(idx[3])
sims02 <- c(sims02, sims)

sims <- sims02

save(sims, file=paste("sims02", format(Sys.time(), "%Y%m%d%H%M"),
	".RData", sep=""))
