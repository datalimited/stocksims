# merge.R - DESC
# merge.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $
# Created:
# Modified:

# XX {{{
# }}}


library(FLCore)

files <- system('ls lh/*.RData', intern=TRUE)

# inputE
idx <- files[grep("inputE", files)]

load(idx[1])
inputE06 <- inputE

load(idx[2])
for(i in 1:length(inputE06))
	inputE06[[i]] <- c(inputE06[[i]], inputE[[i]])

load(idx[3])
for(i in 1:length(inputE06))
	inputE06[[i]] <- c(inputE06[[i]], inputE[[i]])

save(inputE06, file=paste("inputE06", format(Sys.time(), "%Y%m%d%H%M"),
	".RData", sep=""))

# sims
idx <- files[grep("sims", files)]

load(idx[1])
sims06 <- sims

load(idx[2])
sims06 <- c(sims06, sims)

load(idx[3])
sims06 <- c(sims06, sims)

save(sims06, file=paste("sims06", format(Sys.time(), "%Y%m%d%H%M"),
	".RData", sep=""))
