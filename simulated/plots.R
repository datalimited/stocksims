# plots.R - DESC
# plots.R

# Copyright 2003-2012 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# $Id: $

# Overall plot of SSB
fqs <- FLQuants(lapply(sims, function(x) ssb(x$stock)))
fqc <- FLQuants(lapply(sims, function(x) catch(x$stock)))

pdf(file="out/overallPlot.pdf")
print(xyplot(data~year|qname, fqs, strip=FALSE, type='l', ylab='SSB'))
dev.off()

# One page report per dataset

# stock

# df
# ssb, tb, catch, refpts, ssb/ssbMSY
# Overall plot of temporal trend of Catch as a function of factorial design of simulation
fqc <- (lapply(out, function(x) as.data.frame(catch(x$stock))))
fqc<-ldply(fqc, data.frame)
fqlh<-(lapply(out, function(x) as.data.frame(x$val$LH)))
fqlh<-ldply(fqlh, data.frame)
fqid<-(lapply(out, function(x) as.data.frame(x$val$ID)))
fqid<-ldply(fqid, data.frame)
fqed<-(lapply(out, function(x) as.data.frame(x$val$ED)))
fqed<-ldply(fqed, data.frame)
fqsel<-(lapply(out, function(x) as.data.frame(x$val$SEL)))
fqsel<-ldply(fqsel, data.frame)
fqts<-(lapply(out, function(x) as.data.frame(x$val$TS)))
fqts<-ldply(fqts, data.frame)
fqur<-(lapply(out, function(x) as.data.frame(x$val$UR)))
fqur<-ldply(fqur, data.frame)


fqF<-merge(fqc, fqlh, by=".id", all.x=TRUE)
fqF<-merge(fqF, fqid, by=".id", all.x=TRUE)
fqF<-merge(fqF, fqed, by=".id", all.x=TRUE)
fqF<-merge(fqF, fqsel, by=".id", all.x=TRUE)
fqF<-merge(fqF, fqts, by=".id", all.x=TRUE)
fqF<-merge(fqF, fqur, by=".id", all.x=TRUE)

names(fqF)<-c("stock","age","year","unit","season","area","iter","data","LH","ID","ED","TS","SEL","X1Lx","X2L","X1", "X2" ,"UR")

ggplot(fqF,aes(year,data, color=UR))+geom_line()+facet_grid(TS+ID+SEL~ED+LH, scales="free", labeller = label_both)+xlab("Year")+ylab("Catch")
