# R script to extract results from results of data poor methods applied to simulated stocks 
# Chato Osio April 2013

library(plyr)
library(ggplot2)
library(ggplotFL)

# Function to reshape the simulated FLStocks and plot in ggplot to see the entire factorial design of the catches in time

#rename simulated stocks
out<-sims

fqlh<-(lapply(out, function(x) as.data.frame(x$val$LH)))
fqc <- (lapply(out, function(x) as.data.frame(catch(x$stock))))

#convert to DF
fqc<-ldply(fqc, data.frame)
fqlh<-ldply(fqlh, data.frame)

# merge DF with LH and catches
fqF<-merge(fqc, fqlh, by=".id", all.x=TRUE)

# plot 
ggplot(fqF,aes(year, data, color=.id))+geom_line()+facet_grid(~x.val.LH)

# create DF with stock IDs and merge with catch and LH
fqid<-(lapply(out, function(x) as.data.frame(x$val$ID)))
fqid<-ldply(fqid, data.frame)
fqF<-merge(fqF, fqid, by=".id", all.x=TRUE)
ggplot(fqF,aes(data,year, color=.id))+geom_line()+facet_grid(x.val.ID~x.val.LH)

# add to previous DF the Effort Dynamics ED
fqed<-(lapply(out, function(x) as.data.frame(x$val$ED)))  # ED IS CURRENTLY ALL NA NEED TO FIX
fqed<-ldply(fqed, data.frame)
fqF<-merge(fqF, fqed, by=".id", all.x=TRUE)

# plot 
ggplot(fqF,aes(year,data, color=x.val.ID))+geom_line()+facet_grid(x.val.ED+x.val.LH~x.val.ID)
ggplot(fqF,aes(year,data, color=x.val.ID))+geom_line()+facet_grid(x.val.ED+x.val.LH~x.val.ID+.id)

# add to previous SELECTIVITY and TS
# SEL was taken out and went into sensitivity testing only
#fqsel<-(lapply(out, function(x) as.data.frame(x$val$SELF)))
#fqsel<-ldply(fqsel, data.frame)
fqts<-(lapply(out, function(x) as.data.frame(x$val$TS)))
fqts<-ldply(fqts, data.frame)
fqF<-merge(fqF, fqts, by=".id", all.x=TRUE)

#plot
ggplot(fqF,aes(year,data))+geom_line()+facet_grid(x.val.ED+x.val.LH+x.val.TS~x.val.ID)


str(out[[1]]$val)

ggplot(fqF,aes(year,data, color=stock_id))+geom_line()+facet_grid(x.val.TS+x.val.ID~x.val.ED+x.val.LH, scales="free", labeller = label_both)+xlab("Year")+ylab("Catch")

# Rename tho DF to more compact names
names(fqF)<-c("stock","age","year","unit",  "season","area","iter","data","LH","ID","ED","TS")

ggplot(fqF,aes(year,data, color=stock))+geom_line()+facet_grid(TS+ID~ED+LH, scales="free", labeller = label_both)+xlab("Year")+ylab("Catch")

# fqF is the the DF where the stock biomass is stored but no reference points to give a b_bmsy

# END generating DF with ssb and elements of factorial design to plot out
#############################################################################################################



######################################################################################################
# Code generated before models results where available for developing different plotting functions.
# create a data frame with the scenario of each stock
######################################################################################################


# Final Combine
combine<-function(x){
										res=model.frame(FLQuants(catch=x$catch, stock=stock(x$stock)))
										res=cbind(res, bmsy=c(x$refpts[2,5]))
										res=cbind(res, id=name(x$stock))
										res=cbind(res, val=x$val)
					return(res)}

simout<-	lapply(sims, combine)
simout<-  Reduce(rbind,simout)

ggplot(simout,aes(year,catch))+geom_line(aes(color=id))+facet_grid(val.TS+val.ID+val.AR~val.ED+val.LH+val.UR, scales="free", labeller = label_both)+xlab("Year")+ylab("Catch")


#########################################################################################################
# Create a simulated OUTFILE for when the methods results where not available yet: basically get the b_msy and add some noise to it and to the Upper and Lower bounds of it


names(simout)
#Estimate the true b_bmsy
simout$b_bmsy_true<-simout$stock/simout$bmsy

#keep vars of interest and clean up names
simout<-subset(simout, select=c("year","catch","stock","b_bmsy_true", "id","val.LH","val.ID","val.ED","val.SEL","val.TS", "val.AR", "val.UR" ))
names(simout)<-c("year","catch","stock","b_bmsy_true", "stock_id","LH","ID","ED","SEL","TS", "AR", "UR")

# see how it looks like
ggplot(simout,aes(year,catch))+geom_point()+facet_grid(TS+ID+UR~ED+LH+AR, scales="free", labeller = label_both)+xlab("Year")+ylab("Catch")

ggplot(simout,aes(year,b_bmsy_true))+geom_point()+facet_grid(TS+ID+UR~ED+LH+AR, scales="free", labeller = label_both)+xlab("Year")+ylab("b_bmsy_true")

#############################################################################################

# CATCH MSY METHOD ouput file is called OutputLoL
out<-OutputLoL

# Nedd to extract for each stock the estimated ,b_bmsyUpper ,b_bmsyLower, b_bmsy_iq25 , b_bmsy_iq75 , seed, convergence, n_iterations , effective_sample_size , run_time , method_id, LH=, ID=, ED=,  TS=
# rename the list with the stock ids
names(out)<-unlist(lapply(out, function(x) x$stock_id))

#get variables of interest and transform list in DF
outcatchmsy<-lapply(out, function(x) data.frame(year=x$year, b_bmsy=x$b_bmsy[-length(x$b_bmsy)], b_bmsyUpper=x$b_bmsyUpper, b_bmsyLower=x$b_bmsyLower, b_bmsy_iq25=x$b_bmsy_iq25, b_bmsy_iq75=x$b_bmsy_iq75, b_bmsy2U=x$b_bmsy2U, b_bmsy2L=x$b_bmsy2L, seed=x$seed, convergence=x$convergence, n_iterations=x$n_iterations, effective_sample_size=x$effective_sample_size, run_time=x$run_time, method_id=x$method_id ))

outcatchmsy<-ldply(outcatchmsy, data.frame)
names(outcatchmsy)[names(outcatchmsy)==".id"]<-"stock_id"
names(outcatchmsy)[names(outcatchmsy)=="b_bmsy"]<-"b_bmsy_est"
# plot the b_bmsy estimated by CatchMsy and the upper and lower bounds (b_bmsy2L)
library(ggplot2)
ggplot(outcatchmsy, aes(year, b_bmsy_est))+geom_line()+geom_line(aes(year, b_bmsy_est, color="red"))+facet_wrap(~stock_id)+geom_ribbon(data=outcatchmsy,aes(ymin=b_bmsy2L,ymax=b_bmsy2U),alpha=0.3)

# plot the b_bmsy estimated by CatchMsy and the upper and lower bounds (b_bmsyLower)
ggplot(outcatchmsy, aes(year, b_bmsy_est))+geom_line()+geom_line(aes(year, b_bmsy_est, color="red"))+facet_wrap(~stock_id)+geom_ribbon(data=outcatchmsy,aes(ymin=b_bmsyLower,ymax=b_bmsyUpper),alpha=0.3)

# there seem to be three types of problems with the estimated b_bmsy from the CatachMSY method: one is that there are step drops in some yearly estimates that are likely driven by drops in estimated biomass. The upper and lower b_bmsy are constant through years, this is not according to the specifications of the simulation testing and values should be returned by year.
# In several stocks and years there are NaN caused by the calculation method of 
#BoverBmsy2upper<-1+sqrt(1-ct/exp(mean(log(msy)))) 
#In addition: Warning messages:
#1: In sqrt(1 - ct/exp(mean(log(msy)))) : NaNs produced
# here basically if ct (catch) is higher than mean msy you get a sqrt of a negative value and get an NAN. This needs to be fixed. In fact why not derive the quantiles from the R5 data set where there are multiple yearly estimates doing something like this: R6<-aggregate(log(B_Bmsy)~as.numeric(Count)+Stock, data=R5, quantile) ?
# An additional issue seems to be the estimated b_bmsy2L and b_bmsy2U that do not incorporate in their range the estimated b_bmsy.

# MERGE data from with true b_bmsy from stock simulation with simulated stocks
#clean up names
catchmsy<-merge(outcatchmsy, simout, by=c("year","stock_id"), all.x=TRUE)

#plot true vs estimated b_bmsy
ggplot(catchmsy, aes(year, b_bmsy_true))+geom_line()+geom_line(aes(year, b_bmsy_est, color=factor(method_id)))+facet_grid(LH+TS~ED+ID,scales="free_y",space="free_y", labeller = label_both)+geom_ribbon(data=catchmsy,aes(ymin=b_bmsyLower,ymax=b_bmsyUpper),alpha=0.3)+ylim(0,4)

# calculate proprtional errors
catchmsy$prop.error<-with(catchmsy,(b_bmsy_true-b_bmsy_est)/b_bmsy_true)

# plot prop errors by fectorial design
lh<-ggplot(catchmsy,aes(LH, prop.error))+geom_boxplot()
id<-ggplot(catchmsy,aes(ID, prop.error))+geom_boxplot()
ed<-ggplot(catchmsy,aes(ED, prop.error))+geom_boxplot()
ts<-ggplot(catchmsy,aes(TS, prop.error))+geom_boxplot()
grid.arrange(lh, id, ed, ts, ncol=2)



#################################################################################
# COSTELLO METHOD returns an object list called CostelloResults

summary(CostelloResults)
#contains a group level and an Individual results, presume we are looking at the individual ones
summary(CostelloResults$Individual)
# first issue is that stock_id is numeric and not the same as in the sims, needs to be fixed

#have a look at one stock
(CostelloResults$Individual[1,])
(CostelloResults$Individual[1,])

# rename the costello results to "out"
out<-CostelloResults

library("plyr")
setwd("C:/Users/osioogi/Dropbox/FAOSim/Sims")


setwd("C:/Users/osioogi/Dropbox/FAOSim/Costello")

source('Costello Functions.R')

load('CostelloInputs.Rdata')
load("C:/Users/osioogi/Dropbox/FAOSim/sims/input201305081053.RData")

CostelloResults<- CostelloMethod(input,CostelloInputs)
#CostelloMethod spits out two lists formatted in the requested manner. Indivdual has the predictions for each individual fishery passed in input. Group has the group prediction for all fisheries in input

# COSTELLO METHOD returns an object list called CostelloResults

summary(CostelloResults)
#contains a group level and an Individual results, presume we are looking at the individual ones
summary(CostelloResults$Individual)
# first issue is that stock_id is numeric and not the same as in the sims, needs to be fixed

# rename the costello results to "outfile"
outfilecostello<-CostelloResults$Individual
names(outfilecostello)[names(outfilecostello)=="years"]<-"year"
names(outfilecostello)[names(outfilecostello)=="b_bmsy"]<-"b_bmsy_est"

# plot the b_bmsy estimated by CatchMsy and the upper and lower bounds (b_bmsy2L)
library(ggplot2)
ggplot(outfilecostello, aes(year, b_bmsy_est))+geom_line()+geom_line(aes(year, b_bmsy_est, color="red"))+facet_wrap(~stock_id,scales="free_y")+geom_ribbon(data=outfilecostello,aes(ymin=b_bmsyLower,ymax=b_bmsyUpper),alpha=0.3)

# plot the b_bmsy estimated by Costello and the upper and lower bounds (b_bmsyLower)
ggplot(outfilecostello, aes(year, b_bmsy_est))+geom_line()+geom_line(aes(year, b_bmsy_est, color="red"))+facet_wrap(~stock_id,scales="free_y" )+geom_ribbon(data=outfilecostello,aes(ymin=b_bmsy_iq25,ymax=b_bmsy_iq75),alpha=0.3)

# Now that results of Costello are created, compare with the true biomass estimates from the outfile extracted from the simulated stocks at beginning of the script

# merge results with original estimates
#names(simout)[names(simout)=="id"]<-"stock_id"
costello<-merge(simout, outfilecostello, by=c("stock_id", "year"), all.y=TRUE)

#plot true vs estimated b_bmsy
ggplot(costello, aes(year, b_bmsy_true))+geom_line()+geom_line(aes(year, b_bmsy_est, color=factor(method_id)))+facet_grid(LH+ID~TS+ED,scales="free_y",space="free_y", labeller = label_both)+geom_ribbon(data=costello,aes(ymin=b_bmsyLower,ymax=b_bmsyUpper),alpha=0.3)+ylim(0,4)

# calculate proprtional errors
costello$prop.error<-with(costello,(b_bmsy_true-b_bmsy_est)/b_bmsy_true)

# plot prop errors by fectorial design
lh<-ggplot(costello,aes(LH, prop.error))+geom_boxplot()
id<-ggplot(costello,aes(ID, prop.error))+geom_boxplot()
ed<-ggplot(costello,aes(ED, prop.error))+geom_boxplot()
ts<-ggplot(costello,aes(TS, prop.error))+geom_boxplot()
grid.arrange(lh, id, ed, ts, ncol=2)

######################################################################
# Combine the outputs from each method
names(costello)
names(catchmsy)
catchmsy<-subset(catchmsy, select= -c(b_bmsy2U,b_bmsy2L))

temp<-rbind(costello, catchmsy)
ggplot(temp, aes(year, b_bmsy_true))+geom_line()+geom_line(aes(year, b_bmsy_est, color=factor(method_id)))+facet_grid(TS+ED~LH+ID,scales="free_y",space="free_y", labeller = label_both)+geom_ribbon(data=temp,aes(ymin=b_bmsyLower,ymax=b_bmsyUpper),alpha=0.3)+ylim(0,4)

# plot prop errors by fectorial design
lh<-ggplot(temp,aes(LH, prop.error, color=method_id))+geom_boxplot()
id<-ggplot(temp,aes(ID, prop.error, color=method_id))+geom_boxplot()
ed<-ggplot(temp,aes(ED, prop.error, color=method_id))+geom_boxplot()
ts<-ggplot(temp,aes(TS, prop.error, color=method_id))+geom_boxplot()
grid.arrange(lh, id, ed, ts, ncol=2)
