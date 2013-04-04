### Sample script for using Costello method ###

# #By Dan Ovando
# Prepared for CI/FAO Working Group
# 3/19/2013

setwd('/Users/danovando/Desktop/Bren/SFG Work/The Method/CI Working Group/Simulation Analysis')
source('Costello Functions.R')
load('input201303041234.RData')
load('CostelloInputs.Rdata')

CostelloResults<- CostelloMethod(input,CostelloInputs)
#CostelloMethod spits out two lists formatted in the requested manner. Indivdual has the predictions for each individual fishery passed in input. Group has the group prediction for all fisheries in input