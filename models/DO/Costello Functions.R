CostelloMethod<- function(input,CostelloInputs)
{
	StartTime<- proc.time()[1]
	CostelloMimic<- CostelloInputs$Model
	StdBin<- CostelloInputs$StdBin
	
	ids<- names(input)
	
	### Set some options for data fitting ###
	Options<-NA
	Options$Interp<- 0.1
	Options$Developed<- 0.15
	Options$MaxMissing<- 0.1
	Options$MinYears<- 7
	Options$LargeSizeBin<- 150
	Options$SmallSizeBin<- 50
	
	InputMatrix<- as.data.frame(matrix(NA,nrow=length(ids)*60,ncol=5))
	colnames(InputMatrix)<- c('id','idname','year','catch','linf')
	c=0
	for (i in 1:length(ids))
	{
		Temp<- input[i]
		InputMatrix[(c+1):(c+dim(Temp[[1]]$catch)[1]),]<- c(i,ids[i],Temp[[1]]$catch,Temp[[1]]$linf)
		c<- c+dim(Temp[[1]]$catch)[1]
	}
	
	InputMatrix<- InputMatrix[is.na(InputMatrix$id)==F,]
	
	ProcessedSimulatedData<- ProcessSimulationData(InputMatrix,Options)
	
	Coeff<- t(as.matrix(CostelloMimic$coefficients,nrow=1,ncol=length(CostelloMimic$coefficients)))
	ProcessedSimulatedData$constant<- 1
	RegVars<- c('Catch4','Catch3','Catch2','Catch1','CatchNow','ttomax','initslope','cmax','meancatch','runningratio','Stype2','Stype3')
	WhereVars<- match(RegVars,colnames(ProcessedSimulatedData))
	SimulatedIndependent<- ProcessedSimulatedData[,RegVars]
	SimulatedIndependent<- cbind(ProcessedSimulatedData$constant,SimulatedIndependent)
	PredictedSimulatedBvBmsy<- Coeff %*% t(SimulatedIndependent)
	ProcessedSimulatedData$BvBmsy<- as.numeric(t(PredictedSimulatedBvBmsy))
	ProcessedSimulatedData$model<- 1
	
	
	SimCore<- coreprep(ProcessedSimulatedData$id, ProcessedSimulatedData$model, ProcessedSimulatedData$year, ProcessedSimulatedData$BvBmsy)
	
	CorrectedSim<- transbias(SimCore[is.na(SimCore$bvb)==F,],StdBin,2000,0.9,0)
	
	
	IndOutput<- as.data.frame(matrix(NA,nrow=dim((CorrectedSim$ind))[1],ncol=13))
	colnames(IndOutput)<- c('stock_id','b_bmsy','b_bmsyUpper','b_bmsyLower','b_bmsy_iq25','b_bmsy_iq75','years','seed','convergence','n_iterations','effective_sample_size','run_time','method_id')
	
	IndOutput$stock_id<- CorrectedSim$ind$id
	IndOutput$b_bmsy<- CorrectedSim$ind$mean
	IndOutput$b_bmsyUpper<- CorrectedSim$ind$top
	IndOutput$b_bmsyLower<- CorrectedSim$ind$bot
	IndOutput$b_bmsy_iq25<- CorrectedSim$ind$iq25
	IndOutput$b_bmsy_iq75<- CorrectedSim$ind$iq75
	IndOutput$years<- CorrectedSim$ind$years
	IndOutput$method_id<- 'Costello'
	
	
	Output<- as.data.frame(matrix(NA,nrow=dim((CorrectedSim$med))[1],ncol=13))
	colnames(Output)<- c('stock_id','b_bmsy','b_bmsyUpper','b_bmsyLower','b_bmsy_iq25','b_bmsy_iq75','years','seed','convergence','n_iterations','effective_sample_size','run_time','method_id')
	
	Output$stock_id<- 1 #CorrectedSim$med$id
	Output$b_bmsy<- CorrectedSim$med$med
	Output$b_bmsyUpper<- CorrectedSim$med$top
	Output$b_bmsyLower<- CorrectedSim$med$bot
	Output$b_bmsy_iq25<- CorrectedSim$med$iq25
	Output$b_bmsy_iq75<- CorrectedSim$med$iq75
	Output$years<- CorrectedSim$med$year
	Output$method_id<- 'Costello'
	Output$run_time<- proc.time()[1]-StartTime

Results<- list(Individual=IndOutput,Group=Output)
return(Results)	
}

ProcessSimulationData<- function(chist,Options)
{
########### MimicData ##################
#DAN OVANDO#
#Prepared for CI Unassessed Fisheries Working Group
#Feb 23#
# chist<-InputMatrix
# chist<- rawdata
rpack<- as.data.frame(matrix(NA,nrow=1,ncol=dim(chist)[2])) #make blank object to store results in
colnames(rpack)<- colnames(chist)
 rpack$invage<- NA
 rpack$Catch4<- NA
 rpack$Catch3<- NA
 rpack$Catch2<- NA
 rpack$Catch1<- NA
 rpack$CatchNow<- NA
 rpack$ttomax<- NA
 rpack$initslope<- NA
 rpack$cmax<- NA
 rpack$meancatch<- NA
 rpack$runningratio<- NA
 rpack$StypeNum<- NA
 rpack$StypeName<- NA
 
### Add in variables to be created here
####################
### Process data ###
####################

fs<- unique(chist$id)

for (f in 1:length(fs))
{
	temp<- chist[chist$id==fs[f],] #find catch history
	trpack<- as.data.frame(matrix(NA,nrow=dim(temp)[1],ncol=dim(rpack)[2]))
	colnames(trpack)<- colnames(rpack)
	WhereMatches<- match(colnames(chist),colnames(trpack))
	trpack[,WhereMatches]<- temp
	
	cmax<- max(temp$catch,na.rm=T) #max recorded landings
	temp$catch<- temp$catch/cmax #scale landings by max
	start<- which(temp$catch>Options$Developed)[1] #find beginning of catch history
	
	if (is.na(start)==T)
	{
	start<- 0
	}
	
	temp<- temp[start:dim(temp)[1],] #shorten catch history to appropriate length
	trpack<- trpack[start:dim(trpack)[1],] #shorten catch history to appropriate length
	trpack<- trpack[5:dim(trpack)[1],]

	missing_tally<- sum(is.na(temp$catch))/length(temp$catch) #find amount of missing data
	
	if (missing_tally>Options$MaxMissing | length(temp$catch)<Options$MinYears | start==0)
	{
		warning(paste('FISHERY',fs[f],'DOES NOT HAVE ENOUGH CATCH HISTORY TO RUN'))
	}
	c<- 0
if (missing_tally<=Options$MaxMissing & length(temp$catch)>=Options$MinYears & start>0) #If there is enought catch data, continue data preperation
{
	#Fill in missing data by interpolation
	interp<- approx(temp$catch,xout=which(is.na(temp$catch)==T))
	temp$catch[interp$x]<- interp$y
	trpack$ttomax<- which(temp$catch==1)[1] #calculate time to max catch
			trpack$initslope<- sum(temp$catch[4:6])/sum(temp$catch[1:3]) #slope over the first few years
			trpack$cmax<- cmax #max catch
			trpack$meancatch<- mean(temp$catch,na.rm=T) #average catch

		for (t in 1:(dim(temp)[1]-4)) #loop over each fisheries timline and fill in catch data
		{
			c<- c+1
			trpack$invage[t]<- (dim(temp)[1]-3)-t #calculate inverse age
			trpack$Catch4[t]<- temp$catch[t] #create lagged catch
			trpack$Catch3[t]<- temp$catch[t+1] #create lagged catch
			trpack$Catch2[t]<- temp$catch[t+2] #create lagged catch
			trpack$Catch1[t]<- temp$catch[t+3] #create lagged catch
			trpack$CatchNow[t]<- temp$catch[t+4] #create lagged catch
			trpack$runningratio[t]<- temp$catch[t+4]/max(temp$catch[1:(t+3)]) #ratio of current harvest to prior maximum
		}#close loop over t

	if (is.na(trpack$StypeNum)[1])
	{
		
		if (trpack$linf[1]==30)
		{
		trpack$StypeNum<- 2
		trpack$StypeName<- 'sardine'
		}
		if (trpack$linf[1]==70)
		{
		trpack$StypeNum<- 3
		trpack$StypeName<- 'haddock'
		}	
			if (trpack$linf[1]==150)
		{
			trpack$StypeNum<- 1
			trpack$StypeName<- 'tuna'
		}
		

	}
	
		
	rpack<- rbind(rpack,trpack) #stack fisheries on top of each other


	} #close if over missing_tally

} #close loop over fisheries

rpack<-rpack[2:dim(rpack)[1],]

# rpack<- ProcessedData

###Make Species Type Fixed Effects ###
OriginalWidth<- dim(rpack)[2]
UniqueSpeciesTypes<- sort(unique(rpack$StypeNum))
UniqueSpeciesTypes<- UniqueSpeciesTypes[2:length(UniqueSpeciesTypes)]
FixedBlanks<- as.data.frame(matrix(0,nrow= dim(rpack)[1],ncol=length(UniqueSpeciesTypes)))
colnames(FixedBlanks)<-paste('Stype',UniqueSpeciesTypes,sep='')

rpack<- cbind(rpack,FixedBlanks)

for (s in 1:length(UniqueSpeciesTypes))
{
	where<- rpack$StypeNum==UniqueSpeciesTypes[s]
	rpack[where,OriginalWidth+s]<- 1
}


return(rpack)
} #Close function

coreprep<- function(id,mod,years,bvb)
{
	t<- cbind(id,mod,years,bvb)
	colnames(t)=c('id','mod','years','bvb')
	core<- as.data.frame(t)
	core$bvb<- log(core$bvb)
	return(core)	
}


transbias<- function(core,stdbin,J,bin,testvariance)
{
#Function for applying retransformation bias routine described in Costello et al. 2012
#core: the core data passed to function
# residser: an alternative way to run the method, not currently used
# stdbin: bins of standard deviations from the regression over time
# J: number of iterations
# bin: bin betwen small vs large, always leave as 0.9
####################
### Perpare Data ###
####################	
# require(matlab)
 # core<- FAOCore[is.na(FAOCore$bvb)==F,]
 # stdbin<- STDBin
 # J<- 1000
 # bin<- 0.9
 # testvariance<-0 

stdbin$high[is.na(stdbin$high)]<- 0
stdbin$low[is.na(stdbin$low)]<- 0
originalstd<- stdbin

um<- sort(unique(core$mod)) #unique models in core

core$bvb<- exp(core$bvb) #exponentiate raw bvbmsy values 

years<- sort(unique(core$years)) #unique years

fs<- sort(unique(core$id)) #unique fishery ids

bt<- matrix(NA,nrow=dim(core)[1],ncol=1) #blank matrix for results

bt[core$bvb>=bin,1]<- 1 #mark bin locations

bt[core$bvb<bin,1]<- 0 #mark bin locations

core<- cbind(core,bt)

core<- as.data.frame(core)

indtemp<- matrix(NA,nrow=dim(core)[1],ncol=7) #Matrix for individual fishery results

indtemp<- as.data.frame(indtemp)

colnames(indtemp)=c('id','years','mean','top','bot','iq75','iq25')

colnames(core)=c('id','mod','years','bvb','bin') #storage matrix

c<- matrix(NA,nrow=length(years),ncol=6)
c<- as.data.frame(c)	
colnames(c)=c('year','med','top','bot','iq75','iq25')


bin<- matrix(NA,nrow=3,ncol=4)
bin<- as.data.frame(bin)
colnames(bin)=c('<4','4to8','8to12','>12')
rownames(bin)=c('med','bot','top')


collapsed<- matrix(NA,nrow=length(years),ncol=4)
colnames(collapsed)=c('year','med','bot','top')
collapsed<- as.data.frame(collapsed)

under<- matrix(NA,nrow=length(years),ncol=4)
colnames(under)=c('year','med','bot','top')
under<- as.data.frame(under)

over<- matrix(NA,nrow=length(years),ncol=4)
colnames(over)=c('year','med','bot','top')
over<- as.data.frame(over)

####################################
###Perform retransformation bias ###
####################################

	for (y in 1:length(years)) #loop over years
	{
		
		where<- core$years==years[y] #find year locations
		
		tcore<- core[where,] #temporary core
		
		jstore<- matrix(NA,nrow=dim(tcore)[1],ncol=J) #blank matrix for bootstrap
		
		for (m in 1:length(um)) #loop over models
		{
			
			stdbin<- originalstd #reset stdbin
			if (um[m]==99)
			{
				stdbin<- inv_stdbin #ignore this, for invertebrate investigation
			}
			
			whereh<- tcore$mod==um[m] & tcore$bin==1 & is.na(tcore$bvb)==F	#find fisheries in high bin		
			wherel<- tcore$mod==um[m] & tcore$bin==0 & is.na(tcore$bvb)==F #find fisheries in low bin	
			
			
			### Series of steps to deal with some time issues ###
			if (years[y]<=max(stdbin$years) & years[y]>=1950)
			{
				wheres<- stdbin$years==years[y] & stdbin$mod==um[m]
				
				highstd<- stdbin$high[wheres]
				
				lowstd<- stdbin$low[wheres]		
			}
			if (years[y]<min(stdbin$years))
			{
				wheres<- stdbin$years==min(stdbin$years) & stdbin$mod==um[m]
				
				highstd<- stdbin$high[wheres]
				
				lowstd<- stdbin$low[wheres]			
			}
					if (years[y]>max(stdbin$years))
			{
				wheres<- stdbin$years==max(stdbin$years) & stdbin$mod==um[m]
				
				highstd<- stdbin$high[wheres]
				
				lowstd<- stdbin$low[wheres]			
				
			}
		
		 if (testvariance==1) #Tool for doing some diagnostics
		 {
		 	highstd<- highstd*vartest
		 	lowstd<- lowstd*vartest
		 }
		
		
		### Apply error terms ###
		
			jstd<- rnorm(sum(whereh,na.rm=T)*J)
			dim(jstd)<- c(sum(whereh,na.rm=T),J)
			# jstore[whereh,]<- repmat(log(tcore$bvb[whereh]),1,J) +highstd*jstd
			jstore[whereh,]<- rep(log(tcore$bvb[whereh]),J) +highstd*jstd
			
			jstd<- rnorm(sum(wherel,na.rm=T)*J)
			dim(jstd)<- c(sum(wherel,na.rm=T),J)
			# jstore[wherel,]<- repmat(log(tcore$bvb[wherel]),1,J) +lowstd*jstd
			jstore[wherel,]<- rep(log(tcore$bvb[wherel]),J) +lowstd*jstd

		}
		
		
		### Store Results ###
		
		### % collapsed###
		ctemp<- exp(jstore)<=.2
		ctemp<- colSums(ctemp,na.rm=T)/sum(where)
		csort<- sort(ctemp)
		ctop<- csort[ceiling(0.975*length(csort))]
		cbot<- csort[ceiling(0.025*length(csort))]
		cmed<- median(csort,na.rm=T)
		
		### % B > Bmsy ###
		otemp<- exp(jstore)<1
		otemp<- colSums(otemp,na.rm=T)/sum(where)
		osort<- sort(otemp)
		otop<- osort[ceiling(0.975*length(osort))]
		obot<- osort[ceiling(0.025*length(osort))]
		omed<- median(osort,na.rm=T)
		
		### % B<Bmsy ###
		utemp<- exp(jstore)>=1
		utemp<- colSums(utemp,na.rm=T)/sum(where)
		usort<- sort(utemp)
		utop<- usort[ceiling(0.975*length(usort))]
		ubot<- usort[ceiling(0.025*length(usort))]
		umed<- median(usort,na.rm=T)


		### % in different bins ###
		
		b1temp<- exp(jstore)<=0.4
		b1temp<- colSums(b1temp,na.rm=T)/sum(where)
		b1sort<- sort(b1temp)
		b1top<- b1sort[ceiling(0.975*length(b1sort))]
		b1bot<- b1sort[ceiling(0.025*length(b1sort))]
		b1med<- median(b1sort,na.rm=T)

		b2temp<- exp(jstore)<=0.8 & exp(jstore)>0.4
		b2temp<- colSums(b2temp,na.rm=T)/sum(where)
		b2sort<- sort(b2temp)
		b2top<- b2sort[ceiling(0.975*length(b2sort))]
		b2bot<- b2sort[ceiling(0.025*length(b2sort))]
		b2med<- median(b2sort,na.rm=T)

		b3temp<- exp(jstore)<=1.2 & exp(jstore)>0.8
		b3temp<- colSums(b3temp,na.rm=T)/sum(where)
		b3sort<- sort(b3temp)
		b3top<- b3sort[ceiling(0.975*length(b3sort))]
		b3bot<- b3sort[ceiling(0.025*length(b3sort))]
		b3med<- median(b3sort,na.rm=T)

		b4temp<- exp(jstore)>1.2
		b4temp<- colSums(b4temp,na.rm=T)/sum(where)
		b4sort<- sort(b4temp)
		b4top<- b4sort[ceiling(0.975*length(b4sort))]
		b4bot<- b4sort[ceiling(0.025*length(b4sort))]
		b4med<- median(b4sort,na.rm=T)		

		
		### Store results ###
		
		collapsed[y,]<- c(years[y],cmed,cbot,ctop)

		over[y,]<- c(years[y],omed,obot,otop)

		under[y,]<- c(years[y],umed,ubot,utop)
				
		jtemp<- apply(exp(jstore),2,median,na.rm=T) #calculate median of each column
		
		itemp<- apply(exp(jstore),1,mean,na.rm=T)
		
		isort<- t(apply(exp(jstore),1,sort))
				
		top<- isort[,ceiling(0.975*dim(isort)[2])]

		bot<- isort[,ceiling(0.025*dim(isort)[2])]
		
		###Store results for individual fisheries###
		
		indbox=boxplot(t(jstore),plot=F)
		
		indtemp[where,1]<- core$id[where]
		indtemp[where,2]<- years[y]
		indtemp[where,3]<- itemp
		indtemp[where,4]<- top
		indtemp[where,5]<- bot
		indtemp[where,6]<- exp(indbox$stats[4,])
		indtemp[where,7]<- exp(indbox$stats[2,])

		
		jsort<- sort(jtemp) #Sort medians
	
		bin[,1]<- c(b1med,b1bot,b1top)
		bin[,2]<- c(b2med,b2bot,b2top)
		bin[,3]<- c(b3med,b3bot,b3top)
		bin[,4]<- c(b4med,b4bot,b4top)


		med<- mean(jsort,na.rm=T) #mean of medians
		top<- jsort[ceiling(0.975*length(jsort))] #upper CI
		bot<- jsort[ceiling(0.025*length(jsort))] #lower CI
		box<- boxplot(jsort,plot=F)
		
		
		
		c[y,]<- c(years[y],med,top,bot,box$stats[4],box$stats[2])
		# if (y==(length(years)-17))
		# {
			# show(dim(jstore))
		# quartz()
		# a=boxplot(t(jstore))
		# show(a)		
		# }
	}

output<- list(med=c,ind=indtemp,collapsed=collapsed,over=over,under=under,bin=bin,dist=jsort)
return(output)	
}

ProcessSimulationData<- function(chist,Options)
{
########### MimicData ##################
#DAN OVANDO#
#Prepared for CI Unassessed Fisheries Working Group
#Feb 23#
# chist<-InputMatrix
# chist<- rawdata
rpack<- as.data.frame(matrix(NA,nrow=1,ncol=dim(chist)[2])) #make blank object to store results in
colnames(rpack)<- colnames(chist)
 rpack$invage<- NA
 rpack$Catch4<- NA
 rpack$Catch3<- NA
 rpack$Catch2<- NA
 rpack$Catch1<- NA
 rpack$CatchNow<- NA
 rpack$ttomax<- NA
 rpack$initslope<- NA
 rpack$cmax<- NA
 rpack$meancatch<- NA
 rpack$runningratio<- NA
 rpack$StypeNum<- NA
 rpack$StypeName<- NA
 
### Add in variables to be created here
####################
### Process data ###
####################

fs<- unique(chist$id)

for (f in 1:length(fs))
{
	temp<- chist[chist$id==fs[f],] #find catch history
	trpack<- as.data.frame(matrix(NA,nrow=dim(temp)[1],ncol=dim(rpack)[2]))
	colnames(trpack)<- colnames(rpack)
	WhereMatches<- match(colnames(chist),colnames(trpack))
	trpack[,WhereMatches]<- temp
	
	cmax<- max(temp$catch,na.rm=T) #max recorded landings
	temp$catch<- temp$catch/cmax #scale landings by max
	start<- which(temp$catch>Options$Developed)[1] #find beginning of catch history
	
	if (is.na(start)==T)
	{
	start<- 0
	}
	
	temp<- temp[start:dim(temp)[1],] #shorten catch history to appropriate length
	trpack<- trpack[start:dim(trpack)[1],] #shorten catch history to appropriate length
	trpack<- trpack[5:dim(trpack)[1],]

	missing_tally<- sum(is.na(temp$catch))/length(temp$catch) #find amount of missing data
	
	if (missing_tally>Options$MaxMissing | length(temp$catch)<Options$MinYears | start==0)
	{
		warning(paste('FISHERY',fs[f],'DOES NOT HAVE ENOUGH CATCH HISTORY TO RUN'))
	}
	c<- 0
if (missing_tally<=Options$MaxMissing & length(temp$catch)>=Options$MinYears & start>0) #If there is enought catch data, continue data preperation
{
	#Fill in missing data by interpolation
	interp<- approx(temp$catch,xout=which(is.na(temp$catch)==T))
	temp$catch[interp$x]<- interp$y
	trpack$ttomax<- which(temp$catch==1)[1] #calculate time to max catch
			trpack$initslope<- sum(temp$catch[4:6])/sum(temp$catch[1:3]) #slope over the first few years
			trpack$cmax<- cmax #max catch
			trpack$meancatch<- mean(temp$catch,na.rm=T) #average catch

		for (t in 1:(dim(temp)[1]-4)) #loop over each fisheries timline and fill in catch data
		{
			c<- c+1
			trpack$invage[t]<- (dim(temp)[1]-3)-t #calculate inverse age
			trpack$Catch4[t]<- temp$catch[t] #create lagged catch
			trpack$Catch3[t]<- temp$catch[t+1] #create lagged catch
			trpack$Catch2[t]<- temp$catch[t+2] #create lagged catch
			trpack$Catch1[t]<- temp$catch[t+3] #create lagged catch
			trpack$CatchNow[t]<- temp$catch[t+4] #create lagged catch
			trpack$runningratio[t]<- temp$catch[t+4]/max(temp$catch[1:(t+3)]) #ratio of current harvest to prior maximum
		}#close loop over t

	if (is.na(trpack$StypeNum)[1])
	{
		
		if (trpack$linf[1]==30)
		{
		trpack$StypeNum<- 2
		trpack$StypeName<- 'sardine'
		}
		if (trpack$linf[1]==70)
		{
		trpack$StypeNum<- 3
		trpack$StypeName<- 'haddock'
		}	
			if (trpack$linf[1]==150)
		{
			trpack$StypeNum<- 1
			trpack$StypeName<- 'tuna'
		}
		

	}
	
		
	rpack<- rbind(rpack,trpack) #stack fisheries on top of each other


	} #close if over missing_tally

} #close loop over fisheries

rpack<-rpack[2:dim(rpack)[1],]

# rpack<- ProcessedData

###Make Species Type Fixed Effects ###
OriginalWidth<- dim(rpack)[2]
UniqueSpeciesTypes<- sort(unique(rpack$StypeNum))
UniqueSpeciesTypes<- UniqueSpeciesTypes[2:length(UniqueSpeciesTypes)]
FixedBlanks<- as.data.frame(matrix(0,nrow= dim(rpack)[1],ncol=length(UniqueSpeciesTypes)))
colnames(FixedBlanks)<-paste('Stype',UniqueSpeciesTypes,sep='')

rpack<- cbind(rpack,FixedBlanks)

for (s in 1:length(UniqueSpeciesTypes))
{
	where<- rpack$StypeNum==UniqueSpeciesTypes[s]
	rpack[where,OriginalWidth+s]<- 1
}


return(rpack)
} #Close function

