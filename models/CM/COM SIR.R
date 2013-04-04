#################COM SIR ###########################################
#  Catch only model implemented by C.V.Minte-Vera                                             
#  Version March  18, 2013                                                  <><   <><                          
#  Vasconcellos and Cochrane 2005                 <><                                   <><          
#  Biomass dynamic (Schaefer) and harvest dynamic model (logistic)                    
#  assumed the initial harvest rate equals Catch first year / Biomass first year          
#  Biomass first year = carrying capacity                                         <><                
#  lognormal likehood for catch data w/ observation error CV =0.4                           
#  Estimation usi ng Bayesian Sampling Importance Resampling          <><                  
#  Joint prior is the important function, i.e. resampling proportional to the likelihood   
#################   <><   <><   <><   ####################################

#  <>< #  COUNTER,  counter for stock = i
i<- 2

	
# <><  # SIR Controls 
MyData="input201303061732.RData"
My.N.Sim= 5000000  # numbers of samples from the importance function
My.N.Post=5000 # number of samples from the posterior (ressampling part)
My.seed= ceiling(runif(1,0,1e6))# random seed same procedure as ThorII
My.CV = 0.4 ## CV for observations - i.e. for Catch data 
# <>< # Directories 
# for Procedure
ProcDir="C:\\Users\\cminte\\Documents\\Carolina2013\\Assessoria\\EUA\\COM  SIR Final\\"
ProcDir <- "./"
# for Data
MyDir="C:\\Users\\cminte\\Documents\\Carolina2013\\Assessoria\\EUA\\COM  SIR Final\\"
MyDir <- "./"
# for Results
ResDir="C:\\Users\\cminte\\Documents\\Carolina2013\\Assessoria\\EUA\\COM  SIR Final\\"



#  <>< #  PROCEDURE section
#load the functions needed for COM SIR
source(paste(ProcDir, "FunctionsCOM.R",sep="")) ## load  new functions
###

#  <>< #  DATA section
	# load data change data file above as needed
load(paste(MyDir,MyData,sep=""))
    # stocks
#input is the name of the object created for the simulation testing
stocks_id=names(input)
StockName=stocks_id[i]
ct<- input[[i]]$catch$data
yr<- input[[i]]$catch$year
n<- length(yr)
MyData<-as.data.frame(matrix(NA,n,2,dimnames=list(as.character(yr),c("catch", "year"))))
MyData[,1]<-ct
MyData[,2]<-yr


#  <>< # PRIORs section
# Prior for K  is ln(K) ~ U( ln(min.K), ln(max.K))
# bound follows Martell and Froese 2012
min.K=max(ct)
max.K=100*min.K

# Prior on r based on "resilience" following Froese 2012 Table 1

#asymptotic length
Linf<- input[[i]]$linf
#relationship between ln(Linf) and ln(k.growth) based on Froese et al in prep (Table 2 of paper draft sent by Jim T.) log(k.growth)= -0.602*log(Linf)
min.k.growth<-exp(0.696-0.602*log(Linf))
max.k.growth<- exp(1.474-0.602*log(Linf))
#maximum age
Tmax<- input[[i]]$tmax
#age at maturity
Tmat<- input[[i]]$tmat

 if(max.k.growth < 0.05 | min.k.growth< 0.05 | Tmat>10 | Tmax > 30) res = "Very low" else if((max.k.growth < 0.15&max.k.growth >= 0.05)  | (min.k.growth < 0.15&min.k.growth >= 0.05)   | (Tmat>5 & Tmat<=10) | (Tmax > 11 & Tmax <=30)) res = "Low" else if((max.k.growth < 0.3&max.k.growth >= 0.16)  | (min.k.growth < 0.3&min.k.growth >= 0.16)   | (Tmat>2 & Tmat<=4) | (Tmax > 4 & Tmax <=10)) res = "Medium" else if(max.k.growth >= 0.3 | min.k.growth >= 0.3 | Tmat <=1 | Tmax <=3) res = "High" else "Medium" 
 
 #cat(Linf, min.k.growth,max.k.growth,Tmax,Tmat,res,"\n")

    start_r  <- if(res == "Very low"){c(0.015, 0.1)} else if(res == "Low") {c(0.05,0.5)} else if(res == "High") {c(0.6,1.5)}  else {c(0.2,1)} ## Medium, or default if no res is found	
  
    #cat("\n", "Stock ", ResultDir, "\n")

 # <>< # RESULTS section 
   ## Unquote if there is a need to create a directory for results 
   #ResultDir<- paste(ResDir,stock_id[i],"\\",sep="")
   #dir.create(ResultDir,showWarnings = FALSE)
   #setwd(ResultDir)
   
   ###My.Par is used when plots of the model are done, need it here because is passed as argument 
   #these values are used to compute the relative values from prior and posteriors
   #plotted to see the gain in information from the inclusion of the data
   My.Par<-c(800,800,0.6,1.0,0.8,0.5,0)
   My.Par<-t(My.Par)
   My.Par<-as.data.frame(My.Par)
   names(My.Par)<-c( "N1",   "K",    "r",    "z",    "a",    "x",    "h")
  Time=system.time(suppressWarnings(
    COM<-DoProject(MyFile="teste",Myseed=My.seed,TruePar=My.Par,MyData=MyData,EstLogisticM=T, LogisticModel=T,MyCV=My.CV,NormalL=F,Nsim=My.N.Sim,Npost=My.N.Post,logK=T,NormK=F,Normr=F,start.r=start_r,minK=min.K, maxK=max.K,MyYLim=c(-2,8),Obs=F)
    ))
   

    Results= suppressWarnings(data.frame('stock_id'= StockName,'b_bmsy'=apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.5),'b_bmsyUpper' =apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.925),'b_bmsyLower'= apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.075),'b_bmsy_iq25'=apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.25) ,'b_bmsy_iq75'=apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.75),'year'=MyData[,2],'seed'=My.seed,'convergence'=ifelse(COM$Diagno['ESS']>200,"Strong",ifelse(COM$Diagno['ESS']>30,"Weak","Not")),'n_iterations'=My.N.Post,'effective_sample_size'=COM$Diagno['ESS'],'run_time'=Time['elapsed'],'method_id'="COM.SIR"))
    
    MoreResults= suppressWarnings(list('stock_id'= StockName,'method_id'="COM.SIR", 'COM'=COM, 'Resilience'= res,'b_bmsy_mean'=apply(COM$BoverBmsy,MARGIN=1,FUN=mean),'b_bmsyUpper95CI' =apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.975),'b_bmsyLower95CI'= apply(COM$BoverBmsy,MARGIN=1,FUN=quantile,prob=0.025)))  

