
##############
#
# Part 1 -- Models
#
###############

# JAGS model 1 -- Latent Bt, Process errors for Pt and Et
JagsModel = "
model {
  SigmaE ~ dunif(0.001,1)
  SigmaP <- SigmaE
  SigmaQ <- SigmaP

  #logitB1overB0 ~ dunif(-4.6,4.6)
  #lnr ~ dnorm(rPrior[1],rPrior[2])
  lnr <- log(r)
  lnB0 ~ dunif(-4.6,4.6)
  x ~ dunif(0.01,0.5)
  a ~ dunif(0.1,2)

  #r <- exp(lnr)
  r ~ dunif(r_min,r_max)
  B0 <- exp(lnB0)
  ln_Final_Depletion <- log(Bt[Nyears]) - lnB0
  Final_Depletion <- exp(ln_Final_Depletion)
  Final_Depletion_Error_Ratio <- exp(ln_Final_Depletion) / DepletionTrue[Nyears]
  Final_Depletion_Prior[1] ~ dlnorm( ln_Final_Depletion, Final_Depletion_Prior[2])
  MSY <- B0 * r / 4

  #Bt[1] <- B0 * 1/(1+exp(-logitB1overB0))
  Bt[1] <- B0
  lnE0 ~ dunif(-11.5,-2.3)    # ln(0.00001) - ln(0.1)
  E0 <- exp(lnE0)
  Et[1] <- E0
  Dummy <- lnE0
  Bupper <- 2*B0   # Necessary for when r is low, so that Bt doesn't drift to far above B0
  Ct_hat[1] <- 0
  for(YearI in 2:Nyears){
    # Define time-varying precision
    TauE[YearI] <- pow(SigmaE,-2) * pow(EffortSD[YearI],-2)
    TauB[YearI] <- pow(SigmaP,-2) * pow(BioSD[YearI],-2)
    TauQ[YearI] <- pow(SigmaQ,-2) * pow(Q_SD[YearI],-2)
    # Stochastic draw for Bt given Bt_exp
    Pt[YearI-1] <- r*Bt[YearI-1] * ( 1 - Bt[YearI-1]/B0 )
    #ln_Bt_exp[YearI] <- log( max( Bt[YearI-1] + Pt[YearI-1] - Ct[YearI-1], -0.0001/(Bt[YearI-1] + Pt[YearI-1] - Ct[YearI-1]) ) )  # 1e-10 is about the lowest I can set this
    ln_Bt_exp[YearI] <- log( max( Bt[YearI-1] + Pt[YearI-1] - Ct[YearI-1], 1e-12 ) )  # 1e-10 is about the lowest I can set this
    Bt[YearI] ~ dlnorm( ln_Bt_exp[YearI], TauB[YearI]) T(0,Bupper)
    # Set up next effort computation used in next year's stochastic draw for Ct
    Et[YearI] <- min( Ct[YearI] / Bt[YearI], 0.99 )
    # Stochastic draw for Ct given Ct_hat
    Ct_hat[YearI] <- Et[YearI-1] * ( Bt[YearI-1] / (a*B0/2) )^x * Bt[YearI]
    ln_Ct_hat[YearI] <- log( Ct_hat[YearI] )
    Ct[YearI] ~ dlnorm(ln_Ct_hat[YearI],TauQ[YearI])
  }
  TauB_test <- pow(SigmaP,-2)
  Bt_test[1] ~ dlnorm( 0, TauB_test)
  for(YearII in 2:20){
    ln_Bt_test_exp[YearII] <- log( max( Bt_test[YearII-1] + r*Bt_test[YearII-1] * ( 1 - Bt_test[YearII-1]/B0 ), 0.001 ) )
    Bt_test[YearII] ~ dlnorm( ln_Bt_test_exp[YearII], TauB_test) T(0,Bupper)  # IF YOU WANT TO TRUNCATE THE REALLY HIGH BIOMASS
  }
  Bt_test_final <- Bt_test[20]
}
"

# JAGS model 2 -- Latent Bt and Et
JagsModel_2 = "
model {
  SigmaE ~ dunif(0.001,1)
  SigmaP <- SigmaE
  SigmaQ <- SigmaP

  #logitB1overB0 ~ dunif(-4.6,4.6)
  #lnr ~ dnorm(rPrior[1],rPrior[2])
  lnr <- log(r)
  lnB0 ~ dunif(-4.6,4.6)
  x ~ dunif(0.01,0.5)
  a ~ dunif(0.1,2)

  #r <- exp(lnr)
  r ~ dunif(r_min,r_max)
  B0 <- exp(lnB0)
  ln_Final_Depletion <- log(Bt[Nyears]) - lnB0
  Final_Depletion <- exp(ln_Final_Depletion)
  Final_Depletion_Error_Ratio <- exp(ln_Final_Depletion) / DepletionTrue[Nyears]
  Final_Depletion_Prior[1] ~ dlnorm( ln_Final_Depletion, Final_Depletion_Prior[2])
  MSY <- B0 * r / 4

  #Bt[1] <- B0 * 1/(1+exp(-logitB1overB0))
  Bt[1] <- B0
  lnE0 ~ dunif(-11.5,-2.3)    # ln(0.00001) - ln(0.1)
  E0 <- exp(lnE0)
  Et[1] <- E0
  Bupper <- 2*B0   # Necessary for when r is low, so that Bt doesn't drift to far above B0
  Ct_hat[1] <- 0    # necessary to set node
  Et_hat[1] <- Et[1] # necessary to set node
  for(YearI in 2:Nyears){
    # Define time-varying precision
    TauE[YearI] <- pow(SigmaE,-2) * pow(EffortSD[YearI],-2)
    TauB[YearI] <- pow(SigmaP,-2) * pow(BioSD[YearI],-2)
    TauQ[YearI] <- pow(SigmaQ,-2) * pow(Q_SD[YearI],-2)
    # Stochastic draw for Bt given Bt_exp
    Pt[YearI-1] <- r*Bt[YearI-1] * ( 1 - Bt[YearI-1]/B0 )
    ln_Bt_exp[YearI] <- log( max( Bt[YearI-1] + Pt[YearI-1] - Ct[YearI-1], 1e-12 ) )  # 1e-10 is about the lowest I can set this
    Bt[YearI] ~ dlnorm( ln_Bt_exp[YearI], TauB[YearI]) T(0,Bupper)
    # Stochastic draw for Et given Et_exp
    Et_hat[YearI] <- Et[YearI-1] * ( Bt[YearI-1] / (a*B0/2) )^x
    ln_Et_hat[YearI] <- log( Et_hat[YearI] )
    Et[YearI] ~ dlnorm( ln_Et_hat[YearI], TauE[YearI] )
    # Stochastic draw for Ct given Ct_hat
    Ct_hat[YearI] <- Et[YearI] * Bt[YearI]
    ln_Ct_hat[YearI] <- log( Ct_hat[YearI] )
    Ct[YearI] ~ dlnorm(ln_Ct_hat[YearI],TauQ[YearI])
  }
  TauB_test <- pow(SigmaP,-2)
  Bt_test[1] ~ dlnorm( 0, TauB_test)
  for(YearII in 2:20){
    ln_Bt_test_exp[YearII] <- log( max( Bt_test[YearII-1] + r*Bt_test[YearII-1] * ( 1 - Bt_test[YearII-1]/B0 ), 0.001 ) )
    Bt_test[YearII] ~ dlnorm( ln_Bt_test_exp[YearII], TauB_test) T(0,Bupper)  # IF YOU WANT TO TRUNCATE THE REALLY HIGH BIOMASS
  }
  Bt_test_final <- Bt_test[20]
}
"



##############
#
# Part 2 -- estimate parameters for model
#
###############

  #install.packages("FLCore", repos="flr-project.org/Rdevel")
  
  library(R2jags) # Interface with JAGS
  #library(FLCore)
  
  runif(1)
  RunFile = "C:/Users/James.Thorson/Desktop/UW Hideaway/FAO-CI/Simulation design/"

  load(paste(RunFile,"input201303061732.RData",sep=""))

  StockName = names(input)[[1]]
  Ct = input[[1]]$catch$data
  Year = input[[1]]$catch$year
  Nyears = nrow(input[[1]]$catch)
  Bt = rep(1,Nyears)
  B0 = Bt[1]
  DepletionPriorSD = 1000
  
  # R prior
  if(input[[1]]$tmat<1) rPrior1 = 1
    if(input[[1]]$tmat>=1 & input[[1]]$tmat<4) rPrior1 = 2
    if(input[[1]]$tmat>=4 & input[[1]]$tmat<10) rPrior1 = 3
    if(input[[1]]$tmat>=10) rPrior1 = 4
  if(input[[1]]$tmax<3) rPrior2 = 1
    if(input[[1]]$tmax>=3 & input[[1]]$tmax<10) rPrior2 = 2
    if(input[[1]]$tmax>=10 & input[[1]]$tmax<30) rPrior2 = 3
    if(input[[1]]$tmax>=30) rPrior2 = 4
  rPrior = max(rPrior1, rPrior2)
  if(rPrior==1){ r_min=0.6; r_max=1.5 }
    if(rPrior==2){ r_min=0.2; r_max=1.0 }
    if(rPrior==3){ r_min=0.05; r_max=0.5 }
    if(rPrior==4){ r_min=0.015; r_max=0.1 }
  

  ##### Run Settings
  # which effort dynamics model
  # ModelType = 1 : Process errors in biomass and effort dynamics
  # ModelType = 2 : same plus variability in catch equation
  ModelType = 2 # 1=Bt; 2=Bt+Et
  # JAGS settings
  NburninJags = 1e5
    NiterJags = 2e5
    NthinJags = 1e1
  Nchains = 3

  # Write JAGS model
  if(ModelType==1){
    cat(JagsModel, file=paste(RunFile,"dnorm.bug",sep=""))     #
    Params2Save = c("Bt_test_final","Ct_hat","Pt","Bt","B0","r","a","x","SigmaE","SigmaP","MSY","Final_Depletion","Final_Depletion_Error_Ratio","E0") #,"logitB1overB0")
  }
  if(ModelType==2){
    cat(JagsModel_2, file=paste(RunFile,"dnorm.bug",sep=""))     #
    Params2Save = c("Bt_test_final","Et","Et_hat","Ct_hat","Pt","Bt","B0","r","a","x","SigmaE","SigmaP","MSY","Final_Depletion","Final_Depletion_Error_Ratio","E0") #,"logitB1overB0")
  }


  # Run JAGS
  # Nyears = number of years
  # Ct = catch time series
  # Q_SD = relative index of CV in catch equation (i.e. catchability)
  # BioSD = relative index of CV in biomass process errors
  # EffortSD = relative index of CV in effort dynamics process errors
  # DepletionTrue = true Bt/B0 (only used for computing errors)
  # Final_Depletion_Prior = mean and precision for prior on final Bt/B0
  # r_min and r_max = min and max for uniform prior on r (intrinsic growth rate)
  Seed = ceiling(runif(1,0,1e6))
  set.seed(Seed)
  DataJags = list(Nyears=Nyears, Ct=Ct, Q_SD=rep(1,Nyears), BioSD=rep(1,Nyears), EffortSD=rep(1,Nyears), DepletionTrue=Bt/B0, Final_Depletion_Prior=c(Bt[Nyears]/B0,1/DepletionPriorSD^2), r_min=r_min, r_max=r_max) #, SigmaE=SigmaE, SigmaP=SigmaP)
  Time = system.time(
    Jags <- jags(model.file=paste(RunFile,"dnorm.bug",sep=""), working.directory=NULL, data=DataJags, parameters.to.save=Params2Save, n.chains=Nchains, n.thin=NthinJags, n.iter=NiterJags, n.burnin=NburninJags)
  )

  BoverBmsy = Jags$BUGSoutput$sims.list$Bt / outer(Jags$BUGSoutput$sims.list$B0[,1],rep(1,Nyears)) * 2
  Neff = min(Jags$BUGSoutput$summary[,'n.eff'])
  Results = suppressWarnings(data.frame('stock_id'=StockName, 'b_bmsy'=apply(BoverBmsy,MARGIN=2,FUN=mean), 'b_bmsyUpper'=apply(BoverBmsy,MARGIN=2,FUN=quantile,prob=0.925), 'b_bmsyLower'=apply(BoverBmsy,MARGIN=2,FUN=quantile,prob=0.075), 'b_bmsy_iq25'=apply(BoverBmsy,MARGIN=2,FUN=quantile,prob=0.25), 'b_bmsy_iq75'=apply(BoverBmsy,MARGIN=2,FUN=quantile,prob=0.75), 'year'=Year, 'seed'=Seed, 'convergence'=ifelse(Neff>200,"Strong",ifelse(Neff>30,"Weak","Not")), 'n_iterations'=NiterJags*Nchains, 'effective_sample_size'=Neff, 'run_time'=Time['elapsed'], 'method_id'="SSCOM"))
