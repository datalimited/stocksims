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
