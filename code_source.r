
install.packages("fOptions")
library(fOptions)


par(mfrow = c(2, 1), cex = 0.7)
   steps = 110
   CRROptionValue  = rep(NA, times = steps)
   for (n in 3:steps) { 
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "ca", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
    
   }           
   plot(CRROptionValue[3:steps], type = "l", col = "red", ylab = "Option Value COX-ROSS For American Call" )
legend("topright" ,legend=c("COX-ROSS "),col=c("red"), lty=1:2, cex=0.8)

CRRBinomialTreeOption(TypeFlag = "ca", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)

CRRBinomialTreeOption(TypeFlag = "ca", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)

CRRTree = BinomialTreeOption(TypeFlag = "ca", S = 50, X = 50, 
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
     xlab = "n", ylab = "Option Value")
   title(main = "Arbre Binomial pour un Call Américain ") 

par(mfrow = c(2, 1), cex = 0.7)
   steps = 110
   CRROptionValue_p  = rep(NA, times = steps)
   for (n in 3:steps) { 
     CRROptionValue_p[n] = CRRBinomialTreeOption(TypeFlag = "pa", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
    
   }           
   plot(CRROptionValue_p[3:steps], type = "l", col = "red", ylab = "Option Value COX ROSS for  American Put")
legend("topright", legend=c("COX ROSS "),col=c("red"), lty=1:2, cex=0.8)


CRRBinomialTreeOption(TypeFlag = "pa", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)

CRRTree = BinomialTreeOption(TypeFlag = "pa", S = 50, X = 50, 
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
     xlab = "n", ylab = "Option Value")
   title(main = "Arbre Binomial pour un Put Américain ") 

par(mfrow = c(2, 1), cex = 0.7)
   steps = 110
   CRROptionValue  = rep(NA, times = steps)
   for (n in 3:steps) { 
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "ce", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
    
   }           
   plot(CRROptionValue[3:steps], type = "l", col = "Blue", ylab = "Option Value COX-ROSSFor European Call")
legend("topright", legend=c("COX-ROSS"),col=c("Blue"), lty=1:2, cex=0.8)


CRRBinomialTreeOption(TypeFlag = "ce", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)

CRRBinomialTreeOption(TypeFlag = "ce", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)

CRRTree = BinomialTreeOption(TypeFlag = "ce", S = 50, X = 50, 
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
     xlab = "n", ylab = "Option Value")
   title(main = "Arbre Binomial pour un Call Européen ") 

par(mfrow = c(2, 1), cex = 0.7)
   steps = 120
   CRROptionValue =  JROptionValue = TIANOptionValue =
     rep(NA, times = steps)
   for (n in 3:steps) {
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "pe", S = 50,
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
   
   }          
    plot(CRROptionValue[3:steps], type = "l", col = "blue", ylab = "Option Value For European Put")
    legend("topright", legend=c("COX ROSS "),col=c("blue"), lty=1:2, cex=0.8)


CRRBinomialTreeOption(TypeFlag = "pe", S = 50, 
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)

CRRTree = BinomialTreeOption(TypeFlag = "pe", S = 50, X = 50, 
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = 5)
   BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-6, 7),
     xlab = "n", ylab = "Option Value")
   title(main = "Arbre Binomial pour un Put Européen ") 

CRR_Model = function(n,S,K,r,sigma,T,typeOption, PutOrCall){
    At = T/n
    u = exp(sigma*sqrt(At)) #up
    d = 1./u #down
    R = exp(r*At)
    p = (R-d) / (u-d) #probabilité risque neutre
    q = 1-p
    
    prix_sj = matrix(0, nrow = n+1, ncol = n+1)
    prix_sj[1,1] = S
    for (i in 2:(n+1)){
        prix_sj[i,1] = prix_sj[i-1,1]*u
        for (j in 2:i){
            prix_sj[i,j] = prix_sj[i-1,j-1]*d
        }
    }
    prix_option = matrix(0, nrow = n+1, ncol = n+1)
    for (j in 1:(n+1)){
        if(PutOrCall=="C"){
            prix_option[n+1,j] = max(0, prix_sj[n+1,j]-K)
        }else if(PutOrCall=="P"){
            prix_option[n+1,j] = max(0, K-prix_sj[n+1,j])
        }
    }
    
    if (typeOption == "E"){
        for (i in seq(n,1,-1)){
            for (j in 1:i){
                prix_option[i,j] = max(0,1/R*(p*prix_option[i+1,j] + q*prix_option[i+1,j+1]))
            }
        }
    }else if (typeOption == "A"){
        for(i in seq(n,1,-1)){
            for (j in 1:i){
                if (PutOrCall=="P"){
                     prix_option[i,j] = max(0, K-prix_sj[i,j], 1/R*(p*prix_option[i+1,j]+q*prix_option[i+1,j+1]))
                }else if (PutOrCall=="C"){
                    prix_option[i,j] = max(0, prix_sj[i,j]-K, 1/R*(p*prix_option[i+1,j]+q*prix_option[i+1,j+1]))
                }
            }
        }
    }
    return (prix_option[1,1])
    
    
 }

print("Cas d'un call Européen d'horizon N=5:")
print(CRR_Model(n=5,S=50,K=50,r=0.1,sigma=0.4,T=0.4167,typeOption="E", PutOrCall="C"))
print("----------------------------------------")
print("Cas d'un put Européen d'horizon N=5:")
print(CRR_Model(n=5,S=50,K=50,r=0.1,sigma=0.4,T=0.4167,typeOption="E", PutOrCall="P"))
print("----------------------------------------")
print("Cas d'un call Américain d'horizon N=5:")
print(CRR_Model(n=5,S=50,K=50,r=0.1,sigma=0.4,T=0.4167,typeOption="A", PutOrCall="C"))
print("----------------------------------------")
print("Cas d'un put Américain d'horizon N=5:")
print(CRR_Model(n=5,S=50,K=50,r=0.1,sigma=0.4,T=0.4167,typeOption="A", PutOrCall="P"))
print("----------------------------------------")

BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  
  value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(value)}
  
  if(type=="P"){
  d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  
  value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
  return(value)}
}

BS_call=BlackScholes(50,50,0.1,0.4167,0.4,"C")
BS_call

steps = 110
   BSOptionValue  = rep(NA, times = steps)
   for (n in 3:steps) { 
     BSOptionValue[n] = BlackScholes(50,50,0.1,0.4167,0.4,"C")
    
   } 
plot(BSOptionValue[3:steps],type = "l",ylab = "Option Value with Black Scholes Model Call Option",
     col="Blue")
legend("topright", legend=c("Black & Scholes"),col=c("blue"), lty=1:2, cex=0.8)


BS_put=BlackScholes(50,50,0.1,0.4167,0.4,"P")
BS_put

steps = 110
   BSOptionValue  = rep(NA, times = steps)
   for (n in 3:steps) { 
     BSOptionValue[n] = BlackScholes(50,50,0.1,0.4167,0.4,"P")
    
   } 
plot(BSOptionValue[3:steps],type = "l",ylab = "Option Value with Black Scholes Model Put Option", col="Blue")
legend("topright", legend=c("Black & Scholes"),col=c("blue"), lty=1:2, cex=0.8)

BlackScholesDiv <- function(S, K, r, T, sig, type,q){
  
  d1 <- (log(S/K) + (r - q + sig^2/2)*T) / (sig*sqrt(T))
  d2 <- d1 - sig*sqrt(T)
  
  if(type=="C"){

  value <- (S*exp(-q * T)*pnorm(d1) - K*exp(-r * T) * pnorm(d2))
  return(value)}
  
  if(type=="P"){
  value <-  (K*exp(-r * T)*pnorm(-d2) - S*exp(-q * T)*pnorm(-d1))
  return(value)}
}

q = 0.0205 #dividende

call_div = BlackScholesDiv(S=50, K=50, r=0.1, T=0.4167, sig=0.4, type="C",q=0.0205)
print(call_div)

put_div=BlackScholesDiv(S=50, K=50, r=0.1, T=0.4167, sig=0.4, type="P",q=0.0205)
print(put_div)

par(mfrow = c(2, 1), cex = 0.7)
steps = 120
CRROptionValue =  JROptionValue = TIANOptionValue = rep(NA, times = steps)
for(n in 3:steps) {
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "ca", S = 50,
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
   
   }          
plot(CRROptionValue[3:steps], type = "l", col = "red", ylab = "Option Value For Américain Call")

   # Add Result from BAW Approximation:
BAWValue =  BAWAmericanApproxOption(TypeFlag = "c", S = 50, X = 50,
Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)@price
abline(h = BAWValue, lty = 3)
title(main = "Convergence du modèle CRR vers le modèle stochastique d'une call Americaine.")


legend("topright", legend=c("COX-ROSS ","Black & Scholes"),col=c("red","black"), lty=1:2, cex=0.8)
   


par(mfrow = c(2, 1), cex = 0.7)
   steps = 120
   CRROptionValue =  JROptionValue = TIANOptionValue =
     rep(NA, times = steps)
   for (n in 3:steps) {
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "pa", S = 50,
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
   
   }          
    plot(CRROptionValue[3:steps], type = "l", col = "red", ylab = "Option Value For Américain Call")
    legend("topright", legend=c("COX ROSS ","Black & Scholes"),col=c("red","black"), lty=1:2, cex=0.8)

   # Add Result from BAW Approximation:
   BAWValue =  BAWAmericanApproxOption(TypeFlag = "p", S = 50, X = 50,
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)@price
   abline(h = BAWValue, lty = 3)
   title(main = "Convergence du modèle CRR vers le modèle stochastique d'une put Américaine.")


par(mfrow = c(2, 1), cex = 0.7)
   steps = 120
   CRROptionValue =  JROptionValue = TIANOptionValue =
     rep(NA, times = steps)
   for (n in 3:steps) {
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "ce", S = 50,
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
   
   }          
    plot(CRROptionValue[3:steps], type = "l", col = "Blue", ylab = "Option Value For European Call")
legend("topright", legend=c("COX ROSS ","Black & Scholes"),col=c("Blue","black"), lty=1:2, cex=0.8)

   # Add Result from BAW Approximation:
   BAWValue =  BAWAmericanApproxOption(TypeFlag = "c", S = 50, X = 50,
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)@price
   abline(h = BAWValue, lty = 3)
   title(main = "Convergence du modèle CRR vers le modèle stochastique d'une call Européenne.")

   



par(mfrow = c(2, 1), cex = 0.7)
   steps = 120
   CRROptionValue =  JROptionValue = TIANOptionValue =
     rep(NA, times = steps)
   for (n in 3:steps) {
     CRROptionValue[n] = CRRBinomialTreeOption(TypeFlag = "pe", S = 50,
       X = 50, Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4, n = n)@price
   
   }          
    plot(CRROptionValue[3:steps], type = "l", col = "blue", ylab = "Option Value For European Put")
legend("topright", legend=c("COX ROSS ","Black & Scholes"),col=c("blue","black"), lty=1:2, cex=0.8)

   # Add Result from BAW Approximation:
   BAWValue =  BAWAmericanApproxOption(TypeFlag = "p", S = 50, X = 50,
     Time = 0.4167, r = 0.1, b = 0.1, sigma = 0.4)@price
   abline(h = BAWValue, lty = 3)
   title(main = "Convergence du modèle CRR vers le modèle stochastique.")


CRR_Model_corr = function(n,S,K,r,sigma,T,typeOption, PutOrCall){
    At = T/n
    u = exp(sigma*sqrt(At)+1/n*log(K/S)) #up
    d = 1./u #down
    R = exp(r*At)
    p = (R-d) / (u-d) #probabilité risque neutre
    q = 1-p
    
    prix_sj = matrix(0, nrow = n+1, ncol = n+1)
    prix_sj[1,1] = S
    for (i in 2:(n+1)){
        prix_sj[i,1] = prix_sj[i-1,1]*u
        for (j in 2:i){
            prix_sj[i,j] = prix_sj[i-1,j-1]*d
        }
    }
    prix_option = matrix(0, nrow = n+1, ncol = n+1)
    for (j in 1:(n+1)){
        if(PutOrCall=="C"){
            prix_option[n+1,j] = max(0, prix_sj[n+1,j]-K)
        }else if(PutOrCall=="P"){
            prix_option[n+1,j] = max(0, K-prix_sj[n+1,j])
        }
    }
    
    if (typeOption == "E"){
        for (i in seq(n,1,-1)){
            for (j in 1:i){
                prix_option[i,j] = max(0,1/R*(p*prix_option[i+1,j] + q*prix_option[i+1,j+1]))
            }
        }
    }else if (typeOption == "A"){
        for(i in seq(n,1,-1)){
            for (j in 1:i){
                if (PutOrCall=="P"){
                     prix_option[i,j] = max(0, K-prix_sj[i,j], 1/R*(p*prix_option[i+1,j]+q*prix_option[i+1,j+1]))
                }else if (PutOrCall=="C"){
                    prix_option[i,j] = max(0, prix_sj[i,j]-K, 1/R*(p*prix_option[i+1,j]+q*prix_option[i+1,j+1]))
                }
            }
        }
    }
    return (prix_option[1,1])
    
    
 }

iterations_call = rep(NA,400)
for (n in 1:400){
iterations_call[n] = CRR_Model_corr(n=n,S=50,K=50,r=0.1,sigma=0.4,T=0.4167,typeOption="E", PutOrCall="C")
}
plot(iterations_call, type="l", col="green")
abline(h=BlackScholes(50,50,0.1,0.4167,0.4,"C"), col="red")
legend("topright", legend=c("Cox Ross Rubinstein", "Black & Scholes"),
       col=c("green", "red"), lty=1:2, cex=0.8)


