###################IMPLICIT VOLATILITY#######################

install.packages('RQuantLib', type="binary")
library(RQuantLib)


iv= EuropeanOptionImpliedVolatility(type="call", value=1.902,underlying=79.29,strike=83, dividendYield=0, riskFreeRate=0.055,
maturity=0.12054, volatility=0.2)


##################VOLATILITY SMILE###########################

data.smilev2 <- read.csv("~/Desktop/M1 ES/TER/DONNÉE TER /data smilev3.csv", sep=";")
cn= c('igimplvol','callprice','strikeprice')
colnames(data.smilev2)=cn

lim=c(0,200)
plot(data.smilev2[,3],data.smilev2[,1], main = "Volatility smile: Implicit volatility from ig", xlab="Strike price", ylab="Implicit volatility",type='l')
abline(v = 8200, col = "red",ylim=lim)
legend("topright", legend = "Prix spot:82$", col = "red", lty = 1)

#
data.smilev2$callprice= data.smilev2$callprice/100
data.smilev2$strikeprice=data.smilev2$strikeprice/100


calculate_implied_volatility2 <- function(x, y) {
  tryCatch({
    volatility <- EuropeanOptionImpliedVolatility(type = "call",
                                                  value = x,
                                                  underlying = 82,
                                                  strike = y,
                                                  dividendYield = 0,
                                                  riskFreeRate = 0.055,
                                                  maturity = 0.0630,
                                                  volatility = 0.001)
    return(volatility)
  }, error = function(e) {
    return(NA)  
  })
}

resultats_implvol <- numeric(nrow(data.smilev2))

# Boucle for pour calculer les volatilités implicites
for (i in 1:nrow(data.smilev2)) {
  resultats_implvol[i] <- calculate_implied_volatility2(data.smilev2[i, 2], data.smilev2[i, 3])
}

data.smilev2$resultats_implvol <- resultats_implvol

plot(data.smilev2[,3],data.smilev2[,4], main = "Volatility smile: Implicit volatility", xlab="Strike price", ylab="Implicit volatility",type='l')
abline(v = 82, col = "red",ylim=lim)
legend("topright", legend = "Prix spot:82$", col = "red", lty = 1)

###########Volatilité historique1##############################

#cours 6M

BRENT.6M <- read.delim("~/Desktop/M1 ES/TER/DONNÉE TER /BRENT 6M.txt")

cours6M = BRENT.6M$clot
u6M =log(cours6M[2:length(cours6M)] / cours6M[1:(length(cours6M)-1)])
meanu6M = mean(u6M)
sigma6M= sqrt((sum((u6M-meanu6M)^2))/length(u6M))

#cours 3M

BRENT.3M <- read.delim("~/Desktop/M1 ES/TER/DONNÉE TER /BRENT 3M.txt")

cours3M = BRENT.3M$clot

u3M = log(cours3M[2:length(cours3M)] / cours3M[1:(length(cours3M)-1)])
meanu3M = mean(u3M)
sigma3M= sqrt((sum((u3M-meanu3M)^2))/length(u3M))

#Cours 10A 

BRENT.10A <- read.delim("~/Desktop/M1 ES/TER/DONNÉE TER /BRENT 10A.txt")

cours10A = BRENT.10A$clot
u10A = log(cours10A[2:length(cours10A)] / cours10A[1:(length(cours10A)-1)])
meanu10A = mean(u10A)
sigma10A= sqrt((sum((u10A-meanu10A)^2))/length(u10A))

#Cours 1A

BRENT.1A <- read.delim("~/Desktop/M1 ES/TER/DONNÉE TER /BRENT 1 AN.txt")

cours1A = BRENT.1A$clot
u1A = log(cours1A[2:length(cours1A)] / cours1A[1:(length(cours1A)-1)])
meanu1A = mean(u1A)
sigma1A= sqrt((sum((u1A-meanu1A)^2))/length(u1A))

#Cours5A

BRENT.5A <- read.delim("~/Desktop/M1 ES/TER/DONNÉE TER /BRENT 5ANS.txt")
cours5A = BRENT.5A$clot
u5A = log(cours5A[2:length(cours5A)] / cours5A[1:(length(cours5A)-1)])
meanu5A = mean(u5A)

#Formule Black-Scholes

BSM = function(x){
  d1 <- ((log(85.33/83)+(0.055+x^2/2)*1/(x*sqrt(1))))
  d2 <- d1-x*sqrt(1)
  C <- 85.33*pnorm(d1,0,1)-83*exp(-1*0.055*(1))*pnorm(d2,0,1)
return(C)
}

vectsigm=c(0.01383 , 0.01878 ,0.01902 , 0.02887 , 0.02528)

resultatsBSM <- sapply(vectsigm, BSM)



