# au prealable, vous devez executer l'instruction suivante
# install.packages('randtoolbox')

setwd("~/Documents/3if/S2/proba/proba/tp_R")
library(randtoolbox)
source('generateurs.R')

# compile .Rmd to .pdf
install.packages('tinytex')
tinytex::install_tinytex()

# Q1 - creation of RANDU and StandardMinimal
## cast 10 values starting with the number 31 as seed
cat(RANDU(10, 31))
cat(StandardMinimal(10, 31))
cat(VonNeumann(1000, 31))

# Q2.1 - visual testing with histograms
## cut the window in 2 rows, 2 columns, graphs filled successively
par(mfrow=c(2,2))
## histogram of RANDU distribution for k=1000, seed=31
hist(RANDU(1000, 31), main="RANDU")
hist(StandardMinimal(1000, 31), main="StdMinimal")
hist(VonNeumann(1000, 1, 31), main="VonNeumann")
hist(MersenneTwister(1000, 1, 31), main="MersTwister")

# Q2.2 - visual testing with graphs
par(mfrow=c(2,2))
n <- 1000 # size of vector
u1 <- RANDU(n, 31)
u2 <- StandardMinimal(n, 31)
u3 <- VonNeumann(n, 1, 31)
u4 <- MersenneTwister(n, 1, 31)
plot(u1[1:(n-1)], u1[2:n], xlab='val precedantes', ylab='val obtenues', main="RANDU")
plot(u2[1:(n-1)], u2[2:n], xlab='val precedantes', ylab='val obtenues', main="StdMinimal")
plot(u3[1:(n-1)], u3[2:n], xlab='val precedantes', ylab='val obtenues', main="VonNeumann")
plot(u4[1:(n-1)], u4[2:n], xlab='val precedantes', ylab='val obtenues', main="MersTwister")

# Q3 - computation of Pvaleur ?> 0.001
## some tests
test1 <- binary(2039833732)
cat(bitsNecessary(12)) # 4 bits necessary to convert 12 in binary
cat(bitsNecessary(1.164936e+14))
binary(1.164936e+14)
cat(is.numeric(12))
cat(is.numeric(1.164936e+14))
cat(2^31) # the max number possible we should have ...

## test the process on RANDU
## generate x and nb for every test, 100 times each
## generate 100 seeds
seeds <- sample.int(100000000,100)
## generate a vector x of 1000 numbers by our generators
sumPValeur <- 0
for (i in 1:100) {
  ## generate a vector x of 1000 numbers by our generators
  x <- VonNeumann(1000, seeds[i])
  ## initialize nb, the vector of bits to consider for every number of x
  nb <- rep(0,1000)
  ## for every number of x, determine how many bits are to be considered
  for(j in 1:1000) {
    nb[j] <- bitsNecessary(x[j])
  }
  ## determine the pValeur for the sequence x and sum it with the others
  sumPValeur <- sumPValeur + Frequency(x, nb)
}
avgPValeur <- sumPValeur/100
cat(avgPValeur)

## tests for VonNeumann
cat(VonNeumann(100, 94), '\n')
cat(' single pValeur VonNeumann : ', computeAvgPValeur(VonNeumann, 1000, 1, 9999, TRUE), '\n')

results <- VonNeumann(100, 33)
i <- 1
for(i in 1:length(results)) {
  cat(results[i], ' ')
  if(i %% 15 == 0) {
    cat('\n')
  }
}

nbBitsForGivenSequence <- function(lengthSeq) {
  nb = rep(0, lengthSeq)
  for(j in 1:lengthSeq) {
    nb[j] <- bitsNecessary(x[j])
  }
  return(nb)
}

## results
cat('average pValeur VonNeumann : ', computeAvgPValeur(VonNeumann, 1000, 100, 9999, FALSE), '\n')
cat('average pValeur RANDU : ', computeAvgPValeur(RANDU, 1000, 100), '\n')
cat('average pValeur StandardMinimal : ', computeAvgPValeur(StandardMinimal, 1000, 100), '\n')
cat('average pValeur MersenneTwister : ', computeAvgPValeur(MersenneTwister, 1000, 100), '\n')

# Q4
cat('average pValeur (runs) VonNeumann : ', computeAvgPValeurRuns(VonNeumann, 1000, 100, 9999, FALSE), '\n')
cat('average pValeur (runs) RANDU : ', computeAvgPValeurRuns(RANDU, 1000, 100), '\n')
cat('average pValeur (runs) StandardMinimal : ', computeAvgPValeurRuns(StandardMinimal, 1000, 100), '\n')
cat('average pValeur (runs) MersenneTwister : ', computeAvgPValeurRuns(MersenneTwister, 1000, 100), '\n')

# Q5
## testzone
u <- RANDU(1000, 31)
cat(order.test(u, d=4, echo=FALSE)$p.value)
## WARN Data != Values, v <- MersenneTwister(1000, 32) != v <- as.numeric(MersenneTwister(1000, 32))
# Difference between Data and Value : https://stackoverflow.com/questions/38687880/difference-between-data-and-values-in-r 
v <- as.numeric(MersenneTwister(1000, 32))
v1 <- as.numeric(MersenneTwister(1000, 32))
cat(order.test(v, d=4, echo=FALSE)$p.value)
cat(order.test(v1, d=4, echo=FALSE)$p.value)

## results
cat('average pValeur (order) VonNeumann : ', computeAvgPValeurOrder(VonNeumann, 1000, 100, 9999), '\n')
cat('average pValeur (order) RANDU : ', computeAvgPValeurOrder(RANDU, 1000, 100), '\n')
cat('average pValeur (order) StandardMinimal : ', computeAvgPValeurOrder(StandardMinimal, 1000, 100), '\n')
cat('average pValeur (order) MersenneTwister : ', computeAvgPValeurOrder(MersenneTwister, 1000, 100), '\n')


############################################################
##  Section 2
############################################################

sVN <- 9721
sMT <- 2504
Nsimu <- 1000
Nrepet <- 20

vn <- VonNeumann(Nsimu,Nrepet,sVN)
mt <- MersenneTwister(Nsimu,Nrepet,sMT)

par(mfrow=c(1,2))
hist(mt[,1],xlab='',main='Mersenne Twister')
hist(vn[,1],xlab='',main='Von Neumann')

par(mfrow=c(1,2))
plot(mt[1:(Nsimu-1),1],mt[2:Nsimu,1],xlab='MT(i)', ylab='MT(i+1)', main='Mersenne Twister')
plot(vn[1:(Nsimu-1),1],vn[2:Nsimu,1],xlab='VN(i)', ylab='VN(i+1)', main='Von Neumann')

# Sequence de bits pour les tests
(bit_mt <- binary(mt[1,1]))

