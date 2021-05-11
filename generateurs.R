# random number generators

## WARN : the parenthesis are crucial when using modulo !!!
## cat((65539*122322123222)%%(2^31)) != cat(65539*122322123222%%(2^31))
RANDU <- function(k, graine) {
  x <-  rep(graine,k)
  # start index 1 (not 0), end index k (not k-1)
  # S0 (at index 1) is already initialized, start at S1 (idx 2)
  for(i in seq(2,k,1)) {
    x[i] <- ((65539*x[i-1]) %% (2^31)) 
  }
  return(x)
}

StandardMinimal <- function(k, graine) {
  x <-  rep(graine,k)
  for(i in seq(2,k,1)) {
    x[i] <- ((16807*x[i-1]) %% (2^31 - 1)) 
  }
  return(x)
}

VonNeumann <- function(n, p=1, graine)
{
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1))
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}

VonNeumann <- function(n, graine)
{
  p <- 1
  x <-  rep(graine,n*p+1)
  for(i in 2:(n*p+1))
  {
    numbers <- strsplit(format(x[i-1]^2,scientific=FALSE),'')[[1]]
    while(length(numbers)>4){ 
      numbers <- numbers[2:(length(numbers)-1)] 
    }
    # NB: as.numeric converts numbers from string to double
    x[i] <- as.numeric(numbers)%*%(10^seq(length(numbers)-1,0,-1)) 
  }
  x <- matrix(x[2:(n*p+1)],nrow=n,ncol=p)
  return(x)
}

MersenneTwister <- function(n, p=1, graine)
{
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}

MersenneTwister <- function(n, graine)
{
  p <- 1
  set.seed(graine,kind='Mersenne-Twister')
  x <- sample.int(2^32-1,n*p)
  x <- matrix(x,nrow=n,ncol=p)
  return(x)
}

# other functions

# Q3
## returns a sequence of 32 bits for a given positive integer x
binary <- function(x)
{
  if((x<2^31)&(x>=0))
    return( as.integer(rev(intToBits(as.integer(x)))) )
  else{
    if((x<2^32)&(x>0))
      return( c(1,binary(x-2^31)[2:32]) )
    else{
      cat('Erreur dans binary : le nombre etudie n est pas un entier positif en 32 bits.\n')
      return(c())
    }
  }
}

## x - observed vector of numbers, nb - number of bits to consider
## WARN : starts by the bit of lowest weight !
## WARN : test at least 100 bit 
Frequency <- function(x, nb) {
  sumConvertedBits <- 0
  allConsideredBits <- 0
  # convert all numbers into a sum of converted bits
  for(i in 1:length(x)) {
    seq32Bits <- binary(x[i])
    nbBitsToConsider <- nb[i]
    allConsideredBits <- allConsideredBits + nbBitsToConsider
    for(j in 1:nbBitsToConsider) {
      # conversion in +1 or -1 of a number's bits
      # start by the bit of lowest weight, j in [1:32] or less
      bit0or1 <- seq32Bits[32+1 - j]
      sumConvertedBits <- sumConvertedBits + (2*bit0or1 - 1)
    }
  }
  # computation of sObs of all numbers
  sObs <- abs(sumConvertedBits)/sqrt(allConsideredBits)
  # computation of pValeur of all numbers
  pValeur <- 2*(1 - pnorm(sObs))
  return(pValeur)
}

## get the number of bits to use to convert a decimal to binary
bitsNecessary <- function(decimalNumber) {
  if(decimalNumber == 0) return(1);
  return(log2(decimalNumber) + 1);
}

## compute the average pValeur of a generator
## specify the number of numbers generated for each sequence on which to compute a pValeur 
## specify how many time to repeat the test (how much pValeur to compute)
computeAvgPValeur <- function(generator, lengthSeq, repetition, maxSeed=100000000, printSeeds=FALSE) {
  seeds <- sample.int(maxSeed,repetition)
  if(printSeeds) {
    cat('seeds : ', seeds, '\n')
  }
  ## generate a vector x of 1000 numbers by our generators
  sumPValeur <- 0
  for (i in 1:repetition) {
    ## generate a vector x of 1000 numbers by our generators
    x <- generator(lengthSeq, seeds[i])
    ## initialize nb, the vector of bits to consider for every number of x
    nb <- rep(0,lengthSeq)
    ## for every number of x, determine how many bits are to be considered
    for(j in 1:lengthSeq) {
      nb[j] <- bitsNecessary(x[j])
    }
    ## determine the pValeur for the sequence x and sum it with the others
    sumPValeur <- sumPValeur + Frequency(x, nb)
    if(is.na(Frequency(x, nb))) {
      cat("x = ", x, ", nb = ", nb)
    }
  }
  avgPValeur <- sumPValeur/repetition
  return(avgPValeur)
}

# Q4
## consider a sequence of decimal numbers in a single seq of bits
## loop once on every number of x
## compute proportionOf1 and vObs at the same time
## then evaluate the test and continue if necessary to get pValue
runs <- function(x, nb) {
  allConsideredBits <- 0
  sumOf1 <- 0
  vObs <- 0
  ## init the first bit of first number to be considered
  lastBit <- binary(x[1])[1]
  ## pre-compute proportionOf1 and compute vObs
  for(i in 1:length(x)) {
    seq32Bits <- binary(x[i])
    nbBitsToConsider <- nb[i]
    allConsideredBits <- allConsideredBits + nbBitsToConsider
    for(j in 1:nbBitsToConsider) {
      # start by the bit of lowest weight, j in [1:32] or less
      bit0or1 <- seq32Bits[32+1 - j]
      ## pre-compute proportionOf1
      sumOf1 <- sumOf1 + bit0or1
      ## compute vObs
      if(lastBit != bit0or1) {
        vObs <- vObs + 1
      }
    }
  }
  ## compute proportionOf1 and do the test
  proportionOf1 <- sumOf1/allConsideredBits
  if(abs(proportionOf1 - (1/2)) >= (2/sqrt(allConsideredBits))) {
    return(0.0)
  }
  ## compute pValeur
  pValeur = 2*(1 - pnorm((abs(vObs - 2*allConsideredBits*proportionOf1*(1 - proportionOf1)))/(2*sqrt(allConsideredBits)*proportionOf1*(1 - proportionOf1))))
  return(pValeur)
}

computeAvgPValeurRuns <- function(generator, lengthSeq, repetition, maxSeed=100000000, printSeeds=FALSE) {
  seeds <- sample.int(maxSeed,repetition)
  if(printSeeds) {
    cat('seeds : ', seeds, '\n')
  }
  ## generate a vector x of 1000 numbers by our generators
  sumPValeur <- 0
  for (i in 1:repetition) {
    ## generate a vector x of 1000 numbers by our generators
    x <- generator(lengthSeq, seeds[i])
    ## initialize nb, the vector of bits to consider for every number of x
    nb <- rep(0,lengthSeq)
    ## for every number of x, determine how many bits are to be considered
    for(j in 1:lengthSeq) {
      nb[j] <- bitsNecessary(x[j])
    }
    ## determine the pValeur for the sequence x and sum it with the others
    sumPValeur <- sumPValeur + runs(x, nb)
  }
  avgPValeur <- sumPValeur/repetition
  return(avgPValeur)
}

# Q5

computeAvgPValeurOrder <- function(generator, lengthSeq, repetition, maxSeed=100000000) {
  seeds <- sample.int(maxSeed,repetition)
  sumPValeur <- 0
  for (i in 1:repetition) {
    u <- as.numeric(generator(lengthSeq, seeds[i]))
    sumPValeur <- sumPValeur + order.test(u, d=4, echo=FALSE)$p.value
  }
  avgPValeur <- sumPValeur/repetition
  return(avgPValeur)
}
