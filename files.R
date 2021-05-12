# Q6

FileMM1 <- function(lambda, mu, D) {
  result <- list(departs=c(), arrivees=c())
  arrivees = c()
  departs = c()
  
  temps = 0
  client = 1
  while(temps < D)
  {
    d_ = rexp(1, rate=mu)
    t_ = rexp(1, rate=lambda)
    
    arrivees[client] = temps
    departs[client] = arrivees[client] + d_
    
    # si le client précédent n'a pas fini quand on arrive
    if(client > 1 && departs[client - 1] > arrivees[client])
    {
      departs[client] = departs[client] + (departs[client - 1] - arrivees[client])
    }
    
    temps = temps + t_
    client = client + 1
  }
  
  departs = departs[departs <= D]
  
  result$departs = departs
  result$arrivees = arrivees
  
  return(result)
}

# Q7
VisualisationComportement <- function(arrivees, departs) {
  ts = c()
  ns = c()
  
  maxTime = departs[length(departs)]
  for(i in 0:maxTime) {
    ts[i] = i
    
    nb_arrives = sum(arrivees <= i)
    nb_partis = sum(departs <= i)
    
    ns[i] = nb_arrives - nb_partis 
  }
  
  return(list(t=ts, n=ns))
}


# Q8
CalculNombreMoyenPersonnes <- function(valeursN) {
  return(mean(valeursN))
}

CalculAttenteMoyenne <- function(arrivees, departs) {
  attentes = c()
  for(i in 1:length(departs))
  {
    attentes[i] = departs[i] - arrivees[i]
  }
  return(mean(attentes))
}
