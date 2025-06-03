library(PBSddesolve)
library("deSolve")


#Minimal model function
minimal_model <- function(t, y, parameters) {
  
  P1 = parameters[1]
  P2 = parameters[2]
  P3 = parameters[3]
  n = parameters[4]
  gamma = parameters[5]
  h = parameters[6]
  Gb = parameters[7]
  Ib = parameters[8]
  
  G = y[1]
  X = y[2]
  I = y[3]
  
  # Glucose
  dG <- -(P1 + X) * G + P1 * Gb
  # Auxiliary variable X
  dX <- -P2 * X + P3 * (I - Ib)
  # Insuline
  dI <- -n * (I - Ib) + gamma * t * pmax(G - h, 0)
  
  return(list(c(dG, dX, dI)))
  
}

# Dynamic Model function
dynamic_model <- function(t, y, parameters) {
  b0 <- parameters[1]
  b1 <- parameters[2]
  b2 <- parameters[3]
  b3 <- parameters[4]
  b4 <- parameters[5]
  b5 <- parameters[6]
  b6 <- parameters[7]
  b7 <- parameters[8]
  alpha <- parameters[9]
  Gb <- parameters[10]
  Ib <- parameters[11]
  
  G <- y[1]
  I <- y[2]
  
  if (t < b5)
    Glag <- Gb
  else
    Glag <- lagvalue(t - b5, 1)
  
  dG <- -b1 * G - (b4 * I * G) / (alpha * G + 1) + b7
  dI <- b6 * Glag - b2 * I
  
  list(c(dG, dI))
}