library(deSolve)
library(PBSddesolve)


# Model function
model <- function(t, y, parameters) {
  # Parameters
  b0 <- parameters[1]        # theoretical glycemia at time 0 after the glucose bolus
  b1 <- parameters[2]        # spontaneous glucose "rst order disappearance rate constant
  b2 <- parameters[3]        # apparent"rst-order disappearance rate constantfor insulin
  b3 <- parameters[4]        # first-phase insulin concentration increase
  b4 <- parameters[5]        # rate of insulin-dependent uptake of glucose per unit of 
                             # blood insulin concentration
  b5 <- parameters[6]        # time delay
  b6 <- parameters[7]        # insulin secretion rate per unit of blood glucose concentration
  b7 <- parameters[8]        # constant release of glucose from liver
  alpha <- parameters[9]     # half-saturation constant
  Gb <- parameters[10]       # base level of blood glucose
  Ib <- parameters[11]       # base level of blood insulin
  
  # Variables
  G <- y[1]
  I <- y[2]
  
  # time delayed glucose concentration
  if (t < b5)
    Glag <- Gb 
  else
    Glag <- lagvalue(t - b5, 1)
    
  dG <- -b1 * G - (b4 * I * G) / (alpha * G + 1) + b7
  dI <- b6 * Glag - b2 * I
  
  list(c(dG, dI))

}

# Parameters
params <- c(
  b0 = 209,
  b1 = 0.0226,
  b2 = 0.1262,
  b3 = 1.64,
  b4 = 0.0000564,
  b5 = 15,        
  b6 = 0.064,
  b7 = 1.93,
  alpha = 0.01,
  Gb = 88,
  Ib = 68.6
  
)

# Initial state
G0 <- params["Gb"] + params["b0"]
I0 <- params["Ib"] + params["b3"] * params["b0"]
initial_state <- c(G = G0, I = I0)

times <- seq(0, 180, by = 0.1)

# Simulation
out <- dede(
  y = initial_state,
  times = times,
  func = model,
  parms = params,
  method = "lsoda"
)

colnames(out) <- c("time", "G", "I")


# Plot
matplot(out[, "time"], out[, c("G", "I")], type = "l", lty = 1, col = c("blue", "red"), 
        ylim=c(0, 400),
        xlab = "time (min)", ylab = "Concentration", main = "IVGTT glucose–insulin interaction dynamic model")
legend("topright", legend = c("Glucose", "Insulin"), col = c("blue", "red"), lty = 1)



