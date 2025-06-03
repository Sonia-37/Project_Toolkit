source("models.R")

#h(G) function

h_function <- function(G) {
  0.021/(24*60) * 1/(1 + (8.4/G)^1.7) - 0.025/(24*60) * 1/(1 + (G/4.8)^8.5)
}

svg("h_function.svg", width=8, height = 6)
G_vals <- seq(0,10,0.03)
plot(G_vals, h_function(G_vals), xlab="Glucose (mM)", type="l", ylab="Change of beta cells functional mass")
title("h(G) function - beta cell production rate")
dev.off()

params <- c(
  u0 = 1/30,
  Si = 5 * 10^-4,
  C = 10^-3,
  p = 0.03,
  gamma = 0.3,
  mu_production = 0.021/(24 * 60),
  mu_removal = 0.025/(24 * 60),
  alfa = 7.85
)

#BIG MODEL
init <- c(
  G = 5,
  I = 0.6,
  B = 0.5
)
time <- seq(0, 140, by = 0.5)

out <- ode(
  y = init,
  times = time,
  func = BIG_model,
  parms = params,
  rtol = 1e-9, 
  atol = 1e-9
)

colnames(out) <- c("time", "G", "I", "B")
out <- as.data.frame(out)

plot(out$time, out$I)