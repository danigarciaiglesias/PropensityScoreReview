experimento <- function (n,b) {
  
  data <- vector ()
  
  data$Id <- c (1:n)
  data$Tto <- c (rep(1,n/2), rep (0,n/2))
  
  data$A1 <- c (rbinom (n/2,1,0.75), rbinom (n/2,1,0.25))
  data$A2 <- c (rbinom (n/2,1,0.7), rbinom (n/2,1,0.30))
  data$A3 <- c (rbinom (n/2,1,0.65), rbinom (n/2,1,0.35))
  data$A4 <- c (rbinom (n/2,1,0.6), rbinom (n/2,1,0.40))
  data$A5 <- c (rbinom (n/2,1,0.55), rbinom (n/2,1,0.45))
  
  data$B1 <- rbinom (n,1,0.5)
  data$B2 <- rbinom (n,1,0.5)
  data$B3 <- rbinom (n,1,0.5)
  data$B4 <- rbinom (n,1,0.5)
  data$B5 <- rbinom (n,1,0.5)
  
  data$C1 <- c (rbinom (n/2,1,0.7), rbinom (n/2,1,0.3))
  data$C2 <- c (rbinom (n/2,1,0.6), rbinom (n/2,1,0.4))
  data$C3 <- c (rbinom (n/2,1,0.55), rbinom (n/2,1,0.45))
  data$C4 <- c (rbinom (n/2,1,0.4), rbinom (n/2,1,0.6))
  data$C5 <- c (rbinom (n/2,1,0.3), rbinom (n/2,1,0.7))
  
  data$D1 <- rbinom (n,1,0.3)
  data$D2 <- rbinom (n,1,0.4)
  data$D3 <- rbinom (n,1,0.5)
  data$D4 <- rbinom (n,1,0.6)
  data$D5 <- rbinom (n,1,0.7)
  
  data$E <- rnorm(n,0,1)
  
  #data$exponente <- data$Tto*(2) + data$B1*(-1) + data$B2*(-0.5) + data$B3*(1) + data$B4*1.5 + data$B5*2 + rnorm(n,0,1)
  data$exponente <- data$Tto*(b) + data$A1*1.5 + data$A2*1 + data$A3+0.5 + data$A4*(-0.5) + data$A5*(-1.5) + data$B1*(-1) + data$B2*(-0.5) + data$B3*(1) + data$B4*1.5 + data$B5*2 + data$E
  #data$exponente <- data$Tto*(b)
  data$prob_resultado <- exp(data$exponente) / (1+exp(data$exponente))
  
  resultado <- vector ()
  for (k in 1:n) { resultado <- c (resultado, rbinom(1,1,data$prob_resultado[k])) }
  data$resultado <- resultado
  
  data$riesgos <- b
  
  return (data)
  
}