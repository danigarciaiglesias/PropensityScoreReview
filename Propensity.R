library(MatchIt)

propensity <- function (data) {
  
  data <- as.data.frame(data)
  
  #Multi model
  multi_model <- glm (data$resultado~data$Tto + data$A1 + data$A2 + data$A3 + data$A4 + data$A5 + data$B1 + data$B2 + data$B3 + data$B4 + data$B5 + data$C1 + data$C2 + data$C3 + data$C4 + data$C5, family="binomial")

  #Non adjusted model
  single_model <- glm (data$resultado~data$Tto, family="binomial")

  #propensity score matching
  data_match <- matchit(Tto~A1 + A2 + A3 + A4 + A5 + B1 + B2 + B3 + B4 + B5 + C1 + C2 + C3 + C4 + C5 + D1 + D2 + D3 + D4 + D5, method = "nearest", caliper=0.2, data=data)
  data_matched <- match.data(data_match)
  
  #analisis multi
  multi_model_match <- glm (data_matched$resultado~data_matched$Tto + data_matched$A1 + data_matched$A2 + data_matched$A3 + data_matched$A4 + data_matched$A5 + data_matched$B1 + data_matched$B2 + data_matched$B3 + data_matched$B4 + data_matched$B5 + data_matched$C1 + data_matched$C2 + data_matched$C3 + data_matched$C4 + data_matched$C5, family="binomial")
  
  #OR
  OR_match <- glm(data_matched$resultado~data_matched$Tto, family="binomial")

  #Matched %
  PorcMatched <- data_match$nn[3,1]/data_match$nn[1,1]
  
  #Tto - Selection Bias
  tableBiasA <- cbind (
    c (
      sum(data$A1[data$Tto==1]), sum(data$A2[data$Tto==1]), sum(data$A3[data$Tto==1]), sum(data$A4[data$Tto==1]), sum(data$A5[data$Tto==1]), 
      sum(data$B1[data$Tto==1]), sum(data$B2[data$Tto==1]), sum(data$B3[data$Tto==1]), sum(data$B4[data$Tto==1]), sum(data$B5[data$Tto==1]), 
      sum(data$C1[data$Tto==1]), sum(data$C2[data$Tto==1]), sum(data$C3[data$Tto==1]), sum(data$C4[data$Tto==1]), sum(data$C5[data$Tto==1]), 
      sum(data$D1[data$Tto==1]), sum(data$D2[data$Tto==1]), sum(data$D3[data$Tto==1]), sum(data$D4[data$Tto==1]), sum(data$D5[data$Tto==1])
    ),
    c (
      sum(data$A1[data$Tto==0]), sum(data$A2[data$Tto==0]), sum(data$A3[data$Tto==0]), sum(data$A4[data$Tto==0]), sum(data$A5[data$Tto==0]), 
      sum(data$B1[data$Tto==0]), sum(data$B2[data$Tto==0]), sum(data$B3[data$Tto==0]), sum(data$B4[data$Tto==0]), sum(data$B5[data$Tto==0]), 
      sum(data$C1[data$Tto==0]), sum(data$C2[data$Tto==0]), sum(data$C3[data$Tto==0]), sum(data$C4[data$Tto==0]), sum(data$C5[data$Tto==0]), 
      sum(data$D1[data$Tto==0]), sum(data$D2[data$Tto==0]), sum(data$D3[data$Tto==0]), sum(data$D4[data$Tto==0]), sum(data$D5[data$Tto==0])
    )
  )
  tableBiasB <- cbind (
    c (
      sum(data_matched$A1[data_matched$Tto==1]), sum(data_matched$A2[data_matched$Tto==1]), sum(data_matched$A3[data_matched$Tto==1]), sum(data_matched$A4[data_matched$Tto==1]), sum(data_matched$A5[data_matched$Tto==1]), 
      sum(data_matched$B1[data_matched$Tto==1]), sum(data_matched$B2[data_matched$Tto==1]), sum(data_matched$B3[data_matched$Tto==1]), sum(data_matched$B4[data_matched$Tto==1]), sum(data_matched$B5[data_matched$Tto==1]), 
      sum(data_matched$C1[data_matched$Tto==1]), sum(data_matched$C2[data_matched$Tto==1]), sum(data_matched$C3[data_matched$Tto==1]), sum(data_matched$C4[data_matched$Tto==1]), sum(data_matched$C5[data_matched$Tto==1]), 
      sum(data_matched$D1[data_matched$Tto==1]), sum(data_matched$D2[data_matched$Tto==1]), sum(data_matched$D3[data_matched$Tto==1]), sum(data_matched$D4[data_matched$Tto==1]), sum(data_matched$D5[data_matched$Tto==1])
    ),
    c (
      sum(data_matched$A1[data_matched$Tto==0]), sum(data_matched$A2[data_matched$Tto==0]), sum(data_matched$A3[data_matched$Tto==0]), sum(data_matched$A4[data_matched$Tto==0]), sum(data_matched$A5[data_matched$Tto==0]), 
      sum(data_matched$B1[data_matched$Tto==0]), sum(data_matched$B2[data_matched$Tto==0]), sum(data_matched$B3[data_matched$Tto==0]), sum(data_matched$B4[data_matched$Tto==0]), sum(data_matched$B5[data_matched$Tto==0]), 
      sum(data_matched$C1[data_matched$Tto==0]), sum(data_matched$C2[data_matched$Tto==0]), sum(data_matched$C3[data_matched$Tto==0]), sum(data_matched$C4[data_matched$Tto==0]), sum(data_matched$C5[data_matched$Tto==0]), 
      sum(data_matched$D1[data_matched$Tto==0]), sum(data_matched$D2[data_matched$Tto==0]), sum(data_matched$D3[data_matched$Tto==0]), sum(data_matched$D4[data_matched$Tto==0]), sum(data_matched$D5[data_matched$Tto==0])
    )
  )
  
  chiA <- chisq.test(tableBiasA)
  chiB <- chisq.test(tableBiasB)
  biasReduction <- (chiA$statistic-chiB$statistic)/chiA$statistic
  
  #Resultado
  resultado <- c (
    summary(single_model)$coefficients[2,1],
    summary(single_model)$coefficients[2,2],
    (data$riesgos[1] - summary(single_model)$coefficients[2,1]) / data$riesgos[1],
    summary(multi_model)$coefficients[2,1],
    summary(multi_model)$coefficients[2,2],
    (data$riesgos[1] - summary(multi_model)$coefficients[2,1]) / data$riesgos[1],
    (data$riesgos[1] - summary(single_model)$coefficients[2,1]) / data$riesgos[1] - (data$riesgos[1] - summary(multi_model)$coefficients[2,1]) / data$riesgos[1],
    summary(multi_model_match)$coefficients[2,1],
    summary(multi_model_match)$coefficients[2,2],
    (data$riesgos[1] - summary(multi_model_match)$coefficients[2,1]) / data$riesgos[1],
    (data$riesgos[1] - summary(single_model)$coefficients[2,1]) / data$riesgos[1] - (data$riesgos[1] - summary(multi_model_match)$coefficients[2,1]) / data$riesgos[1],
    summary(OR_match)$coefficients[2,1],
    summary(OR_match)$coefficients[2,2],
    (data$riesgos[1] - summary(OR_match)$coefficients[2,1]) / data$riesgos[1],
    (data$riesgos[1] - summary(single_model)$coefficients[2,1]) / data$riesgos[1] - (data$riesgos[1] - summary(OR_match)$coefficients[2,1]) / data$riesgos[1],
    PorcMatched,
    biasReduction
  )
  
  return (resultado)
  
}