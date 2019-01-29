source ("Experimento.R")
source ("Propensity.R")
source ("Plot.R")

set.seed(123)

AnalisisEnd <- vector ()
resultado_propensity_End <- vector ()

riesgos <- seq (-5,0.1,0.1)

for (b in riesgos) {
  
  resultado_propensity <- vector ()
  
  for (k in 1:200) {
    
    data <- experimento (500, b)
    resultado_propensity <- rbind ( resultado_propensity, propensity (data))
    
  }
  
  resultado_propensity_End <- rbind (resultado_propensity_End, resultado_propensity)
  
  nombres_col <- c("Single", "Single_Precission", "Single_RelativeError", "Multi", "Multi_Precission","Multi_RelativeError","Multi_RelativeError_Reduction", "Match_Multi", "Match_Multi_Precission", "Match_Multi_RelativeError", "Match_Multi_RelativeError_Reduction", "Match_OR", "Match_OR_Precission", "Match_OR_RelativeError", "Match_OR_RelativeError_Reduction", "PorcMatch", "BiasReduction")
  colnames(resultado_propensity) <- nombres_col
  resultado_propensity <- as.data.frame(resultado_propensity)
  
  #Analisis
  TTest <- list (
    Single=t.test(resultado_propensity$Single),
    Single_Error=t.test(resultado_propensity$Single_RelativeError),
    Multi=t.test(resultado_propensity$Multi),
    Multi_Error=t.test(resultado_propensity$Multi_RelativeError),
    Multi_Error_Reduction=t.test(resultado_propensity$Multi_RelativeError_Reduction),
    Match_Multi=t.test(resultado_propensity$Match_Multi),
    Match_Multi_Error=t.test(resultado_propensity$Match_Multi_RelativeError),
    Match_Multi_Error_Reduction=t.test(resultado_propensity$Match_Multi_RelativeError_Reduction),
    Match_OR=t.test(resultado_propensity$Match_OR),
    Match_OR_Error=t.test(resultado_propensity$Match_OR_RelativeError),
    Match_OR_Error_Reduction=t.test(resultado_propensity$Match_OR_RelativeError_Reduction)
  )
  AnalisisCI <- c (
    TTest$Single$estimate,TTest$Single$conf.int[1], TTest$Single$conf.int[2],
    TTest$Single_Error$estimate,TTest$Single_Error$conf.int[1], TTest$Single_Error$conf.int[2],
    TTest$Multi$estimate,TTest$Multi$conf.int[1], TTest$Multi$conf.int[2],
    TTest$Multi_Error$estimate,TTest$Multi_Error$conf.int[1], TTest$Multi_Error$conf.int[2],
    TTest$Multi_Error_Reduction$estimate,TTest$Multi_Error_Reduction$conf.int[1], TTest$Multi_Error_Reduction$conf.int[2],
    TTest$Match_Multi$estimate,TTest$Match_Multi$conf.int[1], TTest$Match_Multi$conf.int[2],
    TTest$Match_Multi_Error$estimate,TTest$Match_Multi_Error$conf.int[1], TTest$Match_Multi_Error$conf.int[2],
    TTest$Match_Multi_Error_Reduction$estimate,TTest$Match_Multi_Error_Reduction$conf.int[1], TTest$Match_Multi_Error_Reduction$conf.int[2],
    TTest$ Match_OR$estimate,TTest$Match_OR$conf.int[1], TTest$Match_OR$conf.int[2],
    TTest$ Match_OR_Error$estimate,TTest$Match_OR_Error$conf.int[1], TTest$Match_OR_Error$conf.int[2],
    TTest$ Match_OR_Error_Reduction$estimate,TTest$Match_OR_Error_Reduction$conf.int[1], TTest$Match_OR_Error_Reduction$conf.int[2]
  )
  
  AnalisisMultiT <- c (
    t.test(resultado_propensity$Multi, resultado_propensity$Single, paired=T)$estimate,
    t.test(resultado_propensity$Multi, resultado_propensity$Single, paired=T)$p.value,
    t.test(resultado_propensity$Multi, resultado_propensity$Match_Multi, paired=T)$estimate,
    t.test(resultado_propensity$Multi, resultado_propensity$Match_Multi, paired=T)$p.value,
    t.test(resultado_propensity$Multi, resultado_propensity$Match_OR, paired=T)$estimate,
    t.test(resultado_propensity$Multi, resultado_propensity$Match_OR, paired=T)$p.value
  )
  AnalisisSingleT <- c (
    t.test(resultado_propensity$Single, resultado_propensity$Match_Multi, paired=T)$estimate,
    t.test(resultado_propensity$Single, resultado_propensity$Match_Multi, paired=T)$p.value,
    t.test(resultado_propensity$Single, resultado_propensity$Match_OR, paired=T)$estimate,
    t.test(resultado_propensity$Single, resultado_propensity$Match_OR, paired=T)$p.value
  )
  
  #Analisis Error
  Datos_ANOVA_AnalisisError <- list (
    Valores= c (resultado_propensity$Single_RelativeError, resultado_propensity$Multi_RelativeError, resultado_propensity$Match_OR_RelativeError),
    Etiquetas=c(rep("Single",k),rep("Multi",k),rep("Propensity",k))
  )
  fit <- aov (Datos_ANOVA_AnalisisError$Valores~Datos_ANOVA_AnalisisError$Etiquetas)
  AnalisisError <- c (
    summary(fit)[[1]][["F value"]][1],
    summary(fit)[[1]][["Pr(>F)"]][1],
    t.test(resultado_propensity$Multi_RelativeError_Reduction, resultado_propensity$Match_OR_RelativeError_Reduction, paired=T)$estimate,
    t.test(resultado_propensity$Multi_RelativeError_Reduction, resultado_propensity$Match_OR_RelativeError_Reduction, paired=T)$p.value
  )
  
  BiasReduction <- c (t.test (resultado_propensity$BiasReduction)$estimate, t.test (resultado_propensity$BiasReduction)$conf.int[1], t.test (resultado_propensity$BiasReduction)$conf.int[2])
  
  AnalisisCorTestMulti <- c (
    cor.test(resultado_propensity$Multi,resultado_propensity$Single)$estimate,
    cor.test(resultado_propensity$Multi,resultado_propensity$Single)$p.value,
    cor.test(resultado_propensity$Multi,resultado_propensity$Match_Multi)$estimate,
    cor.test(resultado_propensity$Multi,resultado_propensity$Match_Multi)$p.value,
    cor.test(resultado_propensity$Multi,resultado_propensity$Match_OR)$estimate,
    cor.test(resultado_propensity$Multi,resultado_propensity$Match_OR)$p.value
  )
  
  AnalisisCorTestSingle <- c (
    cor.test(resultado_propensity$Single,resultado_propensity$Match_Multi)$estimate,
    cor.test(resultado_propensity$Single,resultado_propensity$Match_Multi)$p.value,
    cor.test(resultado_propensity$Single,resultado_propensity$Match_OR)$estimate,
    cor.test(resultado_propensity$Single,resultado_propensity$Match_OR)$p.value
  )
  
  AnalisisPorcMatched <- c (
    t.test(resultado_propensity$PorcMatch)$estimate,
    t.test(resultado_propensity$PorcMatch)$conf.int[1],
    t.test(resultado_propensity$PorcMatch)$conf.int[2]
  )
  
  AnalisisEnd <- rbind (
    AnalisisEnd,
    c (AnalisisMultiT, AnalisisSingleT, AnalisisCI, AnalisisError, BiasReduction, AnalisisCorTestMulti, AnalisisCorTestSingle, AnalisisPorcMatched, b)
  )

}

colnames(resultado_propensity_End) <- nombres_col
resultado_propensity_End <- as.data.frame(resultado_propensity_End)

nombres_col_SingleT <- paste ("TTest", rep(c ("Single_MatchMulti", "Single_MatchOR"), each=2), c("estimate", "p"), sep="_")
nombres_col_MultiT <- paste ("TTest", rep(c ("Multi_Single", "Multi_MatchMulti", "Multi_MatchOR"), each=2), c("estimate", "p"), sep="_")
nombres_col_CI <- paste (rep(c("Single", "Single_Error" ,"Multi", "Multi_Error", "Multi_Error_Reduction", "MatchMulti", "MatchMulti_Error", "MatchMulti_Error_Reduction", "MatchOR", "MatchOR_Error", "MatchOR_Error_Reduction"), each=3), c("Mean", "LimInf", "LimSup"), sep="_")
nombres_col_Error <- paste (rep (c("RelativeError_ANOVA", "ErrorReduction_TTest"), each=2), c("estimate", "p"), sep="_")
nombres_col_BiasReduction <- paste ("BiasReduction", c("Mean", "LimInf", "LimSup"), sep="_")
nombres_col_CorSingle <- paste ("CorT", rep(c ("Single_MatchMulti", "Single_MatchOR"), each=2), c("estimate", "p"), sep="_")
nombres_col_CorMulti <- paste ("CorT", rep(c ("Multi_Single", "Multi_MatchMulti", "Multi_MatchOR"), each=2), c("estimate", "p"), sep="_")
nombres_col_PorcMatch <- c ("PorcMatch_Mean", "PorcMatch_LimInf", "PorcMatch_LimSup")
nombres_col <- c (nombres_col_MultiT, nombres_col_SingleT, nombres_col_CI, nombres_col_Error, nombres_col_BiasReduction, nombres_col_CorMulti, nombres_col_CorSingle, nombres_col_PorcMatch, "Riesgo")
colnames(AnalisisEnd) <- nombres_col
AnalisisEnd <- as.data.frame(AnalisisEnd)

write.csv(AnalisisEnd, "Output.csv")

#Plot
plot (AnalisisEnd$Riesgo, AnalisisEnd$Riesgo, "l", col="red", xlab="Real Risk", ylab="Estimated Risk", main="Multivariate Analysis Vs Propensity Score Matching", ylim=c(-5.5,1))
polygon (c(AnalisisEnd$Riesgo, rev(AnalisisEnd$Riesgo)), c(AnalisisEnd$Single_LimInf, rev(AnalisisEnd$Single_LimSup) ) , col="green", border=NA)
points (AnalisisEnd$Riesgo, AnalisisEnd$Single_Mean, pch=5)
polygon (c(AnalisisEnd$Riesgo, rev(AnalisisEnd$Riesgo)), c(AnalisisEnd$MatchOR_LimInf, rev(AnalisisEnd$MatchOR_LimSup) ) , col="sky blue", border=NA)
points (AnalisisEnd$Riesgo, AnalisisEnd$OR_All_Mean, pch=3)
polygon (c(AnalisisEnd$Riesgo, rev(AnalisisEnd$Riesgo)), c(AnalisisEnd$Multi_LimInf, rev(AnalisisEnd$Multi_LimSup) ) , col="grey", border=NA)
points (AnalisisEnd$Riesgo, AnalisisEnd$MatchOR_Mean, pch=3)
points (AnalisisEnd$Riesgo, AnalisisEnd$Multi_Mean, pch=1)
lines (AnalisisEnd$Riesgo, AnalisisEnd$Riesgo, col="red")
lines (c(-5,0), c(0,0), lty=3)
legend (-2.3,-3.3, lty=c(3,1,0,0,0), pch=c(NA,NA,1,3,5), col=c("black", "red", "black", "black", "black"), c ("Null Effect", "Teorical Real Risk", "Multivariate Estimated Risk", "Propensity Score Estimated Risk", "Non adjusted Estimated Risk"))

#Plot2
plot (AnalisisEnd$Riesgo, AnalisisEnd$PorcMatch_Mean, "p", pch=5, xlab="Real Risk", ylab="%", main="Propensity Score Matching", ylim=c(0,1), xlim=c(-5,-1))
polygon (c(AnalisisEnd$Riesgo, rev(AnalisisEnd$Riesgo)), c(AnalisisEnd$MatchOR_Error_Reduction_LimInf, rev(AnalisisEnd$MatchOR_Error_Reduction_LimSup) ) , col="grey", border=NA)
polygon (c(AnalisisEnd$Riesgo, rev(AnalisisEnd$Riesgo)), c(AnalisisEnd$PorcMatch_LimInf, rev(AnalisisEnd$PorcMatch_LimSup) ) , col="green", border=NA)
polygon (c(AnalisisEnd$Riesgo, rev(AnalisisEnd$Riesgo)), c(AnalisisEnd$BiasReduction_LimInf, rev(AnalisisEnd$BiasReduction_LimSup) ) , col="sky blue", border=NA)
points (AnalisisEnd$Riesgo, AnalisisEnd$PorcMatch_Mean, pch=5)
points (AnalisisEnd$Riesgo, AnalisisEnd$BiasReduction_Mean, pch=3)
points (AnalisisEnd$Riesgo, AnalisisEnd$MatchOR_Error_Reduction_Mean, pch=1)



