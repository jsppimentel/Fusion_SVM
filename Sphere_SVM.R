Sphere = function(Treino, Teste, n_amostra = 50, epsilon = .5,
                  K, R = F, VAR){
  
  ## Coreset
  
  T1 = Sys.time()
  
  CORESET = Cor_Ctg(DT = Treino, n_amostra, epsilon, NN = K, VAR)
  
  #   cat("Coresets finished! \n")
  
  T2 = Sys.time()
  
  ## Modelagem
  
  Modelo = ksvm(as.factor(Y) ~ ., data = CORESET, type = "C-svc",
                kernel = "rbfdot")
  Pred = predict(Modelo, newdata = Teste)
  Resul = metricas(table(Pred, Teste$Y))
  
  cat("Sphere", K, "finished! \n")
  
  T3 = Sys.time()
  
  # Resultados
  
  Tempos = data.frame(Coresets = T2 - T1, Modelo = T3 - T2, Total = T3 - T1) 
  
  if(R){
    return(list(Coreset = CORESET, Tempo = Tempos, Modelo_Sphere = Modelo,
                Predicao = Pred, Metricas = Resul, SV = Modelo@nSV))}
  else{return(data.frame(Tempos, Resul, N = nrow(CORESET), SV = Modelo@nSV))}
  
} # Sphere SVM