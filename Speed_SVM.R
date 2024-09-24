Speed = function(Treino, Teste, n = 1000, n_weaks = 10,
                 K = 4, R = F){
  
  T1 = Sys.time()
  
  # Weak's SVM's
  
  bases = modelos = predicao = list()
  
  ## Samples
  
  for(i in 1:n_weaks){bases[[i]] = Treino[sample(1:nrow(Treino), size = n),]}
  
  ## Models
  
  for(i in 1:n_weaks){modelos[[i]] = ksvm(as.factor(Y) ~ ., data = bases[[i]],
                                          type = "C-svc",
                                          kernel = "rbfdot", prob.model = T)}
  
  ## Prediction
  
  for(i in 1:n_weaks){predicao[[i]] = predict(modelos[[i]], newdata = Treino,
                                              type = "prob")[,2]}
  
  pred = suppressMessages(bind_cols(predicao))
  pred_class = pred %>% mutate_all(funs(ifelse(. > .5, 1, 0))) %>% data.frame
  
  ## Calculation of metrics for Weak SVM
  
  RES = array(dim = c(n_weaks,4), dimnames = list(NULL, c("ACC", "SEN", "ESP", "F1")))
  for(i in 1:n_weaks){RES[i,] = metricas(table(pred_class[,i], Treino$Y))}
  
  # Final sample selection
  
  A = apply(pred, 1, sd)
  Final = data.frame(Treino, A)
  Final = Final %>% filter(A > quantile(A)[K],) %>% select(-A)
  
  # Final Model
  
  T2 = Sys.time()
  
  modelo = ksvm(as.factor(Y) ~ ., data = Final, type = "C-svc", kernel = "rbfdot")
  predicao = predict(modelo, newdata = Teste)
  metricas = metricas(table(predicao, Teste$Y))
  
  cat("Speed", n_weaks, "Weaks finished! \n")
  
  T3 = Sys.time()
  
  # Resultados
  
  Tempos = data.frame(Amostra = T2 - T1, Modelo = T3 - T2, Total = T3 - T1) 
  
  if(R){
    return(list(Bases = bases, WeaksSVMs = modelos, WeaksPred = pred, SD = A,
                WeaksMetricas = RES, Tempo = Tempos, BaseFinal = Final, 
                Predicao = predicao, SVM = modelo, Metricas = metricas,
                SV = modelo@nSV))}
  else{return(data.frame(Tempos, metricas, N = nrow(Final), SV = modelo@nSV))}
} # Speed Up SVM