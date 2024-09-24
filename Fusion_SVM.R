Fusion = function(Treino, Teste, n = 1000, n_weaks = 10, K = 4, R = F,
                  n_sphere = 50, K_sphere = 2500, eps = 0.5, VAR){
  
  T1 = Sys.time()
  
  # Weak's SVM's
  
  bases = cores = modelos = predicao = list()
  
  ## Coresets
  
  for(i in 1:n_weaks){cores[[i]] = Cor_Ctg(Treino, n_amostra = n_sphere,
                                           epsilon = eps, NN = K_sphere, VAR)}
  
  #    cat("Coresets finished! \n")
  
  T2 = Sys.time()
  
  ## Samples
  
  for(i in 1:n_weaks){bases[[i]] = cores[[i]][sample(1:nrow(cores[[i]]), size = n),]}
  
  ## Models
  
  for(i in 1:n_weaks){modelos[[i]] = ksvm(as.factor(Y) ~ ., data = bases[[i]],
                                          type = "C-svc", kernel = "rbfdot",
                                          prob.model = T)}
  
  ## Prediction
  
  for(i in 1:n_weaks){predicao[[i]] = predict(modelos[[i]], newdata = Treino,
                                              type = "prob")[,2]}
  
  pred = suppressMessages(bind_cols(predicao))
  pred_class = pred %>% mutate_all(funs(ifelse(. > .5, 1, 0))) %>% data.frame
  
  #    cat("Weaks finished! \n")
  
  # Final sample selection
  
  A = apply(pred, 1, sd)
  Final = data.frame(Treino, A)
  Final = Final %>% filter(A > quantile(A)[K],) %>% select(-A)
  
  #    cat("'Support Vector' selectioned! \n")
  
  # Final Model
  
  T3 = Sys.time()
  
  modelo = ksvm(as.factor(Y) ~ ., data = Final, type = "C-svc",
                kernel = "rbfdot")
  predicao = predict(modelo, newdata = Teste)
  Resul_Speed = metricas(table(predicao, Teste$Y))
  
  cat("Fusion", n_weaks, "Weaks finished! \n")
  
  T4 = Sys.time()
  
  # Resultados
  
  Tempos = data.frame(Coresets = T2 - T1, Amostra = T3 - T1,
                      Modelo = T4 - T3, Total = T4 - T1) 
  
  if(R){
    return(list(Bases = bases, WeaksSVMs = modelos, WeaksPred = pred, SD = A,
                Tempo = Tempos, BaseFinal = Final, Predicao = predicao,
                SVM = modelo, Metricas = Resul_Speed, SV = modelo@nSV))}
  else{return(data.frame(Tempos, Resul_Speed, N = nrow(Final), SV = modelo@nSV))}
} # Fusion W

Fusion_2 = function(Treino, Teste, n = 1000, n_weaks = 10, K = 4, R = F,
                    n_sphere = 50, K_sphere = 2500, eps = 0.5, VAR){
  
  T1 = Sys.time()
  
  # Weak's SVM's
  
  bases = modelos = predicao = list() # cores = 
  
  ## Coresets
  
  for(i in 1:n_weaks){bases[[i]] = Cor_Ctg(Treino, n_amostra = n_sphere,
                                           epsilon = eps, NN = K_sphere, VAR)}
  
  #    cat("Coresets finished! \n")
  
  T2 = Sys.time()
  
  ## Models
  
  for(i in 1:n_weaks){modelos[[i]] = ksvm(as.factor(Y) ~ ., data = bases[[i]],
                                          type = "C-svc", kernel = "rbfdot",
                                          prob.model = T)}
  
  ## Prediction
  
  for(i in 1:n_weaks){predicao[[i]] = predict(modelos[[i]], newdata = Treino,
                                              type = "prob")[,2]}
  
  pred = suppressMessages(bind_cols(predicao))
  pred_class = pred %>% mutate_all(funs(ifelse(. > .5, 1, 0))) %>% data.frame
  
  #    cat("Weaks finished! \n")
  
  # Final sample selection
  
  A = apply(pred, 1, sd)
  Final = data.frame(Treino, A)
  Final = Final %>% filter(A > quantile(A)[K],) %>% select(-A)
  
  #    cat("'Support Vector' selectioned! \n")
  
  # Final Model
  
  T3 = Sys.time()
  
  modelo = ksvm(as.factor(Y) ~ ., data = Final, type = "C-svc",
                kernel = "rbfdot")
  predicao = predict(modelo, newdata = Teste)
  Resul_Speed = metricas(table(predicao, Teste$Y))
  
  cat("Fusion", n_weaks, "CoreWeaks finished! \n")
  
  T4 = Sys.time()
  
  # Resultados
  
  Tempos = data.frame(Coresets = T2 - T1, Amostra = T3 - T1,
                      Modelo = T4 - T3, Total = T4 - T1) 
  
  if(R){
    return(list(Bases = bases, WeaksSVMs = modelos, WeaksPred = pred, SD = A,
                Tempo = Tempos, BaseFinal = Final, Predicao = predicao,
                SVM = modelo, Metricas = Resul_Speed, SV = modelo@nSV))}
  else{return(data.frame(Tempos, Resul_Speed, N = nrow(Final), SV = modelo@nSV))}
} # Fusion CW