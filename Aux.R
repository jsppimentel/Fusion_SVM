metricas = function(tab){
  
  ACC = function(x){sum(diag(x))/sum(x)} # Acuracia
  
  SEN = function(x){x[2,2]/sum(x[,2])} # Sensibilidade - 1 classificado corretamente
  
  ESP = function(x){x[1,1]/sum(x[,1])} # Especificidade - 0 classsificado corretamente
  
  F1 = function(x){
    Precisao = x[2,2]/(x[2,2]+x[1,2])
    Recall = x[2,2]/(x[2,2]+x[2,1])
    F1 = (2*Precisao*Recall)/(Precisao + Recall)
    return(F1)
  } # F1 Score
  
  array(c(ACC(tab), SEN(tab), ESP(tab), F1(tab)), dim = c(1,4),
        dimnames = list(NULL, c("Acuracia","Sensibilidade","Especificidade","F1-Score")))
  
} # Metricas

Pre = function(DATA, m, prop = 0.8){
  
  AMOSTRA = DATA[sample(1:nrow(DATA), size = m),]
  
  indice = sample(1:nrow(AMOSTRA), size = m*prop)
  Treino = AMOSTRA[indice,] # Treino
  Teste = AMOSTRA[-indice,] # Teste
  
  cat("Pre-processing finished!! \n")
  
  return(list(X1 = Treino, X2 = Teste))
  
} ## Processamento Treino/Teste

Cor_Ctg = function(DT, n_amostra = 50, epsilon = 0.5, NN, VAR){
  
  n = nrow(DT)
  
  ID = sample(1:n, n_amostra)                                   # Obs p/ amostra
  
  XR = DT %>% select(all_of(VAR)); XR = XR[ID,]                 # Amostra X
  YR = DT %>% select(Y); YR = YR[ID,]                           # Amostra Y
  ZR = DT %>% select(-Y) %>% select(-all_of(VAR)); ZR = ZR[ID,] # Variaveis categoricas
  
  CORESET = array(0, dim = c(1, ncol(DT))) %>% as.data.frame()
  names(CORESET) = c(names(XR), names(ZR), "Y")
  
  CENTRO = XR %>% apply(2, range) %>% apply(2, mean)            # Centro
  RAIO = mean((apply(XR, 2, max) - apply(XR, 2, min))/4)        # Raio
  
  for(i in 1:NN){
    DIST = dist(rbind(XR, CENTRO)) %>% as.matrix()
    DIST = DIST[nrow(DIST), -nrow(DIST)] %>% as.numeric()       # Distancias
    
    MAX = DIST %>% which.max()                                  # Ponto mais distante
    MIN = DIST %>% which.min()                                  # Ponto mais proximo
    
    if(DIST[MAX] > (1+epsilon)*RAIO & nrow(XR) > 1){
      
      ELEMENTO = cbind(XR[MAX,], ZR[MAX,], YR[MAX])             # Core Vector
      names(ELEMENTO) = names(CORESET)
      CORESET = rbind(CORESET, ELEMENTO)                        # Adicionado Core Vector
      
      Pt1 = XR[MAX,] - XR[MIN,]
      Pt2 = XR[MAX,] - CENTRO
      Pt3 = sqrt(sum((XR[MAX,] - XR[MIN,])^2))^2
      Pt4 = sqrt(sum(Pt2^2))^2
      
      Pt3 = ifelse(Pt3 == 0, 0.1, Pt3)
      
      RHO = sum(Pt1*Pt2)/Pt3
      BETHA = RHO - sqrt(abs(RHO^2 - (Pt4 - RAIO^2)/Pt3))
      
      CENTRO = CENTRO + BETHA*Pt1                                 # Atualizacao Centro
      
      XR = XR[-MAX,]                                              # Exclusao do Core Vector
    }
    else{
      epsilon = epsilon/2                                         # Atualizacao epsilon
      ID = sample(1:n, n_amostra)                                 # Novo ID
      XR = DT %>% select(all_of(VAR)); XR = XR[ID,]                   # Amostra X
      YR = DT %>% select(Y); YR = YR[ID,]                     # Amostra Y
      ZR = DT %>% select(-Y) %>% select(-all_of(VAR)); ZR = ZR[ID,]   # Variaveis categoricas
    }
  }
  
  return(CORESET[-1,])
}