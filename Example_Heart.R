### REQUIRED PACKAGES

library(kernlab)
library(tidyverse)

### FUNCTIONS

source("Aux.R")
source("Speed_SVM.R")
source("Sphere_SVM.R")
source("Fusion_SVM.R")

### HEART DISEASE DATASET WITH 500,000 OBSERVATIONS AND BALANCED

dt = read.csv("heart_disease.csv")

### DIVISION OF DATA INTO TRAINING AND TEST

D = Pre(dt, m = 10000)

### IDENTIFICATION OF NUMERICAL VARIABLES PRESENT IN THE DATA

V = c("BMI", "MentHlth", "PhysHlth", "Age")

### MODELING

# Speed UP SVM

Speed(Treino = D$X1,                # Training Set
      Teste = D$X2,                 # Test Set
      n = 1000,                     # Sample size
      n_weaks = 10,                 # Number of samples
      R = F)                        # Skip all results

# Sphere SVM

Sphere(Treino = D$X1,               # Training Set
       Teste = D$X2,                # Test Set
       n_amostra = 50,              # Coreset size
       epsilon = 0.5,               # Epsilon parameter
       K = 1000,                    # Maximum Final Sample Size
       R = F,                       # Skip all results
       VAR = V)                     # Identification of numerical variables

# Fusion W SVM

Fusion(Treino = D$X1,               # Training Set
       Teste = D$X2,                # Test Set
       n = 1000,                    # Sample size
       n_weaks = 10,                # Number of samples
       R = F,                       # Skip all results
       n_sphere = 50,               # Coreset size
       K_sphere = 2500,             # Maximum Final Sample Size
       eps = 0.5,                   # Epsilon parameter
       VAR = V)                     # Identification of numerical variables

# Fusion CW SVM

Fusion_2(Treino = D$X1,             # Training Set
         Teste = D$X2,              # Test Set
         n = 1000,                  # Sample size
         n_weaks = 10,              # Number of samples
         R = F,                     # Skip all results
         n_sphere = 50,             # Coreset size
         K_sphere = 2500,           # Maximum Final Sample Size
         eps = 0.5,                 # Epsilon parameter
         VAR = V)                   # Identification of numerical variables
