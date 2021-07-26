library(dplyr)
library(stringr)
library(sem)


mastar_nps1 = read.csv("master_nps.csv")
mastar_nps = mastar_nps[,-1]
mastar_1_cor = round(cor(mastar_nps),digits = 2)
mastar_1_cor

sem_step1 = readMoments(names=c("美味しい","アクセス","価格","できたて","本格的","品揃え","個性","雰囲気","愛想","特能","気軽","推薦度"))
1.00
-0.19 1.00
-0.42 0.17 1.00
0.23 -0.11 -0.17 1.00
0.53 -0.27 -0.45 0.30 1.00
0.26 -0.05 -0.24 0.17 0.34 1.00
0.33 -0.13 -0.25 0.18 0.40 0.31 1.00
0.22 -0.03 -0.24 0.17 0.24 0.12 0.26 1.00
0.04 0.05 0.03 -0.03 0.03 -0.05 0.17 0.18 1.00
-0.13 0.12 0.15 -0.05 -0.18 -0.04 -0.06 0.14 0.09 1.00
-0.20 0.24 0.17 -0.10 -0.29 0.01 -0.10 -0.17 0.07 0.32 1.00
0.53 -0.16 -0.29 0.15 0.44 0.32 0.32 0.16 0.07 -0.09 -0.04 1.00

sem_step1

step1_model = specifyEquations()
美味しい = a1*本物
本格的 = a2*本物
できたて = a3*本物
品揃え = a4*パワー
雰囲気 = a5*パワー
個性 = a6*パワー
愛想 = a7*パワー
本物 = d1*パワー
推薦度 = b1*本物  + b4*価格 + b5*特能 + b6*アクセス + b7*気軽
V(本物) = 1
V(パワー) = 1
V(手間) = 1
V(アクセス) = 1
V(気軽) = 1

sem_01 = sem(model = step1_model,S = sem_step1, N = 372,fixed.x = c("価格","特能","アクセス","気軽"))
summary(sem_01, digits = 5)
standardizedCoefficients(sem_01)



step1_model
options(scipen = 100)
options(fit.indices = c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI",
                        "SRMR", "AIC", "AICc", "BIC", "CAIC"))

