###SEMの実行
library(dplyr)
library(stringr)
library(sem)


#データの読み込み
mastar_box = read.csv("master_box.csv")
mastar_box = mastar_box[,-1]

#専門を１に、コンビニ・スーパーを2に変換
mastar_box_1 = dplyr::mutate(mastar_box,業態=gsub(業態,pattern = "専門",replacement = 1))
mastar_box_1 = dplyr::mutate(mastar_box_1,業態=gsub(業態,pattern = c("コンビニ"),replacement = 0))
mastar_box_1 = dplyr::mutate(mastar_box_1,業態=gsub(業態,pattern = c("スーパー"),replacement = 0))

mastar_box_1$業態 = as.numeric(mastar_box_1$業態)

#相関行列を計算
mastar_1_cor = round(cor(mastar_box_1),digits = 2)
mastar_1_cor


#SEM用の相関行列を作成
sem_step1 = readMoments(names=c("美味しい","アクセス","価格","できたて","本格的","品揃え","個性","雰囲気","愛想","特能","気軽","業態"))
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
0.59 -0.32 -0.59 0.29 0.63 0.25 0.32 0.43 0.00 -0.14 -0.42 1.00
#この行まで入力したらコンソールを改行して入力を終了させる

sem_step1

#測定方程式・構造方程式の作成
step1_model = specifyEquations()
美味しい = a1*本物
本格的 = a2*本物
できたて = a3*本物
品揃え = a4*パワー
雰囲気 = a5*パワー
個性 = a6*パワー
愛想 = a7*パワー
アクセス = a8*手間
気軽 = a9*手間
本物 = d1*パワー
業態 = b1*本物 + b3*手間 + b4*価格 + b5*特能
V(本物) = 1
V(パワー) = 1
V(手間) = 1
#相関行列と同様にコンソールを改行して入力を終了させる

step1_model

#表示のオプション(Rを再起動させるとリセットされるっぽい？)
options(scipen = 100)　　　#指数表記の回避
options(fit.indices = c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI",
                        "SRMR", "AIC", "AICc", "BIC", "CAIC"))　　#適合度指標をたくさん表示させる


#相関行列、方程式を基にSEMを実行
sem_01 = sem(model = step1_model,S = sem_step1, N = 372,fixed.x = c("価格","特能"))
summary(sem_01, digits = 5)　　　　　　##適合度指標など+非標準化係数
standardizedCoefficients(sem_01)       ##標準化係数


          

