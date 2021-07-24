###NPSの測定

#データの読み込み
raw_data = read.csv("ver1.2 1204.csv", fileEncoding = "UTF-8")

colnames(raw_data) = c("time","coffe-expert use","coffee-expert why","coffee-expert nps",
                       "coffe-conveni use","coffee-conveni why","coffee-conveni nps",
                       "coffe-super use","coffee-super why","coffee-super nps",
                       "bread-expert use","bread-expert why","bread-expert nps",
                       "bread-conveni use","bread-conveni why","bread-conveni nps",
                       "bread-super use","bread-super why","bread-super nps",
                       "cake-expert use","cake-expert why","cake-expert nps",
                       "cake-conveni use","cake-conveni why","cake-conveni nps",
                       "cake-super use","cake-super why","cake-super nps")  


#正規のNPS(0-6批判者/9-10推奨者)
NPS = function(x=c(4,7,10,13,16,19,22,25,28)){
   (sum(raw_data[x]>= 9,na.rm = T)/sum(raw_data[x]>=0,na.rm = T)-
                     sum(raw_data[x]<= 6,na.rm = T)/sum(raw_data[x]>=0,na.rm = T))*100
}


NPS_coffee_exp = NPS(x=4)
NPS_coffee_con = NPS(x=7)
NPS_coffee_sup = NPS(x=10)
NPS_bread_exp = NPS(x=13)
NPS_bread_con = NPS(x=16)
NPS_bread_sup = NPS(x=19)
NPS_cake_exp = NPS(x=22)
NPS_cake_con = NPS(x=25)
NPS_cake_sup = NPS(x=28)

cat(paste("コーヒー(専門店)のNPS             ",
          round(NPS_coffee_exp,digits=1),
          "\n", 
          "コーヒー(コンビニエンスストア)のNPS ",
          round(NPS_coffee_con,digits = 1),
          "\n", 
          "コーヒー(スーパー)のNPS            ",
          round(NPS_coffee_sup,digits = 1),
          "\n", 
          "パン(専門店)のNPS                  ",
          round(NPS_bread_exp,digits=1),
          "\n", 
          "パン(コンビニエンスストア)のNPS      ",
          round(NPS_bread_con,digits = 1),
          "\n", 
          "パン(スーパー)のNPS                ",
          round(NPS_bread_sup,digits = 1),
          "\n", 
          "ケーキ(専門店)のNPS                ",
          round(NPS_cake_exp,digits=1),
          "\n", 
          "ケーキ(コンビニエンスストア)のNPS    ",
          round(NPS_cake_con,digits = 1),
          "\n", 
          "ケーキ(スーパー)のNPS              ",
          round(NPS_cake_sup,digits = 1),
          "\n", 
          sep="") ) 




#補正NPS(0-4批判者/8-10推奨者)
NPS = function(x=c(4,7,10,13,16,19,22,25,28)){
  (sum(raw_data[x]>= 8,na.rm = T)/sum(raw_data[x]>=0,na.rm = T)-
     sum(raw_data[x]<= 4,na.rm = T)/sum(raw_data[x]>=0,na.rm = T))*100
}

NPS_coffee_exp = NPS(x=4)
NPS_coffee_con = NPS(x=7)
NPS_coffee_sup = NPS(x=10)
NPS_bread_exp = NPS(x=13)
NPS_bread_con = NPS(x=16)
NPS_bread_sup = NPS(x=19)
NPS_cake_exp = NPS(x=22)
NPS_cake_con = NPS(x=25)
NPS_cake_sup = NPS(x=28)

cat(paste("コーヒー(専門店)のNPS             ",
          round(NPS_coffee_exp,digits=1),
          "\n", 
          "コーヒー(コンビニエンスストア)のNPS ",
          round(NPS_coffee_con,digits = 1),
          "\n", 
          "コーヒー(スーパー)のNPS            ",
          round(NPS_coffee_sup,digits = 1),
          "\n", 
          "パン(専門店)のNPS                  ",
          round(NPS_bread_exp,digits=1),
          "\n", 
          "パン(コンビニエンスストア)のNPS      ",
          round(NPS_bread_con,digits = 1),
          "\n", 
          "パン(スーパー)のNPS                ",
          round(NPS_bread_sup,digits = 1),
          "\n", 
          "ケーキ(専門店)のNPS                ",
          round(NPS_cake_exp,digits=1),
          "\n", 
          "ケーキ(コンビニエンスストア)のNPS    ",
          round(NPS_cake_con,digits = 1),
          "\n", 
          "ケーキ(スーパー)のNPS              ",
          round(NPS_cake_sup,digits = 1),
          "\n", 
          sep="") ) 





#全業種まとめてのNPS
##正規のNPS
expert_NPS = ((sum(raw_data[4]>= 9,na.rm = T)+sum(raw_data[13]>= 9,na.rm = T)+sum(raw_data[22]>= 9,na.rm = T))/
                (sum(raw_data[4]>= 0,na.rm = T)+sum(raw_data[13]>= 0,na.rm = T)+sum(raw_data[22]>= 0,na.rm = T))-
                (sum(raw_data[4]<= 6,na.rm = T)+sum(raw_data[13]<= 6,na.rm = T)+sum(raw_data[22]<= 6,na.rm = T))/
                (sum(raw_data[4]>= 0,na.rm = T)+sum(raw_data[13]>= 0,na.rm = T)+sum(raw_data[22]>= 0,na.rm = T)))*100

conveni_NPS = ((sum(raw_data[7]>= 9,na.rm = T)+sum(raw_data[16]>= 9,na.rm = T)+sum(raw_data[25]>= 9,na.rm = T))/
                (sum(raw_data[7]>= 0,na.rm = T)+sum(raw_data[16]>= 0,na.rm = T)+sum(raw_data[25]>= 0,na.rm = T))-
                (sum(raw_data[7]<= 6,na.rm = T)+sum(raw_data[16]<= 6,na.rm = T)+sum(raw_data[25]<= 6,na.rm = T))/
                (sum(raw_data[7]>= 0,na.rm = T)+sum(raw_data[16]>= 0,na.rm = T)+sum(raw_data[25]>= 0,na.rm = T)))*100

super_NPS = ((sum(raw_data[10]>= 9,na.rm = T)+sum(raw_data[19]>= 9,na.rm = T)+sum(raw_data[28]>= 9,na.rm = T))/
                (sum(raw_data[10]>= 0,na.rm = T)+sum(raw_data[19]>= 0,na.rm = T)+sum(raw_data[28]>= 0,na.rm = T))-
                (sum(raw_data[10]<= 6,na.rm = T)+sum(raw_data[19]<= 6,na.rm = T)+sum(raw_data[28]<= 6,na.rm = T))/
                (sum(raw_data[10]>= 0,na.rm = T)+sum(raw_data[19]>= 0,na.rm = T)+sum(raw_data[28]>= 0,na.rm = T)))*100




##補正NPS
corrected_expert_NPS = ((sum(raw_data[4]>= 8,na.rm = T)+sum(raw_data[13]>= 8,na.rm = T)+sum(raw_data[22]>= 8,na.rm = T))/
                (sum(raw_data[4]>= 0,na.rm = T)+sum(raw_data[13]>= 0,na.rm = T)+sum(raw_data[22]>= 0,na.rm = T))-
                (sum(raw_data[4]<= 4,na.rm = T)+sum(raw_data[13]<= 6,na.rm = T)+sum(raw_data[22]<= 4,na.rm = T))/
                (sum(raw_data[4]>= 0,na.rm = T)+sum(raw_data[13]>= 0,na.rm = T)+sum(raw_data[22]>= 0,na.rm = T)))*100

corrected_conveni_NPS = ((sum(raw_data[7]>= 8,na.rm = T)+sum(raw_data[16]>= 8,na.rm = T)+sum(raw_data[25]>= 8,na.rm = T))/
                 (sum(raw_data[7]>= 0,na.rm = T)+sum(raw_data[16]>= 0,na.rm = T)+sum(raw_data[25]>= 0,na.rm = T))-
                 (sum(raw_data[7]<= 4,na.rm = T)+sum(raw_data[16]<= 4,na.rm = T)+sum(raw_data[25]<= 4,na.rm = T))/
                 (sum(raw_data[7]>= 0,na.rm = T)+sum(raw_data[16]>= 0,na.rm = T)+sum(raw_data[25]>= 0,na.rm = T)))*100

corrected_super_NPS = ((sum(raw_data[10]>= 8,na.rm = T)+sum(raw_data[19]>= 8,na.rm = T)+sum(raw_data[28]>= 8,na.rm = T))/
               (sum(raw_data[10]>= 0,na.rm = T)+sum(raw_data[19]>= 0,na.rm = T)+sum(raw_data[28]>= 0,na.rm = T))-
               (sum(raw_data[10]<= 4,na.rm = T)+sum(raw_data[19]<= 4,na.rm = T)+sum(raw_data[28]<= 4,na.rm = T))/
               (sum(raw_data[10]>= 0,na.rm = T)+sum(raw_data[19]>= 0,na.rm = T)+sum(raw_data[28]>= 0,na.rm = T)))*100



cat(paste("専門店のNPS  ",
          round(expert_NPS,digits=1),
          "\n", 
          "コンビニのNPS  ",
          round(conveni_NPS,digits = 1),
          "\n", 
          "スーパーのNPS  ",
          round(super_NPS,digits = 1),
          "\n", 
          "専門店の補正NPS  ",
          round(corrected_expert_NPS,digits=1),
          "\n", 
          "コンビニの補正NPS  ",
          round(corrected_conveni_NPS,digits = 1),
          "\n", 
          "スーパーの補正NPS  ",
          round(corrected_super_NPS,digits = 1),
          "\n", 
          sep="") ) 


