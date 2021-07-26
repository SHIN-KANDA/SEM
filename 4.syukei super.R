#アンケート集計（専門店編）
##専門店を選んだ理由をマトリックスに直す作業
###を上書きして選んだ理由１１種＋推薦度の１２列のマトリックス作成

#最後で専門・コンビニも含めてまとめた後にCSVファイルへ変換済み

data_000 = read.csv("ver1.2 1204.csv",fileEncoding = "UTF-8")
colnames(data_000) = c("time","coffe-expert use","coffee-expert why","coffee-expert nps",
                      "coffe-conveni use","coffee-conveni why","coffee-conveni nps",
                      "coffe-super use","coffee-super why","coffee-super nps",
                      "bread-expert use","bread-expert why","bread-expert nps",
                      "bread-conveni use","bread-conveni why","bread-conveni nps",
                      "bread-super use","bread-super why","bread-super nps",
                      "cake-expert use","cake-expert why","cake-expert nps",
                      "cake-conveni use","cake-conveni why","cake-conveni nps",
                      "cake-super use","cake-super why","cake-super nps")  

#コンビニの利用理由の列のみ抽出
data_cof_000 = data_000[,c(9:10)]
data_bre_000 = data_000[,c(18:19)]
data_cak_000 = data_000[,c(27:28)]

#無回答（利用しない人の回答）を削除
sup_cof_user = data_cof_000[-which(data_cof_000$`coffee-super why` %in% ""),]
sup_bre_user = data_bre_000[-which(data_bre_000$`bread-super why` %in% ""),]
sup_cak_user = data_cak_000[-which(data_cak_000$`cake-super why` %in% ""),]

##空ボックス用意→各選択肢を選んでいるかで0/1を入れていく
#コーヒー スーパーについて
box_coffee_000 = matrix(nrow = 34,ncol = 11)

for (a in 1:34) {
  if(length(grep("美味しい",sup_cof_user[a,1]))==0){box_coffee_000[a,1]=0}
  else{box_coffee_000[a,1]=1}
}
for (b in 1:34) {
  if(length(grep("店舗が近い・アクセスが良い",sup_cof_user[b,1]))==0){box_coffee_000[b,2]=0}
  else{box_coffee_000[b,2]=1}
}
for (c in 1:34) {
  if(length(grep("価格が安い",sup_cof_user[c,1]))==0){box_coffee_000[c,3]=0}
  else{box_coffee_000[c,3]=1}
}
for (d in 1:34) {
  if(length(grep("できたてを購入できる",sup_cof_user[d,1]))==0){box_coffee_000[d,4]=0}
  else{box_coffee_000[d,4]=1}
}
for (e in 1:34) {
  if(length(grep("商品が本格的",sup_cof_user[e,1]))==0){box_coffee_000[e,5]=0}
  else{box_coffee_000[e,5]=1}
}
for (f in 1:34) {
  if(length(grep("種類が豊富・品揃えが良い",sup_cof_user[f,1]))==0){box_coffee_000[f,6]=0}
  else{box_coffee_000[f,6]=1}
}
for (g in 1:34) {
  if(length(grep("店や商品の個性がある",sup_cof_user[g,1]))==0){box_coffee_000[g,7]=0}
  else{box_coffee_000[g,7]=1}
}
for (h in 1:34) {
  if(length(grep("店の雰囲気が良い",sup_cof_user[h,1]))==0){box_coffee_000[h,8]=0}
  else{box_coffee_000[h,8]=1}
}
for (i in 1:34) {
  if(length(grep("店員が親切・愛想が良い",sup_cof_user[i,1]))==0){box_coffee_000[i,9]=0}
  else{box_coffee_000[i,9]=1}
}
for (j in 1:34) {
  if(length(grep("ほかの食品・日用品等と合わせて購入できる",sup_cof_user[j,1]))==0){box_coffee_000[j,10]=0}
  else{box_coffee_000[j,10]=1}
}
for (k in 1:34) {
  if(length(grep("気軽に購入できる・素早く購入できる",sup_cof_user[k,1]))==0){box_coffee_000[k,11]=0}
  else{box_coffee_000[k,11]=1}
}

npsbox_cof_000 = cbind(box_coffee_000,sup_cof_user$`coffee-super nps`)

#パン専門店について
box_bread_000 = matrix(nrow = 60,ncol = 11)

for (a in 1:60) {
  if(length(grep("美味しい",sup_bre_user[a,1]))==0){box_bread_000[a,1]=0}
  else{box_bread_000[a,1]=1}
}
for (b in 1:60) {
  if(length(grep("店舗が近い・アクセスが良い",sup_bre_user[b,1]))==0){box_bread_000[b,2]=0}
  else{box_bread_000[b,2]=1}
}
for (c in 1:60) {
  if(length(grep("価格が安い",sup_bre_user[c,1]))==0){box_bread_000[c,3]=0}
  else{box_bread_000[c,3]=1}
}
for (d in 1:60) {
  if(length(grep("できたてを購入できる",sup_bre_user[d,1]))==0){box_bread_000[d,4]=0}
  else{box_bread_000[d,4]=1}
}
for (e in 1:60) {
  if(length(grep("商品が本格的",sup_bre_user[e,1]))==0){box_bread_000[e,5]=0}
  else{box_bread_000[e,5]=1}
}
for (f in 1:60) {
  if(length(grep("種類が豊富・品揃えが良い",sup_bre_user[f,1]))==0){box_bread_000[f,6]=0}
  else{box_bread_000[f,6]=1}
}
for (g in 1:60) {
  if(length(grep("店や商品の個性がある",sup_bre_user[g,1]))==0){box_bread_000[g,7]=0}
  else{box_bread_000[g,7]=1}
}
for (h in 1:60) {
  if(length(grep("店の雰囲気が良い",sup_bre_user[h,1]))==0){box_bread_000[h,8]=0}
  else{box_bread_000[h,8]=1}
}
for (i in 1:60) {
  if(length(grep("店員が親切・愛想が良い",sup_bre_user[i,1]))==0){box_bread_000[i,9]=0}
  else{box_bread_000[i,9]=1}
}
for (j in 1:60) {
  if(length(grep("ほかの食品・日用品等と合わせて購入できる",sup_bre_user[j,1]))==0){box_bread_000[j,10]=0}
  else{box_bread_000[j,10]=1}
}
for (k in 1:60) {
  if(length(grep("気軽に購入できる・素早く購入できる",sup_bre_user[k,1]))==0){box_bread_000[k,11]=0}
  else{box_bread_000[k,11]=1}
}

npsbox_bre_000 = cbind(box_bread_000,sup_bre_user$`bread-super nps`
                       )
#ケーキ専門店について
box_cake_000 = matrix(nrow = 18,ncol = 11)

for (a in 1:18) {
  if(length(grep("美味しい",sup_cak_user[a,1]))==0){box_cake_000[a,1]=0}
  else{box_cake_000[a,1]=1}
}
for (b in 1:18) {
  if(length(grep("店舗が近い・アクセスが良い",sup_cak_user[b,1]))==0){box_cake_000[b,2]=0}
  else{box_cake_000[b,2]=1}
}
for (c in 1:18) {
  if(length(grep("価格が安い",sup_cak_user[c,1]))==0){box_cake_000[c,3]=0}
  else{box_cake_000[c,3]=1}
}
for (d in 1:18) {
  if(length(grep("できたてを購入できる",sup_cak_user[d,1]))==0){box_cake_000[d,4]=0}
  else{box_cake_000[d,4]=1}
}
for (e in 1:18) {
  if(length(grep("商品が本格的",sup_cak_user[e,1]))==0){box_cake_000[e,5]=0}
  else{box_cake_000[e,5]=1}
}
for (f in 1:18) {
  if(length(grep("種類が豊富・品揃えが良い",sup_cak_user[f,1]))==0){box_cake_000[f,6]=0}
  else{box_cake_000[f,6]=1}
}
for (g in 1:18) {
  if(length(grep("店や商品の個性がある",sup_cak_user[g,1]))==0){box_cake_000[g,7]=0}
  else{box_cake_000[g,7]=1}
}
for (h in 1:18) {
  if(length(grep("店の雰囲気が良い",sup_cak_user[h,1]))==0){box_cake_000[h,8]=0}
  else{box_cake_000[h,8]=1}
}
for (i in 1:18) {
  if(length(grep("店員が親切・愛想が良い",sup_cak_user[i,1]))==0){box_cake_000[i,9]=0}
  else{box_cake_000[i,9]=1}
}
for (j in 1:18) {
  if(length(grep("ほかの食品・日用品等と合わせて購入できる",sup_cak_user[j,1]))==0){box_cake_000[j,10]=0}
  else{box_cake_000[j,10]=1}
}
for (k in 1:18) {
  if(length(grep("気軽に購入できる・素早く購入できる",sup_cak_user[k,1]))==0){box_cake_000[k,11]=0}
  else{box_cake_000[k,11]=1}
}

npsbox_cak_000 = cbind(box_cake_000,sup_cak_user$`cake-super nps`)





mastar_nps = rbind(npsbox_cof,npsbox_cof_00,npsbox_cof_000,
                   npsbox_bre,npsbox_bre_00,npsbox_bre_000,
                   npsbox_cak,npsbox_cak_00,npsbox_cak_000)
mastar_nps = as.data.frame(mastar_nps)
colnames(mastar_nps) = c("美味しい","アクセス","価格","できたて",
                         "本格的","品揃え","個性","雰囲気","愛想","特能","気軽","推薦度")

write.csv(mastar_nps,"master_nps.csv")

