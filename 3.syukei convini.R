#アンケート集計（専門店編）
##専門店を選んだ理由をマトリックスに直す作業
###を上書きして選んだ理由１１種＋推薦度の１２列のマトリックス作成

#syuukei super.R の最後でまとめた後にCSVファイルへ変換済み

data_00 = read.csv("ver1.2 1204.csv",fileEncoding = "UTF-8")
colnames(data_00) = c("time","coffe-expert use","coffee-expert why","coffee-expert nps",
                     "coffe-conveni use","coffee-conveni why","coffee-conveni nps",
                     "coffe-super use","coffee-super why","coffee-super nps",
                     "bread-expert use","bread-expert why","bread-expert nps",
                     "bread-conveni use","bread-conveni why","bread-conveni nps",
                     "bread-super use","bread-super why","bread-super nps",
                     "cake-expert use","cake-expert why","cake-expert nps",
                     "cake-conveni use","cake-conveni why","cake-conveni nps",
                     "cake-super use","cake-super why","cake-super nps")  

#コンビニの利用理由の列のみ抽出
data_cof_00 = data_00[,c(6:7)]
data_bre_00 = data_00[,c(15:16)]
data_cak_00 = data_00[,c(24:25)]

#無回答（利用しない人の回答）を削除
con_cof_user = data_cof_00[-which(data_cof_00$`coffee-conveni why` %in% ""),]
con_bre_user = data_bre_00[-which(data_bre_00$`bread-conveni why` %in% ""),]
con_cak_user = data_cak_00[-which(data_cak_00$`cake-conveni why` %in% ""),]

##空ボックス用意→各選択肢を選んでいるかで0/1を入れていく
#コーヒー コンビニについて
box_coffee_00 = matrix(nrow = 38,ncol = 11)

for (a in 1:38) {
  if(length(grep("美味しい",con_cof_user[a,1]))==0){box_coffee_00[a,1]=0}
  else{box_coffee_00[a,1]=1}
}
for (b in 1:38) {
  if(length(grep("店舗が近い・アクセスが良い",con_cof_user[b,1]))==0){box_coffee_00[b,2]=0}
  else{box_coffee_00[b,2]=1}
}
for (c in 1:38) {
  if(length(grep("価格が安い",con_cof_user[c,1]))==0){box_coffee_00[c,3]=0}
  else{box_coffee_00[c,3]=1}
}
for (d in 1:38) {
  if(length(grep("できたてを購入できる",con_cof_user[d,1]))==0){box_coffee_00[d,4]=0}
  else{box_coffee_00[d,4]=1}
}
for (e in 1:38) {
  if(length(grep("商品が本格的",con_cof_user[e,1]))==0){box_coffee_00[e,5]=0}
  else{box_coffee_00[e,5]=1}
}
for (f in 1:38) {
  if(length(grep("種類が豊富・品揃えが良い",con_cof_user[f,1]))==0){box_coffee_00[f,6]=0}
  else{box_coffee_00[f,6]=1}
}
for (g in 1:38) {
  if(length(grep("店や商品の個性がある",con_cof_user[g,1]))==0){box_coffee_00[g,7]=0}
  else{box_coffee_00[g,7]=1}
}
for (h in 1:38) {
  if(length(grep("店の雰囲気が良い",con_cof_user[h,1]))==0){box_coffee_00[h,8]=0}
  else{box_coffee_00[h,8]=1}
}
for (i in 1:38) {
  if(length(grep("店員が親切・愛想が良い",con_cof_user[i,1]))==0){box_coffee_00[i,9]=0}
  else{box_coffee_00[i,9]=1}
}
for (j in 1:38) {
  if(length(grep("ほかの食品・日用品等と合わせて購入できる",con_cof_user[j,1]))==0){box_coffee_00[j,10]=0}
  else{box_coffee_00[j,10]=1}
}
for (k in 1:38) {
  if(length(grep("気軽に購入できる・素早く購入できる",con_cof_user[k,1]))==0){box_coffee_00[k,11]=0}
  else{box_coffee_00[k,11]=1}
}

npsbox_cof_00 = cbind(box_coffee_00,con_cof_user$`coffee-conveni nps`)

#パン専門店について
box_bread_00 = matrix(nrow = 62,ncol = 11)

for (a in 1:62) {
  if(length(grep("美味しい",con_bre_user[a,1]))==0){box_bread_00[a,1]=0}
  else{box_bread_00[a,1]=1}
}
for (b in 1:62) {
  if(length(grep("店舗が近い・アクセスが良い",con_bre_user[b,1]))==0){box_bread_00[b,2]=0}
  else{box_bread_00[b,2]=1}
}
for (c in 1:62) {
  if(length(grep("価格が安い",con_bre_user[c,1]))==0){box_bread_00[c,3]=0}
  else{box_bread_00[c,3]=1}
}
for (d in 1:62) {
  if(length(grep("できたてを購入できる",con_bre_user[d,1]))==0){box_bread_00[d,4]=0}
  else{box_bread_00[d,4]=1}
}
for (e in 1:62) {
  if(length(grep("商品が本格的",con_bre_user[e,1]))==0){box_bread_00[e,5]=0}
  else{box_bread_00[e,5]=1}
}
for (f in 1:62) {
  if(length(grep("種類が豊富・品揃えが良い",con_bre_user[f,1]))==0){box_bread_00[f,6]=0}
  else{box_bread_00[f,6]=1}
}
for (g in 1:62) {
  if(length(grep("店や商品の個性がある",con_bre_user[g,1]))==0){box_bread_00[g,7]=0}
  else{box_bread_00[g,7]=1}
}
for (h in 1:62) {
  if(length(grep("店の雰囲気が良い",con_bre_user[h,1]))==0){box_bread_00[h,8]=0}
  else{box_bread_00[h,8]=1}
}
for (i in 1:62) {
  if(length(grep("店員が親切・愛想が良い",con_bre_user[i,1]))==0){box_bread_00[i,9]=0}
  else{box_bread_00[i,9]=1}
}
for (j in 1:62) {
  if(length(grep("ほかの食品・日用品等と合わせて購入できる",con_bre_user[j,1]))==0){box_bread_00[j,10]=0}
  else{box_bread_00[j,10]=1}
}
for (k in 1:62) {
  if(length(grep("気軽に購入できる・素早く購入できる",con_bre_user[k,1]))==0){box_bread_00[k,11]=0}
  else{box_bread_00[k,11]=1}
}

npsbox_bre_00 = cbind(box_bread_00,con_bre_user$`bread-conveni nps`)

#ケーキ専門店について
box_cake_00 = matrix(nrow = 37,ncol = 11)

for (a in 1:37) {
  if(length(grep("美味しい",con_cak_user[a,1]))==0){box_cake_00[a,1]=0}
  else{box_cake_00[a,1]=1}
}
for (b in 1:37) {
  if(length(grep("店舗が近い・アクセスが良い",con_cak_user[b,1]))==0){box_cake_00[b,2]=0}
  else{box_cake_00[b,2]=1}
}
for (c in 1:37) {
  if(length(grep("価格が安い",con_cak_user[c,1]))==0){box_cake_00[c,3]=0}
  else{box_cake_00[c,3]=1}
}
for (d in 1:37) {
  if(length(grep("できたてを購入できる",con_cak_user[d,1]))==0){box_cake_00[d,4]=0}
  else{box_cake_00[d,4]=1}
}
for (e in 1:37) {
  if(length(grep("商品が本格的",con_cak_user[e,1]))==0){box_cake_00[e,5]=0}
  else{box_cake_00[e,5]=1}
}
for (f in 1:37) {
  if(length(grep("種類が豊富・品揃えが良い",con_cak_user[f,1]))==0){box_cake_00[f,6]=0}
  else{box_cake_00[f,6]=1}
}
for (g in 1:37) {
  if(length(grep("店や商品の個性がある",con_cak_user[g,1]))==0){box_cake_00[g,7]=0}
  else{box_cake_00[g,7]=1}
}
for (h in 1:37) {
  if(length(grep("店の雰囲気が良い",con_cak_user[h,1]))==0){box_cake_00[h,8]=0}
  else{box_cake_00[h,8]=1}
}
for (i in 1:37) {
  if(length(grep("店員が親切・愛想が良い",con_cak_user[i,1]))==0){box_cake_00[i,9]=0}
  else{box_cake_00[i,9]=1}
}
for (j in 1:37) {
  if(length(grep("ほかの食品・日用品等と合わせて購入できる",con_cak_user[j,1]))==0){box_cake_00[j,10]=0}
  else{box_cake_00[j,10]=1}
}
for (k in 1:37) {
  if(length(grep("気軽に購入できる・素早く購入できる",con_cak_user[k,1]))==0){box_cake_00[k,11]=0}
  else{box_cake_00[k,11]=1}
}

npsbox_cak_00 = cbind(box_cake_00,con_cak_user$`cake-conveni nps`)

