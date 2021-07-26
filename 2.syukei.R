#アンケート集計（専門店編)
##専門店を選んだ理由をマトリックスに直す作業
###を上書きして選んだ理由１１種＋推薦度の１２列のマトリックス作成

#syuukei super.R の最後でまとめた後にCSVファイルへ変換済み

data_0 = read.csv("ver1.2 1204.csv",fileEncoding = "UTF-8")
colnames(data_0) = c("time","coffe-expert use","coffee-expert why","coffee-expert nps",
                     "coffe-conveni use","coffee-conveni why","coffee-conveni nps",
                     "coffe-super use","coffee-super why","coffee-super nps",
                     "bread-expert use","bread-expert why","bread-expert nps",
                     "bread-conveni use","bread-conveni why","bread-conveni nps",
                     "bread-super use","bread-super why","bread-super nps",
                     "cake-expert use","cake-expert why","cake-expert nps",
                     "cake-conveni use","cake-conveni why","cake-conveni nps",
                     "cake-super use","cake-super why","cake-super nps")  

#専門店の利用理由の列のみ抽出
data_cof = data_0[,c(3:4)]
data_bre = data_0[,c(12:13)]
data_cak = data_0[,c(21:22)]

#無回答（利用しない人の回答）を削除
exp_cof_user = data_cof[-which(data_cof$`coffee-expert why` %in% ""),]
exp_bre_user = data_bre[-which(data_bre$`bread-expert why` %in% ""),]
exp_cak_user = data_cak[-which(data_cak$`cake-expert why` %in% ""),]

##空ボックス用意→各選択肢を選んでいるかで0/1を入れていく
#コーヒー専門店について
box_coffee = matrix(nrow = 37,ncol = 11)

for (a in 1:37) {
  if(length(grep("美味しい",exp_cof_user[a,1]))==0){box_coffee[a,1]=0}
  else{box_coffee[a,1]=1}
}
for (b in 1:37) {
  if(length(grep("店舗が近い・アクセスが良い",exp_cof_user[b,1]))==0){box_coffee[b,2]=0}
  else{box_coffee[b,2]=1}
}
for (c in 1:37) {
  if(length(grep("価格が安い",exp_cof_user[c,1]))==0){box_coffee[c,3]=0}
  else{box_coffee[c,3]=1}
}
for (d in 1:37) {
  if(length(grep("できたてを購入できる",exp_cof_user[d,1]))==0){box_coffee[d,4]=0}
  else{box_coffee[d,4]=1}
}
for (e in 1:37) {
  if(length(grep("商品が本格的",exp_cof_user[e,1]))==0){box_coffee[e,5]=0}
  else{box_coffee[e,5]=1}
}
for (f in 1:37) {
  if(length(grep("種類が豊富・品揃えが良い",exp_cof_user[f,1]))==0){box_coffee[f,6]=0}
  else{box_coffee[f,6]=1}
}
for (g in 1:37) {
  if(length(grep("店や商品の個性がある",exp_cof_user[g,1]))==0){box_coffee[g,7]=0}
  else{box_coffee[g,7]=1}
}
for (h in 1:37) {
  if(length(grep("店の雰囲気が良い",exp_cof_user[h,1]))==0){box_coffee[h,8]=0}
  else{box_coffee[h,8]=1}
}
for (i in 1:37) {
  if(length(grep("店員が親切・愛想が良い",exp_cof_user[i,1]))==0){box_coffee[i,9]=0}
  else{box_coffee[i,9]=1}
}
for (j in 1:37) {
  if(length(grep("店内で飲めるのが良い",exp_cof_user[j,1]))==0){box_coffee[j,10]=0}
  else{box_coffee[j,10]=1}
}
for (k in 1:37) {
  if(length(grep("気軽に購入できる・素早く購入できる",exp_cof_user[k,1]))==0){box_coffee[k,11]=0}
  else{box_coffee[k,11]=1}
}

npsbox_cof = cbind(box_coffee,exp_cof_user$`coffee-expert nps`)


#パン専門店について
box_bread = matrix(nrow = 49,ncol = 11)

for (a in 1:49) {
  if(length(grep("美味しい",exp_bre_user[a,1]))==0){box_bread[a,1]=0}
  else{box_bread[a,1]=1}
}
for (b in 1:49) {
  if(length(grep("店舗が近い・アクセスが良い",exp_bre_user[b,1]))==0){box_bread[b,2]=0}
  else{box_bread[b,2]=1}
}
for (c in 1:49) {
  if(length(grep("価格が安い",exp_bre_user[c,1]))==0){box_bread[c,3]=0}
  else{box_bread[c,3]=1}
}
for (d in 1:49) {
  if(length(grep("できたてを購入できる",exp_bre_user[d,1]))==0){box_bread[d,4]=0}
  else{box_bread[d,4]=1}
}
for (e in 1:49) {
  if(length(grep("商品が本格的",exp_bre_user[e,1]))==0){box_bread[e,5]=0}
  else{box_bread[e,5]=1}
}
for (f in 1:49) {
  if(length(grep("種類が豊富・品揃えが良い",exp_bre_user[f,1]))==0){box_bread[f,6]=0}
  else{box_bread[f,6]=1}
}
for (g in 1:49) {
  if(length(grep("店や商品の個性がある",exp_bre_user[g,1]))==0){box_bread[g,7]=0}
  else{box_bread[g,7]=1}
}
for (h in 1:49) {
  if(length(grep("店の雰囲気が良い",exp_bre_user[h,1]))==0){box_bread[h,8]=0}
  else{box_bread[h,8]=1}
}
for (i in 1:49) {
  if(length(grep("店員が親切・愛想が良い",exp_bre_user[i,1]))==0){box_bread[i,9]=0}
  else{box_bread[i,9]=1}
}
for (j in 1:49) {
  if(length(grep("店内で食べられるのが良い",exp_bre_user[j,1]))==0){box_bread[j,10]=0}
  else{box_bread[j,10]=1}
}
for (k in 1:49) {
  if(length(grep("気軽に購入できる・素早く購入できる",exp_bre_user[k,1]))==0){box_bread[k,11]=0}
  else{box_bread[k,11]=1}
}

npsbox_bre = cbind(box_bread,exp_bre_user$`bread-expert nps`)


#ケーキ専門店について
box_cake = matrix(nrow = 37,ncol = 11)

for (a in 1:37) {
  if(length(grep("美味しい",exp_cak_user[a,1]))==0){box_cake[a,1]=0}
  else{box_cake[a,1]=1}
}
for (b in 1:37) {
  if(length(grep("店舗が近い・アクセスが良い",exp_cak_user[b,1]))==0){box_cake[b,2]=0}
  else{box_cake[b,2]=1}
}
for (c in 1:37) {
  if(length(grep("価格が安い",exp_cak_user[c,1]))==0){box_cake[c,3]=0}
  else{box_cake[c,3]=1}
}
for (d in 1:37) {
  if(length(grep("できたてを購入できる",exp_cak_user[d,1]))==0){box_cake[d,4]=0}
  else{box_cake[d,4]=1}
}
for (e in 1:37) {
  if(length(grep("商品が本格的",exp_cak_user[e,1]))==0){box_cake[e,5]=0}
  else{box_cake[e,5]=1}
}
for (f in 1:37) {
  if(length(grep("種類が豊富・品揃えが良い",exp_cak_user[f,1]))==0){box_cake[f,6]=0}
  else{box_cake[f,6]=1}
}
for (g in 1:37) {
  if(length(grep("店や商品の個性がある",exp_cak_user[g,1]))==0){box_cake[g,7]=0}
  else{box_cake[g,7]=1}
}
for (h in 1:37) {
  if(length(grep("店の雰囲気が良い",exp_cak_user[h,1]))==0){box_cake[h,8]=0}
  else{box_cake[h,8]=1}
}
for (i in 1:37) {
  if(length(grep("店員が親切・愛想が良い",exp_cak_user[i,1]))==0){box_cake[i,9]=0}
  else{box_cake[i,9]=1}
}
for (j in 1:37) {
  if(length(grep("店内で食べられるのが良い",exp_cak_user[j,1]))==0){box_cake[j,10]=0}
  else{box_cake[j,10]=1}
}
for (k in 1:37) {
  if(length(grep("気軽に購入できる・素早く購入できる",exp_cak_user[k,1]))==0){box_cake[k,11]=0}
  else{box_cake[k,11]=1}
}

npsbox_cak = cbind(box_cake,exp_cak_user$`cake-expert nps`)



