###SEMの準備


#各業態の専門店利用の理由データを結合＋「専門」ラベルの貼り付け
box_cof_0 = read.csv("box_coffee.csv")
box_bre_0 = read.csv("box_bread.csv")
box_cak_0 = read.csv("box_cake.csv")

box_cof_0 = box_cof_0[,-1]
box_bre_0 = box_bre_0[,-1]
box_cak_0 = box_cak_0[,-1]

box_expert = rbind(box_cof_0,box_bre_0,box_cak_0)
box_expert$v12 = rep("専門", nrow(box_expert))
colnames(box_expert) = c("美味しい","アクセス","価格","できたて","本格的","品揃え","個性","雰囲気","愛想","特能","気軽","業態")



#各業態のコンビニエンスストア利用の理由データを結合＋「コンビニ」ラベルの貼り付け
box_cof_00 = read.csv("box_coffee_00.csv")
box_bre_00 = read.csv("box_bread_00.csv")
box_cak_00 = read.csv("box_cake_00.csv")

box_cof_00 = box_cof_00[,-1]
box_bre_00 = box_bre_00[,-1]
box_cak_00 = box_cak_00[,-1]

box_conveni = rbind(box_cof_00,box_bre_00,box_cak_00)
box_conveni$v12 = rep("コンビニ", nrow(box_conveni))
colnames(box_conveni) = c("美味しい","アクセス","価格","できたて","本格的","品揃え","個性","雰囲気","愛想","特能","気軽","業態")



#各業態のスーパー利用の理由データを結合＋「スーパー」ラベルの貼り付け
box_cof_000 = read.csv("box_coffee_000.csv")
box_bre_000 = read.csv("box_bread_000.csv")
box_cak_000 = read.csv("box_cake_000.csv")

box_cof_000 = box_cof_000[,-1]
box_bre_000 = box_bre_000[,-1]
box_cak_000 = box_cak_000[,-1]

box_super = rbind(box_cof_000,box_bre_000,box_cak_000)
box_super$v12 = rep("スーパー", nrow(box_super))
colnames(box_super) = c("美味しい","アクセス","価格","できたて","本格的","品揃え","個性","雰囲気","愛想","特能","気軽","業態")



#上で作った３つを結合
mastar_box = rbind(box_expert,box_conveni,box_super)
#write.csv(mastar_box,"master_box.csv")