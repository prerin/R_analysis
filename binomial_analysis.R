#学習データ
learn_data <- data0203[1:1260,2:9]

#評価対象データ
new_data <- data0203[1261,3:9]

#ロジスティック回帰モデルの構築
fit3 <- glm(y~., 
            family=binomial(link = "logit") , 
            data=learn_data)

#ロジスティック回帰モデルの定数と係数
coef(fit3)

#ロジスティック回帰モデルの評価対象データの予測値
pred_newy <- predict(fit3,newdata=new_data)

pred_newy

pred_newp <- exp(pred_newy)/(1+exp(pred_newy))

pred_newp

#ロジスティック回帰モデルの予測値（学習データと対象データすべて）
pred_y <- predict(fit3,newdata=learn_data[,2:8])

predict_fit3 <- exp(pred_y)/(1+exp(pred_y))

#受注か失注かの判定（判定の予測）
hantei <- ifelse(predict_fit3>0.5,1,0)

hantei

new_data0203 <- cbind(data0203[1:1260,],predict_fit3,hantei) 

#実現値の予測値（判定の予測）のクロス集計
table(new_data0203$y,new_data0203$hantei)
