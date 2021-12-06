#学習データ
learn_data <- data0202[1:35,3:7]

#評価対象データ
new_data <- data0202[36,4:7]

#ポアソン回帰モデルの構築
fit2 <- glm(y~x1+x2+x3+x4, 
            family=poisson(link = "log") , 
            data=learn_data)

#ポアソン回帰モデルの定数と係数
coef(fit2)

#ポアソン回帰モデルの対象データの予測値
exp(predict(fit2,newdata=new_data))

#ポアソン回帰モデルの予測値（学習データと対象データすべて）
predict_fit2 <- exp(predict(fit2,newdata=data0202[,4:7]))

new_data0202 <- cbind(data0202,predict_fit2) 

#実現値と予測値の描写
plot(new_data0202$y,
     col="blue",
     type="l",
     ylim=c(0,40),
     xlab="時系列（月単位）", 
     ylab="受注件数")

lines(new_data0202$predict_fit2,
      col="red",
      type="l",
      lty=3)

