#
#説明変数の調整
#

#目的変数
y <- data0301$y

#目的変数の自己相関
y_acf <- acf(y)
cbind(y_acf$lag,y_acf$acf)

#目的変数の偏自己相関
y_pcf <- pacf(y)
cbind(y_pcf$lag,y_pcf$acf)

#
#Step.1-2
#

#散布図
plot(data0301[,4:8])

#相関係数
round(cor(data0301[,4:8]),3)

#相関係数
x4 <- data0301$x4

#交差相関
y_x4 <- ccf(y,x4)

cbind(y_x4$lag,y_x4$acf)

#
#Step.1-3
#

#散布図
plot(data0302[,4:9])

#相関係数
round(cor(data0302[,4:9]),3)
round(cor(data0302[,c(6,7,9)]),3)

#学習データ
learn_data <- as.data.frame(
  cbind(data0302$y,data0302$x2,data0302$x3,data0302$x4_new)
)

colnames(learn_data) <- c('y','x2','x3','x4_new')

#線形回帰モデルの構築
fit1 <- glm(y~., 
            family=gaussian(link="identity") , 
            data=learn_data)

#線形回帰モデルの定数と係数
coef(fit1)

#主成分分析用のデータ
pca_data <- as.data.frame(
  cbind(data0302$x3,data0302$x4_new)
)

colnames(pca_data) <- c('x3','x4_new')

#主成分分析
pca <- prcomp(pca_data, scale=T)

#主成分得点
pca$x

cor(pca$x)

#固有ベクトル（タテ）
pca$rotation

summary(pca)

#主成分分析用のデータの平均（列ごと）
pca_data_mean <- apply(pca_data[,1:2],2,mean)

#主成分分析用のデータの標準偏差（列ごと）
pca_data_sd <- apply(pca_data[,1:2],2,sd)

pca_data_mean_sd <- rbind(pca_data_mean,pca_data_sd )

rownames(pca_data_mean_sd) <- c("mean", "sd")    

pca_data_mean_sd

#線形回帰モデル用の学習データ（主成分得点使用）
learn_data <- as.data.frame(cbind(data0302$y,data0302$x2,pca$x[,1]))
colnames(learn_data) <- c('y','x2','pc1')

#線形回帰モデルの構築（主成分得点使用）
fit2 <- glm(y~., family=gaussian(link="identity") , data=learn_data)
coef(fit2)

######################
#Step.2
######################

#
# 主成分回帰モデル（パターン1）
#

#学習データ
learn_data <- as.data.frame(cbind(
  data0302$y,
  data0302$d_mon,
  data0302$d_wed,
  data0302$d_thu,
  data0302$d_fri,
  data0302$d_sat,
  data0302$d_sun,
  data0302$x2,
  pca$x[,1]
))

colnames(learn_data) <- c('y',
                          'd_mon',
                          'd_wed',
                          'd_thu',
                          'd_fri',
                          'd_sat',
                          'd_sun',
                          'x2',
                          'pc1')

#線形回帰モデルの構築
fit3 <- glm(y~., 
            family=gaussian(link="identity") ,
            data=learn_data)

#線形回帰モデルの定数と係数
coef(fit3)

transform(coef(fit3))

#
# Ridge回帰モデル（パターン2）
#

#学習データ
learn_data <- as.matrix(cbind(
  data0302$y,
  data0302$d_mon,
  data0302$d_wed,
  data0302$d_thu,
  data0302$d_fri,
  data0302$d_sat,
  data0302$d_sun,
  data0302$x2,
  data0302$x3,
  data0302$x4_new
))

colnames(learn_data) <- c('y','d_mon','d_wed','d_thu','d_fri','d_sat','d_sun','x2','x3','x4_new')

learn_data_y <- learn_data[,1]
learn_data_x <- learn_data[,-1]

#クロスバリデーションを利用したRidge回帰モデルのラムダの計算
ridgeModel<-cv.glmnet(
  y=learn_data_y,
  x=learn_data_x,
  family="gaussian",
  alpha=0,
  upper.limits=c(Inf,Inf,Inf,Inf,Inf,Inf,0,Inf,Inf),
  lower.limits=c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,0,0)
)

#Ridge回帰モデルの構築
ridgeModel2<-glmnet(
  y=learn_data_y,
  x=learn_data_x,
  family="gaussian",
  alpha=0,
  lambda=ridgeModel$lambda.min,
  upper.limits=c(Inf,Inf,Inf,Inf,Inf,Inf,0,Inf,Inf),
  lower.limits=c(-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,0,0)
)

#Ridge回帰モデルの定数と係数
coef(ridgeModel2)

##
#主成分回帰モデルによる将来予測
##

learn_data <- as.data.frame(cbind(
  data0302$y,
  data0302$d_mon,
  data0302$d_wed,
  data0302$d_thu,
  data0302$d_fri,
  data0302$d_sat,
  data0302$d_sun,
  data0302$x2,
  pca$x[,1]
))

colnames(learn_data) <- c('y',
                          'd_mon',
                          'd_wed',
                          'd_thu',
                          'd_fri',
                          'd_sat',
                          'd_sun',
                          'x2',
                          'pc1')

#線形回帰モデルの構築
fit3 <- glm(y~., 
            family=gaussian(link="identity") ,
            data=learn_data)

#線形回帰モデルの定数と係数
coef(fit3)

transform(coef(fit3))

#
#予測モデルの構築
#

#学習データ
data0302_learn <- data0302[106:140,]

learn_data <- as.data.frame(cbind(
  data0302_learn$y,
  data0302_learn$d_mon,
  data0302_learn$d_wed,
  data0302_learn$d_thu,
  data0302_learn$d_fri,
  data0302_learn$d_sat,
  data0302_learn$d_sun,
  data0302_learn$x2,
  pca$x[106:140,1]
))

colnames(learn_data) <- c('y','d_mon','d_wed','d_thu','d_fri','d_sat','d_sun','x2','pc1')

#線形回帰モデルの構築
fit4 <- glm(y~., 
            family=gaussian(link="identity") , 
            data=learn_data)

#線形回帰モデルの定数と係数
transform(coef(fit4))

#
#日販予測
#

plan01_predict <- predict(fit4,newdata=data03_plan01[,-1])
plan02_predict <- predict(fit4,newdata=data03_plan02[,-1])
plan03_predict <- predict(fit4,newdata=data03_plan03[,-1])
plan04_predict <- predict(fit4,newdata=data03_plan04[,-1])

plan_predict <- cbind(plan01_predict,
                      plan02_predict,
                      plan03_predict,
                      plan04_predict)

colnames(plan_predict) <- c('現状維持','対策案1','対策案2','対策案3')

rownames(plan_predict) <- c('2016/10/24','2016/10/25','2016/10/26','2016/10/27','2016/10/28','2016/10/29','2016/10/30')

plan_predict

apply(plan_predict,2,sum)
