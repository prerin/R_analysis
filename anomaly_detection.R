#異常検知の対象データ行の指定
target <- 33

#学習データの行の指定
learn_data_start <- target-30
learn_data_end <- target-1

#学習データの設定
learn_data <- ts(data01[learn_data_start:learn_data_end,2])

#異常検知の対象データの設定
y <- as.numeric(data01[target,2])

#学習データの描写
plot(learn_data)

#階差データの生成
#1階差
diff(learn_data)
#２階差
diff(diff(learn_data))

#階差データの描写
plot(diff(learn_data))

#ARモデルの構築
ARmodel <- auto.arima(learn_data,
                      max.p = 5,
                      max.q = 0,
                      max.d = 2,
                      max.P = 0,
                      max.Q = 0,
                      max.D = 0)

#ARモデルの構築結果
ARmodel

#ARモデルの予測値（学習データ）
ARmodel_fitted <-fitted(ARmodel)

#ARモデルの実現値と予測値の描写
plot(learn_data,
     col="blue",
     type="l",
     ylim=c(0,500),
     xlab="時系列", 
     ylab="日販")

lines(ARmodel_fitted,
      col="red",
      type="l",
      lty=3)

#ARモデルの残差
ARmodel_res <- residuals(ARmodel)

#ARモデルの残差の描写
plot(ARmodel_res,ylim=c(-100,100))

#学習データの外れ値処理
learn_data2 <- ifelse(abs(ARmodel_res)>20,
                      ARmodel_fitted,
                      learn_data)

#
#外れ値処理後の学習データで再構築
#

#ARモデルの構築
ARmodel <- auto.arima(learn_data2,
                      max.p = 5, 
                      max.q = 0, 
                      max.d = 2,
                      max.P = 0, 
                      max.Q = 0, 
                      max.D = 0)

#ARモデルの構築結果
ARmodel

#ARモデルの予測値（学習データ）
ARmodel_fitted <- fitted(ARmodel)

#ARモデルの実現値と予測値の描写
plot(learn_data2,
     col="blue",
     type="l",
     ylim=c(0,500),
     xlab="時系列", 
     ylab="日販")

lines(ARmodel_fitted,col="red",type="l",lty=3)

#ARモデルの残差
ARmodel_res <- residuals(ARmodel)

#ARモデルの残差の標準偏差
ARmodel_res_sd <- sd(ARmodel_res)

#ARモデルの残差の平均
ARmodel_res_mean <- mean(ARmodel_res)

#ARモデルの評価対象の予測
ARmodel_yosoku <- forecast(ARmodel,h=1)$mean

#ARモデルの評価対象の残差
ARmodel_gap <- y - ARmodel_yosoku

#外れ値スコア算出
LOF <- -log(dnorm(ARmodel_gap,ARmodel_res_mean,ARmodel_res_sd))

LOF

########################
#関数
########################

LOF <- function(data01,target,trim) {
  
  #データセット
  learn_data_start <- target-30
  learn_data_end <- target-1
  learn_data <- ts(data01[learn_data_start:learn_data_end,2])
  y <- as.numeric(data01[target,2])
  
  #ARモデル構築（外れ値処理前）
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #ARモデル構築（外れ値処理）
  learn_data2 <- ifelse(abs(ARmodel_res)>trim,
                        ARmodel_fitted,
                        learn_data)
  learn_data <- learn_data2 
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #残差の標準偏差と平均値
  ARmodel_res_sd<-sd(ARmodel_res)
  ARmodel_res_mean<-mean(ARmodel_res)
  
  #評価対象の予測と残差
  ARmodel_yosoku <- forecast(ARmodel,h=1)$mean
  ARmodel_gap <- y-ARmodel_yosoku
  
  #外れ値度
  LOF <- -log(dnorm(ARmodel_gap,ARmodel_res_mean,ARmodel_res_sd))
  
  #出力
  output_data <- c(LOF,ARmodel_res_mean,ARmodel_res_sd,y,ARmodel_yosoku,ARmodel_gap)
  output_name <- c("LOF", "Mean", "SD","Measured value","Predicted value","Gap")  
  names(output_data) <- output_name 
  return(output_data)
  
}

LOF(data01,33,20)
LOF(data01,134,20)


LOF2 <- function(data01,target) {
  
  #データセット
  learn_data_start <- target-30
  learn_data_end <- target-1
  learn_data <- ts(data01[learn_data_start:learn_data_end,2])
  y <- as.numeric(data01[target,2])
  
  #ARモデル構築（外れ値処理前）
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #残差の標準偏差と平均値
  ARmodel_res_sd<-sd(ARmodel_res)
  ARmodel_res_mean<-mean(ARmodel_res)
  
  #ARモデル構築（外れ値処理）
  learn_data2 <- ifelse(abs((ARmodel_res-ARmodel_res_mean)/ARmodel_res_sd)>2,
                        ARmodel_fitted,
                        learn_data)
  
  learn_data <- learn_data2 
  ARmodel <- auto.arima(learn_data,
                        max.p = 5, max.q = 0, max.d = 2,
                        max.P = 0, max.Q = 0, max.D = 0)
  ARmodel_fitted<-fitted(ARmodel)
  ARmodel_res<-residuals(ARmodel)
  
  #残差の標準偏差と平均値
  ARmodel_res_sd<-sd(ARmodel_res)
  ARmodel_res_mean<-mean(ARmodel_res)
  
  #評価対象の予測と残差
  ARmodel_yosoku <- forecast(ARmodel,h=1)$mean
  ARmodel_gap <- y-ARmodel_yosoku
  
  #外れ値度
  LOF <- -log(dnorm(ARmodel_gap,ARmodel_res_mean,ARmodel_res_sd))
  
  #出力
  output_data <- c(LOF,ARmodel_res_mean,ARmodel_res_sd,y,ARmodel_yosoku,ARmodel_gap)
  output_name <- c("LOF", "Mean", "SD","Measured value","Predicted value","Gap")  
  names(output_data) <- output_name 
  return(output_data)
  
}

LOF2(data01,33)
LOF2(data01,134)
