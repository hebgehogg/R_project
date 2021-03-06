install.packages("xlsx")
install.packages("car")
install.packages("moments")
install.packages(pkgs = c("nortest"))
install.packages("psych")
install.packages("corrplot")
install.packages("ggm")
install.packages("Hmisc")

library(Hmisc)
library(ggm) 
library(xlsx)
library("moments")
library("car")
library(nortest)
library(psych)
library(pastecs)
library(ggplot2)
library(corrgram)
library("corrplot")

#ЗАДАНИЕ 1
#1.	Загрузите данные из файла VILLA2.csv 
df <- read.csv2("villa2.csv", 1)
View(df)
#2.	Посмотрите на данные. Если нужно, то удалите из базы выделяющиеся и аномальные наблюдения. 
str(df)
#3.	Построить поле корреляции. 
plot(df,las = 1, bty = "l", col = "red", pch = 19)
#4. Оценить модель     с помощью метода наименьших квадратов. Найдите характеристики модели как t-статистики и коэффициент детерминации R2 и скорректированный коэффициент детерминации
model <- lm(data = df, df$Price ~ df$house+df$area)
model
summary(model)
  #критерии Шварца и Акайке
AIC(model)
BIC(model)
#5.	Проверить модель на мультиколлинеарность
vif(model)
mean(vif(model))
#6.	Подберите функциональную форму зависимости цены коттеджа от его параметров, учитывая такие факторы как t-статистики и коэффициент детерминации R2  и скорректированный коэффициент детерминации, критерии Шварца и Акайке. Обоснуйте выбор наилучшей модели
model1 <- lm(data = df, df$Price ~ log(df$house) +log(df$area) + df$Eco)
model1
summary(model1)
#критерии Шварца и Акайке
AIC(model1)
BIC(model1)

model2 <- lm(data = df, log(df$Price) ~ log(df$house) + df$area +log(df$Dist) + df$Eco)
model2
summary(model2)
#критерии Шварца и Акайке
AIC(model2)
BIC(model2)

model3 <- lm(data = df, log(df$Price) ~ log(df$house) + log(df$area) +log(df$Dist) + df$Eco)
model3
summary(model3)
#критерии Шварца и Акайке
AIC(model3)
BIC(model3)

model4 <- lm(data = df, df$Price ~ df$house + df$area*df$Dist + df$Eco)
model4
summary(model4)
#критерии Шварца и Акайке
AIC(model4)
BIC(model4)











