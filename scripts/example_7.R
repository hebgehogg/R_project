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
#3.	Постройте красивые графики, характеризующие зависимость между переменными.  
corr.test(df)
corrplot(cor(df), "pie", "lower")
corrplot(cor(df))
#4.	Постройте корреляционную матрицу (коэффициентов Пирсона). Визуализируйте ее. Сделайте выводы о том, какая взаимосвязь между переменными. 
cor(df)
#5.	На уровне значимости 0,05 проверить гипотезу о значимости парных коэффициентов корреляции.
rcorr(as.matrix(df)) 
#6.	Найдите частные коэффициенты корреляции. Сделайте выводы.
pcor(c(1,2), cov(df))
pcor(c(1,3), cov(df))
pcor(c(1,4), cov(df))
pcor(c(1,5), cov(df))
pcor(c(1,6), cov(df))
pcor(c(2,3), cov(df))
pcor(c(2,4), cov(df))
pcor(c(2,5), cov(df))
pcor(c(2,6), cov(df))
pcor(c(3,4), cov(df))
pcor(c(3,5), cov(df))
pcor(c(3,6), cov(df))
pcor(c(4,5), cov(df))
pcor(c(4,6), cov(df))
pcor(c(5,6), cov(df))

#ЗАДАНИЕ 2
#визуализация матрицы
d <- array(c(12, 12, 6, 1, 15, 10, 4, 0, 10, 20, 25, 15, 5, 25, 30, 20, 0, 5, 10, 15), dim = c(4, 5))
colnames(d) <- c("Среднее", "Неполное высшее", "Высшее", "Два высших","MBA")
rownames(d) <- c("До 20", "от 20 до 40", "От 40 до 60", "Свыше 60")
d
#таблица сопряженности
#На уровне значимости 0,05 проверьте гипотезу о независимости уровня дохода от образования. Сделайте выводы.
#критерий хи-квадрат
chisq.test(d)
mosaicplot(d, shade = TRUE)
