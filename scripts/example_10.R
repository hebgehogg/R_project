install.packages("xlsx")
install.packages("car")
install.packages("moments")
install.packages(pkgs = c("nortest"))
install.packages("psych")
install.packages("corrplot")
install.packages("ggm")
install.packages("Hmisc")
install.packages("factoextra")

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
library(dplyr)
library(factoextra)

#ЗАДАНИЕ 1
#1.	Загрузите данные из файла VILLA2.csv 
df <- read.csv2("villa2.csv", 1)
View(df)
#2.	Посмотрите на данные. Если нужно, то удалите из базы выделяющиеся и аномальные наблюдения. 
str(df)
#3.	Выберите оптимальное число кластеров, начертите соответствующие графики и проверьте гипотезу.
  #отберем только те столбцы, которые нужны нам для кластерного анализа. 
d <- data %>% select(Price:Eco)
View(d)
  #Поскольку мы будем использовать одну и ту же базу для разных кластеризаций, то сохраним нашу базу в основной переменной как date0
date0 <- d
View(date0)
  #Матрица расстояний (расстояния между всеми парами точек (считает евклидово расстояние))
Mdist <- dist(scale(df))
  #иерархический кластерный анализ (метод средней связи)
hc <- hclust(Mdist, method = "average")
hc
  #построим дендрограмму c 2 кластерами
plot(hc, cex = 0.6, main = "2 clusters") 
rect.hclust(hc, k = 2, border = "red") 
  #Вытащим из полученного разбиения на кластеры метки для наблюдений
groups2 <- cutree(hc, k = 2) 
d1 <- df %>% mutate(groups2 = factor(groups2))
View(d1)
ggplot(data = d1, aes(x = Price, y = Eco, color = groups2)) + geom_point()
  #образует плотные группы - оставляем 2
  #Оценка кластеризации: формально
kruskal.test(d1$Price ~ d1$Eco)
  #Так как p-value близко к 0, на уровне значимости 5% можно отвергнуть эту гипотезу: распределения отличаются по кластерам (медианы этих показателей по разным кластерам не равны)
#4.	Проведите кластеризацию методом К-средних.
  #Для начала опять воспользуемся нашей первоначальной базой
d2 <- date0
  #кластерный анализ методом k-средних
cl <- kmeans(d2, 2)
cl
  #сохраним их отдельным столбцом в исходную базу данных df
d2$kmeans2 <- cl$cluster
View(d2)
#5.	Визуализируйте результаты кластеризации.
fviz_cluster(cl, data = d2, ellipse.type = 'convex')
