install.packages("xlsx")
install.packages("car")
install.packages("moments")
install.packages(pkgs = c("nortest"))
install.packages("psych")

library(xlsx)
library("moments")
library("car")
library(nortest)
library(psych)

library(pastecs)
library(ggplot2)


#1.	Загрузите данные из файла VILLA.xls
df <- read.xlsx("villa.xls", 1)
View(df)

#2.	Определите тип данных, с которыми Вы работаете.
str(df)

#3.	Рассчитайте и проинтерпретируйте описательные статистики по каждой переменной, включая фиктивную переменную.
df <- subset(df, df$Eco != 2)
summary(df)
describe(df)

#4.	Проанализируйте исходную выборку на наличие статистических выбросов, используя анализ ящичковых диаграмм. Сделайте выводы.
boxplot(df)
#Выбросы есть у Price:
length(boxplot.stats(df$Price)$out)
#Выбросы есть у area:
length(boxplot.stats(df$area)$out)

#5.	Проверьте однородность всех  переменных с помощью коэффициента вариации по каждой переменной. Сделайте выводы.
#Вариация (стандартное отклонение поделить на среднее значение)
sd(df$N, na.rm = TRUE)/mean(df$N, na.rm = TRUE)*100
sd(df$Price, na.rm = TRUE)/mean(df$Price, na.rm = TRUE)*100
sd(df$Dist, na.rm = TRUE)/mean(df$Dist, na.rm = TRUE)*100
sd(df$house, na.rm = TRUE)/mean(df$house, na.rm = TRUE)*100
sd(df$area, na.rm = TRUE)/mean(df$area, na.rm = TRUE)*100
sd(df$Eco, na.rm = TRUE)/mean(df$Eco, na.rm = TRUE)*100

#6.	Проверьте нормальность распределения переменной Price с помощью:
    #a.	гистограммы
hist(df$Price, main = "Плотность распределения", xlab = "Цена", ylab = "Плотность", col = "pink", freq = FALSE)
lines(density(df$Price), lwd = 2, col = "red")
    #b.	коэффициентов асимметрии и эксцесса
#Эксцесс
kurtosis(df$Price, na.rm = TRUE)
#Асимметрия
skewness(df$Price, na.rm = TRUE)
    #c.	графика Q-Qplot
qqPlot(df$Price)
    #d.	проверки гипотезы о нормально распределении ( на уровне значимости 0,05) с помощью критериев:
#Колмогорова-Смирнова
print( ks.test(df$Price, "pnorm", mean = mean(df$Price, na.rm = T), sd = sd(df$Price, na.rm = T)))
#Шапиро-Уилка
shapiro.test(df$Price)
#Лиллифорса
lillie.test(df$Price)
#Крамера-фон Мизеса
cvm.test(df$Price)
#Андерсона-Дарлинга
ad.test(df$Price)
#Шапиро-Франсиа
sf.test(df$Price)
#хи-квадрат Пирсона
pearson.test(df$Price)