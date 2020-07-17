install.packages("xlsx")
install.packages("moments")
install.packages(pkgs = c("nortest"))
install.packages("ggplot2")

library(xlsx)
library("moments")
library(nortest)
library(ggplot2)

#1 задание: 
  #Загрузите данные из файла VILLA.xls 
df <- read.xlsx("villa.xls", 1)
View(df)
  #Определите тип данных, с которыми Вы работаете.
str(df)
  #Проверьте нормальность распределения переменной Price с помощью критерия Лиллифорса.
lillie.test(df$Price)
#p-value = 5.461e-05(0.0000546) - слишком мал, поэтому гипотиза о нормальном распределении ОТВЕРГАЕТСЯ
df <- subset(df, df$Eco != 2)
print(wilcox.test(Price ~ Eco, data = df))

#2 задание: psych_survey.csv
df2 <- read.csv2("psych_survey.csv", 1)
View(df2)
  #Проверить гипотезу о  равенстве среднего роста (height) у студентов с разным любимым предметом (subject)
      #Любимый предмет. Респонденту нужно было выбрать один ответ из 5 предложенных вариантов: Математика, Биология, Русский язык, Иностранный язык, Ни один из вышеперечисленных предметов.

matl <- subset(df2, subject == 1)
bio1 <- subset(df2, subject == 2)
rusl <- subset(df2, subject == 3)
bugl <- subset(df2, subject == 4)
nonl <- subset(df2, subject == 5)
ks.test(matl$height, "pnorm",
        mean = mean(matl$height, na.rm = T),
        sd = sd(matl$height, na.rm = T))
ks.test(bio1$height, "pnorm",
        mean = mean(bio1$height, na.rm = T),
        sd = sd(bio1$height, na.rm = T))
ks.test(rusl$height, "pnorm",
        mean = mean(rusl$height, na.rm = T),
        sd = sd(rusl$height, na.rm = T))
ks.test(bugl$height, "pnorm",
        mean = mean(bugl$height, na.rm = T),
        sd = sd(bugl$height, na.rm = T))
ks.test(nonl$height, "pnorm",
        mean = mean(nonl$height, na.rm = T),
        sd = sd(nonl$height, na.rm = T))
shapiro.test(matl$height)
shapiro.test(bio1$height)
shapiro.test(rusl$height)
shapiro.test(bugl$height)
shapiro.test(nonl$height)
anova <- aov(height ~ subject, data = df2)
summary(anova)