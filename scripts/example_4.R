install.packages("xslx")
library(xlsx)
install.packages("carData")
library("car")
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2) # для построения графиков
install.packages("moments")
library("moments")
install.packages("DescTools")
library(DescTools)

################################################################ Задание 1 ################################################################
#1. Импортируйте набор данных с именем «AppleStore» в R.
AppleStore <- read.xlsx("AppleStore.xlsx", 1)
View(AppleStore)

#2. Создайте новый фрейм данных, который будет содержать все переменные, кроме «id» и «currency». Назовите этот новый фрейм данных как df2.
df2 <- subset(AppleStore, select = -c(id, currency))
View(df2)

#3. Изучите структуру нового набора данных (df2) и предоставьте анализ общей информации об этом наборе данных (что такое единица наблюдения, 
#сколько переменных и наблюдений, какие переменные находятся в наборе данных и какие они типы)
str(df2)
#кол-во наблюдений
nrow(df2)
#кол-во переменных
ncol(df2)

#4.Анализ суммарной статистики переменных «цена», «user_rating» и 
#«lang_num», «size_bytes».
summary(df2[, c("price", "user_rating", "lang_num", "size_bytes")])

#5.Приложение имеет наибольшее количество языков:
max(df2$lang_num)

#6.Квантили переменных «цена», «user_rating» и «lang_num»
quantile(df2$price)
quantile(df2$user_rating)
quantile(df2$lang_num)

#7.Для всех количественных переменных рассчитать коэффициенты эксцесса 
#и асимметрии и коэффициент вариации.
#Эксцесс
#Так как E > 0 распределение является островершинным(у всех)
kurtosis(df2$size_bytes, na.rm = TRUE)
kurtosis(df2$price, na.rm = TRUE)
kurtosis(df2$rating_count_tot, na.rm = TRUE)
kurtosis(df2$user_rating, na.rm = TRUE)
kurtosis(df2$lang_num, na.rm = TRUE)
#Асимметрия
# A>0 правосторонняя асимметрия, A<0 левостороняя асимметрия
skewness(df2$size_bytes, na.rm = TRUE)
skewness(df2$price, na.rm = TRUE)
skewness(df2$rating_count_tot, na.rm = TRUE)
skewness(df2$user_rating, na.rm = TRUE)
skewness(df2$lang_num, na.rm = TRUE)
#Вариация (стандартное отклонение поделить на среднее значение)
# COV < 10% - степень рассеивания данных незначительная
# 10% < COV < 20% - средней
# 20% < COV =< 33% - значительной
sd(df2$size_bytes)/mean(df2$size_bytes)*100
sd(df2$price)/mean(df2$price)*100
sd(df2$rating_count_tot)/mean(df2$rating_count_tot)*100
sd(df2$user_rating)/mean(df2$user_rating)*100
sd(df2$lang_num)/mean(df2$lang_num)*100

#8.Для всех количественных переменных построить  Boxplot. 
#Обязательно сделать подписи на графике. Сделать выводы о наличии выбросов. 
#Все что выше "усов" - выбросы
boxplot(df2$lang_num, ylab = "Кол-во языков")
boxplot(df2$size_bytes, ylab = "Байты")
boxplot(df2$price, ylab = "Цены")
boxplot(df2$rating_count_tot, ylab = "Общий рейтинг")
boxplot(df2$user_rating, ylab = "Рейтинг пользователей")
#Во всех переменных есть выбросы

#9.Для всех качественных данных построить круговые диаграммы. 
pie(table(df2$prime_genre), radius = 0.9, cex = 0.6, main = "Жанры")
#кач. переменная "Name" имеет слишком много полей, но реализуется так: pie(table(df2$name), radius = 0.9, cex = 0.6, main = "Имена")

#10.Для количественных переменных гистограммы с плотностью нормального распределения.
hist(df2$size_bytes, main = "Плотность распределения", xlab = "Размер", ylab = "Плотность", col = "pink", freq = FALSE)
hist(df2$price, main = "Плотность распределения", xlab = "Цена", ylab = "Плотность", col = "pink", freq = FALSE)
hist(df2$rating_count_tot, main = "Плотность распределения", xlab = "Рейтинг пользователей", ylab = "Плотность", col = "pink", freq = FALSE)
hist(df2$user_rating, main = "Плотность распределения", xlab = "Рейтинг", ylab = "Плотность", col = "pink", freq = FALSE)
hist(df2$lang_num, main = "Плотность распределения", xlab = "Количество языков", ylab = "Плотность", col = "pink", freq = FALSE)

#11.Какой жанр наиболее распространен? Подсказка: чтобы выяснить это, 
#преобразуйте переменную prime_genre в множитель и просмотрите ее сводную статистику.
maxfunc <- function(arg){
        temp <- as.data.frame(arg %>% group_by(arg$prime_genre) %>% tally())
        a <- temp[which.max(temp$n),1]
        return(as.character(a))
}
maxfunc(df2)
#Самай популярный жанр - Games

#12.Создайте новый фрейм данных из существующего фрейма данных df2, чтобы новый фрейм данных содержал только приложения, соответствующие 
#наиболее распространенному жанру. Рассчитайте сводную статистику переменных, которые вы проанализировали в (4) для нового фрейма данных, 
#и сравните их с результатами в (4).
df3 <- df2[df2$prime_genre == maxfunc(df2), ]
View(df3)
summary(df2[, c("price", "user_rating", "lang_num", "size_bytes")])
summary(df3[, c("price", "user_rating", "lang_num", "size_bytes")])

#13.Проверьте, используя критерий Колмогорова-Смирнова, гипотезу о нормальности распределения показателя «цена»  по группам. Проделайте 
#то же самое, используя критерий Шапиро-Уилка. Сделайте выводы.
#Две группы: где только игры, и без игр
g <- df2[df2$prime_genre == "Games", ]
ng <- df2[df2$prime_genre != "Games", ]
#Критерий Колмогорова-Смирнова
#palues > 0.05 => нормальное распределение
mean_g <- mean(g$price)
sd_g <- sd(g$price)
ks.test(g$price, pnorm, mean_g, sd_g)
mean_ng <- mean(ng$price)
sd_ng <- sd(ng$price)
ks.test(ng$price, pnorm, mean_ng, sd_ng)
#Критерий Шапиро-Уилка
shapiro.test(g$price)
shapiro.test(ng$price)
#Критерий Колмогорова-Смирнова и критерий Шапиро-Уилка выдали 1 результат



################################################################ Задание 2 ################################################################
library(ggplot2)
library(dplyr)
#1.Данные по населению Белгородской и Калужской областей за 2016
df <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv", encoding = "UTF-8")
View(df)

#2.Создайте переменную young_share — процент населения возраста, 
#моложе трудоспособного. Создайте переменную trud_share — 
#процент населения трудоспособного возраста и old_share — 
#процент населения возраста, старше трудоспособного.
df <- df %>% mutate(young_share = young_total/popul_total * 100,
                    trud_share = wa_total/popul_total * 100,
                    old_share = ret_total/popul_total * 100)
View(df)

#3.Постройте гистограмму для доли трудоспособного населения 
#в процентах. Измените цвет гистограммы, добавьте rugs. 
#Добавьте вертикальную линию, которая отчерчивает медианное 
#значение доли трудоспособного населения в процентах.
ggplot(data = df, aes(x = trud_share)) + 
        geom_histogram(fill = "tomato", color = "black", bins = 10) +
        labs(title = "Доля трудоспособного населения в процентах", x = "Люди в трудоспособном возрасте в %") +
        geom_vline(xintercept = median(df$trud_share), color = "red", 
                   lwd = 1, lty = 2) + geom_rug()

#4.Постройте сглаженные графики плотности распределения 
#для доли трудоспособного населения в процентах по регионам 
#(два графика в одной плоскости). Настройте цвета и прозрачность заливки. 
# сглаженные графики плотности
ggplot(data = df, aes(x = trud_share, group = region, fill = region)) +
        geom_density(alpha = 0.5) +
        scale_fill_manual(values = c("blue", "pink"))
#По графикам плотности определите, имеет ли смысл для 
#визуализации распределения доли трудоспособного 
#населения строить скрипичные диаграммы (violin plot).
# скрипичные диаграммы
ggplot(data = df, aes(x = "", y = trud_share, group = region, fill = region)) +
        geom_violin() + scale_fill_manual(values = c("blue", "pink"))

#5.Постройте диаграмму рассеяния для переменных young_share 
#и old_share. Можно ли сказать, что чем больше процент молодого 
#населения (моложе трудоспособного населения), 
#тем меньше процент пожилых людей (старше труд возраста)? ОТВЕТ: НЕТ
ggplot(data = df, aes(x = young_share, y = old_share)) + geom_point(color = "pink", shape = 10, size = 5) + theme_bw()

#6.Создайте переменную male_share — доля мужского населения в 
#районе/городе (в процентах). Создайте переменную male, которая 
#принимает значение 1, если доля мужчин в муниципальном 
#районе/городе больше доли женщин, и значение 0 — во всех остальных случаях.
df <- df %>% mutate(female = young_female + wa_female + ret_female, male = young_male + wa_male + ret_male) %>% 
        mutate(fe_share = female / popul_total * 100, ma_share = male / popul_total * 100) %>%
        mutate(Male = factor(ifelse(ma_share > fe_share, 1, 0)))
View(df)

#7.Постройте пузырьковую диаграмму (bubble plot) для переменных 
#young_share и old_share, учитывая информацию о доле мужчин в 
#районе и о том, преобладают ли мужчины в районе или нет.
ggplot(data = df, aes(x = young_share, y = old_share)) + geom_point(aes(size = ma_share, color = Male))

#8.Постройте столбиковую диаграмму (bar plot), которая показывала бы, 
#сколько в базе данных районов Белгородской области, а сколько — Калужской.
ggplot(data = df, aes(x = region)) + geom_bar(fill = "pink", color = "black") + 
        scale_y_continuous(breaks = seq(0, 30, by = 1))
#В Калужской больше


################################################################ Задание 3 ################################################################
titanic <- read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
View(titanic) 
#1.Выведите описательные статистики для всех переменных в таблице. Выберите два показателя (один количественный, один качественный) и 
#проинтерпретируйте все выведенные по ним значения статистик.
summary(titanic)
#Колличественная
summary(titanic[, "Pclass"])
#Качественная
summary(titanic[, "Name"])

#2.Постройте для показателя Age гистограмму, поменяйте ее цвет, добавьте название (заголовок) графика. Напишите, людей какого 
#возраста в базе больше и меньше всего.
ggplot(data = titanic, aes(x = Age)) + 
        geom_histogram(fill = "tomato", color = "black", bins = 10) +Ы
        labs(title = "Возраст пассажиров", x = "Возраст") + geom_rug()
#болььше всего от 20 до 30 лет, меньше всего от 70 до 80

#3.Постройте для показателя Age ящик с усами. Напишите, есть ли в выборке нетипичные значения (выбросы), и если есть, то сколько.
boxplot(titanic$Age)
length(boxplot.stats(titanic$Age)$out)
#Выбросы есть - 8

#4.Постройте 95%-ный доверительный интервал для доли женщин среди выживших. Постройте 95%-ный доверительный интервал для доли мужчин среди выживших. 
#Проинтерпретируйте полученные интервалы. Какой из доверительных интервалов длинее? Пересекаются ли доверительные интервалы?
install.packages("DescTools")
library(DescTools)
BinomCI(summary(subset(titanic, Survived == 1)$Sex)["female"], length(titanic$Sex), conf.level = 0.95)
BinomCI(summary(subset(titanic, Survived == 1)$Sex)["male"], length(titanic$Sex), conf.level = 0.95)
#Длинее female
#Не пересекаются