df <- read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
View(df) 

# Задание 1
#кол-во наблюденицй (строк)
nrow(df)
#кол-во переменных (столбцов)
ncol(df)
#структура датафрейма (наблюдения, переменные, типы переменных, примеры значений)
str(df)
#значения БЕЗ пропусков
sum(complete.cases(df))
#таблица содержащая строки с пропущенными значениями
df_na <- df[!complete.cases(df), ]
#график частоты, с которой встречаются пропущенные значения в переменных
aggr(df)
#больше всего пропусков в поле "Age"
#график определяющий паттерны пропущенных значений
matrixplot(df)
#на 100% пропущенные значения не системные (связанно со спецификой вопросов)
#удаление пропущенных значений
df <- na.omit(df)
sum(!complete.cases(df))

# Задание 2
#добавление новой переменной и заполнение
df$female <- factor(df$Sex)
df$female <- gsub("female","1", df$female)
df$female <- gsub("male","0", df$female)
df$female <- as.integer(df$female)
View(df)
#пассажиры старше 25 лет и не старше 45 лет, которые путешествовали вторым или третьим классом
df2 <- df[(df$Age > 25 & df$Age <= 45) & (df$Pclass == 2 | df$Pclass == 3), ]
View(df2)
#кол-во пассажиров мужского пола
nrow(subset(df, female == 0))
#кол-во пассажиров женского пола
nrow(subset(df, female == 1))
#самый молодой пассажир
View(df[min(df$Age) == df$Age, ])
#самый старый пассажир
View(df[max(df$Age) == df$Age, ])
#средний возраст пассажиров первого класса, которые выжили в катастрофе
Alive <- df[(df$Survived == 1) & (df$Pclass == 1), ]
mean(Alive$Age)

