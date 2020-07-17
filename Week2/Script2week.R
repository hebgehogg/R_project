# Часть 1: ввод и вывод
# Задача 1
PersonName <- readline(prompt = "Введите свое имя: ")
cat("Hallo,",PersonName)
# Задача 2
FirNum <- readline(prompt = "Введите первое число: ")
FirNum <- sub(",",".", FirNum)
FirNum <- as.numeric(FirNum)
SecNum <- readline(prompt = "Введите второе число: ")
SecNum <- sub(",",".", SecNum)
SecNum <- as.numeric(SecNum)
FirNum + SecNum
# Задача 3
KH <- readline(prompt = "Введите скорость в км/ч: ")
KH <- as.numeric(KH)
KH <- (KH*1000)/3600
cat("Скорость в м/с",KH)

# Часть 2: векторы
# Задача 1
vec <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
as.numeric(vec)
vec[1]
vec[length(vec)]
vec[3:5]
vec[vec == 2 & !is.na(vec)]
vec[vec > 4 & !is.na(vec)]
vec[vec %% 3 == 0 & !is.na(vec)]
vec[vec %% 3 == 0 & vec > 4 & !is.na(vec)]
vec[(vec < 1 | vec > 5) & !is.na(vec)]
which(vec == 0 & !is.na(vec))
which((!vec < 2 & !vec > 8) & !is.na(vec))
# Задача 2
vec[length(vec)] = NA
vec
# Задача 3
which(is.na(vec))
# Задача 4
length(vec[is.na(vec)])
# Задача 5
IdRes <- 1:100
IdRes
# Задача 6
Country <- rep(c('France','Italy','Spain'), each = 5)
Year <- rep(c('2016','2017','2018','2019','2020'),3)
tableRes <- data.frame(Country,Year)
tableRes
# Задача 7
income <- c(10000, 32000, 28000, 150000, 65000, 1573)
as.numeric(income)
SrInc <- sum(income)/length(income)
cat("Среднее: ", SrInc)
income_class <- income 
income_class <- replace(income_class, income_class < SrInc, 0)
income_class <- replace(income_class, income_class >= SrInc, 1)
income_class