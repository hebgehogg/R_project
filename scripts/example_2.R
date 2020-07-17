# Задача 1
income <- 155000
income
log_income <- log(income)
log_income
income_pre <- 500000
if (income >income_pre) {
  cat("Доход больше в текущем месяце");
} else {
  cat("Доход больше в прошлом месяце");
}

# Задача 2
x <- 2
y <- 4
y <- y - x
x <- y + x
y <- x - y
x
y

# Задача 3
x <- 3.5
class(x)
y <- "2,6"
class(y)
z <- 1.78
class(z)
h <- TRUE
class(h)

# Задание 4
g  <- c(4, 7, -1, 21, 2, 0, 14)
g
q_sq <- '^'(g,2)
q_sq
g_log <- log(g)
g_log
#Nan, так как ln(-1) это комплексное число, -Inf, так как е в степени минус бесконечности стремиться к 0 
g[g >= 0]
which(g %% 7 == 0)
g_log[(g_log %% 2 == 0 & g_log > 5)]

# Задание 5
turnout <- c(100, 124, 121, 130, 150, 155, 144, 132, 189, 145, 125, 110, 118, 129, 127)
CountPod <- which(turnout %% 5 == 0)
CountPod
PartPod <- (length(CountPod)*100)/length(turnout)
round(PartPod,2)

# Задание 6
z <- c(8, NA, 7, 10, NA, 15, NA, 0, NA, NA, 87)
which(is.na(z))

# Задание 7
s <- c("4,5", "6,8", "9,2", "1,75")
n <- gsub(",",".", s)
n <- as.numeric(n)
n

# Задание 8
Fir <- matrix(c(1,50,1,75), nrow = 2, ncol = 2)
Sec <- matrix(c(100,6625), ncol = 1)
FF <- t(Fir) %*% Fir
FS <- t(Fir) %*% Sec
solve(FF,FS)

A <- matrix(c(1,2,3,4,2,7,6,9,3,6,3,8,4,9,8,2), ncol = 4, nrow = 4)
solve(A)
t(A)
sum(diag(t(A)))
det(A)
A <- A[c(1,3,4),c(1,2,4)]
A <- A * '^'(-1,2 + 3)
