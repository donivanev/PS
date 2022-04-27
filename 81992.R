### Doni Ivanov
### 81992

### 1 ###

# 1.1
# 1.2

shots = c(8, 5, 12, 11, 12, 8, 6, 7, 11, 7, 11, 13, 15,
          12, 17, 12, 9, 15, 8, 11, 11, 13, 10, 8, 12, 12, 11,
          13, 12, 14, 9, 11, 13, 10, 10, 12, 13, 10, 15, 12, 15, 12)

x = c(0.34, 0.47, 0.56, 0.69, 0.83, 0.91)

f = function(x) {
  log(dbinom(shots, 30, x))
}

y = sapply(x, f)

# 1.3

plot(y[,2], main = "CHART", xlab = "Days", ylab = "Probabilities", type = "l")
lines(y[,3], lty = "dashed", col = "blue")
lines(y[,4], lty = "dotted", col = "red")
lines(y[,5], lty = "dashed", col = "green")
lines(y[,6], lty = "dotted", col = "yellow")

# 1.4

# We choose the first probability (0.34) because it is the smallest and its graph is 
# above the others

### 2 ###

# 2.1

# mtcars[1:5, ]
#                      mpg   cyl  disp  hp   drat   wt     qsec    vs  am  gear  carb
# Mazda RX4            21.0   6   160   110  3.90   2.620  16.46   0   1    4     4
# Mazda RX4 Wag        21.0   6   160   110  3.90   2.875  17.02   0   1    4     4
# Datsun 710           22.8   4   108   93   3.85   2.320  18.61   1   1    4     1
# Hornet 4 Drive       21.4   6   258   110  3.08   3.215  19.44   1   0    3     1
# Hornet Sportabout    18.7   8   360   175  3.15   3.440  17.02   0   0    3     2

# 2.2

#Maserati Bora
max(mtcars$hp)
sort(mtcars$wt)[c(1:5)]

# 2.3

mtcars$mpg
mtcars$cyl
boxplot(mtcars$mpg, mtcars$cyl)

# 2.4

mtcars$hp
mtcars$mpg
plot(mtcars$hp, mtcars$mpg)
lines(lowess(mtcars$hp, mtcars$mpg), col = 'green')
cor(mtcars$hp, mtcars$mpg)

# 2.5

mtcars$hp
top20 = mtcars[mtcars$hp > quantile(mtcars$hp, prob = 1 - length(mtcars$hp) / 100), ]
min(top20$hp)

# 2.6

mean(mtcars$hp < 100)

# 2.7

mean(mtcars$cyl[mtcars$gear == 5] == 8)

### 3 ###

counter <- 0
arr <- c(1:20)

while(1) {

  r = sample(1:20, 1, replace = T)
  counter = counter + 1
  
  if (r %in% arr) {
    arr[r] = 0
  }
  
  if (all(arr == 0)) {
     counter
     break
  }
}

counter