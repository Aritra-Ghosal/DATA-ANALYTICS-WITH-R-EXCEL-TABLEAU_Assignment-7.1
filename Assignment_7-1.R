#Question 1
library(tidyr)
library(ggplot2)
ggplot(gather(mtcars), aes(value)) + geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#Question 2
y = mtcars$mpg
group = mtcars$am
y; group
baselineMeans = tapply(mtcars$mpg, mtcars$am, mean)
baselineMeansDiff = baselineMeans[2] - baselineMeans[1]
tStat = function(w, g) mean(w[g == 1]) - mean(w[g == 0])
observedDiff = tStat(y, group)
baselineMeansDiff - observedDiff
permutations = sapply(1:100000, function(i) tStat(y, sample(group)))
par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
hist(permutations, main = "Distribution of shuffled group mean differences") # distribution of difference of averages of permuted groups
plot(permutations, type = "b", main = "Shuffled group mean trials", xlab = "trial", ylab = "shuffled group mean differences", ylim = c(-14, 14))
abline(h = observedDiff, col = "red", lwd = 3)
mean(permutations > observedDiff)
#### generate subset: automatic and manual cars ####
cars_auto = subset(mtcars, am == 0)
cars_manu = subset(mtcars, am == 1)
pairs(mtcars)
par(mfrow = c(2, 2), mar = c(2, 3, 2, 3))
with(mtcars, plot(hp, mpg, type = "n", main = "mpg vs. hp - by transmission type")) # no data
with(cars_auto, points(hp, mpg, col = "red", pch = 20))
with(cars_manu, points(hp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
model1_auto = lm(mpg ~ hp, data = cars_auto)
model1_manu = lm(mpg ~ hp, data = cars_manu)
abline(model1_auto, col = "red", lwd = 2)
abline(model1_manu, col = "blue", lwd = 2)
abline(v = 175, lty = 2)
with(mtcars, plot(wt, mpg, type = "n", main = "mpg vs. weight - by transmission type")) # no data
with(cars_auto, points(wt, mpg, col = "red", pch = 20))
with(cars_manu, points(wt, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
abline(v = 3.2, lty = 2)
with(mtcars, plot(drat, mpg, type = "n", main = "mpg vs. drat - by transmission type")) # no data
with(cars_auto, points(drat, mpg, col = "red", pch = 20))
with(cars_manu, points(drat, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
model2_auto = lm(mpg ~ drat, data = cars_auto)
model2_manu = lm(mpg ~ drat, data = cars_manu)
abline(model2_auto, col = "red", lwd = 2)
abline(model2_manu, col = "blue", lwd = 2)
abline(v = 175, lty = 2)
with(mtcars, plot(disp, mpg, type = "n", main = "mpg vs. disp - by transmission type")) # no data
with(cars_auto, points(disp, mpg, col = "red", pch = 20))
with(cars_manu, points(disp, mpg, col = "blue", pch = 20))
legend("topright", pch = 20, col = c("red", "blue"), legend = c("auto", "manu")) # add legend
labels = with(mtcars, paste(as.character(disp), as.character(mpg), sep = ",")) # generate point labels
with(mtcars, text(disp, mpg, labels = labels, cex = 0.7, pos = 2))
abline(v = 167.6, lty = 2)

#Question 3
library(psych)
boxplot(mtcars$disp,mtcars$hp,col = "red")
boxplot(mtcars$mpg,mtcars$cyl,mtcars$qsec,mtcars$vs,mtcars$am,mtcars$gear,mtcars$carb,col = "blue")
boxplot(mtcars$drat,mtcars$wt,col = "green")
