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

#Question 3
library(psych)
boxplot(mtcars$disp,mtcars$hp,col = "red")
boxplot(mtcars$mpg,mtcars$cyl,mtcars$qsec,mtcars$vs,mtcars$am,mtcars$gear,mtcars$carb,col = "blue")
boxplot(mtcars$drat,mtcars$wt,col = "green")
