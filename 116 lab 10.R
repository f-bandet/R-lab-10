# Name: Faye Bandet
# Date: 10/3/19
# ISTA 116 Section B || Section Leader : Jacob Heller
# Lab Assignment 10
# Collaborator(s): Nick Ackerman

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

#1
ames.gr.liv.area.sample <- sample(ames$Gr.Liv.Area, 60, replace = FALSE)
mean(ames.gr.liv.area.sample)
# 1495.1 is the mean of the data set.

#2
se <- sd(ames.gr.liv.area.sample)/sqrt(length(ames.gr.liv.area.sample))
se
# 68.6059 is the standard error.

#3
lower <- mean(ames.gr.liv.area.sample)-(1.96*se)
upper <- mean(ames.gr.liv.area.sample)+(1.96*se)
c(upper, lower)
# Lower is 1360.632, the upper is 1629.568

#4
mean(ames$Gr.Liv.Area)
# The true mean is 1499.69, which is within the confidance interval.

#5
area.means.repl <- replicate(50, {
  ames.gr.liv.area.sample <- sample(ames$Gr.Liv.Area, 60, replace = FALSE)
  se.60 <- sd(ames.gr.liv.area.sample)/sqrt(length(ames.gr.liv.area.sample))
  lower <- mean(ames.gr.liv.area.sample)-(1.96*se.60)
  upper <- mean(ames.gr.liv.area.sample)+(1.96*se.60)
  c(lower, upper)})
area.means.repl
dim(area.means.repl)
# The final product is a matrix with 2 rows and 50 columns long.

#6
lower.bounds <- area.means.repl[1,]
upper.bounds <- area.means.repl[2,]
lower.bounds
upper.bounds
# Assigned the upper and lower bounds to variables. 

#7
plot_ci(lower.bounds, upper.bounds, mean(ames$Gr.Liv.Area))
#Three of the fifty sets do not contain the true mean. This is similar to my expectations because the confidance interval is 95% which means 2 or 3 sets might fall outide of the expected mean of the confidance interval.

#8
# I'll use 2.576 as the 99% confidance error and what I use to sample.
(1-.99)/2
# 0.005 which is 2.576

#9
area.means.repl.2 <- replicate(50, {
  ames.gr.liv.area.sample <- sample(ames$Gr.Liv.Area, 60, replace = FALSE)
  se.60 <- sd(ames.gr.liv.area.sample)/sqrt(length(ames.gr.liv.area.sample))
  lower <- mean(ames.gr.liv.area.sample)-(2.576*se.60)
  upper <- mean(ames.gr.liv.area.sample)+(2.576*se.60)
  c(lower, upper)})
area.means.repl.2
lower.bounds.2 <- area.means.repl.2[1,]
upper.bounds.2 <- area.means.repl.2[2,]
plot_ci(lower.bounds.2, upper.bounds.2, mean(ames$Gr.Liv.Area))
# Plot was made

#10
# No mean values fell outside of the confidance interval, which makes sense because the confidance interval is almost 100% so it is very unlikely that a mean would fall outside the confidance interval. That is what I expected.
