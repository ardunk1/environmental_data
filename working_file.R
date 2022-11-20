load(here)
library(here)
hab.sta <- read.csv(here( "data", "hab.sta.csv"))
bird.sta <- read.csv(here("data", "bird.sta.csv"))
summary(bird.sta)
str(bird.sta)
str(hab.sta)

pairs(hab.sta$elev, hab.sta$slope, hab.sta$aspect)
pairs(hab.sta[,c("elev", "slope", "aspect")], main = "bird habitat pairs plot")


hist(bird.sta$RUGR)
hist(bird.sta$WIWA, main = "Observation of WIWA", xlab = "Number of Birds")


rm(list = ls())

pol_n_predation <- 26
pol_n_no_predation <- 184
pol_n_total <- pol_n_predation + pol_n_no_predation
pol_predation_rate <- pol_n_predation / pol_n_total


psd_n_predation <- 25
psd_n_no_predation <- 706
psd_n_total <- psd_n_predation + psd_n_no_predation 
psd_predation_rate <- psd_n_predation / psd_n_total

print(
    paste0(
      "The seed predation rate for Polyscias fulva is: ",
      round(pol_predation_rate, digits = 3)
    )
)

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)
  )
)

################

seed_pred_ratio <- (pol_predation_rate / psd_predation_rate)
print(
  paste0(
    "The seed predation proportion of pol compared to psd is: ",
    round(seed_pred_ratio, digits = 3)
  )
)

################################

dpois(x = 8, lambda = 10.4)

dbinom(0, size = 6, prob = .66)

pbinom(4, 6, prob = 0.66, lower.tail = T)
pbinom(5, 6, prob = 2/3, lower.tail = F)


dnorm(0.5, mean = 0, sd = 1)
dnorm(1, mean = 0, sd = 1)

pnorm(0.5, mean = 0, sd = 1)
pnorm(1.0, mean = 0, sd = 1)
pnorm(2.0, mean = 0, sd = 1) - pnorm(1.0, mean = 0, sd = 1)


n <- 1000
x <- seq(from = -6, to =6, length.out = n)
y <- dnorm(x, mean = 0, sd = 1)
plot(y ~x, type = "l", ylab = "Probability Density")
y_2 <- dnorm(x, mean = 0, sd = 2)
y_3 <- dnorm(x, mean = -2, sd = 1)

plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~x, type = "l", lty = 2)
points(y_3 ~x, type = "l", lty = 2)


y_cdf_1 = pnorm(x, mean = 0, sd = 1)
y_cdf_2 = pnorm(x, mean = 0, sd = 2)
y_cdf_3 = pnorm(x, mean = -2, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density")
points(y_cdf_2 ~ x, type = "l", lty = 2)
points(y_cdf_3 ~x, type = "l", lty = 3)


################Library of Babel Activity###########3
cat(letters[sample(26, size = 3, replace = T)], sep = "")
W <- 26^3
w1 <- 26^4

##############In-Class Confidence Intervals #####
qnorm(c(0.025, 0.975))
alpha <- .1
qnorm(alpha/2, 0, 1)
qnorm(c(0.05, 0.95))


qt(10)
qt(c(0.025, 0.975), 50)


tval <- qt(c(0.025, 0.975), 49)
tval
std <- 3.14
m <- 10
ss <- 50

sse <- std/sqrt(50)
CIrad <- sse*tval
CI <- m + CIrad
CI

#############Likelihood Excercise#############33
#Use bird.sta and hab.sta data - loaded from top of notebook
dpois(x=2, lambda = 4.5)
dpois(x=6, lambda = 4.5)
dpois(x=2, lambda = 4.5) * dpois(x=6, lambda = 4.5)

wiwa_counts <- c(2,6)
dpois(x=wiwa_counts, lambda = 4.5) #calculate multiple likelihoods at 1 time
prod(dpois(x=wiwa_counts, lambda = 4.5)) #calculate the product to 2 likelihoods (chance of observing each of the values)

sum(log(dpois(x= wiwa_counts, lambda = 4.5)))
sum(log(dpois(x= wiwa_counts, lambda = 4.2)))

#load bird data
dat_bird <- read.csv(here::here("data", "bird.sta.csv"))
dat_habitat <- read.csv(here::here("data", "hab.sta.csv"))
dat_all <- merge(dat_bird, dat_habitat)

#check out data numerically
summary(dat_all$WIWA)

#check out data graphcially
hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 7)
hist(dat_all$WIWA, breaks = 0:7)
hist(dat_all$WIWA, breaks = 0:7 - 0.5)

par(mfrow = c(1,2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat)+1) - 0.5, main = "Histogram of \nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of \nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1)))

#Q1:
dpois(x = wiwa_counts, lambda = 2)
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
sum(log(dpois(x = wiwa_counts, lambda = 4.1)))

#Q3:
par(mfrow = c(1,1))
hist(dat_all$WIWR)
datWIWR <- dat_all$WIWR
hist(datWIWR, breaks = 0:(max(datWIWR)+1) - 0.5, main = "Histogram of \n Winter Wren Counts")
summary(datWIWR)

#Q4:

sum(log(dpois(x = datWIWR, lambda = 1.6)))

#Q7:
#calculate sample probability of success (i.e., count != 0)
success <- sum(datWIWR != 0)
prob_success <- success / 1046


#Q9:

sum(dbinom(datWIWR, size = 1045, prob = prob_success, log = TRUE))

#Q12:

set.seed(1)
vec_rnorm = rnorm(n=10, mean = 0, sd = 1)
dnorm(vec_rnorm, mean = 0, sd = 1, log = TRUE)

sum(dnorm(vec_rnorm, mean = ., sd = 1, log = TRUE))

#Q15:
require(palmerpenguins)
penguins <- penguins
flippers <- penguins$flipper_length_mm
flippers_No_NA <- na.omit(flippers)

mean(flippers_No_NA)
sd(flippers_No_NA)

sum(dnorm(flippers_No_NA, mean = 200.91, sd = 14.06, log = TRUE))


####################In Class Models 1 ###################
require(palmerpenguins)
penguins <- penguins


dat_ade <- droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass(g)")


#Q1: Create boxplots of body mass for male and female Adelie penguins

boxplot(dat_ade$body_mass_g ~ dat_ade$sex, main = "Adelie Penguins: Body Mass by Sex", xlab = "Sex", ylab = "body mass(g)")

#Q2: 
fem_ade <- droplevels(subset(dat_ade, sex == "female"))
t.test(fem_ade$body_mass_g, mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

#Q4: 1 sample t-test that male Adelie penguins have body mass > 4000 g
male_ade <- droplevels(subset(dat_ade, sex == "male"))
t.test(male_ade$body_mass_g, alternative = c("g"), mu = 4000, paired = FALSE, var.equal = FALSE, conf.leve = 0.95)

#Q6: 2 sample t-test that male and female Adelie penguins have different body masses
t.test(fem_ade$body_mass_g, male_ade$body_mass_g, mu = 0, paired = TRUE, var.equal = FALSE, conf.level = 0.95)

#Q8: 2-sample (one tailed) t-test that male Adelie penguins are heavier than females
t.test( male_ade$body_mass_g, fem_ade$body_mass_g, alternative = c("g"), mu = 0, paired = TRUE, var.equal = FALSE, conf.level = 0.95)

##########In Class Models 2 - Model Coefficients#####33

#Walk through
data(iris)
fit_species <- lm(
  Sepal.Length ~ Species,
  data = iris)
summary(fit_species)

unique(iris$Species)

plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)"
)

fit_petals <- lm(
  Petal.Width ~ Petal.Length, 
  data = iris)
summary(fit_petals)

5.0060+1.5820

boxplot(
  Sepal.Length~Species,
  data = iris)


hist(residuals(fit_species))
shapiro.test(residuals(fit_species))

hist(residuals(fit_petals))
shapiro.test(residuals(fit_petals))


######################GINKO DATA ASSIGNMENT#######################
hab.sta <- read.csv(here( "data", "hab.sta.csv"))
getwd()

g1 <- read.csv(here("data", "ginkgo_data_2022.csv"))

head(g1)

#Q1: How many trees total in the dataset
length(unique(g1$site_id)) #22

#Q2:How many trees had seeds?

seed_trees <- droplevels(subset(g1, seeds_present == "TRUE"))
head(seed_trees)

length(unique(seed_trees$site_id)) #4

#Q3: conditional boxplot of a continuous variable conditioned
# on seeds_present

boxplot(max_width ~ seeds_present, data = g1)

#Q5 - Scatterplot of max leaf depth (x) v max leaf width (y)

plot(g1$max_depth, g1$max_width, xlab = "Maximum leaf depth (mm)", 
     ylab = "Maximum leaf width(mm)", main = "Maximum Leaf Width by Depth")
