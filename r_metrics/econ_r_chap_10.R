library(AER)
library(plm)
library(stargazer)

data(Fatalities)

is.data.frame(Fatalities)

dim(Fatalities) #Gives the dimensions of the data

str(Fatalities) #Gives a display of data

head(Fatalities)

View(Fatalities)

#Define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

#Subset the data
Fatalities1982 <- subset(Fatalities, year=="1982")
Fatalities1988 <- subset(Fatalities, year=="1988")

#Cross Sectional Regressions
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

#plot of observations for a year and the regression line
plot(x = Fatalities1982$beertax,
     y = Fatalities1982$fatal_rate,
     xlab = "Beer Tax (in 1988 Dollars)",
     ylab = "Fatality Rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0,4.5),
     pch = 20,
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5)

plot(x = Fatalities1988$beertax,
     y = Fatalities1988$fatal_rate,
     xlab = "Beer Tax (in 1988 Dollars)",
     ylab = "Fatality Rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0,4.5),
     pch = 20,
     col = "steelblue")

abline(fatal1988_mod, lwd = 1.5)


#Need to fix this code so that it outputs to a directory
filename = paste("traffic")
figures <- file.path("C:/Users/carso/OneDrive/Documents/6_programming/r/r_econometrics/figures/",
                     paste(filename,".png",sep=""))
png(file = figures)
  plot(x = Fatalities1988$beertax,
       y = Fatalities1988$fatal_rate,
       xlab = "Beer Tax (in 1988 Dollars)",
       ylab = "Fatality Rate (fatalities per 10000)",
       main = mytitle,
       ylim = c(0,4.5),
       pch = 20,
       col = "steelblue")
  abline(fatal1988_mod, lwd = 1.5)
dev.off()  

#To Demean the Variables within the dataset
Fatalities_demeaned <- with(Fatalities, 
                            data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
                            beertax = beertax - ave(beertax, state)))

#Estimating a Fixed Effects Regression
summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))

fatal_fe_mod <- plm(fatal_rate ~ beertax,
                    data = Fatalities,
                    index = c("state","year"),
                    model = "within")

coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")

# discretize the minimum legal drinking age
Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandadory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

# the set of observations on all variables for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]

# estimate all seven models
fatalities_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fatalities_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fatalities_mod3 <- plm(fatal_rate ~ beertax + state + year,
                       index = c("state","year"),
                       model = "within",
                       effect = "twoways", 
                       data = Fatalities)

fatalities_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles,
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod6 <- plm(fatal_rate ~ beertax + year + drinkage 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)

# gather clustered standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(fatalities_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod7, type = "HC1"))))

# generate the table
stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3, 
          fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7, 
          digits = 3,
          header = FALSE,
          type = "latex", 
          se = rob_se,
          title = "Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))
