## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:

NH11 <- readRDS("NatHealth2011.rds")
labs <- attributes(NH11)$labels

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev
# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))

## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".

# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels

cbind(predDat, predict(hyp.out, type = "response",
                        se.fit = TRUE, interval="confidence",
                        newdata = predDat))

##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

install.packages("effects")
library(effects)
plot(allEffects(hyp.out))

install.packages("mice")
library("mice")

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

summary(NH11$r_maritl) #no NA's but 74 obs with unkown marital status - let's drop these since they represent
                       # a very small portion of data (i.e. >5%)
NH11_modified <- subset(NH11, r_maritl != "9 Unknown marital status")
summary(NH11_modified$age_p) #no NA's - good to go
summary(NH11_modified$everwrk) #18910 NA's (57% of data) - we can't fill in the missing values for this data
                               #since this is the response we're trying to predict. We need to limit our model to
                               #only the data for which we know thre response:

NH11_modified2 <- subset(NH11_modified, !is.na(everwrk)) 
#we still have a few instances where everwrk is not simply 
#yes or no, so let's limit our dataset to only instances of yes/no

NH11_modified3 <- subset(NH11_modified2, everwrk != "7 Refused")
NH11_modified4 <- subset(NH11_modified3, everwrk != "8 Not ascertained")
NH11_modified5 <- subset(NH11_modified3, everwrk != "9 Don't know")

#this portion was included in solutions but not quite sure why it's needed?
NH11_modified6 <- transform(NH11_modified5,
                            everwrk = factor(everwrk,
                                             levels = c("1 Yes", "2 No")),
                            r_maritl = droplevels(r_maritl))

summary(NH11_modified5)
summary(NH11_modified5$everwrk)
table(NH11_modified5$everwrk)

everworked <- glm(everwrk ~ age_p + r_maritl, data = NH11_modified6, family = binomial)
coef(summary(everworked))
summary(everworked)

plot(allEffects(everworked))


##   2. Predict the probability of working for each level of marital
##      status.

summary(NH11_modified5$r_maritl)
predDat2 <- with(NH11,
                expand.grid(r_maritl = c("1 Married - spouse in household","2 Married - spouse not in household", "4 Widowed", "5 Divorced", "6 Separated", "7 Never married", "8 Living with partner"),
                            age_p = mean(age_p, na.rm = TRUE)))

cbind(predDat2, predict(everworked, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat2))

#Results
#                             r_maritl  fit (probabilities)
#1     1 Married - spouse in household  0.13305723 
#2 2 Married - spouse not in household  0.13887208 
#3                           4 Widowed  0.23372328 
#4                          5 Divorced  0.06890182 
#5                         6 Separated  0.11890330 
#6                     7 Never married  0.17753453 
#7               8 Living with partner  0.08949485


##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
