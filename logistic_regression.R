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

NH11 <- readRDS("C:/Users/M/Desktop/R/7.1/Logistic Regression/logistic_regression/dataSets/NatHealth2011.rds")
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
install.packages("allEffects")
library(effects)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).


library("dplyr")
NH11sub <- select(NH11, everwrk, age_p, r_maritl)
str(NH11sub)
summary(NH11sub)
table(NH11$everwrk)
#everwrk has 18949 NAs, 14065 entries with data. Can't assume work status, so remove all NAs

NH11wrk <- filter(NH11sub, everwrk != "NA")
str(NH11wrk)
summary(NH11wrk)

#filter to only have yes no responses and drop levels

NH11biwrk1 <- filter(NH11sub, everwrk == "1 Yes")
NH11biwrk2 <- filter(NH11sub, everwrk == "2 No")
NH11biwrk <- bind_rows(NH11biwrk1, NH11biwrk2)
NH11biwrk <- droplevels(NH11biwrk)
table(NH11biwrk$everwrk)

#There are 12153 instances of everwrk, and 1887 of not#

baselineprediction = 12153/(12153+1887)

#~87% accuracy if assume always yes#

# Split into train and test sets

library("caTools")
split = sample.split(NH11biwrk$everwrk, SplitRatio = 0.75)
NH11biwrk.Train = subset (NH11biwrk, split == TRUE)
NH11biwrk.Test = subset(NH11biwrk, split == FALSE)

nrow(NH11biwrk.Train)
nrow(NH11biwrk.Test)

#NH11biwrk.Train can be used for modelling
model1 <- glm(everwrk ~ age_p + r_maritl, family = binomial, data = NH11biwrk.Train)
summary(model1)

predict.train = predict(model1, type="response")
tapply(predict.train, NH11biwrk.Train$everwrk, mean)

#Thresholding use t > 0.2#

table(NH11biwrk.Train$everwrk, predict.train>0.2)


#Test model with ROC Curve#

library("ROCR")

ROCRpred = prediction(predict.train, NH11biwrk.Train$everwrk)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
abline(a=0, b= 1)



##   2. Predict the probability of working for each level of marital
##      status.

#NH11biwrk can be used for modelling
model2 <- glm(everwrk ~ r_maritl, family = binomial, data = NH11biwrk)
summary (model2)
predict2 = predict(model2, type="response")
unique(predict2)





