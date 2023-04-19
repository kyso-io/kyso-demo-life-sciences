# Resources:
# Book on ZAP/ZANB
# "Mixed Effects Models and Extensions in Ecology with R" p 291
system("open https://books.google.com/books?id=vQUNprFZKHsC&pg=PA287&lpg=PA287&dq=zanb+r+models&source=bl&ots=kbsMxR1J_u&sig=eKhJZUprPHYSMr37DUDXy9PdWhk&hl=en&sa=X&ei=GjMkVaHXM4v9oATs3oFg&ved=0CC8Q6AEwBg#v=snippet&q=ZANB&f=false")

# Stata Hurdle models: offers pi
system("open http://www.stata-journal.com/sjpdf.html?articlenum=st0040")
# Cran-R pscl packages
system("open http://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf")
# Rstudio on count
system("open https://rpubs.com/kaz_yos/pscl-2")

##########################################
# Packages and directories
##########################################
setwd("~/Dropbox/stat637-generalized-lm/final-project")
library(MASS) # glm.nb
library(mgcv) # negbin option for glm
library(xtable) # xtable funciton
library(pscl) # zeroinfl and hurdle models
library(lmtest) # lmtest function
library(sqldf)
library(ggplot2)

# Wrapper function for xtable output:
xtabprint <- function(obj,tab.caption,cap.place,H = "ht",dig,outfile,labe = NULL){
  obj1 <- xtable(obj,caption =tab.caption,digits =dig,label = labe)
  print(obj1,
        file = outfile, caption.placement = cap.place,
        table.placement = H,
        include.rownames = T,backslash = T, 
        sanitize.colnames.function = identity,
        sanitize.text.function = identity,
        sanitize.rownames.function = identity,
        hline.after = c(0))
}

##########################################
# Data Preparation
##########################################
dat <- read.csv("bounce-working.csv")
dat <- dat[,-1]
dat$IL = as.factor(dat$IL)
dat$Income_Level = as.factor(dat$IL)

##########################################
# 3. Exploratory Analysis. Plots, tables, summaries
##########################################
### Tables
summary1 <- sqldf("select IL, avg(odfull) as Mean_OD, count(odfull) as N_OD
      from dat group by IL")
rownames(summary1) <- c("Low","Med","High")
colnames(summary1) <- c(1,"Mean","Households") 
summary1 <- summary1[,-1]
xtabprint(summary1,tab.caption = "Summary Statistics on Overdrafts",cap.place = "bottom","H",dig = 2,outfile = "table_summary1.txt",labe = "fig:summary1")

## Plot overdrafts stacked
qplot(x = odfull, group = Income_Level, fill = Income_Level, ylab = "Count", xlab = "Overdrafts",data = dat)

## Plot odbyinc
png("plot_odbyinc.png")
qplot(x = od, 
      facets = ~Income_Level, 
      fill = Income_Level,
      main = "Overdrafts by Income Level",
      ylab = "Count",
      xlab = "Overdraft",
      data = dat)
dev.off()


# Demonstration of zero-hurdle data:
png("plot_zerohurd.png")
npop2 = 3000
set.seed(11)
y1 <- rnegbin(npop2,mu = 5,theta = 1.5)
nn <- sum(as.numeric(y1==0))
y1[which(y1==0)] <- sample(c(1:10),nn,replace = T)
temp <- data.frame(y = c(y1,rep(0,800)), Population = c(rep("Count",npop2),rep("Binary",800)))
temp$Population <- relevel(temp$Population,ref = "Count")
qplot(y,group = Population,fill =Population, data = temp, binwidth = 1,
      ylab = "Count",xlab = "Overdrafts",
      main = "Simulated Negative Binomial Hurdle Data")
dev.off()

# Demonstration of zero-inflated data.
png("plot_zeroinf.png")
npop2 = 3000
set.seed(10)
temp <- data.frame(y = c(rnegbin(npop2,mu = 5,theta = 1.5),rep(0,400)), Population = c(rep("Count",npop2),rep("Binary",400)))
temp$Population = as.factor(temp$Population)
temp$Population = relevel(temp$Population, ref = "Count")
qplot(y,group = Population,fill =Population, data = temp,binwidth = 1,
      xlab = "Overdrafts", ylab = "Count",
      main = "Simulated Negative Binomial Zero-inflated Data")
dev.off()

##########################################
# 4. Technical Analysis. Fit model. Check diagnostics, outliers, convergence. Revise. Compare.
##########################################
temp <- data.frame(odfull = dat$odfull, IL = factor(dat$IL))
temp$IL <- relevel(temp$IL, ref = "1")
mod.nb1 <- glm.nb(odfull ~ IL,data = temp, link = "log")
temp$IL <- relevel(temp$IL, ref = "2")
mod.nb2 <- glm.nb(odfull ~ IL,data = temp, link = "log")
temp$IL <- relevel(temp$IL, ref = "3")
mod.nb3 <- glm.nb(odfull ~ IL,data = temp)
# The SE is the SE of the difference.
# Produce summary tables
sum1 <- round(summary(mod.nb1)$coefficients,8)
sum2 <- round(summary(mod.nb2)$coefficients,8)
sum3 <- round(summary(mod.nb3)$coefficients,8)

### Estimate the mean difference
co1 <- coef(mod.nb1)
log.mu1 <- co1[1]
log.mu2 <- co1[1] + co1[2]
log.mu3 <- co1[1] + co1[3]
# Test: model1 estimate for 3 with intercept of glm.nb3. Should be same.
# round(log.mu3,5) == round(coef(mod.nb3)[1],5) # TRUE
log.mu <- cbind(log.mu1,log.mu2,log.mu3)
mu <- exp(log.mu)

### Table: GLM coefficients
sum.tab <- rbind(sum1[1,],sum2[1,],sum3[1,])
sum.out1 <- cbind(sum.tab[,1],t(mu),sum.tab[,2:4],t(log.mu),t(mu))
rownames(sum.out1) <- c("IL1","IL2","IL3")
colnames(sum.out1) <- c("$\\hat \\beta_j$","exp($\\hat \\beta_j$)","SE($\\hat \\beta_j$)","z-val","$Pr(>z)$","$ln(\\mu)$","$\\mu$")
xtabprint(sum.out1,tab.caption = "GLM coefficient estimates for each income level as the reference group. Backed-out means are estimated.","bottom","ht",3,"RESULTS-glmcomp1.txt",labe = "fig:glmcomp1")

### Table: Tests of significance
sum.tab2 <- rbind(sum1[2,],sum1[3,],sum2[3,])
sum.out2 <- cbind(sum.tab2[,1],exp(sum.tab2[,1]),sum.tab2[,2:4])
rownames(sum.out2) <- c("IL2 v. IL1","IL3 v. IL1","IL3 v. IL2")
colnames(sum.out2) <- c("$\\hat \\beta_{diff} = \\hat \\beta_j - \\hat \\beta_k$","exp($\\hat \\beta_{diff}$)","SE($\\hat \\beta_{diff}$)","z-val","$Pr(>z)$")
# Write out the files.
xtabprint(sum.out2,"Three contrasts of IL2 v. IL1, IL3 v. IL1, and IL3 v. IL2. SE represents the standard error for the difference between the two income levels.","bottom","ht",3,"RESULTS-glmcomp2.txt",labe = "fig:glmcomp2")

###### Hurdle Models: Poisson and Negative Binomial #######
### To conduct test on whether hurdle is needed:
# Conduct tests
mod.hurdtest1 <- hurdle(odfull ~ IL, data = dat,dist= "poisson",zero.dist = "poisson",link = "log")
ht1 <- hurdletest(mod.hurdtest1)
mod.hurdtest2 <- hurdle(odfull ~ IL, data = dat,dist= "negbin",zero.dist = "negbin",link = "log")
ht2 <- hurdletest(mod.hurdtest2)
# Compile diagnostics:
tab.ht <- cbind(c(ht1$Df[2],ht2$Df[2]),
                c(ht1$Chisq[2],ht2$Chisq[2]),
                c(ht1$"Pr(>Chisq)"[2],ht2$"Pr(>Chisq)"[2])
)
colnames(tab.ht) <- c("Df","$X^2$","$p$-value")
rownames(tab.ht) <- c("Poisson","NegBin")
xtabprint(tab.ht,tab.caption = "Testing for the presence of a hurdle in both negative binomial and Poisson models.",cap.place = "bottom","ht",dig=3,outfile = "table_ht.txt",labe = "fig:table_ht")

### Fitting hurdle models
## Poisson
zipf1 <- hurdle(odfull ~ IL | 1, data = dat,
                     dist= "poisson",zero.dist = "binomial",link = "logit")
hP11 <- hurdle(odfull ~ 1, data = dat,
                  dist= "poisson",zero.dist = "binomial",link = "logit")
hPff <- hurdle(odfull ~ IL|IL, data = dat,
       dist= "poisson",zero.dist = "binomial",link = "logit")
hplr.f1.ff <- lrtest(zipf1,hPff) # Since there is no significant difference, the addition of hurdle coefficients does not significantly improve the model fit.
hplr.11.f1 <- lrtest(hP11,zipf1) # Coefficients significantly improve fit on count portion, and not zero portion.

## Negative Binomial
zanbf1 <- hurdle(odfull ~ IL|1, data = dat,
                     dist= "negbin",zero.dist = "binomial",link = "logit")
hNB11 <- hurdle(odfull ~ 1, data = dat,
                     dist= "negbin",zero.dist = "binomial",link = "logit")
hNBff <- hurdle(odfull ~ IL, data = dat,
                  dist= "negbin",zero.dist = "binomial",link = "logit")
(hnblr.f1.ff <- lrtest(zanbf1,hNBff)) # Models without the zero-coefficients aren't significantly better, therefore go with the simpler models.
(hnblr.11.f1 <- lrtest(hNB11,zanbf1)) # Coefficients significantly improve fit on count portion, not on binary portion.

# Compare best
lr.comp <- lrtest(zipf1,zanbf1)

# Combine results on LRTest comparisons
tab.lr <- rbind(c(hplr.f1.ff[2,3:5]),c(hplr.f1.11[2,3:5]),c(hnblr.f1.ff[2,3:5]),c(hnblr.11.f1[2,3:5]),c(lr.comp[2,3:5]))
rownames(tab.lr) <- c("ZAP F1 v. FF","ZAP 11 v. F1","ZANB F1 v. FF","ZANB 11 v. F1","ZAP F1 v. ZANB F1")
xtabprint(tab.lr,tab.caption = "Likelihood ratio tests of nested models.","bottom","ht",dig=3,outfile = "table_lrtest.txt",labe = "fig:lrtest")


### MODEL SELECTION: ZURR 291
## 2. Model Validation through Residual Plots: Analysis of pearson residuals to assess model fit. Drop any model with patternization. QQ plots.
res.poi <- residuals(mod.hp1,type = "pearson")
res.nb <- residuals(mod.hn1,type = "pearson")
res2 <- residuals(mod.hnfull)
yhat <- predict(mod.hp1,type = "response")

png("plot_residp.png")
par(mfrow = c(1,2))
plot(res.poi,ylim = c(0,35), main = "Pearson Residuals \n for Poisson Hurdle", ylab = "Pearon Resid")
qqnorm(res.poi,ylim = c(-5,35),main = "Poi Normal Q-Q Plot")
qqline(res.poi,col = "red",lty = 2,distribution = function(p) qpois(p,lambda = 10))
abline(b = 1,a = 0,col = "red")
legend("topleft", col = "red",lty = c(2,1), legend = c("qqline","y=x"),bty = "n")
dev.off()

png("plot_residnb.png")
par(mfrow = c(1,2))
plot(res.nb,ylim = c(0,35), main = "Pearson Residuals \n for NB Hurdle",ylab = "Pearson Resid")
qqnorm(res.nb,ylim = c(-5,35),main = "NB Normal Q-Q Plot")
qqline(res.nb,col = "red",lty = 2)
abline(b = 1,a = 0,col = "red")
legend("topleft", col = "red",lty = c(2,1), legend = c("qqline","y=x"),bty = "n")
dev.off()

## 3. Information Criterion  AIC
AIC(mod.hp1,mod.hn1) # Go with NB model

## 4. Likelihood ratio tests. We can compare ZIP to ZINB and ZAP to ZANB with LRT since Poisson and Negative binomial are nested:
# LR Test:
lrtest(mod.hp1,mod.hn1) # Go with NB model

## 5. Compare Observed and Fitted Values: p. 292 of Mixed Effects Models and Extensions in Ecology with R
# An estimated intercept of 0 and a slope of 1 indicates a perfect fit.
dat$fit.nb <- fitted(mod.hn1)
dat$fit.poi <- fitted(mod.hp1)
fit.modnb <- lm(odfull ~ fit.nb, data = dat)
fit.modp <- lm(odfull ~ fit.poi, data = dat)
summary(fit.modp);confint(fit.modp) # Intercept is not significantly different from 0, and slope is not significantly different from 1. Therefore, fit is good.
summary(fit.modnb); confint(fit.modnb) # Fit is also good, nearly identical.

plot(dat$odfull, dat$fit.nb, main = "Fitted v. Observed NB")
abline(fit.modnb)

# Compare RMSE and mean absolute error
RMSE.poi <- sqrt(mean((dat$fit.poi - dat$odfull)^2))
RMSE.nb <- sqrt(mean((dat$fit.nb - dat$odfull)^2))



### Final Model Coefficients
tab.hurdco <- cbind(summary(mod.hp1)$coefficients$count,
summary(mod.hn1)$coefficients$count[1:3,])
xtabprint(tab.hurdco,"Coefficient Estimates for Poisson and negative binomial hurdle models.",cap.place = "bottom","ht",dig = 3,outfile = "table_tabhurdco.txt",labe = "fig:tabhurdco")

tab.hurdnb <- rbind(summary(mod.hn1)$coefficients$count,summary(mod.hn1)$coefficients$zero)
rownames(tab.hurdnb) <- c("$\\beta_0$","$\\beta_1 (IL_2)$","$\\beta_2 (IL_3)$","$log(\\theta)$","$\\gamma_0$")
xtabprint(tab.hurdnb,"Coefficient estimates for negative binomial hurdle model.",cap.place = "bottom","ht",dig =3, outfile = "table_tabhurdnb.txt",labe="fig:tabhurdnb")




###### Zero-inflated Negative Binomial #######
#system("open http://www.ats.ucla.edu/stat/r/dae/zinbreg.htm")
# For modeling the unobserved state (zero vs. count), a binary model is used that captures the probability of zero inflation. in the simplest case only with an intercept but potentially containing regressors. For this zero-inflation model, a binomial model with different links can be used, typically logit or probit.

zeroinfl(odfull ~ IL, dist = "negbin",link = "log", data = temp)
# A simple inflation model where all zero counts have the same probability of belonging to the zero component can by specified by the formula y ~ x1 + x2 | 1. 
# No regressors for zero inflation
zeroinfl(odfull ~ IL|1, dist = "negbin",link = "log", data = temp)
# An inflation modle where zero counts have differing probabilities of belonging to the zero component.
zinb1 <- zeroinfl(odfull ~ IL, dist = "negbin",link = "log", data = temp)
summary(zinb1)

zeroinfl(odfull ~ IL |1, dist = "negbin",data = dat)

# alphas correspond to log[p_i/(1-p_i)] = x_i'alpha_i.
# betas correspond to log[µ_i] = x_i'alp_i
zeroinfl(odfull ~ IL, dist = "negbin",link = "log", data = temp)
temp$IL <- relevel(temp$IL, ref = "2")
hurdle(odfull ~ IL, data = temp, dist = "negbin")

ll <- function(x){exp(x)/(1+exp(x))}
zero1 <- summary(zinb1)$coefficients$zero


zPff <- zeroinfl(odfull ~ IL|IL, dist = "poisson",link = "log",data = dat)
zPf1 <- zeroinfl(odfull ~ IL|1, dist = "poisson",link = "log",data = dat)
zP11 <- zeroinfl(odfull ~ 1|1, dist = "poisson",link = "log",data = dat)

zNBff <- zeroinfl(odfull ~ IL|IL, dist = "negbin",link = "log",data = dat)
zinbf1 <- zeroinfl(odfull ~ IL|1, dist = "negbin",link = "log",data = dat)
zNB11 <- zeroinfl(odfull ~ 1|1, dist = "negbin",link = "log",data = dat)

### ZIP/ ZINB LRTs
lrz1 <- lrtest(zPf1,zPff) #not sig diff
lrz2 <- lrtest(zP11,zPff) # sig diff: ff better
lrz3 <- lrtest(zP11,zPf1) # sig diff: f1 is better
lrz4 <- lrtest(zinbf1,zNBff) # no signif diff
lrz5 <- lrtest(zNB11,zNBff) # slight sig diff: ff is better
lrz6 <- lrtest(zNB11,zinbf1) # sligh sig diff: f1 is better

lrz7 <- lrtest(zPf1,zinbf1) # sig diff: zinbf1 is better

tab.lrz <- rbind(c(lrz1[2,3:5]),c(lrz3[2,3:5]),c(lrz4[2,3:5]),c(lrz6[2,3:5]),c(lrz7[2,3:5]))
rownames(tab.lrz) <- c("ZIP F1 v. FF","ZIP 11 v. F1","ZINB F1 v. FF","ZINB 11 v. F1","ZIP F1 v. ZINB F1")
xtabprint(tab.lrz,tab.caption = "Likelihood ratio tests of ZIP/ZINB nested models.","bottom","ht",dig=3,outfile = "table_lrtestZ.txt",labe = "fig:lrtestZ")

###### Compare ZIP to ZINB
# 2: Comapre residuals
res.zip <- residuals(zipf1,type = "pearson")
res.zinb <- residuals(zinbf1, type = "pearson")
qqnorm(res.zip)
qqline(res.zip)

qqnorm(res.zinb)
qqline(res.zinb)

# 3. Compare AIC
)




######### Comparing best models
zanbf1 <- hurdle(odfull ~ IL|1, data = dat, dist = "negbin")
zipf1 <- hurdle(odfull ~ IL|1, data = dat, dist = "poisson")

# 3. AIC
AIC(zanbf1,zinbf1)
# 4. Can't do. Not same model

# 5. Observed v. fitted
dat$fit.zanbf1 <- fitted(zanbf1)
dat$fit.zinbf1 <- fitted(zinbf1)
par(mfrow = c(1,2))
plot(dat$fit.zanbf1,dat$odfull, main = "Hurdle NB Observed v. Fitted", xlab = "Fitted", ylab = "Observed")
plot(dat$fit.zinbf1,dat$odfull, main = "ZINB Observed v. Fitted", xlab = "Fitted", ylab = "Observed")
dev.off()

# 6. Compare all the coefficients, compare MSE
glmP <- glm(odfull ~ IL, data = dat, family = "poisson")
glmQP <- glm(odfull ~ IL, data = dat, family = quasipoisson())
glmNB <- glm.nb(odfull ~ IL, data = dat)

dat$fit.glmP <- predict(glmP,type = "response")
dat$fit.QP <- predict(glmQP,type = "response")
dat$fit.glmNB <- predict(glmNB,type = "response")

#
table(predict(zinbf1,type = "count"))
table(predict(zinbf1,type = "response"))
table(predict(zinbf1,type = "zero"))
pp <- table(predict(zinbf1,type = "prob"))
cbind(pp,dat$IL)

# Calculate MSE
digi <- 3
MSE.glmP <- round(mean((dat$fit.glmP - dat$odfull)^2),digi)
MSE.glmQP <- mean((dat$fit.QP - dat$odfull)^2)
MSE.glmNB <- mean((dat$fit.glmNB - dat$odfull)^2)
MSE.zanbf1 <- mean((dat$fit.zanbf1 - dat$odfull)^2)
MSE.zinbf1 <- mean((dat$fit.zinbf1 - dat$odfull)^2)
MSE.out <- c(MSE.glmP,"",MSE.glmQP,"",MSE.glmNB,"",MSE.zanbf1,"",MSE.zinbf1,"")

# Coefficient tables
s.glmP <- summary(glmP)$coefficients
s.glmQP <- summary(glmQP)$coefficients
s.glmNB <- summary(glmNB)$coefficients
s.zanbf1 <- summary(zanbf1)$coefficients$count[1:3,]
s.zinbf1 <- summary(zinbf1)$coefficients$count[1:3,]
s.out <- round(cbind(s.glmP[,1:2],s.glmQP[,1:2],s.glmNB[,1:2],s.zanbf1[,1:2],s.zinbf1[,1:2]),digi)

sz.zanbf1 <- summary(zanbf1)$coefficients$zero
sz.zinbf1 <- summary(zinbf1)$coefficients$zero
sz.out <- round(c(rep(NA,6),sz.zanbf1[,1:2],sz.zinbf1[,1:2]),digi)

# AIC of models
AIC.glmP <- round(AIC(glmP),digi)
AIC.glmQP <- round(AIC(glmQP),digi)
AIC.glmNB <- round(AIC(glmNB),digi)
AIC.zanbf1 <- round(AIC(zanbf1),digi)
AIC.zinbf1 <- round(AIC(zinbf1),digi)
AIC.out <- c(AIC.glmP,"","","",AIC.glmNB,"",AIC.zanbf1,"",AIC.zinbf1,"")

# BIC
n <- nrow(dat)
# AIC(glmP,k=log(n));BIC(glmP) # Check, these are the same!
BIC.glmP <- round(BIC(glmP),digi)
BIC.glmQP <- round(BIC(glmQP),digi)
BIC.glmNB <- round(BIC(glmNB),digi)
BIC.zanbf1 <- round(AIC(zanbf1,k = log(n)),digi)
BIC.zinbf1 <- round(AIC(zinbf1,k = log(n)),digi)
BIC.out <- c(BIC.glmP,"","","",BIC.glmNB,"",BIC.zanbf1,"",BIC.zinbf1,"")

logLik.glmP <- round(logLik(glmP),digi)
logLik.glmQP <- round(logLik(glmQP),digi)
logLik.glmNB <- round(logLik(glmNB),digi)
logLik.zanbf1 <- round(logLik(zanbf1),digi)
logLik.zinbf1 <- round(logLik(zinbf1),digi)
logLik.out <- c(logLik.glmP,"","","",logLik.glmNB,"",logLik.zanbf1,"",logLik.zinbf1,"")

# Get theta estimates for different NB models
theta.glmNB <- c(glmNB$theta,glmNB$SE.theta)
theta.zanbf1 <- exp(summary(zanbf1)$coefficients$count[4,1:2]) # Not significant
theta.zinbf1 <- exp(summary(zinbf1)$coefficients$count[4,1:2])
theta.out <- c("","","","",round(theta.glmNB,digi),"$8.90e-06$","$3.09e+11$",round(theta.zinbf1,digi))

# Combining things
fivemods <- rbind(s.out,sz.out,logLik.out,AIC.out,BIC.out,theta.out)
rownames(fivemods) <- c("$\\beta_0 (IL_1)$","$\\beta_1 (IL_2)$","$\\beta_2 (IL_3)$","$\\gamma_0$","LogLik","AIC","BIC","$\\hat \\theta$")
colnames(fivemods) <- rep(c("$\\hat \\beta$","SE($\\hat \\beta$)"),5)


xtabprint(fivemods,tab.caption ="Comparison of five models.",cap.place = "bottom","ht",dig=4,outfile="table_fivemods.txt",labe="fig:fivemods")
# \begin{tabular}{rllllll | llll}
# &\multicolumn{2}{c}{glm.P} & \multicolumn{2}{c}{glm.QP}& \multicolumn{2}{c}{glm.NB}& \multicolumn{2}{c}{zanbf1} & \multicolumn{2}{c}{zinbf1}\\


#### Compare the means for the different models
#### Final model coefficients:
sum.zanbf1 <- rbind(summary(zanbf1)$coefficients$count,
      summary(zanbf1)$coefficients$zero)
rownames(sum.zanbf1) <- c("$\\beta_0 (IL_1)$","$\\beta_1 (IL_2)$","$\\beta_2 (IL_3)$","log($\\theta$))","$\\gamma_0$")
xtabprint(sum.zanbf1,tab.caption = "Summary for NB hurdle model.","bottom","ht",dig = 4,outfile = "table_sumzanbf1.txt",labe="fig:sumzanbf1")


# Explore the model as a funciton of income
dat$inc2 <- dat$income/1000
hurdle(odfull ~ inc2|1,data = dat,dist = "negbin")
m1 <- glm.nb(odfull ~ inc2,data = dat) # increase on the log mean number of overdrafts
yhat1 <- predict(m1,type = "response")
phat1 <- predict(m1,type = "prob")
plot(dat$odfull, predict(m1,type = "response"))
plot(p1, dat$income)


### Means of five models
m4 <- unique(predict(zanbf1,data.frame(IL = c("1","2","3"))))
m5 <- unique(predict(zinbf1,data.frame(IL = c("1","2","3"))))
m3 <- unique(predict(glmNB,data.frame(IL = c("1","2","3")),type = "response"))
m1 <- unique(predict(glmP,data.frame(IL = c("1","2","3")),type = "response"))
m2 <- unique(predict(glmNB,data.frame(IL = c("1","2","3")),type = "response"))

tabx <- cbind(m1,m2,m3,m4,m5)
rownames(tabx) <- c("IL1","IL2","IL3")
colnames(tabx) <- c("GLM.P","GLM.QP","GLM.NB","ZANBF1","ZINBF1") 
xtabprint(tabx,"Predicted average number of overdrafts for each income level, by model","bottom","ht",dig = 3,outfile = "table_means.txt",labe="fig:means")

##########################################
# 5. Results. Tables, plots, inference, CIs.
##########################################
# Coefficients
png("plot_propinc.png")
dat$propinc <- (dat$odfull*25)/dat$income
qplot(y = income, x = propinc, group = IL, col = IL,
      main = "Income v. Proportion of Income \n Spent on Overdraft Expenses 2012",
      ylab = "Income",
      xlab = "Proportion of Income",
      data = dat[which(dat$income<100000),])
dev.off()


