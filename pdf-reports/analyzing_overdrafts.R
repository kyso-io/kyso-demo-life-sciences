####################################################
# Working Directories and libraries  ####
####################################################
rm(list = ls())
writeup = "~/Dropbox/stat-masters-project/project-writeup/"
project = "~/Dropbox/stat-masters-project"
setwd(paste(writeup))
setwd(paste(project))
library(sqldf)
library(xtable)
library(ggplot2)
library(MASS)
# Notation:
# t = truncates off the 20k.
# t_ = truncates off the bottom 20k plus is a zero-truncated.

####################################################
# FUNCTIONS:
####################################################
# Function for finding (a,b) parameters in the (a,b,0) class
# k = whatever value of k claims you have where you have data on 1 less and 1 more
# a > 0 means Negative Binomial
# a = 0 means Poisson
# a < 0 means Binomial
ab.values <- function (k, Pk.minus.1, Pk, Pk.plus.1) {
  b = -k*(k+1)*(Pk.plus.1/Pk - Pk/Pk.minus.1)
  a = Pk / Pk.minus.1 - b / k # Also: Pk.plus.1*(k+1)/Pk - k*Pk/Pk.minus.1
  out <- matrix(c(a,b),ncol = 2)
  colnames(out) <- c("a","b")
  out
}
ab.values(1,0.735461802,0.072976055,0.046750285)


# Expected value of Number of claims given a,b
exN <- function(a,b){ (a + b)/(1-a) }
varN <- function(a,b){ (a + b)/(1-a)^2 }

# Create functions to calculate parameters given a,b
poi.ab <- function(a,b){ # a = 0 means Poisson
  a = 0
  lambda = b
  p0 = dpois(0,lambda)
  out <- matrix(c(lambda,p0),ncol = 2)
  colnames(out) <- c("lam","p0")
  out
}
bin.ab <- function(a,b){ # a < 0 means Binomial
  q <- a/(a-1)
  m <- -(1 + b/a)
  p0 <- (1-q)^m
  #If m is given:
  #p0 <- (1-q)^m
  out <- matrix(c(m,q,p0),ncol = 3)
  colnames(out) <- c("m","q","p0")
  out
}
nbin.ab <- function(a,b){ # a > 0 means Negative Binomial
    beta <- a / (1-a)
    r = 1+b/a
    p0 = (1/(1+beta))^r
    out <- matrix(c(beta,r,p0),ncol = 3)
    colnames(out) <- c("beta","r","p0")
    out
}

# Calculate the a,b values for the available pairs on any matrix
# Matrix must be organized n x p where n = k values and p
ab.mat <- function(mat,group){
  p = group
  out <- matrix(NA,ncol = 3 ,nrow = (nrow(mat)-2))
  for(i in 2:(nrow(mat)-1)){
    k = i-1
    out[k,c(2:3)] <- ab.values(k,Pk.minus.1 = mat[i-1,p],
                               Pk =  mat[i,p],
                               Pk.plus.1 = mat[(i+1),p])
    out[k,1] <- k
  }
  colnames(out) <- c("k","a","b")
  out
}

# Function to calculate k*n_k/n_{k-1} ratios.
ratio.vals <- function(tab,col){
  n <- nrow(tab)
  nkmin1 <- tab[,col][1:(n-1)] # Get the n_{k-1} values. k = 0:14
  nk <- tab[,col][2:n] # Get the n_k values. k = 1:15
  k = 1:(n-1) # n-1 is 14. We only have kvalues 1:14
  out <- as.data.frame(cbind(nkmin1,nk,k,ratios = k*nk/nkmin1))
  c(0,out$ratios) # To be plotted of the ratio kn_k/n_k-1 against k
}

# My own pdf
dnegbin <- function(n,b,r){
  choose(n+r-1,n)*(1/(1+b))^r*(b/(1+b))^n
}
dnegbin(1,b = .2,r = 1) # n = number of successes
# r = size, prob = beta/1+beta
dnbinom(1,size = 2,prob = .2/(1+.2))

# dnbinom: p^n(1-p)^r; 
# n = size = (# of successful trials); 
# r = number of failures;
p <- beta.ab / (1+beta.ab)
dnbinom(0:14,size = ,prob = p)
dnbinom(1,size = 1,prob = .2/(1+.2))

####################################################
# setting up the bank overdraft data.  ####
####################################################
# Read in "bounce" (overdraft) data and provide new, shorter names
dat <- as.data.frame(read.csv("bounce.csv",sep = ",", as.is = T))
dat <-as.data.frame(dat)
names(dat) <- c("id","name","od13","od12","cred13","cred12","avgbal13","address","city","state","zip")

# Keep only the data that's of use
dropped.cols <- c(1,2,7,9,10,11)
dat <- dat[,-dropped.cols]

# Count the additional columns
dat$od_cnt12<-as.numeric(dat$od12)/25
dat$od_cnt13<-as.numeric(dat$od13)/25

# Bin the data by Income Level and OD level
dat <- sqldf("select od12,od13,od_cnt12, od_cnt13, cred12, cred13, address from dat group by address")
dat<- sqldf("select *, 
            case 
              when cred12 between -10000 and 40000 then 1
              when cred12 between 40000 and 60000 then 2
              else 3
            end as inc_level12, 
            case 
              when cred13 between -10000 and 40000 then 1
              when cred13 between 40000 and 60000 then 2
              else 3
            end as inc_level13, 
            case
              when od_cnt12 = 0 then 0
              when od_cnt12 between 1 and 5 then 1
              when od_cnt12 between 6 and 10 then 2
              when od_cnt12 between 11 and 20 then 3
              when od_cnt12 >20 then 4
            end as od_level12,
            case
              when od_cnt13 = 0 then 0
              when od_cnt13 between 1 and 5 then 1
              when od_cnt13 between 6 and 10 then 2
              when od_cnt13 between 11 and 20 then 3
              when od_cnt13 >20 then 4
            end as od_level13,
            case 
              when od_cnt12 < 15 then od_cnt12 
              when od_cnt12 > 15 then 15
            end as od_cnt12clean,
            case 
              when od_cnt13 < 15 then od_cnt12 
              when od_cnt13 > 15 then 15
            end as od_cnt13clean
            from dat
            ")

####################################################
# Constructing data sets  ####
####################################################
# Keep original data regular as dat1
dat1 <- dat

# Truncate income level at 20k
dat20k <- dat[which(dat$cred12>20000),]

# Get just the 2012 data
dat12 <- dat[,c(12,8)] #keeps just income level, and od count < 15 ("od_cnt12clean")
dat12t <- dat20k[,c(12,8)]

# Data sets of just the individual counts, separated by income level
od1 <- dat12$od_cnt12clean[which(dat12$inc_level12==1)]
od2 <- dat12$od_cnt12clean[which(dat12$inc_level12==2)]
od3 <- dat12$od_cnt12clean[which(dat12$inc_level12==3)]

# Zero-truncated data sets
datD1 <- od1[which(od1>0)]-1
datD2 <- od2[which(od2>0)]-1
datD3 <- od3[which(od3>0)]-1

# Data Sets for gglplot
ggtemp <- dat20k
ggtemp$Income <- ggtemp$inc_level12
ggtemp$Income[which(ggtemp$Income == 1)] <- "Low"
ggtemp$Income[which(ggtemp$Income == 2)] <- "Med"
ggtemp$Income[which(ggtemp$Income == 3)] <- "High"
ggtemp$Income <- factor(ggtemp$Income)
ggtemp1 <- ggtemp[which(ggtemp$cred12<100000),]
ggtemp2 <- ggtemp[which(ggtemp$cred12>100000),]

####################################################
# 5-14-14 report tables for 2012 bank analysis  ####
####################################################
# Table 2 in 5-14-14 analysis, binned
table12 = with(dat, table(inc_level12,od_level12))
# Table 3 in 5-14-14 analysis, binned, proportions
# prop.table(table12, margin = 1)
# Table 4 in 5-14-14 analysis, binned, truncated @ 20k
table12t = with(dat20k, table(inc_level12,od_level12))
# Table 5 in 5-14-14 analysis, binned, truncated @ 20k, proportions
# prop.table(table12t, margin = 1)

####################################################
# working data tables used in this analysis####
####################################################
# count tables
t12 <- table(dat12)
t12t <- table(dat12t)

# Pr(x = k) tables for 2012, by income level
prop12 <- prop.table(table(dat12),margin = 2)
prop12t <- prop.table(table(dat12t),margin = 2)

# Pr(x = k) tables for 2012, by income level, truncating the 0 values out.
prop12_ <- prop.table(table(dat12)[2:16,],margin = 2) # Wrong way
prop12t_ <- prop.table(table(dat12t)[2:16,],margin = 2)

# Rename to make things simpler: 
tabD <- t12t #tabD for data
pD <- prop12t # freq table for data
tabT <- table(dat12t)[2:16,] # table truncated
rownames(tabT) <- 0:14
pT <- prop.table(tabT,margin = 2) # zero-truncated probabilities
rownames(pT) <- 0:14


####################################################
# Exploration: Descriptive TABLES used in thesis  ####
####################################################
##
# Table 1: A small table of unbinned data
# Table 2: Table 12: A table of binned data by overdrafts (not truncated)
# Table 3: A table of summary statistics on individuals
# Table 4: A table of overdraft money.
# Table 5: The table12t overdraft table produced, with totals.
##
setwd(writeup)

### Table 1: OD by count
out1 <- t(t12)[,1:8]
obj <- xtable(out1,caption ="Unbinned data.")
print(obj,file = "tab_unbinned.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

### Table 2: OD Binnings
out2 <- table12
colnames(out2) <- c("0","1-5","6-10","11-20",">20")
obj <- xtable(out2,caption ="Binned data.")
print(obj,file = "tab_binned.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

### Table 3: Summary Statistics Table
out3 <- sqldf("select inc_level12 as il12, 
        count(od_cnt12clean) as N,
        --min(od_cnt12clean) as min, 
        --median(od_cnt12clean) as med, 
        --max(od_cnt12clean) as max, 
        avg(od_cnt12clean) as meanod, 
        stdev(od_cnt12clean) as std,
        avg(cred12) as avginc,
        stdev(cred12) as sdinc
      from dat20k group by il12")
out3 <- out3[,2:ncol(out3)]
colnames(out3) <- c("N","Mean","SD","Mean","SD" )
rownames(out3) <- c("$IL_1$ (\\$20-40K)","$IL_2$ (\\$40-60K)","$IL_3$ (\\$60K+)")
obj <- xtable(out3,caption ="Summary Statistics by Income Level",label = "fig:summary")
print(obj,file = "summary.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)
#&\multicolumn{5}{|c}{Overdrafts} & \multicolumn{2}{|c}{Yearly Deposits} \\ \hline

### Table 4: MONEY SPENT
# How much money was spent?
all <- sum(temp1$od_cnt12)*25
money.low <- sum(temp1$od_cnt12[which(temp1$Income == "Low")])*25
money.mid <-sum(temp1$od_cnt12[which(temp1$Income == "Med")])*25
money.high <- sum(temp1$od_cnt12[which(temp1$Income == "High")])*25
money.tab <- rbind(money.low,money.mid,money.high)
money.props <- round(money.tab/all,2)
money.reps <- cbind(table(temp1$Income),round(prop.table(table(temp1$Income)),2))
money.tab <- cbind(money.tab,money.props,money.reps)
money.tab <- rbind(money.tab,colSums(money.tab))
rownames(money.tab) <- c("IL1","IL2","IL3","Total")
colnames(money.tab) <- c("Rev.","Proportion Rev.","Num. Ppl","Prop.Ppl")
obj <- xtable(money.tab,caption ="Statistics on overdraft revenue broken up by income level. Shows further the proportion of revenue that came from each income level and the number and proportion of clients in each income level.",label = "fig:summary")
print(obj,file = "money.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

### Table 5: Table 12 truncated
out3 <- rbind(table12t,colSums(table12t))
colnames(out3) <- c("0","1-5","6-10","11-20",">20")
rownames(out3) <- c("IL1","IL2","IL3","Total")
obj <- xtable(out3,caption ="Overdrafts binned by number of overdrafts with all income observations less than $20k removed.",digits = 0)
print(obj,file = "RESULTS-table-binned.txt",table.placement = "H",caption.placement = "bottom",include.rownames = T,backslash = T,
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

### Table 6: Table 12 truncated as proportions
out4 <- prop.table(table12t,margin = 1)
colnames(out4) <- c("0","1-5","6-10","11-20",">20")
rownames(out4) <- c("IL1","IL2","IL3")
obj <- xtable(out4,caption ="Overdraft level proportions.",digits = 3)
print(obj,file = "RESULTS-table-binnedp.txt",table.placement = "H",caption.placement = "bottom",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

####################################################
# Exploration: Descriptive PLOTS used in Project
####################################################
setwd("~/Dropbox/stat-masters-project/project-writeup/")
png("sect3-credod.png")
qplot(x = od_cnt12, xlab = "Number of Overdrafts",
      cex.lab = 10,
      y = cred12/1000, ylab = "Income (in 1000s)",
      main = "Overdrafts by Income Level",
      col = Income,
      data = ggtemp1
      )
dev.off()
setwd("~/Dropbox/stat-masters-project")

dev.off()
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "light grey")
points(x = temp1$od_cnt12, y = temp1$cred12/1000,pch = 19,cex = .5,
       col = (as.numeric(temp1$Income) + 1),
       xlab = "Number of Overdrafts",
)
plot(x = temp1$od_cnt12, y = temp1$cred12/1000,pch = 19,cex = .5,
     col = (as.numeric(temp1$Income) + 1),
     xlab = "Number of Overdrafts",
     ylab = "Income (in $1000's)",
     main = "Number of Overdrafts by Income",
     cex.lab = 1.3,
)
dev.off()

setwd("~/Dropbox/stat-masters-project/project-writeup/")
png("sect3-credod2.png")
qplot(x = od_cnt12, xlab = "Number of Overdrafts",
      y = cred12, ylab = "Yearly Deposits",
      main = "Overdrafts for Individuals with +$100k",
      legend = F,
      #geom = "bar",
      colour = "blue",
      data = temp2
)
dev.off()
setwd("~/Dropbox/stat-masters-project")


## Perhaps we could model the proportion of their income spent on ODs? 1%, 2%, 3%, etc?
####################################################
# (a,b) Estimation ####
####################################################
#### Estimate (a,b,0) ####
# Option 1: Using regular data.
# Option 2: Using truncated data.
# Option 3: Using weighted LS with truncated data.

### Option 1: FAIL. a > 1
### All "possible" a,b values.
ab1 <- ab.mat(tabD,1)
# Calculate the parameters of the distributions for each a,b pair
p1 <- poi.ab(a = ab1[,2],b = ab1[,3])
n1 <- nbin.ab(a = ab1[,2],b = ab1[,3])
b1 <- bin.ab(a = ab1[,2],b = ab1[,3])
# Create a matrix
ab.out <- round(cbind(ab1,p1,n1,b1),2)
# Create a matrix
obj <- xtable(ab.out,caption ="Individual (a,b) values for low-income, unmodified data. Values for $a$ greater than 1 create imposssible estimations for parameters of interest and require our analysis to use an (a,b,1) method.",digits = 2)
print(obj,file = "RESULTS-abvals.txt",table.placement = "H",caption.placement = "bottom",include.rownames = F,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

# Fit ratios and LM object.
ratiosD1 <- ratio.vals(tabD,1)[-16]
xD = 0:14
plot(xD,ratiosD1,type = "o",lty = 2,main = "Income level 1, Original Data")
# Fit the model to estimate (a,b)
modD <- lm(ratios1 ~ xD)
abline(modD)
summary(modD) # FAIL. a > 1

#### Option 2: zero-truncated data still a >1
# Ratios for truncated data, income level 1
ratiosT1 <- ratio.vals(tabT,1)[-15]
xT <- 0:(length(ratiosT1)-1)
plot(xT,ratiosT1,type = "o",lty = 2, ylim = c(-10,30), main = "Unweighted LS on Ratios for IL1")
abline(h = 0)
# Fit the lm.
modT1<- lm(ratiosT1 ~ xT)
abline(modT1)
# Assess outliers - outside 95% CI
xvals <- seq(from = 0, to = 15, length = 1000)
newdat <- data.frame(xT = xvals)
cis <- predict(modT1,newdat,interval = "prediction")
lines(x = xvals, y = cis[,2], col = "blue",lty = 2)
lines(x = xvals, y = cis[,3], col = "blue",lty = 2)

# Repeat for level 2 and 3
ratiosT2 <- ratio.vals(tabT,2)[-15]
ratiosT3 <- ratio.vals(tabT,3)[-15]
(modT2 <- lm(ratiosT2 ~ xT))
(modT3 <- lm(ratiosT3 ~ xT))

# Ouput table:
abT1 <- coef(modT1)
abT2 <- coef(modT2)
abT3 <- coef(modT3)
abT <- rbind(abT1,abT2,abT3)
abT <- cbind(abT[,2],abT[,1])
rownames(abT) <- c("IL1","IL2","IL3")
colnames(abT) <- c("a","b")

### Option 3: Weighted LS: SUCCESS, with some adjustment
weight0 <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
weight1 <- c(1,1,1,1,1,1,1,1,1,1,1,1,0,1) # Drops just the outlier
weight83 <- c(1,1,1,1,1,1,1,1,1,1,1,1,.45,1)
weight2 <- 1/(1:14)
modTw1 <- lm(ratiosT1 ~ xT, weights = weight83)
modTw1
# Plot the data
png("RESULTS-plot1slope.png")
plot(xT,ratiosT1,type = "o",lty = 2, ylim = c(-10,30), main = "Ratios for IL1 with 95% CI",
     xlab = "n",ylab = expression(np[n]/p[(n-1)])
     )
#95\% for un-weighted model
abline(modT,col = "red")
cis <- predict(lm(ratiosT1 ~ xT, weights = weight0),
               newdat,interval = "prediction")
lines(x = xvals, y = cis[,2], col = "red",lty = 2)
lines(x = xvals, y = cis[,3], col = "red",lty = 2)
# 95% CI for weighted model
abline(modTw1, col = "blue")
cis <- predict(lm(ratiosT1 ~ xT, weights =weight1),
               newdat,interval = "prediction")
lines(x = xvals, y = cis[,2], col = "blue",lty = 2)
lines(x = xvals, y = cis[,3], col = "blue",lty = 2)
legend("topleft",lty = c(1,2,1,2),col = c("red","red","blue","blue"),
       legend = c("OLS","95% CI","WLS","95% CI"),bty = "n")
dev.off()

# EXPLORATION Use WLS to find outliers
# rat <- ratiosT3
# mod <- lm(rat~xT)
# plot(xT,rat,type = "o")
# abline(mod)
# cis <- predict((mod2<<-lm(rat ~ xT, weights =c(1,1,1,1,1,1,1,1,0,0,0,0,0,0))),
#                newdat,interval = "prediction")
# abline(mod2,col = "blue")
# lines(x = xvals, y = cis[,2], col = "blue",lty = 2)
# lines(x = xvals, y = cis[,3], col = "blue",lty = 2)
# summary(mod2)
# End EXPLORATION

# Form WLS models for IL 2 and 3.
modTw2 <- lm(ratiosT2 ~ xT, weights =c(1,1,1,1,1,1,1,1,1,1,1,1,1,0))
modTw3 <- lm(ratiosT3 ~ xT, weights =c(1,1,1,1,1,1,1,1,1,0,1,1,0,1))

# Compile (a,b) truncated and weighted estimates
abTw1 <- coef(modTw1)
abTw2 <- coef(modTw2)
abTw3 <- coef(modTw3)
abTw <- rbind(abTw1,abTw2,abTw3)
abTw <- cbind(abTw[,2],abTw[,1])

# Compare models:
AIC(modT);AIC(modTw1)
AIC(modT2);AIC(modTw2)
AIC(modT3);AIC(modTw3)

# Final Plot and models:
png("RESULTS-plot3slopes.png")
plot(xT,ratiosT1,type = "o",lty = 2, ylim = c(-5,30),
     main = "Ratios and Selected Models For Income Levels",
     xlab = "n",ylab = expression(np[n]/p[(n-1)]))
lines(xT,ratiosT2,type = "o",lty = 2, col = 2)
lines(xT,ratiosT3,type = "o",lty = 2,col = 3)
abline(h = 0)
abline(modTw1)
abline(modTw2,col = 2)
abline(modTw3, col = 3)
legend("topleft",
       legend =  c("IL-1 Ratios","IL-1 Fit","IL-2 Ratios","IL-2 Fit","IL-3 Ratios","IL-3 Fit"), 
       col = c(1,1,2,2,3,3), lty = c(2,1,2,1,2,1),bty = "n")
dev.off()

# Compile table (a,b) truncated and weighted estimates with ci's
abTw1ci <- cbind(coef(modTw1),confint(modTw1))
abTw2ci <- cbind(coef(modTw2),confint(modTw2))
abTw3ci <- cbind(coef(modTw3),confint(modTw3))
temp <- rbind(abTw1ci,abTw2ci,abTw3ci)
abTwci <- rbind(c(temp[2,],temp[1,]),c(temp[4,],temp[3,]),c(temp[6,],temp[5,]))
rownames(abTwci) <- c("IL1","IL2","IL3")
colnames(abTwci) <- c("a","2.5%","97.5%","b","2.5%","97.5%")
abTwci

### Estimate parameters using abTw ####
# Option 1: Using (a,b)
# Option 2: Using Moments, and then method of moments.
###
# Option 1: SUCCESS.
beta.abTw <- abTw[,1]/(1-abTw[,1])
r.abTw <- abTw[,2]/abTw[,1] + 1
params.abTw <- rbind(beta.abTw,r.abTw)
rownames(params.abTw) <- c("beta","r")

# Option 2: FAIL.
EN <- (abTw[,1] + abTw[,2])/(1-abTw[,1])
VN <- (abTw[,1] + abTw[,2])/((1-abTw[,1])^2)
beta.ENmom <- (VN-EN)/EN # Returns Impossible values.
r.ENmom <- (EN^2)/(VN-EN) # Returns impossible values

### Estimate probabilities using abTw ####
# Model probabilities using abTw parameters
k = 0:14
p1Tw <- dnegbin(k,b = beta.abTw[1],r = r.abTw[1])
p2Tw <- dnegbin(k,b = beta.abTw[2],r = r.abTw[2])
p3Tw <- dnegbin(k,b = beta.abTw[3],r = r.abTw[3])
pTw <- rbind(p1Tw,p2Tw,p3Tw)
colnames(pTw) <- k
#rownames(pTw) <- c("IL1","IL2","IL3")

# Plot the Probabilties
plot(p1Tw,type = "h")
plot(p2Tw,type = "h")
plot(p3Tw,type = "h")
matplot(t(pTw),type = "o",
        main = "Modeled Probabilities (Zero-Truncated, WLS)",
        ylab = "P(X=x)",xlab = "Number of Overdrafts")


####################################################
# Which (a,b,0) Family? II: Compare sample variance to sample mean.  ####
####################################################
# Compare var 'v' w/ mean 'm'. 
# v < m, binomial
# v = m, poisson
# v > m, negative binomial

# Calculate mean and variance, using original data
# m1 = mean(od1,na.rm = T)
# v1 = var(od1, na.rm = T)
# v1 - m1
# v1 > m1 # negative binomial
# m2 = mean(od2,na.rm = T)
# v2 = var(od2, na.rm = T)
# v2 - m2
# v2 > m2 # negative binomial
# m3 = mean(od3,na.rm = T)
# v3 = var(od3, na.rm = T)
# v3 - m3
# v3 > m3 #negative binomial

### Calculate mean and variance using Zero-truncated data. ###
### Calculate mean and variance.
m1 = mean(datD1,na.rm = T)
v1 = var(datD1, na.rm = T)
v1 > m1 # negative binomial
m2 = mean(datD2,na.rm = T)
v2 = var(datD2, na.rm = T)
v2 > m2 # negative binomial
m3 = mean(datD3,na.rm = T)
v3 = var(datD3, na.rm = T)
v3 > m3 #negative binomial

### Bootstrap: D = variance - mean
# Bootstrap function
bootstrap <- function(temp,B){
  D <- numeric(B)
  for (i in 1:B){
    samp <- sample(temp,replace = T)
    v <- var(samp,na.rm = T)
    mn <- mean(samp,na.rm = T)
    D[i] <- v - mn
  }
  D
}
# Run the bootstraps 
B = 10000
boot1 <- bootstrap(datD1,B)
boot2 <- bootstrap(datD2,B)
boot3 <- bootstrap(datD3,B)
# 95% CI on bootstrap
bootci1 <- quantile(boot1,c(.025,.975))
bootci2 <- quantile(boot2,c(.025,.975))
bootci3 <- quantile(boot3,c(.025,.975))
# Table of results
boottab <- rbind(c(v1,m1,v1-m1,bootci1),
c(v2,m2,v2-m2,bootci2),
c(v3,m3,v3-m3,bootci3)
)
colnames(boottab) <- c("V(N)","E(N)","D = V-E","2.5%","97.5%")
rownames(boottab) <- c("IL1","IL2","IL3")
boot.obj <- xtable(boottab,caption ="Bootstrap Results")
setwd(writeup)
print(boot.obj,file = "RESULTS-bootstrap.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

#### Estimating parameters relationships Using MOM: ####
# Option 1: Use E(N) moments and solve
# Option 2: Use method of moments relationships
###
# Option 1: EN ... too hard.

# Option 2: Negative Binomial:
beta.mom1 <- (v1-m1)/m1
r.mom1 <- (m1^2)/(v1-m1)
beta.mom2 <- (v2-m2)/m2
r.mom2 <- (m2^2)/(v2-m2)
beta.mom3 <- (v3-m3)/m3
r.mom3 <- (m3^2)/(v3-m3)
beta.mom <- cbind(IL1 = beta.mom1,IL2 = beta.mom2,IL3 = beta.mom3)
r.mom <- cbind(IL1 = r.mom1, IL2 = r.mom2, IL3 = r.mom2)
params.mom <- rbind(beta.mom,r.mom)
rownames(params.mom) <- c("beta","r")

### Estimating (a,b) relationships Using MOM: ####
# Option 1: Use E(N) moments and solve
# Option 2: Use method of moments relationships
###
# Option 1: ... too hard.

# Option 2: Use MoM. SUCCESS.
a.mom = beta.mom/(1+beta.mom)
b.mom = (r.mom-1)*(a.mom)
abmom <- cbind(t(a.mom),t(b.mom))
colnames(abmom) <- c("a","b")

#### Estimating Probabilities Using MOM: ####
k = 0:14
p1mom <- dnegbin(k,b = beta.mom[1],r = r.mom[1])
p2mom <- dnegbin(k,b = beta.mom[2],r = r.mom[2])
p3mom <- dnegbin(k,b = beta.mom[3],r = r.mom[3])
pmom <- rbind(p1mom,p2mom,p3mom)
colnames(pmom) <- k
matplot(t(pmom),type = "o",
        main = "Modeled Probabilities (Met.o.Mom)",
        ylab = "P(X=x)",xlab = "Number of Overdrafts")

####################################################
# Comparing Methods ####
####################################################
### Goals:
# 0) Calculate $\X$ values
# 1) Compare (a,b) values from slope, E(N), and mom
# 2) Compare parameters estiamted from (a,b) and mom
# 3) Compare probabilities from parameters from (a,b)
###
# 0) X2 values
# On bins
chi1 <- chisq.test(table12t)
# On overdraft count
chi2 <- chisq.test(tabD) # normal data
chi3 <- chisq.test(tabT) # zero-truncated
# Combine into table
chi <- rbind(chi1,chi2,chi3)[,c(1,2,3)]
rownames(chi) <- c("Binned Data","Unbinned, Unmodified Data","Unbinned, Zero-Truncated Data")
colnames(chi) <- c("$\\X$ stat","df","p-val")
obj <- xtable(chi,caption ="$\\X$ estimates for three different data sets.",digits = 3)
print(obj,file = "RESULTS-chi.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

# 1.1) Produce (a,b,1) estiamtes unweighted
# abTw
obj <- xtable(abTw,caption ="\$(a,b,1)\$ estimates still yield \$a\$ values greater than 1.",digits = 3)
print(obj,file = "RESULTS-abTw.txt",table.placement = "H",caption.placement = "bottom",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

# 1.2) Compare Weighted (a,b) with confidence intervals.
# abTwci;abmom
aball <- cbind(abTwci,abmom)
aball
obj90 <- xtable(aball,caption ="$(a,b,1)$ estimates from weighted least squares and $(a,b)$ estimates from relationship to moments")
print(obj90,file = "RESULTS-aball.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)

# 2) Compare parameters
params.abTw; params.mom
params.comp <-rbind(c(beta.abTw,r.abTw),c(beta.mom,r.mom))
rownames(params.comp) = c("$WLS(a,b,1)$","$M. of M.$")
obj <- xtable(params.comp,caption ="Paremeter Estimation by Technique")
print(obj,file = "RESULTS-paramscomp.txt",table.placement = "H",caption.placement = "top",include.rownames = T,backslash = T, 
      sanitize.colnames.function = identity,
      sanitize.rownames.function = identity)
#&\multicolumn{3}{|c}{$\beta$} & \multicolumn{3}{|c}{$r$} \\ 

# 3) Compare Probabilities
# Plotting all 3*2 Income levels for different methods
dev.off()
pall <- rbind(pTw,pmom)
# Output
png("RESULTS-pall.png")
matplot(t(pall), 
        col = c(1,2,3,1,2,3),lty = c(1,1,1,2,2,2),
        type = c("l","l","l","l","l","l"),
        main = "Modeled Probabilities Under (a,b,0) \n and Method of Moments Parameter Estimation",
        xlab = "Number of Overdrafts",
        ylab = "Probability of Overdraft",
        xaxt = 'n') # to get rid of axis and get it right
axis(side = 1,1:15, labels =  0:14)
legend("topright", bty = "n",
       col = c(1,2,3,1,2,3),lty = c(1,1,1,2,2,2),
       legend = c("IL1-AB","IL2-AB","IL3-AB","IL1-MoM","IL2-MoM","IL3-MoM"))
dev.off()

# Plot of differences in estimation
png("RESULTS-pdiffs.png")
pdiffs <- rbind(pall[1,]-pall[4,],pall[2,]-pall[5,],pall[3,]-pall[6,])
matplot(t(pdiffs),lty = 1,type = "l",
        main = "Difference Between (a,b,0) and MoM \n Probability Estimation", 
        ylab = expression('p'['(a,b,0)']*' - p'["mom"]),
        cex.lab = 1.2,
        xaxt = 'n')
axis(side = 1,1:15, labels =  0:14)
abline(h = 0,col = "blue")
legend("topright", bty = "n",col = c(1,2,3),lty = 1,legend = c("IL1","IL2","IL3"))
dev.off()

# Comparing Just two overdrafts at a time
# par(mfrow = c(3,1))
# plows <- rbind(pall[1,],pall[4,])
# matplot(t(plows),col = 1,lty = c(1,2),type = c("l","l"))

####################################################
# Re-scaling probabilities ####
####################################################
### Goal: Scale the probabilities to get the final probability chart
# Use weighted LS. FAIL: Doesn't sum to 1.
p0s <- prop.table(tabD,margin = 2)[1,]
p0s
fact <- 1/(1-p0s)
out <- pTw/fact
p0s + rowSums(out)
pfinal <- cbind(p0s,out)
colnames(pfinal) <- 0:15

### Plot pfinal.
png("RESULTS-finalprobs.png")
matplot(t(pfinal),lty = 1,type = "l",
        main = "Final Probabilities", 
        ylab = "Probability of Overdraft")
axis(side = 1,1:15, labels =  0:14)
abline(h = 0,col = "blue")
legend("topright", bty = "n",col = c(1,2,3),lty = 1,legend = c("IL1","IL2","IL3"))
dev.off()


# Create bar plots of the same information
temp <- t(pfinal)
temp <- cbind(c(temp[,1],temp[,2],temp[,3]),c(rep(1,16),rep(2,16),rep(3,16)))
colnames(temp) <- c("Freq","Income")
rownames(temp) <- 1:nrow(temp)
temp <- as.data.frame(temp)
temp$cnt <- rep(0:15,3)
temp$Income <- c(rep("1-Low",16),rep("2-Med",16),rep("3-High",16))
png("RESULTS-finalprobs-barplot.png")
qplot(y = Freq, x = cnt, geom= "bar", 
      stat = "identity", # to have the bar represent frequency
      fill = Income,
      facets = ~Income,
      main = "Final Probabilities",
      xlab = "Number of Overdrafts",
      ylab = "Probability of Overdraft",
      legend = "bal",
      data = temp)
dev.off()

####################################################
# GLM analysis  ####
####################################################
### Modelling. Goal: Detect significant differences and do pair-wise comparisons.
# Goal 1: Fit all 3 models
# Goal 2: Fit the negative binomial and assess significance / pairwise differences.
###
### Goal 1: All 3 Models, Just to see
# Fit Poisson and Neg Bin. SUCCESS.
mod.pois <- glm(od_cnt12 ~ factor(inc_level12),data = dat, family = poisson)
mod.nb <- glm.nb(od_cnt12 ~ factor(inc_level12),data = dat)
# Binomial is different. FAIL:
data.frame(table(dat$od_cnt12,dat$inc_level12))
mod.bin <- glm(od_cnt12 ~ factor(inc_level12),data = dat, family = binomial)

### Goal 2: Fitting Negative Binomial distribution
# 1) Fit the negative binomial model
# 2) Pairwise differences
###
# 1) NB - full model
# Randomly generate data:
IL <- sample(c(1,2,3),100,replace = T)
od_cnt <- rbinom(100,10,.5)
temp <- data.frame(y = od_cnt, IL = IL)
temp$IL <- as.factor(IL)
temp$IL <- relevel(temp$IL, ref = "3")
summary(glm.nb(y ~ IL, data = temp))$coefficients
temp$IL <- relevel(temp$IL, ref = "1")
summary(glm.nb(y ~ IL, data = temp))$coefficients

# Trying Contrasts:
glm.nb(y ~ IL, data = temp,contrasts = list(a = "contr.sum"))

# Example:
ff <- log(Volume) ~ log(Height) + log(Girth)
utils::str(m <- model.frame(ff, trees))
mat <- model.matrix(ff, m)
dd <- data.frame(a = gl(3,4), b = gl(4,1,12)) # balanced 2-way
options("contrasts")
model.matrix(~ a + b, dd)
model.matrix(~ a + b, dd, contrasts = list(a = "contr.sum"))
b = model.matrix(~ a + b, dd, contrasts = list(a = "contr.sum"))  
glm.nb(y ~ IL, data = temp,contrasts = b)


temp <- dat1
temp$inc_level12 <- as.factor(temp$inc_level12)
temp <- within(temp, relevel(inc_level12,ref = "3"))
mod1 <- glm.nb(od_cnt12 ~ factor(inc_level12),data = temp)
summary(mod1)
# BW: Since all variables are significant, we conclude that there is no significant difference between the income levels and their effect on overdrafts.
# 2) Pairwise differences
# Change the data so you can test two levels at a time.


