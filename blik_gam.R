# Learning Unnatural Language Quantifiers

## Semih Can Aktepe

# 1. Load the required packages
install.packages("mgcv")
install.packages("itsadug")

# 2. Import the required packages
library(mgcv)
library(itsadug)

# 3. Import the datasets from your directory
eye <- load("~/eye.rdata")
response <- load("~/response.rdata")

# 4. Generalized Additive Models for the eye-tracking data

## 4.1. Girls on the Beach Referent

gb <- bam(GirlsBeachLooks ~ Quantifier +
             s(Time, by=Quantifier) +
             s(Time, by=Counterbalance) +
             s(Time, by=Sex) +
             ti(Time, Index, by=Quantifier),
           data=eye, family=quasibinomial, method="fREML"))

## 4.2. Boys on the Beach Referent

bb <- bam(BoysBeachLooks ~ Quantifier +
             s(Time, by=Quantifier) +
             s(Time, by=Counterbalance) +
             s(Time, by=Sex) +
             ti(Time, Index, by=Quantifier),
           data=eye, family=quasibinomial, method="fREML"))

## 4.3. Girls on the Grass Referent

gg <- bam(GirlsGrassLooks ~ Quantifier +
             s(Time, by=Quantifier) +
             s(Time, by=Counterbalance) +
             s(Time, by=Sex) +
             ti(Time, Index, by=Quantifier),
           data=eye, family=quasibinomial, method="fREML"))

# 5. GAM for the response and reaction time data

## 5.1. Accuracy model

macc <- bam(accuracy ~ quantifier + s(t_index, by=quantifier) +
            s(t_index, id, by=quantifier, bs="fs", m=1),
          data=response, family=binomial, method="fREML")

### 5.1.1. Pairwise comparison between quantifiers

contr <- matrix(0, nrow = 6, ncol = length(coef(res)))
colnames(contr) <- names(coef(res))
rownames(contr) <- c("NotAll - NotOnly", "NotAll - NotAlone", "NotAll - Equi", "NotOnly - NotAlone", "NotOnly - Equi", "NotAlone - Equi")
contr[, 2:4] <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(1, -1, 0), c(1, 0, -1), c(0, 1, -1))
g1 <- glht(res, linfct = contr)
summary(g1)

### 5.1.2. Non-linear smooth graph for the accuracy model
plot_smooth(macc, view="t_index", cond = list(quantifier=1),
            col="red", ylim = c(-1.5,1.5), 
            main="Accuracy Difference Among Quantifiers", 
            xlab="Trial Index", ylab = "Accuracy (logit)", rm.ranef=T)
plot_smooth(macc, view="t_index", cond = list(quantifier=2),
            col="blue",rm.ranef = T, add=T)
plot_smooth(macc, view="t_index", cond = list(quantifier=3),
            col="yellow3",rm.ranef = T, add=T)
plot_smooth(macc, view="t_index", cond = list(quantifier=4),
            col="green4",rm.ranef = T, add=T)
legend(59,-0.5, legend=c("Not All", "Not Only", "Not Alone", "Equi"), 
       col=c("red", "blue", "yellow3", "green4"), lty=c(1,1,1,1))

## random effect plot

plot_smooth(rt, view="t_index", cond = list(quantifier=1),
            col="red", ylim = c(1000,5000),
            main="Reaction Time Difference Among Quantifiers", 
            xlab="Trial Index", ylab = "Reaction Time (ms.)", rm.ranef=T)
plot_smooth(rt, view="t_index", cond = list(quantifier=2),
            col="blue", rm.ranef = T, add=T)
plot_smooth(rt, view="t_index", cond = list(quantifier=3),
            col="yellow3", rm.ranef = T, add=T)
plot_smooth(rt, view="t_index", cond = list(quantifier=4),
            col="green4", rm.ranef = T, add=T)
legend(59,5000, legend=c("Not All", "Not Only", "Not Alone", "Equi"), 
       col=c("red", "blue", "yellow3", "green4"), lty=c(1,1,1,1))


## 5.2. Reaction time model

mrt <- bam(rt ~ quantifier + s(t_index, by=quantifier) +
              s(t_index, id, by=quantifier, bs="fs", m=1),
            data=response, family=gaussian, method="fREML")

### 5.2.1. Pairwise comparison between quantifiers
g2 <- glht(mrt, linfct = contr)
summary(g2)

### 5.2.2. Non-linear smooth graph for the reaction time model

plot_smooth(mrt, view="t_index", cond = list(quantifier=1),
            col="red", ylim = c(1000,5000),
            main="Reaction Time Difference Among Quantifiers", 
            xlab="Trial Index", ylab = "Reaction Time (ms.)", rm.ranef=T)
plot_smooth(mrt, view="t_index", cond = list(quantifier=2),
            col="blue", rm.ranef = T, add=T)
plot_smooth(mrt, view="t_index", cond = list(quantifier=3),
            col="yellow3", rm.ranef = T, add=T)
plot_smooth(mrt, view="t_index", cond = list(quantifier=4),
            col="green4", rm.ranef = T, add=T)
legend(59,5000, legend=c("Not All", "Not Only", "Not Alone", "Equi"), 
       col=c("red", "blue", "yellow3", "green4"), lty=c(1,1,1,1))
