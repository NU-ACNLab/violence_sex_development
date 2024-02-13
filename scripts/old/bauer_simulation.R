### This script contains a simulation from Bauer, Preacher & Gil (2006) written
### in R (p7)
### https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-mediation-with-multilevel-data-method-2/
###
### Ellyn Butler
### January 28, 2024 - January 31, 2024

set.seed(2024)

library(lme4)
library(reshape2)
library(nlme)
library(MASS)
library(lmerTest) #confint

########################### Four sessions per person ###########################

numsub = 200
numses = 4
dmj <- rnorm(numsub, 0, sqrt(.6))
dyj <- rnorm(numsub, 0, sqrt(.4))
emj <- data.frame(e1 = rnorm(numsub, 0, sqrt(.65)),
                  e2 = rnorm(numsub, 0, sqrt(.65)),
                  e3 = rnorm(numsub, 0, sqrt(.65)),
                  e4 = rnorm(numsub, 0, sqrt(.65))
                  )
eyj <- data.frame(e1 = rnorm(numsub, 0, sqrt(.45)),
                  e2 = rnorm(numsub, 0, sqrt(.45)),
                  e3 = rnorm(numsub, 0, sqrt(.45)),
                  e4 = rnorm(numsub, 0, sqrt(.45))
                  )
bvn <- mvrnorm(numsub, mu = c(.6, .6), Sigma = matrix(c(.16, .113, .113, .16), nrow=2, ncol=2))
bvn <- data.frame(bvn)
names(bvn) <- c('a', 'b')
c <- rnorm(numsub, .2, sqrt(.04)) #c-prime
xbarj <- rnorm(numsub, 0, 1)

#NOTE: Level 2 units are people, so j indexes the person


d1 <- data.frame(subid=1:200, sesid=rep(1, numsub), x = NA, m = NA, y = NA)
d2 <- data.frame(subid=1:200, sesid=rep(2, numsub), x = NA, m = NA, y = NA)
d3 <- data.frame(subid=1:200, sesid=rep(3, numsub), x = NA, m = NA, y = NA)
d4 <- data.frame(subid=1:200, sesid=rep(4, numsub), x = NA, m = NA, y = NA)

for (j in 1:numsub) {
  # x
  xs <- xbarj[j] + rnorm(numses, 0, 1)
  d1[j, 'x'] <- xs[1]
  d2[j, 'x'] <- xs[2]
  d3[j, 'x'] <- xs[3]
  d4[j, 'x'] <- xs[4]

  # m
  ms <- dmj[j] + bvn$a[j]*xs + emj[j, ]
  d1[j, 'm'] <- ms[[1]]
  d2[j, 'm'] <- ms[[2]]
  d3[j, 'm'] <- ms[[3]]
  d4[j, 'm'] <- ms[[4]]

  # y
  ys = dyj + bvn$b[j]*ms + c[j]*xs + eyj[j, ]
  d1[j, 'y'] <- ys[[1]]
  d2[j, 'y'] <- ys[[2]]
  d3[j, 'y'] <- ys[[3]]
  d4[j, 'y'] <- ys[[4]]
}

d <- rbind(d1, d2, d3, d4)

# checking correlations
cor(d$x, d$m) # 0.595
cor(d$x, d$y) # 0.605
cor(d$m, d$y) # 0.775

###### Y ~ X
m1 <- lme4::lmer(y ~ x + (1 + x | subid), data = d)
summary(m1)

###### M ~ X
m2 <- lme4::lmer(m ~ x + (1 + x | subid), data = d)
summary(m2)

###### Y ~ M + X
m3 <- lme4::lmer(y ~ m + x + (1 + m + x | subid), data = d) #singular... February 6, 2024: now not???
summary(m3)
#m3.2 <- lme4::lmer(y ~ m + x + (m + x | subid), data = d) #not singular...
#summary(m3.2)

#m3e <- nlme::lme(y ~ m + x, random = ~ 1 + m + x | subid, data = d) #... convergence problem
#m3e.2 <- nlme::lme(y ~ m + x, random = ~ m + x | subid, data = d) #... convergence problem

# reshape #fid to sesid
stacked <- melt(d, id.vars = c("sesid", "subid", "x", "m"),
   measure.vars = c("y", "m"), value.name = "z")
stacked <- within(stacked, {
  sy <- as.integer(variable == "y")
  sm <- as.integer(variable == "m")
})

## show all data for id 1
stacked[stacked$subid == 1, ]

## fit model... ~~~ SINGULAR ISSUE ~~~
mm1 <- lme4::lmer(z ~ 0 + sm + sm:x + sy + sy:m + sy:x +
               (0 + sm + sm:x + sy + sy:m + sy:x | subid) +
               (0 + sm | sesid), data = stacked) # singular

mm1.2 <- lme4::lmer(z ~ 0 + sm + sm:x + sy + sy:m + sy:x +
                     (0 + sm + sm:x + sy + sy:m + sy:x | subid) +
                     (0 + sm | sesid), data = stacked) # singular

mm2 <- lme4::lmer(z ~ 0 + sm + sm:x + sy + sy:m + sy:x +
               (0 + sm | subid) +
               (0 + sm | sesid), data = stacked) # still singular

mm3 <- lme4::lmer(z ~ 0 + sm +
               (0 + sm | subid) +
               (0 + sm | sesid), data = stacked) # still singular!!!

mm4 <- lme4::lmer(z ~ 0 + sm + sm:x + sy + sy:m + sy:x +
               (0 + sm + sm:x + sy + sy:m + sy:x | subid), data = stacked) # singular

mm5 <- lme4::lmer(z ~ 0 + sm + sm:x + sy + sy:m + sy:x +
               (0 + sm | subid), data = stacked) # NOT singular!!!

mm6 <- lme4::lmer(z ~ 0 + sm + sm:x + sy + sy:m + sy:x +
              (0 + sy | subid) + (0 + sm | sesid), data = stacked)

mm7 <- lme4::lmer(z ~ -1 + sm + sm:x + sy + sy:m + sy:x +
              (-1 + sm + sm:x + sy + sy:m + sy:x | subid) +
              (-1 + sm | sesid), data = stacked) # singular

mm8 <- lme4::lmer(z ~ 1 + sm + sm:x + sy + sy:m + sy:x +
              (1 + sm + sm:x + sy + sy:m + sy:x | subid) +
              (1 + sm | sesid), data = stacked) # rank deficient and singular

## view summary and save summary object to 'smm'
(smm <- summary(mm))

remat <- smm@REmat[, -c(1:2)]
mode(remat) <- "numeric"

## add rownames by collapsing group and name
rownames(remat) <- apply(smm@REmat[, 1:2], 1, paste, collapse = "")

## variance and standard deviation of y
remat["Residual", c("Variance", "Std.Dev.")]

## variance of m
sum(remat[c("fidsm", "Residual"), "Variance"])

## standard deviation of m
sqrt(sum(remat[c("fidsm", "Residual"), "Variance"]))

## fit model
mm.alt <- lme(z ~ 0 + sm + sm:x + sy + sy:m + sy:x,
    data = stacked, random = ~0 + sm + sm:x + sy + sy:m +
        sy:x | id, weights = varIdent(form = ~1 | variable))

## view summary
summary(mm.alt)

## view fixed effects estimates
fixef(mm)

## product of 'a' and 'b' paths
(ab <- prod(fixef(mm)[c("sm:x", "sy:m")]))

## covariance between random effects
(rcov <- VarCorr(mm)[["id"]]["sm:x", "sy:m"])

## indirect effect
ab + rcov

## total effect
ab + rcov + fixef(mm)["x:sy"]
