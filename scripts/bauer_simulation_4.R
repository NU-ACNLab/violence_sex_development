### This script contains a simulation from Bauer, Preacher & Gil (2006) written
### in R (p7)
### https://stats.oarc.ucla.edu/r/faq/how-can-i-perform-mediation-with-multilevel-data-method-2/
###
### Ellyn Butler
### January 28, 2024 - February 13, 2024

set.seed(2024)

library(lme4)
library(reshape2)
library(nlme)
library(MASS)
#library(lmerTest) #confint

############################# 4 sessions per person ############################

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
c <- rnorm(numsub, .2, sqrt(.04))
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
cor(d$x, d$m) # 0.64
cor(d$x, d$y) # 0.64
cor(d$m, d$y) # 0.79

###### Y ~ X
ctrl <- lmeControl(opt='optim')
m1 <- nlme::lme(y ~ x, random = ~ 1 + x | subid, data = d,
                control = ctrl)
summary(m1)

###### M ~ X
m2 <- nlme::lme(m ~ x, random = ~ 1 + x | subid, data = d,
                control = ctrl)
summary(m2)

###### Y ~ M + X
m3 <- nlme::lme(y ~ m + x, random = list(subid = pdDiag(~ m + x)), data = d,
                control = ctrl) # not converging: intervals(m3, which='var-cov')... Noelle not surprised by this
                # Constrain some of the covariance entries to be the same, or
                # some other assumption about the nature of the covariance matrix
                # TO DO: Noelle will write out model formulations
                # TO DO: How does pdDiag work?
summary(m3)

# reshape #fid to sesid
stacked <- melt(d, id.vars = c("sesid", "subid", "x", "m"),
   measure.vars = c("y", "m"), value.name = "z")
stacked <- within(stacked, {
  sy <- as.integer(variable == "y")
  sm <- as.integer(variable == "m")
})

## show all data for id 1
stacked[stacked$subid == 1, ]

## fit model
mm <- nlme::lme(z ~ 0 + sm + sm:x + sy + sy:m + sy:x, data = stacked,
                  random = list(~ 0 + sm + sm:x + sy + sy:m + sy:x | subid, ~ 0 + sm | sesid),
                  control = ctrl)


## view summary and save summary object to 'smm'
(smm <- summary(mm))

## fit model
mm.alt <- nlme::lme(z ~ 0 + sm + sm:x + sy + sy:m + sy:x,
    data = stacked, random = pdBlocked(list(subid = pdDiag(~ 0 + sm + sm:x + sy + sy:m +
        sy:x), sesid = list(~ 0 + sm))), control = ctrl) #weights = varIdent(form = ~1 | variable),
        # TO DO: Figure out how to have sesid nested within subid

## view summary
summary(mm.alt)

## view fixed effects estimates
fixef(mm)

## product of 'a' and 'b' paths
(ab <- prod(fixef(mm)[c("sm:x", "sy:m")]))

## covariance between random effects
(rcov <- as.numeric(VarCorr(mm)[row.names(VarCorr(mm)) == 'sy:m', 5]))
#should be close to .113/.16

## indirect effect
ab + rcov # .6^2 + .113/.16 = 1.066... very close! good

## total effect
ab + rcov + fixef(mm)["x:sy"]


# TO DO
## can do with 2 sessions?
## assess significance? check out paper
