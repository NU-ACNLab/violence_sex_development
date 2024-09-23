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
library(lmerTest) #confint

############################ 20 sessions per person ############################

numsub = 200
numses = 20
dmj <- rnorm(numsub, 0, sqrt(.6))
dyj <- rnorm(numsub, 0, sqrt(.4))
emj <- data.frame(e1 = rnorm(numsub, 0, sqrt(.65)),
                  e2 = rnorm(numsub, 0, sqrt(.65)),
                  e3 = rnorm(numsub, 0, sqrt(.65)),
                  e4 = rnorm(numsub, 0, sqrt(.65)),
                  e5 = rnorm(numsub, 0, sqrt(.65)),
                  e6 = rnorm(numsub, 0, sqrt(.65)),
                  e7 = rnorm(numsub, 0, sqrt(.65)),
                  e8 = rnorm(numsub, 0, sqrt(.65)),
                  e9 = rnorm(numsub, 0, sqrt(.65)),
                  e10 = rnorm(numsub, 0, sqrt(.65)),
                  e11 = rnorm(numsub, 0, sqrt(.65)),
                  e12 = rnorm(numsub, 0, sqrt(.65)),
                  e13 = rnorm(numsub, 0, sqrt(.65)),
                  e14 = rnorm(numsub, 0, sqrt(.65)),
                  e15 = rnorm(numsub, 0, sqrt(.65)),
                  e16 = rnorm(numsub, 0, sqrt(.65)),
                  e17 = rnorm(numsub, 0, sqrt(.65)),
                  e18 = rnorm(numsub, 0, sqrt(.65)),
                  e19 = rnorm(numsub, 0, sqrt(.65)),
                  e20 = rnorm(numsub, 0, sqrt(.65))
                  )
eyj <- data.frame(e1 = rnorm(numsub, 0, sqrt(.45)),
                  e2 = rnorm(numsub, 0, sqrt(.45)),
                  e3 = rnorm(numsub, 0, sqrt(.45)),
                  e4 = rnorm(numsub, 0, sqrt(.45)),
                  e5 = rnorm(numsub, 0, sqrt(.45)),
                  e6 = rnorm(numsub, 0, sqrt(.45)),
                  e7 = rnorm(numsub, 0, sqrt(.45)),
                  e8 = rnorm(numsub, 0, sqrt(.45)),
                  e9 = rnorm(numsub, 0, sqrt(.45)),
                  e10 = rnorm(numsub, 0, sqrt(.45)),
                  e11 = rnorm(numsub, 0, sqrt(.45)),
                  e12 = rnorm(numsub, 0, sqrt(.45)),
                  e13 = rnorm(numsub, 0, sqrt(.45)),
                  e14 = rnorm(numsub, 0, sqrt(.45)),
                  e15 = rnorm(numsub, 0, sqrt(.45)),
                  e16 = rnorm(numsub, 0, sqrt(.45)),
                  e17 = rnorm(numsub, 0, sqrt(.45)),
                  e18 = rnorm(numsub, 0, sqrt(.45)),
                  e19 = rnorm(numsub, 0, sqrt(.45)),
                  e20 = rnorm(numsub, 0, sqrt(.45))
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
d5 <- data.frame(subid=1:200, sesid=rep(5, numsub), x = NA, m = NA, y = NA)
d6 <- data.frame(subid=1:200, sesid=rep(6, numsub), x = NA, m = NA, y = NA)
d7 <- data.frame(subid=1:200, sesid=rep(7, numsub), x = NA, m = NA, y = NA)
d8 <- data.frame(subid=1:200, sesid=rep(8, numsub), x = NA, m = NA, y = NA)
d9 <- data.frame(subid=1:200, sesid=rep(9, numsub), x = NA, m = NA, y = NA)
d10 <- data.frame(subid=1:200, sesid=rep(10, numsub), x = NA, m = NA, y = NA)
d11 <- data.frame(subid=1:200, sesid=rep(11, numsub), x = NA, m = NA, y = NA)
d12 <- data.frame(subid=1:200, sesid=rep(12, numsub), x = NA, m = NA, y = NA)
d13 <- data.frame(subid=1:200, sesid=rep(13, numsub), x = NA, m = NA, y = NA)
d14 <- data.frame(subid=1:200, sesid=rep(14, numsub), x = NA, m = NA, y = NA)
d15 <- data.frame(subid=1:200, sesid=rep(15, numsub), x = NA, m = NA, y = NA)
d16 <- data.frame(subid=1:200, sesid=rep(16, numsub), x = NA, m = NA, y = NA)
d17 <- data.frame(subid=1:200, sesid=rep(17, numsub), x = NA, m = NA, y = NA)
d18 <- data.frame(subid=1:200, sesid=rep(18, numsub), x = NA, m = NA, y = NA)
d19 <- data.frame(subid=1:200, sesid=rep(19, numsub), x = NA, m = NA, y = NA)
d20 <- data.frame(subid=1:200, sesid=rep(20, numsub), x = NA, m = NA, y = NA)


for (j in 1:numsub) {
  # x
  xs <- xbarj[j] + rnorm(numses, 0, 1)
  d1[j, 'x'] <- xs[1]
  d2[j, 'x'] <- xs[2]
  d3[j, 'x'] <- xs[3]
  d4[j, 'x'] <- xs[4]
  d5[j, 'x'] <- xs[5]
  d6[j, 'x'] <- xs[6]
  d7[j, 'x'] <- xs[7]
  d8[j, 'x'] <- xs[8]
  d9[j, 'x'] <- xs[9]
  d10[j, 'x'] <- xs[10]
  d11[j, 'x'] <- xs[11]
  d12[j, 'x'] <- xs[12]
  d13[j, 'x'] <- xs[13]
  d14[j, 'x'] <- xs[14]
  d15[j, 'x'] <- xs[15]
  d16[j, 'x'] <- xs[16]
  d17[j, 'x'] <- xs[17]
  d18[j, 'x'] <- xs[18]
  d19[j, 'x'] <- xs[19]
  d20[j, 'x'] <- xs[20]

  # m
  ms <- dmj[j] + bvn$a[j]*xs + emj[j, ]
  d1[j, 'm'] <- ms[[1]]
  d2[j, 'm'] <- ms[[2]]
  d3[j, 'm'] <- ms[[3]]
  d4[j, 'm'] <- ms[[4]]
  d5[j, 'm'] <- ms[[5]]
  d6[j, 'm'] <- ms[[6]]
  d7[j, 'm'] <- ms[[7]]
  d8[j, 'm'] <- ms[[8]]
  d9[j, 'm'] <- ms[[9]]
  d10[j, 'm'] <- ms[[10]]
  d11[j, 'm'] <- ms[[11]]
  d12[j, 'm'] <- ms[[12]]
  d13[j, 'm'] <- ms[[13]]
  d14[j, 'm'] <- ms[[14]]
  d15[j, 'm'] <- ms[[15]]
  d16[j, 'm'] <- ms[[16]]
  d17[j, 'm'] <- ms[[17]]
  d18[j, 'm'] <- ms[[18]]
  d19[j, 'm'] <- ms[[19]]
  d20[j, 'm'] <- ms[[20]]

  # y
  ys = dyj + bvn$b[j]*ms + c[j]*xs + eyj[j, ]
  d1[j, 'y'] <- ys[[1]]
  d2[j, 'y'] <- ys[[2]]
  d3[j, 'y'] <- ys[[3]]
  d4[j, 'y'] <- ys[[4]]
  d5[j, 'y'] <- ys[[5]]
  d6[j, 'y'] <- ys[[6]]
  d7[j, 'y'] <- ys[[7]]
  d8[j, 'y'] <- ys[[8]]
  d9[j, 'y'] <- ys[[9]]
  d10[j, 'y'] <- ys[[10]]
  d11[j, 'y'] <- ys[[11]]
  d12[j, 'y'] <- ys[[12]]
  d13[j, 'y'] <- ys[[13]]
  d14[j, 'y'] <- ys[[14]]
  d15[j, 'y'] <- ys[[15]]
  d16[j, 'y'] <- ys[[16]]
  d17[j, 'y'] <- ys[[17]]
  d18[j, 'y'] <- ys[[18]]
  d19[j, 'y'] <- ys[[19]]
  d20[j, 'y'] <- ys[[20]]
}

d <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15,
           d16, d17, d18, d19, d20)

# checking correlations
cor(d$x, d$m) # 0.54
cor(d$x, d$y) # 0.52
cor(d$m, d$y) # 0.72

###### Y ~ X
m1 <- lme4::lmer(y ~ x + (1 + x | subid), data = d)
summary(m1)

###### M ~ X
m2 <- lme4::lmer(m ~ x + (1 + x | subid), data = d)
summary(m2)

###### Y ~ M + X
m3 <- nlme::lme(y ~ m + x, random = ~ m + x | subid, data = d) # not singular... use this
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
ctrl <- lmeControl(opt='optim')
mm <- nlme::lme(z ~ 0 + sm + sm:x + sy + sy:m + sy:x, data = stacked,
                  random = list(~ 0 + sm + sm:x + sy + sy:m + sy:x | subid, ~ 0 + sm | sesid),
                  control = ctrl) #"invalid formula for groups"
                  # https://stats.stackexchange.com/questions/40647/lme-error-iteration-limit-reached


## view summary and save summary object to 'smm'
(smm <- summary(mm))

## fit model
mm.alt <- nlme::lme(z ~ 0 + sm + sm:x + sy + sy:m + sy:x,
    data = stacked, random = ~0 + sm + sm:x + sy + sy:m +
        sy:x | subid, weights = varIdent(form = ~1 | variable), control = ctrl)

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
