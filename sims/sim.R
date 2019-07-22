# Load libraries
library(simr)

# Load data from OSF repository
setwd("/home/blazej/R/Projekty/Power.fr/")
dataf <- read.csv("Rdataframe.csv", stringsAsFactors = F)




##%######################################################%##
#                                                          #
####                   2nd batch of 5                   ####
#                                                          #
##%######################################################%##

rm(list=ls())
dataf <- read.csv("Rdataframe.csv", stringsAsFactors = F)
ns = 5000
ss<-3

# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(ss)
p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))


set.seed(ss)
p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)

fixef(m2)["anxC"] <- 0.05

set.seed(ss)
p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))

set.seed(ss)
p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))

# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(ss)
p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))

fixef(m3)["anxC:negC"] <- 0.02
set.seed(ss)
p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))


l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
save(l, file = paste0("sim", ns, "seed", ss))



# ss<-ss +1
# 
# 
# # Model 1
# m1 <- lmer(scc ~ naC + (naC|subj), dataf)
# summary(m1)
# 
# fixef(m1)["naC"] <- -0.15
# 
# set.seed(ss)
# p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# 
# set.seed(ss)
# p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# 
# # Model 2
# m2 <- lmer(scc ~ anxC + (1|subj), dataf)
# 
# fixef(m2)["anxC"] <- 0.05
# 
# set.seed(ss)
# p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# set.seed(ss)
# p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# # Model 3
# m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)
# 
# fixef(m3)["anxC:negC"] <- 0.01
# set.seed(ss)
# p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# fixef(m3)["anxC:negC"] <- 0.02
# set.seed(ss)
# p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# 
# l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
# save(l, file = paste0("sim", ns, "seed", ss))
# 
# ss<-ss +1
# 
# 
# # Model 1
# m1 <- lmer(scc ~ naC + (naC|subj), dataf)
# summary(m1)
# 
# fixef(m1)["naC"] <- -0.15
# 
# set.seed(ss)
# p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# 
# set.seed(ss)
# p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# 
# # Model 2
# m2 <- lmer(scc ~ anxC + (1|subj), dataf)
# 
# fixef(m2)["anxC"] <- 0.05
# 
# set.seed(ss)
# p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# set.seed(ss)
# p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# # Model 3
# m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)
# 
# fixef(m3)["anxC:negC"] <- 0.01
# set.seed(ss)
# p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# fixef(m3)["anxC:negC"] <- 0.02
# set.seed(ss)
# p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# 
# l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
# save(l, file = paste0("sim", ns, "seed", ss))
# 
# 
# ss<-ss +1
# 
# 
# # Model 1
# m1 <- lmer(scc ~ naC + (naC|subj), dataf)
# summary(m1)
# 
# fixef(m1)["naC"] <- -0.15
# 
# set.seed(ss)
# p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# 
# set.seed(ss)
# p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# 
# # Model 2
# m2 <- lmer(scc ~ anxC + (1|subj), dataf)
# 
# fixef(m2)["anxC"] <- 0.05
# 
# set.seed(ss)
# p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# set.seed(ss)
# p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# # Model 3
# m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)
# 
# fixef(m3)["anxC:negC"] <- 0.01
# set.seed(ss)
# p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# fixef(m3)["anxC:negC"] <- 0.02
# set.seed(ss)
# p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# 
# l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
# save(l, file = paste0("sim", ns, "seed", ss))
# 
# 
# ss<-ss +1
# 
# 
# # Model 1
# m1 <- lmer(scc ~ naC + (naC|subj), dataf)
# summary(m1)
# 
# fixef(m1)["naC"] <- -0.15
# 
# set.seed(ss)
# p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# 
# set.seed(ss)
# p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# 
# # Model 2
# m2 <- lmer(scc ~ anxC + (1|subj), dataf)
# 
# fixef(m2)["anxC"] <- 0.05
# 
# set.seed(ss)
# p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))
# 
# set.seed(ss)
# p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))
# 
# # Model 3
# m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)
# 
# fixef(m3)["anxC:negC"] <- 0.01
# set.seed(ss)
# p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# fixef(m3)["anxC:negC"] <- 0.02
# set.seed(ss)
# p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
#                  test=fixed("anxC:negC"))
# 
# set.seed(ss)
# p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
#                test=fixed("anxC:negC"))
# 
# 
# l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
# save(l, file = paste0("sim", ns, "seed", ss))

##%######################################################%##
#                                                          #
####                   3rd batch of 5                   ####
#                                                          #
##%######################################################%##


rm(list=ls())
dataf <- read.csv("Rdataframe.csv", stringsAsFactors = F)
ns = 5000
ss<-3

# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(ss)
p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))


set.seed(ss)
p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)

fixef(m2)["anxC"] <- 0.05

set.seed(ss)
p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))

set.seed(ss)
p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))

# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(ss)
p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))

fixef(m3)["anxC:negC"] <- 0.02
set.seed(ss)
p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))


l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
save(l, file = paste0("sim", ns, "seed", ss))



ss<-ss +1


# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(ss)
p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))


set.seed(ss)
p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)

fixef(m2)["anxC"] <- 0.05

set.seed(ss)
p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))

set.seed(ss)
p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))

# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(ss)
p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))

fixef(m3)["anxC:negC"] <- 0.02
set.seed(ss)
p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))


l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
save(l, file = paste0("sim", ns, "seed", ss))

ss<-ss +1


# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(ss)
p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))


set.seed(ss)
p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)

fixef(m2)["anxC"] <- 0.05

set.seed(ss)
p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))

set.seed(ss)
p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))

# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(ss)
p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))

fixef(m3)["anxC:negC"] <- 0.02
set.seed(ss)
p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))


l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
save(l, file = paste0("sim", ns, "seed", ss))


ss<-ss +1


# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(ss)
p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))


set.seed(ss)
p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)

fixef(m2)["anxC"] <- 0.05

set.seed(ss)
p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))

set.seed(ss)
p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))

# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(ss)
p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))

fixef(m3)["anxC:negC"] <- 0.02
set.seed(ss)
p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))


l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
save(l, file = paste0("sim", ns, "seed", ss))


ss<-ss +1


# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(ss)
p1 <- powerCurve(m1, along="subj", nsim=ns, breaks = seq(20, 100, 5))


set.seed(ss)
p2<-powerCurve(m1, along="day", nsim=ns, breaks = seq(3, 14, 1))


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)

fixef(m2)["anxC"] <- 0.05

set.seed(ss)
p3 <- powerCurve(m2, along="subj", nsim=ns, breaks = seq(20, 100, 5))

set.seed(ss)
p4<-powerCurve(m2, along="day", nsim=ns, breaks = seq(3, 14, 1))

# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(ss)
p5 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p6<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))

fixef(m3)["anxC:negC"] <- 0.02
set.seed(ss)
p7 <- powerCurve(m3, along="subj", nsim=ns, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))

set.seed(ss)
p8<-powerCurve(m3, along="day", nsim=ns, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))


l<-list(p1, p2, p3, p4, p5, p6, p7, p8)
save(l, file = paste0("sim", ns, "seed", ss))