# Nezlek, J., Mrozinski, B., (2019). Applications of multilevel modeling in psychological science: Intensive repeated measures designs. L'année psychologique. Special Issue (in print).
# 
# Supplementary Materials:
# Nezlek, J. B., & Mrozinski, B. (2019, April 7). Applications of multilevel modeling in psychological science: Intensive repeated measures designs. Retrieved from osf.io/74m5r 

# Install required libraries
pkgs <- c("lme4", "simr")
install.packages(pkgs, dependencies = T, clean = T)

# Load libraries
library(simr)

# Load data from OSF repository
dataf <- read.csv("Rdataframe.csv", stringsAsFactors = F)


# Model 1
m1 <- lmer(scc ~ naC + (naC|subj), dataf)
summary(m1)

fixef(m1)["naC"] <- -0.15

set.seed(2019)
p1 <- powerCurve(m1, along="subj", nsim=1000, breaks = seq(20, 100, 5))
save(p1, file = "p1.sim")

set.seed(2019)
p2<-powerCurve(m1, along="day", nsim=1000, breaks = seq(3, 14, 1))
save(p2, file = "p2.sim")


# Model 2
m2 <- lmer(scc ~ anxC + (1|subj), dataf)
summary(m2)

fixef(m2)["anxC"] <- 0.05

set.seed(2019)
p3 <- powerCurve(m2, along="subj", nsim=1000, breaks = seq(20, 100, 5))
save(p3, file = "p3.sim")

set.seed(2019)
p4<-powerCurve(m2, along="day", nsim=1000, breaks = seq(3, 14, 1))
save(p4, file = "p4.sim")


# Model 3
m3 <- lmer(tri ~ anxC * negC + (negC|subj), dataf)
summary(m3)

fixef(m3)["anxC:negC"] <- 0.01
set.seed(2019)
p5 <- powerCurve(m3, along="subj", nsim=1000, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))
save(p5, file = "p5.sim")

set.seed(2019)
p6<-powerCurve(m3, along="day", nsim=1000, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))
save(p6, file = "p6.sim")



fixef(m3)["anxC:negC"] <- 0.02
set.seed(2019)
p7 <- powerCurve(m3, along="subj", nsim=1000, breaks = seq(20, 100, 5), 
                 test=fixed("anxC:negC"))
save(p7, file = "p7.sim")

set.seed(2019)
p8<-powerCurve(m3, along="day", nsim=1000, breaks = seq(3, 14, 1),
               test=fixed("anxC:negC"))
save(p8, file = "p8.sim")

