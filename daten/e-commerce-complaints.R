#
# e-commerce-complaints
#
set.seed(6)
damageA <- round(rnorm(300, 0, 10)^2, 2)
damageA <- damageA - round(mean(damageA), 2) + 500
summary(damageA)
damageB <- round(runif(100, 0, 100), 2)
damageB <- 500 + c(damageB, -damageB)
summary(damageB)
dat <- rbind(data.frame(group = "A", damage = damageA),
			 data.frame(group = "B", damage = damageB))
summary(dat$damage)
boxplot(damage, horizontal = TRUE, data = dat)
boxplot(damage ~ group, horizontal = TRUE, data = dat)
	
write.csv(dat, "e-commerce-complaints.csv", row.names = FALSE)

head(read.csv("e-commerce-complaints.csv"))
