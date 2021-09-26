########################################
# Basics of Survival Analysis
########################################


library(survival)

rats = read.csv("rats.csv")
rats = transform(rats, poisonconc = as.factor(poisonconc))
# Make KM plots for rat survival by sex and by dosage

# Rat survival by sex

ratsex = survfit(Surv(lifetimes, died) ~ sex, data = rats)
plot(ratsex, xlab = "Days", ylab = "Proportion Surviving", main = "Survival Curves for Rats by Sex", lwd = 1, lty = 2:3)
legend("topright", bty = 'n', legend = c("Female", "Male") ,lty = 2:3, lwd = 2)

# Hypothesis test for differences in survival based on sex. Since there are only two groups for sex, rejection of the null hypothesis unambiguously indiates that one sex survives better than the other.

survdiff(Surv(lifetimes, died) ~ sex, data = rats)

# Now we repeat the procedure for poison concentration
par(pty = 's')
ratconc = survfit(Surv(lifetimes, died) ~ poisonconc, data = rats)
plot(ratconc, xlab = "Days", ylab = "Proportion Surviving", main = "Kaplan Meyer Curves for rats by Concentration", lwd = 2, col = rainbow(6), mark.time = F)
abline(h = .5, lty = 2)
legend("topright", bty = 'n', legend = unique(rats$poisonconc) ,col = rainbow(6), lwd = 2)

# Since There are more than two groups, this test will not let us know which groups survive better than others. All we will be able to conclude is that it is not the case that they are all the same.



survdiff(Surv(lifetimes, died) ~ poisonconc, data = rats)


# cox PH regression

cphrats = coxph(Surv(lifetimes, died) ~ poisonconc + sex, data = rats)
summary(cphrats)



