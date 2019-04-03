vumc = read_csv("C:/Users/amrit/Downloads/VUMC Case Data.csv")
vumc$SurgDate = as.Date(vumc$SurgDate, "%m/%d/%Y")
vumc$DOW = as.factor(vumc$DOW)
# look at the data: line plot of # of surgeries
plot(vumc$SurgDate, vumc$Actual, type="l")
plot(vumc$SurgDate, vumc$`T - 28`, type="l")
# take out the holidays
dts = which(vumc$Actual > 30)
plot(vumc$SurgDate[dts], vumc$Actual[dts], type="l")

# Does day of week matter? look at averages
avg_dow <- aggregate(Actual ~ DOW, FUN = mean, data = vumc[dts, ])
barplot(avg_dow$Actual, names.arg = avg_dow$DOW)
#
# determine whether means are significantly different: Mon & Tue
t.test(Actual ~ DOW, data = vumc[vumc$DOW %in% c("Mon", "Tue") &
                                   vumc$Actual > 30, ])
# p-value is large so difference between Mon and Tue is not significant
# but what about other days...
t.test(Actual ~ DOW, data = vumc[vumc$DOW %in% c("Mon", "Fri") &
                                   vumc$Actual > 30, ])
# p-value < 0.01 so difference between Mon and Fri IS significant
# use DOW as a "factor" variable - categorical variable with n levels that
# R treats as n-1 "dummy" (binary) variables with values 0 or 1; regression
# coefficient is mean difference between a factor "level" and the
# "base" level which is the one not listed as a DOWvariable (it's Friday) 
dum <- lm(Actual ~ DOW, data = vumc[dts, ])
summary(dum)
# p-value for all the DOW coefficients are small, so we see all the
# other days are significantly different from Fri.
# Are they significantly different from Mon? Find out by forcing Mon to be
# base level by listing it as first level.
vumc$DOW <- factor(vumc$DOW, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
mdl.dum <- lm(Actual ~ DOW, data = vumc[dts, ])
summary(mdl.dum)
# So Thu and Fri are significantly different: Thu is 5.6 more and Fri is 
# 6.8 fewer on average
# Are emergency surgeries important? Assume they are added on surgery date.
vumc$Emergency <- vumc$Actual - vumc$`T - 1`
plot(vumc$SurgDate[dts], vumc$Emergency[dts], type="l")
# Emergency surgeries do not look special. Any DOW patter?
summary(lm(Emergency ~ DOW, data = vumc[dts, ]))
# similar to other data variation, no reason to model specially.
##############
# Question is prediction - how many days out is best? Look at several options.
# Primary measure of performance is the standard error:
summary(int7 <- lm(Actual ~ `T - 7`, data = vumc, subset = dts))
# Try including DOW:
summary(int7D <- lm(Actual ~ DOW + `T - 7`, data = vumc, subset = dts))
# DOW coeffs are significant and slight improvement of std. error,
# and R2adj is slightly higher so can include
# See whether adding a prior estimate helps.
summary(int710 <- lm(Actual ~ `T - 10` + `T - 7`, data = vumc, subset = dts))
# Again tiny improvement in std. error and slight increase in R2adj
# indicate can include prior date, but not a big difference.
# What about both DOW and prior date?
summary(int710D <- lm(Actual ~ DOW + `T - 10` + `T - 7`, data = vumc,
                     subset = dts))
