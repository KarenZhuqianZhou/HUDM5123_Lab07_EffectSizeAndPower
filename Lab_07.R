# Load the acupuncture dataset in file acupuncture.Rdata

# Set default contrasts to deviation coding for 
# unordered factors
options(contrasts = c("contr.sum", "contr.poly"))
str(acupuncture)
acupuncture$group <- factor(acupuncture$group,
                            levels = c(0,1),
                            labels = c("C", "T"))
str(acupuncture)

# Calculate pk5 - pk1 pain difference scores.
acupuncture$diff <- acupuncture$pk5 - acupuncture$pk1

# Was the intervention effective? 1 = acup; 0 = not
lm1 <- lm(diff ~ group, data = acupuncture)
library(car)
Anova(lm1, type = 3)

library(emmeans)
(emm1 <- emmeans(object = lm1, specs = ~ group))
pairs(emm1)

# Note that the mean of the control group (pain scale
# difference after 1 year) is -4.37; mean difference for
# acupuncture group is -8.33. Group sample sizes:
table(acupuncture$group)

# Group sample variances (108.2080 and 161.5239)
by(data = acupuncture$diff, INDICES = acupuncture$group, FUN = var)

# install.packages("effsize")
library(effsize)
cohen.d(diff ~ group,
        data = acupuncture,
        pooled = TRUE)

# Verify calculations
s_pooled <- sqrt((139*108.2080 + 160*161.5239)/(140 + 161 - 2))
s_pooled # 11.6935
(d <- (-8.33 - -4.37)/11.6935)
# -.3386 agrees with above (small/moderate effect)

# Implement Hedges' correction
cohen.d(diff ~ group,
        data = acupuncture,
        pooled = TRUE,
        hedges.correction = TRUE)

g <- d*(1 - 3/(4*(140 + 161) - 9))
g # -.3378 agrees with above (though note the sign 
  # difference; they apparently switch the order of
  # subtraction in cohen.d() function)

# Effect of age on severity rating?
# Age range in study is 18 to 65.
hist(acupuncture$age, 
     xlab = "Age",
     main = "")
range(acupuncture$age)

# Create a categorical variable based on age terciles
acupuncture$age_3 <- cut(x = acupuncture$age, 
                         breaks = c(18, 39, 50, 65))
table(acupuncture$age_3)

# First, does age interact with treatment?
lm2 <- lm(diff ~ group*age_3, data = acupuncture)
Anova(lm2, type = 3)

emm2 <- emmeans(lm2, ~ group*age_3)
emmip(emm2, group ~ age_3)

# Interaction is not significant, though the
# intervention may have been more effective for younger
# participants.

# Back to main effect of age now. Omnibus test
# for main effect of age is significant. 
Anova(lm2, type = 3)

# Follow up with pairwise main effect comparisons.
# (Ignore the warning because we already checked
# the interaction)
emm3 <- emmeans(lm2, ~ age_3)
emm3
plot(emm3)

# Using Shaffer's planned post-omnibus modification,
# test all three pairwise comparisons at .05.
pairs(emm3, adjust = "none")

# Over the course of the study, pain decreased among
# 18 to 39 year olds by about 5 points more, on average,
# than it decreased among 50 to 65 year olds (t(294) = -2.86; 
# p-value = .005).

# Next, calculate Cohen's d for the comparison. First, 
# get group sample variances and group sizes.

by(data = acupuncture$diff, 
   INDICES = acupuncture$age_3,
   FUN = var)

# Var for 18-39 is 154.3251; var for 50-65 is 138.2354.

table(acupuncture$age_3)
emm3

s_pooled_2 <- sqrt((66*154.3251 + 113*138.2354)/(67 + 114 - 2))
s_pooled_2 # 12.0070
d_2 <- (-9.46 - -4.37)/12.0070
d_2 # -0.42 (moderate effect)

# Another way to calculate Cohen's d in an ANOVA design
# where a factor has more than one level is to use the
# square root of MSE, because, if the constant variance
# assumption holds, then MSE is a consistent estimator 
# for sigma squared, the common error variance. In other
# words, sqrt(MSE) is an extension of the pooled SD,
# s_p, for more than two groups.

# Thus, before proceeding with this method, check if 
# constant variance is tenable across age groups. Again,
# going back to the group sample variances:
by(data = acupuncture$diff, 
   INDICES = acupuncture$age_3,
   FUN = var)

# They differ at most by a factor of about 1.2; not bad.

# Also run Levene's test and find not significant.
leveneTest(y = acupuncture$diff, 
           group = acupuncture$age_3)

# Get MSE from ANOVA model.
Anova(lm4, type = 3)

(mse <- 39417/294) # 134.0714

d_3 <- (-9.46 - -4.37)/sqrt(134.0714)
d_3 # -.44 (also moderate)

# Contrast test of two older age groups against
# younger age group.
contrast(emm3, 
         method = list(c(1, -1/2, -1/2))) # two-tailed

# Contrast is significant (t(294) = -2.465; p-value = .014).
# Cohen's d for contrast can be calculated as contrast estimate
# divided by sqrt(MSE). But NOTE that this method only works if 
# sum of the absolute values of the contrast coefficients add up
# to 2. Here 1 + 1/2 + 1/2 = 2.

(d_4 <- -3.96 / sqrt(134.0714))
# -.34 (small effect)

# Now calculate eta squared for the overall effect of
# age on the change in pain.
Anova(lm4, type = 3)

(eta_sq <- 1103/(1457 + 1103 + 361 + 39417))
# .026 is small effect

# Partial eta squared for age
(eta_sq_p <- 1103/(1103 + 39417))
# .027 is also a small effect

# Eta squared for the contrast; first, get
# sum of squares for the contrast. 815.9342
(SS_contrast <- (-3.96)^2 / ( 1^2/67 + (-1/2)^2/119 + (-1/2)^2/114 ))

(eta_sq_contrast <- SS_contrast/(1457 + 1103 + 361 + 39417))
# 019 is a small effect

# Power analysis will use the pwr package.
# install.packages("pwr")
library(pwr)

# Recall from above that we estimated Cohen's d to be 
# about -.34 for the acupuncture effect on pain rating.
?pwr.t.test
pwr.t.test(d = -.34, sig.level = .05, power = .8)
# We need 137 *per group* to have 80% power.
pwr.t.test(d = -.34, sig.level = .05, power = .9)
# We need 183 *per group* to have 90% power.

# If there is uncertainty about the effect size, it
# can be instructive to make a power curve (i.e., a 
# plot of power across a number of effect sizes) for
# a specific sample size.
out50 <- pwr.t.test(d = seq(-.5, -.2, .01), n = 50)
out50$power
out100 <- pwr.t.test(d = seq(-.5, -.2, .01), n = 100)
out100$power
out150 <- pwr.t.test(d = seq(-.5, -.2, .01), n = 150)
out150$power
out200 <- pwr.t.test(d = seq(-.5, -.2, .01), n = 200)
out200$power
plot(x = seq(-.5, -.2, .01), 
     y = out50$power, 
     ylim = c(.15, 1.0),
     type = "l", lwd = 2,
     xlab = "Cohen's d",
     ylab = "Power",
     main = "Power Curves for n = 50, 100, 150, and 200 per Group")
points(x = seq(-.5, -.2, .01), 
       y = out100$power, 
       type = "l", 
       lty = 2, lwd = 2, col = 2)
points(x = seq(-.5, -.2, .01), 
       y = out150$power, 
       type = "l", 
       lty = 3, lwd = 2, col = 3)
points(x = seq(-.5, -.2, .01), 
       y = out200$power, 
       type = "l",
       lty = 4, lwd = 2, col = 4)
legend(x = "bottomleft", 
       legend = c(50, 100, 150, 200),
       lwd = 2, lty = 1:4, col = 1:4,
       seg.len = 4)


# Power analysis for eta-squared for age effect
# Recall from above that the complete eta squared 
# for age was .026. The software calls for f effect
# size. f is related to omega squared and we will use
# our eta squared as an estimate for omega squared.
f <- sqrt(.026/(1-.026))
f

pwr.anova.test(k = 3, f = .16, 
               sig.level = .05, 
               power = .8)
# 127 per group

