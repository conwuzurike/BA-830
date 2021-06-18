library(data.table)
library(tidyverse)
got_ads_data <- fread('ads_noads.csv', stringsAsFactors = FALSE)
got_ads_data

# Explore the data: Notice there are two modes one at 0 and one that's high. 
hist(got_ads_data[, po.noad], breaks = 10)

# Let's assume that there is NO effect.
# Then we can fill in the potential outcome if there's an ad. 
true.effect <- 4



# Calculate the true ATE
true.effect <- 4
data$po.wad <- data$po.noad + true.effect

mean(got_ads_data$po.noad)
mean(got_ads_data$po.wad)

mean(got_ads_data$po.wad) - mean(got_ads_data$po.noad) # True ATE

# What if we did an experiment?
set.seed(321)
num.people <- nrow(got_ads_data)

treatment <- sample(rep(c(1,0), num.people/2))
treatment

outcome <- ifelse(treatment == 1, data$po.wad, data$po.noad)

mean(outcome[treatment == 1]) - mean(outcome[treatment == 0])

# What if we had gotten a different randomization?
treatment <- sample(rep(c(1,0), num.people/2))
outcome <- ifelse(treatment == 1, data$po.wad, data$po.noad)
mean(outcome[treatment==1]) - mean(outcome[treatment==0])

# Write our own function that "runs one experiment"
rand.est.ate <- function() {
  treatment <- sample(rep(c(1,0), num.people/2))
  outcome <- ifelse(treatment == 1, data$po.wad, data$po.noad)
  return(mean(outcome[treatment==1]) - mean(outcome[treatment==0]))
}
rand.est.ate()
rand.est.ate()
rand.est.ate()


# Samping distribution
samp.dist <- replicate(1000, rand.est.ate())
samp.dist <- as.data.table(samp.dist)
samp.dist
ggplot(samp.dist, aes(x = samp.dist)) + geom_histogram() + abline(v=true.effect, lwd = 5) + geom_vline(xintercept = true.effect, color = 'red')

# First, let's create the data we would have from a particular experiment.
set.seed(246)
actual.treatment <- sample(rep(c(1,0), num.people/2)) #Single randomization
outcome <- ifelse(actual.treatment == 1, data$po.wad, data$po.noad) #Leads to single set of observed values.

obsdata <- data.frame(got_ads_data$fullname, actual.treatment, outcome) #Observed data does not contain potential outcomes.
obsdata

# And we can estimate a single ATE:
our.ate <- mean(obsdata$outcome[obsdata$actual.treatment == 1]) -
  mean(obsdata$outcome[obsdata$actual.treatment == 0])
our.ate


# Our goal is to prove the skeptic wrong. So let's assume the skeptic is right.
# How likely would we be to see this effect estimate by chance?
obsdata$outcome

rand.est.ate <- function(outcomes) {
  some.fake.treatment <- sample(rep(c(1,0), num.people/2))
  return(mean(outcomes[some.fake.treatment == 1]) - 
           mean(outcomes[some.fake.treatment == 0]))
}
rand.est.ate(obsdata$outcome)

distribution.under.sharp.null <- replicate(10000, rand.est.ate(obsdata$outcome))
distribution.under.sharp.null <- as.data.table(distribution.under.sharp.null)

# Compare our single ATE estimate against this distribution
ggplot(distribution.under.sharp.null, aes(x = distribution.under.sharp.null)) + geom_histogram() + abline(v=true.effect, lwd = 5) + geom_vline(xintercept = true.effect, color = 'red')


# What's the probability we would have gotten a value that high under the null hypothesis?
mean(distribution.under.sharp.null >= our.ate)

# Under a different randomization, we might get unlucky though...
set.seed(123)

treatment <- sample(rep(c(1,0), num.people/2))
observed.outcome <- ifelse(treatment == 1, data$po.wad, data$po.noad)
observed.outcome
our.ate <- mean(observed.outcome[treatment == 1]) - mean(observed.outcome[treatment == 0])
our.ate

distribution.under.sharp.null <- as.data.table(distribution.under.sharp.null)


#Compare our single ATE estimate against this distribution
ggplot(distribution.under.sharp.null, aes(x = distribution.under.sharp.null)) + geom_histogram() + abline(v=true.effect, lwd = 5) + geom_vline(xintercept = true.effect, color = 'red')

# How big does the estimate have to be for us to reject?
mean(distribution.under.sharp.null > our.ate)

quantile(distribution.under.sharp.null$distribution.under.sharp.null, .95)

ggplot(distribution.under.sharp.null, aes(x = distribution.under.sharp.null)) + geom_histogram() + abline(v=true.effect, lwd = 5) + geom_vline(xintercept = true.effect, color = 'red')

#Let's look at this more systematically. This function allows us to simulate a study with a given effect size and number of people.
simulate.study <- function(true.effect, num.people){
  po.noad <- round(runif(num.people, min = 50, max = 80))
  po.wad <- po.noad + true.effect
  treatment <- sample(rep(c(1,0), num.people/2))
  outcome <- ifelse(treatment == 1, po.wad, po.noad)
  our.ate <- mean(outcome[treatment == 1]) - mean(outcome[treatment == 0])
  distribution.under.sharp.null <- replicate(500, rand.est.ate(outcome))
  pval <- mean(distribution.under.sharp.null > our.ate)
  return(pval)
}

simulate.study(true.effect = 4, num.people = 20)

p.values <- replicate(500, simulate.study(true.effect = 4, num.people = 20))
p.values <- as.data.table(p.values)
ggplot(p.values, aes(x = p.values)) + geom_histogram() + abline(v=true.effect, lwd = 5) + geom_vline(xintercept = .05, color = 'red')

# Depending on our randomization, our p-value will be different
# The probability that we find a small enough p-value to reject the
# null is our power. What is our power in this particular study?
mean(p.values <= 0.05)

# What increases our power?

# Treatment effect size: If ad effect was bigger
p.values <- replicate(500, simulate.study(true.effect = 16, num.people = 20))
mean(p.values <= 0.05)


# Sample size: If we recruited more subjects
p.values <- replicate(500, simulate.study(true.effect = 4, num.people = 100))
mean(p.values <= 0.05)

# Sanity check: If the true effect were actually 0,
# About 5% of the pvalues should be below 0.05
p.values <- replicate(1000, simulate.study(true.effect = 0, num.people = 20))
mean(p.values <= 0.05)


# Standard errors and confidence intervals -------
num.people <- 100
po.noad <- round(runif(num.people, min = 50, max = 80))
po.wad <- po.noad + true.effect

treatment <- sample(rep(c(1,0), num.people/2))
outcome <- ifelse(treatment == 1, po.wad, po.noad)
treatment
outcome

est.ate.rand <- function(treatment) mean(po.wad[treatment==1]) - mean(po.noad[treatment==0])

samp.dist.100 <- as.data.table(replicate(10000, est.ate.rand(sample(rep(c(1,0), num.people/2)))))
this_plot <- ggplot(samp.dist.100, aes(x = V1)) + geom_histogram() + geom_vline(xintercept = true.effect, lwd = 1, color = 'red') + xlab("ATE HAT")
this_plot
this_sd <- sd(samp.dist.100$V1)

# Confidence Interval
this_plot + geom_vline(xintercept = true.effect + 1.96*this_sd, lwd = 1, color = 'blue') + geom_vline(xintercept = true.effect - 1.96*this_sd, lwd = 1, col = 'blue')

# 95% of the time, estimate within this range
mean(samp.dist.100 > true.effect - 1.96*this_sd & samp.dist.100 < true.effect + 1.96*this_sd)

# Can also get this interval from regression.
regmod <- lm(outcome ~ treatment)
summary(regmod)
