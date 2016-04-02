# My personal favorite graphics/number settings (not necessary)
par(oma=c(0,0,1,1)+0.01, mar=c(4,4,0,0)+0.01, pch=20)
options(scipen=7, digits=3)
set.seed(1234555)


yawn.data <- matrix(c(10,4,24,12), nrow=2, byrow=TRUE,
                dimnames = list(c("Yawned", "No Yawn"),
                                c("Yawn Seed", "No Seed")))
yawn.data

addmargins(yawn.data)

prop.table(yawn.data, margin=1)
prop.table(yawn.data, margin=2)

addmargins(prop.table(yawn.data, 1), 2)
addmargins(prop.table(yawn.data, 2), 1)

# If no association between yawn seed and yawning, then 14 yawners were just tired and would have yawned anyway, no matter which group they were assigned to.  

# The only variability in the number who yawn (or not) comes from the experimenters intervention: randomizing people to one group (seed) or the other (no seed).

# Set up "cards" to represent the 14 yawners (red) and 36 non-yawners (black)
cards <- c(rep("red", 14), rep("black", 36))
cards

# Shuffle (sample) 34 from the cards to represent randomization of people into the yawn seed treatment group
shuffle.ttt <- sample(cards, 34)
shuffle.ttt

# Count how many red cards (yawners) end up in this group by chance alone.
shuffle.ttt == "red"
sum(shuffle.ttt == "red")
ttt.count <- sum(shuffle.ttt == "red")
ttt.count

# Repeat this simulation of data (shuffle) 20 times
set.seed(12345)
ttt.count.20 <- replicate(20, sum(sample(cards, 34)=="red"))
ttt.count.20
# How often did we get the same result as in the Mythbusters' data?
sum(ttt.count.20 == 10)
##### YES.  That is a double equal sign (comparison, not assignment)
sum(ttt.count.20 == 10) / 20

# Is this the "probability" of getting 10 out of 14 yawners into the treatment group just by the random allocation of people to groups?

####################################################### 
# Time out!  What do we mean by the word "probability"?
####################################################### 

# Sequence of 10 coin flips (1 = "success" = "heads")
x <- cumsum(c(0,1,1,0,0,0,1,1,1,1))
x
n <- seq(from=1, to=10, by=1)
n

p <- x/n
p

plot(n, p, ylim=c(0,1), type="l")
abline(h=0.5, lty=2)

# How can we simulate coin flipping in the computer?
# The computer can generate random numbers between a and b
# So, the computer generates samples from the uniform density (model)

# Waiting for the bus...
a <- 0
b <- 10
fu <- 1/(b-a)
plot(c(a,b), c(fu,fu), bty="l", type="l", lwd=3, xlab="x", ylab="Uniform Density (Model)", main="")
lines(c(a,a),c(0,fu), lwd=3)
lines(c(b,b),c(0,fu), lwd=3)

# What is the chance I wait less than 3 minutes?
polygon(x=c(0,3,3,0,0), y=c(0,0,fu,fu,0), col="lightgrey")

# What is the chance I wait at least 8 minutes?
polygon(x=c(8,b,b,8,8), y=c(0,0,fu,fu,0), col="blue")

# The "standard" uniform distribution
# Useful for simulating so many things!!!

# Coin flips have Prob(heads)=0.5, Prob(tails)=0.5
a <- 0
b <- 1
fu <- 1/(b-a)
plot(c(a,b), c(fu,fu), bty="l", type="l", lwd=3, yaxt="n", xlab="x", ylab="Uniform Density (Model)", main="")
lines(c(a,a),c(0,fu), lwd=3)
lines(c(b,b),c(0,fu), lwd=3)

# How tall is this density?
plot(c(a,b), c(fu,fu), bty="l", type="l", lwd=3, xlab="x", ylab="Uniform Density (Model)", main="")
lines(c(a,a),c(0,fu), lwd=3)
lines(c(b,b),c(0,fu), lwd=3)

# Where is the median?
# What number has 50% of area to the left and right?
polygon(x=c(0,0.5,0.5,0,0), y=c(0,0,fu,fu,0), col="lightgrey")

# We know that Prob(number to the left of 0,5) = 0.5 = Prob(heads)
# and Prob(number to the left of 0,5) = 0.5 = Prob(tails)

# Let heads = random number is on the left of 0.5
# Let tails = random number is on the right of 0.5
# Simulate 10 coin flips
set.seed(123456)
xunif <- runif(10)
xunif

xflips <- 1 * (xunif <= 0.5)
xflips
x <- cumsum(xflips)
x
n <- seq(from=1, to=10, by=1)
n
p <- x/n
p

plot(n, p, type="l", ylim=c(0,1))
abline(h=0.5, lty=2)

###################################################
# Probability is the "long run" proportion
###################################################

# Do 100 simulated coin flips
set.seed(12345)
xunif <- runif(100)
xflips <- 1 * (xunif <= 0.5)
x <- cumsum(xflips)
n <- seq(from=1, to=100, by=1)
p <- x/n

plot(n, p, type="l", ylim=c(0,1))
abline(h=0.5, lty=2)

# Do 1,000 simulated coin flips
set.seed(123456)
xunif <- runif(1000)
xflips <- 1 * (xunif <= 0.5)
x <- cumsum(xflips)
n <- seq(from=1, to=1000, by=1)
p <- x/n

plot(n, p, type="l", ylim=c(0,1))
abline(h=0.5, lty=2)

####################################################### 
# OK.  Back to finding the probability of getting 10 out of 14 yawners into the treatment group just by the random allocation 
####################################################### 

set.seed(12345)
ttt.count.10000 <- replicate(10000, sum(sample(cards, 34)=="red"))
hist(ttt.count.10000, labels=TRUE, breaks=seq(0,14,1), right=FALSE, main="")
abline(v=10, lwd=5, lty=2)

# How statisticians measure "evidence" against the null hypothesis:
# What is the chance of getting data as (or more) "extreme" than the data we actually observed?
# "as extreme" means "as far from expected"
sum(ttt.count.10000 >= 10)
sum(ttt.count.10000 >= 10) / 10000

#########################################################
# A less restrictive null hypothesis
#########################################################

# If no association between yawn seed and yawning, then the PROPORTION of yawners in each group would be the same.  

# The only variability in the proportions who yawn (or not) comes from the experimenters intervention: randomizing people to one group (treatment=seed) or the other (control=no seed).

# Null hypothesis in symbols:   p1 = p2 = p 
# What do the symbols mean?
# Is p = 0.5?













p <- 14/50  # our best guess (using the observed data)
p

# Statistics = numbers calculated from the data (estimates for p1 and p2)
phat1.obs <- 10/34
phat2.obs <- 4/16
diff.obs <- phat1.obs - phat2.obs
cbind(phat1.obs, phat2.obs, diff.obs)

# Is this a "significant" difference or could it have occured by chance?

# According to the null hypothesis, each phat is the result of a observing 
# (1) n1=34 people from a population with proportion p yawners
# (2) n2=16 people from the same population with proportion p yawners

################################################
# How can we simulate other datasets that could have occured?
# (under the objective assumption that the treatment has no effect)
################################################

# Remember how we randomly flipped a fair coin using the uniform density
set.seed(12345)
xunif <- runif(1)
xunif
# heads=1 if the number is <= 0.5 (otherwise, tails=0)
1 * (xunif <= 0.5)

# and again
xunif <- runif(1);   xunif;   1 * (xunif <= 0.5)
# and again
xunif <- runif(1);   xunif;   1 * (xunif <= 0.5)
# and again
xunif <- runif(1);   xunif;   1 * (xunif <= 0.5)

# But, we do not want to use a fair coin this time.
# What should we use as the Prob(heads)
# = Prob(success) = P(person yawns) = p?










p <- 14/50  # our best guess (using the observed data)
p

# and repeat n1=34 times
set.seed(12345678)
xunif <- runif(34);   xunif;   1 * (xunif <= p)
# how many heads (yawners) did we happen to get at random (out of 34)?
sum(1 * (xunif <= p))
# what was the simulated proportion of yawners in the treatment group?
sum(1 * (xunif <= p)) / 34
# How does that compare with the estimated proportion in the population?
p

# Let's try this again (another simulated sample of 34 from the population)
xunif <- runif(34);  c(sum(1 * (xunif <= p)), sum(1 * (xunif <= p)) / 34, p)
# ...and again
xunif <- runif(34);  c(sum(1 * (xunif <= p)), sum(1 * (xunif <= p)) / 34, p)
# ...and again
xunif <- runif(34);  c(sum(1 * (xunif <= p)), sum(1 * (xunif <= p)) / 34, p)
# ...and again
xunif <- runif(34);  c(sum(1 * (xunif <= p)), sum(1 * (xunif <= p)) / 34, p)


# So, for the control (n2=16) and treatment (n1=34) groups, these phats could also have occurred
xunif1 <- runif(34)
phat1=sum(1 * (xunif1 <= p)) / 34
xunif2 <- runif(16)
phat2=sum(1 * (xunif2 <= p)) / 16
cbind(phat1, phat2, diff=phat1-phat2)
#... and again and again and again
# and what did we observe in the data?
cbind(phat1.obs, phat2.obs, diff.obs)

# What is the distribution of phat2 - phat1 
set.seed(123456)
phats1 <- replicate(1000, sum(1 * (runif(34) <= p)) / 34)
phats2 <- replicate(1000, sum(1 * (runif(16) <= p)) / 16)
diffs.sim <- phats1 - phats2
hist(diffs.sim, col="lightgrey", main="")
abline(v=diff.obs, lwd=3, lty=2)

# What proportion of simulated outcomes were as (or more) extreme as the observed outcome
sum( diffs.sim >= diff.obs)
sum( diffs.sim >= diff.obs) / 1000


###########################################################
# What is the distribution of possible outcomes when the null hypothesis is true?
###########################################################

# Where should this distribution be centered?
mean(diffs.sim)

# What is the spread of this distribution?
sd(diffs.sim)

# Could the distribution of simulated differences have come from a normal density?

# standardize the outcomes
z <- (diffs.sim - mean(diffs.sim)) / sd(diffs.sim) 
n <- 1000
# Make a fair comparison of the sampling distribution to the normal distribution
par(mfrow=c(3,3)) # set up a graphics window with 9 plots (3 x 3 grid)
lim <- c(-3.5, 3.5) # get ready to use the same x-axis and y-axis for each graph
qqnorm(z, pch=20, xlim=lim, ylim=lim, main="");  qqline(z)
text(-3.5, 3, "Obs Z-Scores", pos=4)
set.seed(1234)
for (i in 1:8) {
  z = rnorm(n)
  qqnorm(z, pch=20, xlim=lim, ylim=lim, main="");  qqline(z)
  text(-3.5, 3, paste("Normal Simulation", i), pos=4)
}
par(mfrow=c(1,1)) # return the settings to 1 graph per page

# So, could we use the normal density to estimate the chance of getting data as extreme or more as observed (if null hypothesis true)?

# Probability from normal density approximation
z <- diff.obs / sd(diffs.sim)
z
1 - pnorm(z)
# Probability from simulation
sum( diffs.sim >= diff.obs) / 1000



#######################################################
#######################################################
################# STOP HERE
#######################################################
#######################################################









#######################################################
# Probability model
# Coin flips: "equally likely" and counting to find probabilities
# Hypergeometric model
#######################################################

# Using the hypergeometric probability model...
# X = number of yawners randomly assigned to yawn seed is a "random variable"
# A random variable has a distribution
# (1) Possible values: x = 0, 1, 2, 3, ..., 14
# (2) Frequency (probability) of occurrance: p(10) = 
dhyper(10, m=14, n=36, k=34)

# p-value = P(X >= 10) = 
1 - phyper(9, m=14, n=36, k=34)
# p-value estimated from 10,000 simulations
sum(seed.count.1000 >= 10) / 1000
