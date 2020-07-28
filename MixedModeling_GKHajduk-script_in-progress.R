######################################
#                                    #
#   Mixed effects modeling in R     #
#                                    #
######################################

## authors: Gabriela K Hajduk, based on workshop developed by Liam Bailey
## contact details: gkhajduk.github.io; email: gkhajduk@gmail.com
## date: 2017-03-09
##

###---- Explore the data -----###

## load the data and have a look at it

load("dragons.RData")

head(dragons)

## Let's say we want to know how the body length affects test scores.

## Have a look at the data distribution:

hist(dragons$testScore)  # seems close to normal distribution - good!

## It is good practice to  standardise your explanatory variables before proceeding - you can use scale() to do that:

dragons$bodyLength2 <- scale(dragons$bodyLength)

## Back to our question: is test score affected by body length?

###---- Fit all data in one analysis -----###

## One way to analyse this data would be to try fitting a linear model to all our data, ignoring the sites and the mountain ranges for now.

library(lme4)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## Let's plot the data with ggplot2

library(ggplot2)

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point()+
  geom_smooth(method = "lm")


### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line

plot(basic.lm, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad


## However, what about observation independence? Are our data independent?
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here

## We could also plot it colouring points by mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
    theme(legend.position = "none")

## From the above plots it looks like our mountain ranges vary both in the dragon body length and in their test scores. This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

## So what do we do?

###----- Run multiple analyses -----###


## We could run many separate analyses and fit a regression for each of the mountain ranges.

## Lets have a quick look at the data split by mountain range
## We use the facet_wrap to do that

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
    facet_wrap(~ mountainRange) +
    xlab("length") + ylab("test score")



##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different mountain ranges

## let's add mountain range as a fixed effect to our basic.lm

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

## now body length is not significant


###----- Mixed effects models -----###

test <- 1 + 3

##----- First mixed model -----##

### model

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

### plots

#### these plots will check the distribution of our linear mixed
#### model to check for normality

plot(mixed.lmer)  # looks alright, no patterns evident

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

### summary

### variance accounted for by mountain ranges

#### mountain range variance = 339.7. Divide that by the total 
#### (residual + mountain range) variance and you get ~60%
#### meaning that mountain range explains around 60% of the variance
#### "left over" after the variance explained by the fixed effect
#### body length 


##-- implicit vs explicit nesting --##

head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

### str() function shows us the breakdown of each variable in 
### our dataset; i.e. if it's numerical or factorial, how many
### levels of factor, also if variables have been scaled/standardized

### create new "sample" variable

#### currently the factor "site" is only implicitly nested: i.e. 
#### all site "a"s are treated as the same even though there
#### are 8 different site "a"s across 8 mountain ranges.
#### we want to explicitly nest these sites within their
#### associated mountain ranges so we make it clear there are 
#### actually 24 unique sample sites (3 sites x 8 mtn ranges)

dragons <- within(dragons, sample <- factor(mountainRange:site))

####this creates 24 unique sample locations: 3 sites x 8 mountain 
#### ranges, instead of 3 sample locations (sites a, b, and c)

##----- Second mixed model -----##

### model

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  # treats the two random effects as if they are crossed
summary(mixed.WRONG)

#### this model is WRONG;it treats site and mtn range as crossed
#### effects (i.e. only 3 sites across all 8 mtn ranges vs. 3 sites
#### per mountain range)

mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  
summary(mixed.lmer2) 

#### the syntax stays the same, but now the nesting is taken into
#### account. we use the "sample" variable created above that 
#### nests site WITHIN mountain range for 24 unique sites
#### (1|mountainRange/site) or (1|mountainRange) + (1|mountainRange:site)
#### would also be appropriate syntax

#### this model only allows for random intercepts, not random slopes
#### it assumes that the relationship b/w body length and test score
#### is the same across mountain ranges and sites

mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons) 

summary(mixed.ranslope)

#### this model allows for random slopes as well. i.e. we are 
#### acknowledging the BASELINE (intercept) and RELATIONSHIP
#### (slope) b.w. body length/test score may be different
#### across populations 


### summary

### plot

#### random intercept-fixed slope plot

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
  )

#### random intercept-random slope plot

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

##----- Summary of the above -----##

### the above steps walk us through acknowledging the structure
### of our experimental setup and how that may influence our results
### the plots show us that there is NO relationship between 
### body size and test scores

##----- Model selection for the keen -----##

### full model

### reduced model

### comparison
