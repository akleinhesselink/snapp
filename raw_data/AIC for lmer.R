rm(list=ls())
setwd("C:\\Users\\Dursoa\\Documents\\Snapp\\CSchallenge\\data")
library(plyr)
library(lmer)
library(MuMIn)
library(car)
library(EnvStats)
library(emmeans)
# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

# abc is only 128 users who completed all 1000 images (distractors removed, skips removed)
#abc <- read.csv('data_for_lmer.csv')
#abc <- droplevels(abc[abc$sppseq < 6,]) # or 11 if all 1000 images wanted

#DT.sm100 had distractors & skips removed but includes users who did not complete all 1000 images
colSums(is.na(DT.sm100))
DT.sm100[is.na(DT.sm100$score1)==T,]
DT.sm100$user_region <- u.r[match(DT.sm100$user_id,u.r$user_id),"region"]
DT.sm100$user_region <-revalue(DT.sm100$user_region, c("north-america"="North America",
                                "south-america"="South America","asia"="Asia",
                                "africa"="Africa",'europe'='Europe',
                                'australasia-oceania'="Australasia/Oceania"))

lmerdata <- DT.sm100[,c("user_id","user_region","answer","global_region","difficulty",
                        "family","genus.corr","binomial.corr","mivs",
                        "sppseq","score1")]
colSums(is.na(lmerdata))
lmerdata <- droplevels(na.exclude(lmerdata))
lmerdata <- droplevels(lmerdata[lmerdata$sppseq < 6,]) # or 11 if all 1000 images wanted
#lmerdata$score1 <- ordered(lmerdata$score1) # error: response must be numeric
# ok to conceptualize distance between 0, 1, 2, and 3 as equal?
lmerdata$regionmatch <- 0
lmerdata$regionmatch[lmerdata$user_region==lmerdata$global_region] <- 1
table(lmerdata$family,lmerdata$score1)

qqPlot(lmerdata$sppseq)
hist(lmerdata$sppseq)
qqPlot(lmerdata$score1)
ks.test(lmerdata$sppseq,y='pnorm',alternative='two.sided') # ties present
ks.test(lmerdata$score1,y='pnorm',alternative='two.sided') # ties present
#BNobject <- bestNormalize(lmerdata$sppseq)
#lmerdata$ASsppseq <- arcsinh_x(lmerdata$sppseq)

lmerdata$global_region <- as.character(lmerdata$global_region)
lmerdata$global_region[lmerdata$global_region=='Australasia/Oceania'] <- "Aaustralia/Oceania"
lmerdata$global_region <- as.factor(as.character(lmerdata$global_region))

lmerdata$family <- as.character(lmerdata$family)
#lmerdata$family[lmerdata$family=='Pythonidae'] <- "APythonidae"
lmerdata$family[lmerdata$family=='Pythonidae'] <- "Boidae&Pythonidae"
lmerdata$family[lmerdata$family=='Boidae'] <- "Boidae&Pythonidae"
lmerdata$family[lmerdata$family=='Leptotyphlopidae'] <- "Scolecophidia"
lmerdata$family[lmerdata$family=='Typhlopidae'] <- "Scolecophidia"
lmerdata$family <- as.factor(as.character(lmerdata$family))
unique(lmerdata$family)

m0 <- lmer(score1 ~ (1|user_id), data = lmerdata)
m1 <- lmer(score1 ~ sppseq + (sppseq|user_id), data = lmerdata)
m2 <- lmer(score1 ~ difficulty + (1|user_id), data = lmerdata)
m3 <- lmer(score1 ~ regionmatch + (1|user_id), data = lmerdata)
m4 <- lmer(score1 ~ global_region + (1|user_id), data = lmerdata)
m5 <- lmer(score1 ~ family + (1|user_id), data = lmerdata)
m6 <- lmer(score1 ~ sppseq + difficulty + (1 + sppseq|user_id), data = lmerdata)
m7 <- lmer(score1 ~ sppseq + global_region + (1 + sppseq|user_id), data = lmerdata)
m8 <- lmer(score1 ~ sppseq + family + (1 + sppseq|user_id), data = lmerdata)
m9 <- lmer(score1 ~ sppseq + regionmatch + (1 + sppseq|user_id), data = lmerdata)
m10 <- lmer(score1 ~ difficulty + global_region + (1|user_id), data = lmerdata)
m11 <- lmer(score1 ~ difficulty + family + (1|user_id), data = lmerdata)
m12 <- lmer(score1 ~ global_region + family + (1|user_id), data = lmerdata)
m12 <- lmer(score1 ~ difficulty + global_region + family + (1|user_id), data = lmerdata)
m12 <- lmer(score1 ~ sppseq + family + global_region + (sppseq|user_id), data = lmerdata)
m13 <- lmer(score1 ~ sppseq + family + difficulty + (sppseq|user_id), data = lmerdata)
m14 <- lmer(score1 ~ sppseq + global_region + difficulty + (sppseq|user_id), data = lmerdata)
m15 <- lmer(score1 ~ sppseq + difficulty + global_region + family + regionmatch + (sppseq|user_id), data = lmerdata)
m16 <- lmer(score1 ~ sppseq + difficulty + global_region + family + (sppseq|user_id), data = lmerdata)

#m17 <- lmer(score1 ~ sppseq * difficulty + global_region + family + (1 + sppseq|user_id), data = lmerdata)
#m18 <- lmer(score1 ~ sppseq * difficulty * global_region + family + (1 + sppseq|user_id), data = lmerdata)
#m19 <- lmer(score1 ~ sppseq * difficulty * global_region * family + (1 + sppseq|user_id), data = lmerdata)
# m20 was interaction with random effect, same AIC
#m21 <- lmer(score1 ~ sppseq * difficulty * regionmatch + global_region + family + (sppseq|user_id), data = lmerdata)

AIC(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16)
Weights(AIC(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16))

summary(m21)
coef(summary(m15)); write.csv(coef(summary(m15)),"coef_summary_m15.csv")
VarCorr(m15)
str(resid(m15)) # random effects
plot(m15) # should be equal numbers above and below 0
qqnorm(resid(m15)) # should form a line
qqline(resid(m15)) # few divations above and below line

table(lmerdata$difficulty,lmerdata$sppseq)

ggplot(fortify(m15), aes(sppseq, score1, color=global_region)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line") +
  ylim(0,3) + geom_hline(yintercept=c(1,2),linetype=2) +
  labs(x='image sequence',y='identification accuracy') +
  facet_grid(difficulty ~ regionmatch,
             labeller = labeller(
               regionmatch = c(`0` = "away game", `1` = "home-team advantage")
              # yfacet = c(`10` = "an y label", `20` = "another y label")
             ))

SAmhardhome <- lmerdata[which(global_region=='South America' & regionmatch == 1 & difficulty == 'hard'),]
# only one person

ggplot(fortify(m15), aes(sppseq, score1, color=family)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line") +
  ylim(0,3) + geom_hline(yintercept=c(1,2),linetype=2) +
  labs(x='image sequence',y='identification accuracy') +
  facet_grid(difficulty ~ regionmatch,
             labeller = labeller(
               regionmatch = c(`0` = "away game", `1` = "home-team advantage")
               # yfacet = c(`10` = "an y label", `20` = "another y label")
             ))

## TODO: think about how to use stan or one of these:
# https://stats.stackexchange.com/questions/238581/how-to-use-ordinal-logistic-regression-with-random-effects
# https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf
# problem is that you're assuming RV exists on a continuous number line from -Inf to Inf when really it's bounded at 0 and 3 and only discrete

## TODO: think about normality/assumptions/transformation for sppseq and score1
## TODO: add interactions? does global_region x family make sense? sensitive to choice of taxa? sensititve to participant learning
## TODO: combine blindsnake families and combine boas & pythons? if yes, consider both families correct?
## TODO: eliminate sppseq > 5 to take out effect of user choice of difficulty (but also OK to leave in)
## TODO: user_region always nested in random? can I compare AIC if not? 'singular fit' appears
## TODO: is the 1 necessary in random effects
## TODO: considered same global region as random nested, but decided against