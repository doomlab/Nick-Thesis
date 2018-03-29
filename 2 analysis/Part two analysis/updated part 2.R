####setup####
##load libraries and data
library(nlme)
library(lme4)
library(cowplot)
library(MOTE)
library(ggplot2)
library(MASS)
library(relaimpo)

dat = read.csv("master.csv")

options(scipen = 999)
options(max.print = 9999)

##cleanup code for graphs
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

####data screening will go below#####
##accuracy
summary(dat)

table(dat$Judged.Value)

dat$Judged.Value[ dat$Judged.Value > 100 ] = NA
summary(dat$Judged.Value)

##missing data
table("judge" = is.na(dat$Judged.Value), "recall" = is.na(dat$Recall))
##12444 data points being used, 1479 excluded

nrow(na.omit(dat))

##outliers
mahal = mahalanobis(dat[ , c(4,5)], 
                    colMeans(dat[ , c(4,5)], na.rm = TRUE),
                    cov(dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(dat[ , c(4,5)]))
cutoff;ncol(dat[ , c(4,5)])
summary(mahal < cutoff) 
noout = subset(dat, mahal < cutoff)


##additivity
cor(noout[ , c(4:15, 17:31, 33:46)], use = "pairwise.complete.obs")

##cutting out morphemes, syllables, and phonemes

####descriptives####
m.recall = tapply(noout$Recall, noout$Judgment, mean)
sd.recall = tapply(noout$Recall, noout$Judgment, sd)

m.judge = tapply(noout$Judged.Value, noout$Judgment, mean)
sd.judge = tapply(noout$Judged.Value, noout$Judgment, sd)

m.recall;sd.recall
m.judge;sd.judge

##mean center variables
noout$ZCOS = scale(noout$COS, scale = F)
noout$ZLSA = scale(noout$LSA, scale = F)
noout$ZFSG = scale(noout$FSG, scale = F)

##create the right scaling 
noout$Judged.Value2 = noout$Judged.Value/100

####replicating interactions from pilot -- judgments####
overall.judge = lme(Judged.Value2 ~ Judgment + 
                      ZCOS * ZLSA * ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(overall.judge) #partial replication, may be due to sampling error, etc

##three-way is not significant, but two-ways are

####moderation -- judgments####
##break down cos
noout$ZCOS_low = noout$ZCOS + sd(noout$ZCOS, na.rm = TRUE)
noout$ZCOS_high = noout$ZCOS - sd(noout$ZCOS, na.rm = TRUE)

##low cosine
lowcos = lme(Judged.Value2 ~ Judgment +
               ZCOS_low * ZLSA * ZFSG,
             data = noout,
             method = "ML",
             na.action = "na.omit",
             random = ~1|Partno)
summary(lowcos)

##high cosine
highcos = lme(Judged.Value2 ~ Judgment +
                ZCOS_high * ZLSA * ZFSG,
              data = noout,
              method = "ML",
              na.action = "na.omit",
              random = ~1|Partno)
summary(highcos)

##now splitting by LSA
noout$ZLSA_low = noout$ZLSA + sd(noout$ZLSA, na.rm = TRUE)
noout$ZLSA_high = noout$ZLSA - sd(noout$ZLSA, na.rm = TRUE)

##low cosine, low lsa
lowcoslowlsa = lme(Judged.Value2 ~ Judgment +
                     ZCOS_low * ZLSA_low * ZFSG, 
                   data = noout, 
                   method = "ML", 
                   na.action = "na.omit",
                   random = ~1|Partno)
summary(lowcoslowlsa)

##low high
lowcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS_low  * ZLSA_high * ZFSG,
                    data = noout,
                    method = "ML",
                    na.action = "na.omit",
                    random = ~1|Partno)

summary(lowcoshighlsa)

#avg low
avgcoslowlsa = lme(Judged.Value2 ~ Judgment +
                     ZCOS * ZLSA_low * ZFSG,
                   data = noout,
                   method = "ML",
                   na.action = "na.omit",
                   random = ~1|Partno)

summary(avgcoslowlsa)

##avg high
avgcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS  * ZLSA_high * ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(avgcoshighlsa)

##high low
highcoslowlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS_high * ZLSA_low * ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(highcoslowlsa)

##high high
highcoshighlsa = lme(Judged.Value2 ~ Judgment +
                       ZCOS_high  * ZLSA_high * ZFSG, 
                     data = noout, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(highcoshighlsa)

####judgment graphs####
##low cos
plot1 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.564, slope = 0.575, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.612, slope = 0.472, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.659, slope = 0.369, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot2 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.588, slope = 0.514, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.615, slope = 0.422, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.641, slope = 0.330, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS")

##high cos
plot3 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.612, slope = 0.453, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.618, slope = 0.372, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.624, slope = 0.292, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot1)
judge.graph = plot_grid( plot1 + theme(legend.position="none"),
                           plot2 + theme(legend.position="none"),
                           plot3 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
judge.graph

##judgments using the new dataset are competitive at all levels of COS
##partial replication -- competitiveness replicates, complimentary aspect at high does not

####replicating interactions from pilot -- Recall####
overall.recall = glmer(Recall ~ (1|Partno) + Judgment + 
                         Judged.Value2 + ZCOS * ZLSA * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(overall.recall) ##three way interaction still exists!

####moderation -- Recall####
#low cosine
lowcos2 = glmer(Recall ~ (1|Partno) +
                  Judgment + Judged.Value2 + ZCOS_low * ZLSA * ZFSG,
                data = noout,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)

##high cosine
hicos2 = glmer(Recall ~ (1|Partno) + 
                 Judgment + Judged.Value2 + ZCOS_high * ZLSA * ZFSG,
               data = noout,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)

summary(lowcos2)
summary(hicos2)

##low cosine low lsa
lowcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                        Judgment + Judged.Value2 + ZCOS_low * ZLSA_low * ZFSG,
                      data = noout,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 1)
summary(lowcoslowlsa2)

##low cosine high lsa
lowcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS_low * ZLSA_high * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowcoshighlsa2)

##high low
highcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS_high * ZLSA_low * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(highcoslowlsa2)

##high high
highcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                          Judgment + Judged.Value2 + ZCOS_high * ZLSA_high * ZFSG,
                        data = noout,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highcoshighlsa2)

avgcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                        Judgment + Judged.Value2 + ZCOS * ZLSA_low * ZFSG,
                      data = noout,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 1)
summary(avgcoslowlsa2)

avgcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS * ZLSA_high * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1) 
summary(avgcoshighlsa2)

####Recall graphs####
##low cos
plot4 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.162, slope = 0.087, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.118, slope = 1.213, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.074, slope = 2.339, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot5 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.167, slope = 1.993, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.303, slope = 1.800, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.440, slope = 1.606, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS")

##high cos
plot6 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .169, slope = 3.900, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .487, slope = 2.386, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .806, slope = 0.872, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot4)
recall.graph =  plot_grid( plot4 + theme(legend.position="none"),
                           plot5 + theme(legend.position="none"),
                           plot6 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
recall.graph

##FSG slopes get stronger as LSA increases at low COS
##FSG gets weaker as LSA increases at higher cosines.
##this is the opposite of the pilot..

####Combined Data set -- data creation####
pilot.dat = read.csv("Melted Data pilot.csv")

##data screening
##accuracy
summary(pilot.dat)

pilot.dat$Judged.Value[ pilot.dat$Judged.Value > 100 ] = NA
summary(pilot.dat$Judged.Value)

##missing pilot.data
table("judge" = is.na(pilot.dat$Judged.Value), "recall" = is.na(pilot.dat$Recall))

##outliers
mahal = mahalanobis(pilot.dat[ , c(4,5)], 
                    colMeans(pilot.dat[ , c(4,5)], na.rm = TRUE),
                    cov(pilot.dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(pilot.dat[ , c(4,5)]))
cutoff;ncol(pilot.dat[ , c(4,5)])
summary(mahal < cutoff) 
noout2 = subset(pilot.dat, mahal < cutoff)

##mean centering network norms
noout2$ZCOS = scale(noout2$COS, scale = F)
noout2$ZLSA = scale(noout2$LSA, scale = F)
noout2$ZFSG = scale(noout2$FSG, scale = F)

##create the right scaling 
noout2$Judged.Value2 = noout2$Judged.Value/100

##add in variables for moderations
noout2$ZCOS_low = noout2$ZCOS + sd(noout2$ZCOS, na.rm = TRUE)
noout2$ZCOS_high = noout2$ZCOS - sd(noout2$ZCOS, na.rm = TRUE)
noout2$ZLSA_low = noout2$ZLSA + sd(noout2$ZLSA, na.rm = TRUE)
noout2$ZLSA_high = noout2$ZLSA - sd(noout2$ZLSA, na.rm = TRUE)

##combining pilot w/ part2
colnames(noout2)[2] = "Judgment"
combined = rbind(noout, noout2)

summary(combined)

##fix POS
combined$POS.2 = gsub("JJ", "ADJ", combined$POS.2)
combined$POS.2 = as.factor(combined$POS.2)
print(levels(combined$POS.2))

combined$POS.2 = factor(combined$POS.2,levels(combined$POS.2)[c(2, 1, 3, 4)])
print(levels(combined$POS.2))
                        
combined$POS.1 = factor(combined$POS.1,levels(combined$POS.1)[c(2, 1, 3, 4)])
print(levels(combined$POS.1))

##fix judgment type
summary(combined$Judgment)

combined$Judgment = gsub("semantic", "Semantic", combined$Judgment)
combined$Judgment = gsub("associative", "Associative", combined$Judgment)
combined$Judgment = gsub("thematic", "Thematic", combined$Judgment)

combined$Judgment = as.factor(combined$Judgment)

summary(combined$Judgment)
##notes for sw models:
##only using length, using log subtlex

####combined data --  judgments####
##stepwise to find best predictors

##need to remove missing
nomiss = na.omit(combined)

#word characteristics
overalljudge.step1 = lm(Judged.Value2 ~ Judgment + 
                          LogSub.1 + LogSub.2 +
                          Length.1 + Length.2 +
                          POS.1 + POS.2,
                        data = nomiss) 
stepAIC(overalljudge.step1, direction = "both")$anova
#best predictors: All of them

#rated properties
overalljudge.step2 = lm(Judged.Value2 ~ Judgment +
                          AOA.1 +  AOA.2 +  Familiarity.1 +
                          Familiarity.2 +  Valence.1 +  Valence.2 + 
                          Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2,
                        data = nomiss) 
stepAIC(overalljudge.step2, direction = "both")$anova
#best predictors: all of them

##semantics
overalljudge.step3 = lm(Judged.Value2 ~ Judgment +  
                          QSS.1 +  TSS.2 +  FSS.1 +
                          FSS.2 +  COSC.1 +  COSC.2 +  Ortho.1 + 
                          Ortho.2 +  Phono.1 +  Phono.2,
                        data = nomiss)
stepAIC(overalljudge.step3, direction = "both")$anova
#best predictors: QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1

##overall model using best predictors
##this will be done hierachically

##lexical properties
##pos makes model blow up
judgeoverall.1 = lme(Judged.Value2 ~ Judgment + 
                      LogSub.1 + LogSub.2 + Length.1 + Length.2, #POS.1 + POS.2,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(judgeoverall.1, correlation = T)

##rated properties
judgeoverall.2 = lme(Judged.Value2 ~ Judgment + 
                       LogSub.1 + LogSub.2 + Length.1 + Length.2 + # POS.1 + POS.2,
                       AOA.1 +  AOA.2 +  Familiarity.1 +
                       Familiarity.2 +  Valence.1 +  Valence.2 + 
                       Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(judgeoverall.2, correlation = T)

##network connections
judgeoverall.3 = lme(Judged.Value2 ~ Judgment + 
                       LogSub.1 + LogSub.2 + Length.1 + Length.2 + # POS.1 + POS.2,
                       AOA.1 +  AOA.2 +  Familiarity.1 +
                       Familiarity.2 +  Valence.1 +  Valence.2 + 
                       Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                       QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(judgeoverall.3, correlation = T)

##network norms
judgeoverall.4 = lme(Judged.Value2 ~ Judgment + 
                       LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                       AOA.1 +  AOA.2 +  Familiarity.1 +
                       Familiarity.2 +  Valence.1 +  Valence.2 + 
                       Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                       QSS.1 +  TSS.2 +  FSS.1 +
                       QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                       ZFSG * ZLSA * ZCOS,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(judgeoverall.4, correlation = T)
##three way is not sig, two of the two ways are, FSG is still strongest predictor.
##these results replicate judgment results from just network norms

####moderation -- sw judgments####
##low cosine
lowcos3 = lme(Judged.Value2 ~ Judgment + 
                LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                AOA.1 +  AOA.2 +  Familiarity.1 +
                Familiarity.2 +  Valence.1 +  Valence.2 + 
                Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                QSS.1 +  TSS.2 +  FSS.1 +
                QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                ZFSG * ZLSA * ZCOS_low,
              data = combined,
              method = "ML", 
              na.action = "na.omit",
              random = ~1|Partno)
summary(lowcos3)

##high cosine
highcos3 = lme(Judged.Value2 ~ Judgment + 
                 LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                 AOA.1 +  AOA.2 +  Familiarity.1 +
                 Familiarity.2 +  Valence.1 +  Valence.2 + 
                 Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                 QSS.1 +  TSS.2 +  FSS.1 +
                 QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                 ZFSG * ZLSA * ZCOS_high,
               data = combined,
               method = "ML", 
               na.action = "na.omit",
               random = ~1|Partno)
summary(highcos3)

##low cosine, low lsa
lowcoslowlsa3 = lme(Judged.Value2 ~ Judgment + 
                      LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                      AOA.1 +  AOA.2 +  Familiarity.1 +
                      Familiarity.2 +  Valence.1 +  Valence.2 + 
                      Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                      QSS.1 +  TSS.2 +  FSS.1 +
                      QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                      ZFSG * ZLSA_low * ZCOS_low,
                    data = combined,
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(lowcoslowlsa3)

##low high
lowcoshighlsa3 = lme(Judged.Value2 ~ Judgment + 
                       LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                       AOA.1 +  AOA.2 +  Familiarity.1 +
                       Familiarity.2 +  Valence.1 +  Valence.2 + 
                       Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                       QSS.1 +  TSS.2 +  FSS.1 +
                       QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                       ZFSG * ZLSA_high * ZCOS_low,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(lowcoshighlsa3)

#avg low
avgcoslowlsa3 = lme(Judged.Value2 ~ Judgment + 
                      LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                      AOA.1 +  AOA.2 +  Familiarity.1 +
                      Familiarity.2 +  Valence.1 +  Valence.2 + 
                      Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                      QSS.1 +  TSS.2 +  FSS.1 +
                      QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                      ZFSG * ZLSA_low * ZCOS,
                    data = combined,
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(avgcoslowlsa3)

##avg high
avgcoshighlsa3 = lme(Judged.Value2 ~ Judgment + 
                       LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                       AOA.1 +  AOA.2 +  Familiarity.1 +
                       Familiarity.2 +  Valence.1 +  Valence.2 + 
                       Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                       QSS.1 +  TSS.2 +  FSS.1 +
                       QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                       ZFSG * ZLSA_high* ZCOS,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(avgcoshighlsa3)

##high low
highcoslowlsa3 = lme(Judged.Value2 ~ Judgment + 
                       LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                       AOA.1 +  AOA.2 +  Familiarity.1 +
                       Familiarity.2 +  Valence.1 +  Valence.2 + 
                       Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                       QSS.1 +  TSS.2 +  FSS.1 +
                       QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                       ZFSG * ZLSA_low * ZCOS_high,
                     data = combined,
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(highcoslowlsa3)

##high high
highcoshighlsa3 = lme(Judged.Value2 ~ Judgment + 
                        LogSub.1 + LogSub.2 + Length.1 +Length.2 + # POS.1 + POS.2,
                        AOA.1 +  AOA.2 +  Familiarity.1 +
                        Familiarity.2 +  Valence.1 +  Valence.2 + 
                        Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2 +
                        QSS.1 +  TSS.2 +  FSS.1 +
                        QSS.1 + TSS.2 + FSS.2 + FSS.1 + COSC.2 + Ortho.2 + Phono.1 +
                        ZFSG * ZLSA_high * ZCOS_high,
                      data = combined,
                      method = "ML", 
                      na.action = "na.omit",
                      random = ~1|Partno)
summary(highcoshighlsa3)

####judgment graphs####
##low cos
plot7 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.818, slope = 0.524, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.866, slope = 0.419, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.914, slope = 0.309, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot8 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.847, slope = 0.469, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.874, slope = 0.391, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.901, slope = 0.311, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS")

##high cos
plot9 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.877, slope = 0.409, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.883, slope = 0.361, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.888, slope = 0.316, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot7)
judge.graph.sw = plot_grid( plot7 + theme(legend.position="none"),
                         plot8 + theme(legend.position="none"),
                         plot9 + theme(legend.position="none"),
                         legend,
                         hjust = -1,
                         nrow = 2
)
judge.graph.sw

####combined data -- recall####
##stepwise to find best predictors
#word characteristics

overallrecall.step1 = glm(Recall ~ Judgment + Judged.Value2 +
                          LogSub.1 + LogSub.2 +
                          Length.1 + Length.2 +
                          POS.1 + POS.2,
                        data = nomiss,
                        family = binomial) 
stepAIC(overallrecall.step1, direction = "both")$anova
#best predictors: all the things...

#rated properties
overallrecall.step2 = glm(Recall ~ Judgment + Judged.Value2 +
                          AOA.1 +  AOA.2 +  Familiarity.1 +
                          Familiarity.2 +  Valence.1 +  Valence.2 + 
                          Imageability.1 +  Imageability.2 +  QCON.1 +  QCON.2,
                        data = nomiss,
                        family = binomial)
stepAIC(overallrecall.step2, direction = "both")$anova
#best predictors: AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + QCON.2

##semantics
overallrecall.step3 = glm(Recall ~ Judgment + Judged.Value2 +
                           QSS.1 +  TSS.2 +  FSS.1 +
                           FSS.2 +  COSC.1 +  COSC.2 +  Ortho.1 + 
                           Ortho.2 +  Phono.1 +  Phono.2,
                         data = nomiss,
                         family = binomial)
stepAIC(overallrecall.step3, direction = "both")$anova
#best predictors: QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2

##hierarchical models based on stepwise results.
##lexical information
##not using pos since it was removed from the judgment models
recalloverall.1 = glmer(Recall ~ (1|Partno) + Judgment + 
                         Judged.Value2 + LogSub.1 + LogSub.2 +
                          Length.1 + Length.2 ,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(recalloverall.1)

##rated properties
recalloverall.2 = glmer(Recall ~ (1|Partno) +  Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                       Length.1 + Length.2 + AOA.1 + 
                         AOA.2 + Familiarity.1 + Familiarity.2 + 
                         Valence.1 + Valence.2 + Imageability.1 + QCON.2,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0) ##model fails to converge when set to 1
summary(recalloverall.2) ##I don't get the error when set to 0

##network connections
recalloverall.3 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                          Length.1 + Length.2 + AOA.1 + 
                          AOA.2 + Familiarity.1 + Familiarity.2 + 
                          Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                          QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                          Ortho.2 + Phono.1 + Phono.2,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)
summary(recalloverall.3)

##network norms
recalloverall.4 = glmer(Recall ~ (1|Partno) +  Judgment +Judged.Value2 + LogSub.1 + LogSub.2 +
                         Length.1 + Length.2 + AOA.1 + 
                         AOA.2 + Familiarity.1 + Familiarity.2 + 
                         Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                         QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                         Ortho.2 + Phono.1 + Phono.2 +
                         ZFSG * ZLSA * ZCOS,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)
summary(recalloverall.4) ##three way interaction is significant

####moderation -- sw recall####
#low cosine
lowcos4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                   Length.1 + Length.2 + AOA.1 + 
                   AOA.2 + Familiarity.1 + Familiarity.2 + 
                   Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                   QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                   Ortho.2 + Phono.1 + Phono.2 +
                 ZCOS_low * ZLSA * ZFSG,
                data = combined,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 0)
summary(lowcos4)

##high cosine
hicos4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                 Length.1 + Length.2 + AOA.1 + 
                 AOA.2 + Familiarity.1 + Familiarity.2 + 
                 Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                 QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                 Ortho.2 + Phono.1 + Phono.2 + 
                 ZCOS_high * ZLSA * ZFSG,
               data = combined,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 0)
summary(hicos4)

##low cosine low lsa
lowcoslowlsa4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                        Length.1 + Length.2 + AOA.1 + 
                        AOA.2 + Familiarity.1 + Familiarity.2 + 
                        Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                        QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                        Ortho.2 + Phono.1 + Phono.2 +
                        ZCOS_low * ZLSA_low * ZFSG,
                      data = combined,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 0)
summary(lowcoslowlsa4)

##low cosine high lsa
lowcoshighlsa4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                         Length.1 + Length.2 + AOA.1 + 
                         AOA.2 + Familiarity.1 + Familiarity.2 + 
                         Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                         QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                         Ortho.2 + Phono.1 + Phono.2 +
                         ZCOS_low * ZLSA_high * ZFSG,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)
summary(lowcoshighlsa4)

##high low
highcoslowlsa4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                         Length.1 + Length.2 + AOA.1 + 
                         AOA.2 + Familiarity.1 + Familiarity.2 + 
                         Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                         QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                         Ortho.2 + Phono.1 + Phono.2 +
                         ZCOS_high * ZLSA_low * ZFSG,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0)
summary(highcoslowlsa4)

##high high
highcoshighlsa4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                          Length.1 + Length.2 + AOA.1 + 
                          AOA.2 + Familiarity.1 + Familiarity.2 + 
                          Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                          QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                          Ortho.2 + Phono.1 + Phono.2 +
                          ZCOS_high * ZLSA_high * ZFSG,
                        data = combined,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 0)
summary(highcoshighlsa4)

##avg low
avgcoslowlsa4 = glmer(Recall ~ (1|Partno) + Judgment + Judged.Value2 + LogSub.1 + LogSub.2 +
                        Length.1 + Length.2 + AOA.1 + 
                        AOA.2 + Familiarity.1 + Familiarity.2 + 
                        Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                        QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                        Ortho.2 + Phono.1 + Phono.2 +
                        ZCOS * ZLSA_low * ZFSG,
                      data = combined,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 0)
summary(avgcoslowlsa4)

##avg high
avgcoshighlsa4 = glmer(Recall ~ (1|Partno) +  Judgment +Judged.Value2 + LogSub.1 + LogSub.2 +
                         Length.1 + Length.2 + AOA.1 + 
                         AOA.2 + Familiarity.1 + Familiarity.2 + 
                         Valence.1 + Valence.2 + Imageability.1 + QCON.2 +
                         QSS.1 + TSS.2 + FSS.1 + FSS.2 + Ortho.1 + 
                         Ortho.2 + Phono.1 + Phono.2 +
                         ZCOS * ZLSA_high * ZFSG,
                       data = combined,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 0) 
summary(avgcoshighlsa4)

####Recall graphs####
##low cos
plot10 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 6.149, slope = 2.020, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 6.043, slope = 2.221, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 5.940, slope = 2.507, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") ##these intercepts are crazy high

##avg cos
plot11 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 6.025, slope = 2.298, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 6.127, slope = 1.866, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 6.233, slope = 1.515, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS")

##high cos
plot12 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 5.899, slope = 2.619, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 6.220, slope = 1.616, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 6.545, slope = 0.690, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot10)
recall.graph.sw =  plot_grid( plot10 + theme(legend.position="none"),
                           plot11 + theme(legend.position="none"),
                           plot12 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
recall.graph.sw 
##oh my god the intercepts are so fucking high the slopes aren't even on the damn graph
##looking at the numbers though, these are competitive at low, complimentary at high
##Just like the pilot