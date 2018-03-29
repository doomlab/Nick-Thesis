####setup####
library(nlme)
library(lme4)
library(cowplot)
library(MOTE)
library(ggplot2)

options(scipen = 999)
options(max.print = 9999)

dat = read.csv("master.csv")

####data screening will go below#####
##accuracy
summary(dat)

table(dat$Judged.Value)

dat$Judged.Value[ dat$Judged.Value > 100 ] = NA
summary(dat$Judged.Value)

##missing data
table("judge" = is.na(dat$Judged.Value), "recall" = is.na(dat$Recall))
##12444 data points being used, 1472 excluded

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

##for now will just use length and morphemes to cut down on correlations

####descriptive statistics will go below####
m.recall = tapply(noout$Recall, noout$Judgment, mean)
sd.recall = tapply(noout$Recall, noout$Judgment, sd)

m.judge = tapply(noout$Judged.Value, noout$Judgment, mean)
sd.judge = tapply(noout$Judged.Value, noout$Judgment, sd)

m.recall;sd.recall
m.judge;sd.judge


####replicating interactions from pilot -- judgments####
##mean center variables
##network norms
noout$ZCOS = scale(noout$COS, scale = F)
noout$ZLSA = scale(noout$LSA, scale = F)
noout$ZFSG = scale(noout$FSG, scale = F)

##single word norms
noout$ZQCON.1 = scale(noout$QCON.1, scale = F)
noout$ZQCON.2 = scale(noout$QCON.2, scale = F)
noout$ZQSS.1 = scale(noout$QSS.1, scale = F)
noout$ZOrtho.1 = scale(noout$Ortho.1, scale = F)
noout$ZOrtho.2 = scale(noout$Ortho.2, scale = F)
noout$ZPhono.1 = scale(noout$Phono.1, scale = F)
noout$ZPhono.2 = scale(noout$Phono.2, scale = F)
noout$ZPhonemes.1 = scale(noout$Phonemes.1, scale = F)
noout$ZPhonemes.2 = scale(noout$Phonemes.2, scale = F)
noout$ZImageability.1 = scale(noout$Imageability.1, scale = F)
noout$ZImageability.2 = scale(noout$Imageability.2, scale = F)
noout$ZFamiliarity.1 = scale(noout$Familiarity.1, scale = F)
noout$ZFamiliarity.2 = scale(noout$Familiarity.2, scale = F)
noout$ZSubtlex.1 = scale(noout$Subtlex.1, scale = F)
noout$ZSubtlex.2 = scale(noout$Subtlex.2, scale = F)
noout$ZLength.1 = scale(noout$Length.1, scale = F)
noout$ZLength.2 = scale(noout$Length.2, scale = F)
noout$ZValence.1 = scale(noout$Valence.1, scale = F)
noout$ZValence.2 = scale(noout$Valence.2, scale = F)
noout$ZSyllables.1 = scale(noout$Syllables.1, scale = F)
noout$ZSyllables.2 = scale(noout$Syllables.2, scale = F)
noout$ZMorphemes.1 = scale(noout$Morphemes.1, scale = F)
noout$ZMorphemes.2 = scale(noout$Morphemes.2, scale = F)
noout$ZAOA.1 = scale(noout$AOA.1, scale = F)
noout$ZAOA.2 = scale(noout$AOA.2, scale = F)
noout$ZFSS.1 = scale(noout$FSS.1, scale = F)
noout$ZFSS.2 = scale(noout$FSS.2, scale = F)
noout$ZCOSC.1 = scale(noout$COSC.1, scale = F)
noout$ZCOSC.2 = scale(noout$COSC.2, scale = F)
noout$ZTSS.2 = scale(noout$TSS.2, scale = F)

##create the right scaling 
noout$Judged.Value2 = noout$Judged.Value/100

##overall model
overall.judge = lme(Judged.Value2 ~ Judgment + 
                  ZCOS * ZLSA * ZFSG, 
                data = noout, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|Partno)
summary(overall.judge) ##three way interacion is not significant

##going to try without the interactions to see what happens
overall.judge2 = lme(Judged.Value2 ~ Judgment + 
                      ZCOS + ZLSA + ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(overall.judge2) 

####moderations will go below -- judgments####
##not going to do moderations for judgments because it wasn't signficant
##break down cos
noout$ZCOS_low = noout$ZCOS + sd(noout$ZCOS, na.rm = TRUE)
noout$ZCOS_high = noout$ZCOS - sd(noout$ZCOS, na.rm = TRUE)

##low cosine
#lowcos = lme(Judged.Value2 ~ Judgment +
             #  ZCOS_low * ZLSA * ZFSG, 
             #data = noout, 
             #method = "ML", 
             #na.action = "na.omit",
             #random = ~1|Partno)

##high cosine
#highcos = lme(Judged.Value2 ~ Judgment +
               # ZCOS_high * ZLSA * ZFSG, 
              #data = noout, 
              #method = "ML", 
              #na.action = "na.omit",
              #random = ~1|Partno)

#summary(lowcos)
#summary(highcos)

##now splitting by LSA
noout$ZLSA_low = noout$ZLSA + sd(noout$ZLSA, na.rm = TRUE)
noout$ZLSA_high = noout$ZLSA - sd(noout$ZLSA, na.rm = TRUE)

##low cosine, low lsa
#lowcoslowlsa = lme(Judged.Value2 ~ Judgment +
                  #   ZCOS_low * ZLSA_low * ZFSG, 
                  # data = noout, 
                  # method = "ML", 
                  # na.action = "na.omit",
                  # random = ~1|Partno)
##low high
#lowcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      #ZCOS_low  * ZLSA_high * ZFSG, 
                    #data = noout, 
                    #method = "ML", 
                   # na.action = "na.omit",
                   # random = ~1|Partno)
##avg low
#avgcoslowlsa = lme(Judged.Value2 ~ Judgment +
                #     ZCOS * ZLSA_low * ZFSG, 
                 #  data = noout, 
                 #  method = "ML", 
                 #  na.action = "na.omit",
                 #  random = ~1|Partno)
##avg high
#avgcoshighlsa = lme(Judged.Value2 ~ Judgment +
          #            ZCOS  * ZLSA_high * ZFSG, 
         #           data = noout, 
         #           method = "ML", 
         #           na.action = "na.omit",
        #            random = ~1|Partno)

#highcoslowlsa = lme(Judged.Value2 ~ Judgment +
     #                 ZCOS_high * ZLSA_low * ZFSG, 
  #                  data = noout, 
  #                  method = "ML", 
       #             na.action = "na.omit",
       #             random = ~1|Partno)
##high high
#highcoshighlsa = lme(Judged.Value2 ~ Judgment +
     #                 ZCOS_high  * ZLSA_high * ZFSG, 
      #               data = noout, 
      #               method = "ML", 
      #               na.action = "na.omit",
      #               random = ~1|Partno)

####judgment graphs####
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

##low cos
##will need to change slope and intercept numbers
#plot1 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  #labs(x = "ZFSG", y = "Judgments") +
#  scale_size_continuous(guide = FALSE) +
 # geom_abline(aes(intercept = .607, slope = .663, linetype = "-1SD ZLSA")) +
 # geom_abline(aes(intercept = .632, slope = .375, linetype = "Average ZLSA")) +
 # geom_abline(aes(intercept = .657, slope = .087, linetype = "+1SD ZLSA")) +
  #scale_linetype_manual(values = c("dotted", "dashed", "solid"),
   #                     breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
   #                     name = "Simple Slope") +
 # coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
#  geom_vline(xintercept = -.30) +
  #geom_hline(yintercept = 0) +
 # cleanup + 
 # labs(title="Low ZCOS") 

##avg cos
#plot2 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
 # labs(x = "ZFSG", y = "Judgments") +
  #scale_size_continuous(guide = FALSE) +
  #geom_abline(aes(intercept = .586, slope = .381, linetype = "-1SD ZLSA")) +
  #geom_abline(aes(intercept = .603, slope = .271, linetype = "Average ZLSA")) +
  #geom_abline(aes(intercept = .621, slope = .161, linetype = "+1SD ZLSA")) +
  #scale_linetype_manual(values = c("dotted", "dashed", "solid"),
 #                       breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
 #                       name = "Simple Slope") +
  #coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
 # geom_vline(xintercept = -.30) +
 # geom_hline(yintercept = 0) +
  #cleanup + 
 # labs(title="Average ZCOS") 

##high cos
#plot3 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
 # labs(x = "ZFSG", y = "Judgments") +
  #scale_size_continuous(guide = FALSE) +
#  geom_abline(aes(intercept = .564, slope = .099, linetype = "-1SD ZLSA")) +
  #geom_abline(aes(intercept = .575, slope = .167, linetype = "Average ZLSA")) +
  #geom_abline(aes(intercept = .586, slope = .236, linetype = "+1SD ZLSA")) +
  #scale_linetype_manual(values = c("dotted", "dashed", "solid"),
     #                   breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
     #                   name = "Simple Slope") +
  #coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  #geom_vline(xintercept = -.30) +
 # geom_hline(yintercept = 0) +
 # cleanup + 
 # labs(title="High ZCOS") 

# arrange plots together
#legend = get_legend(plot1)
#hyp2graphout <- plot_grid( plot1 + theme(legend.position="none"),
     #                      plot2 + theme(legend.position="none"),
     #                      plot3 + theme(legend.position="none"),
     #                      legend,
      #                     hjust = -1,
     #                      nrow = 2
#)
#hyp2graphout

####replicating interactions from pilot -- Recall####
overall.recall = glmer(Recall ~ (1|Partno) + Judgment + 
                    Judged.Value2 + ZCOS * ZLSA * ZFSG,
                  data = noout,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(overall.recall) ##three way interaction still exists!

####moderation stuff####
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

####moderation graphs -- recall####
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
hyp3graphout <- plot_grid( plot4 + theme(legend.position="none"),
                           plot5 + theme(legend.position="none"),
                           plot6 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
hyp3graphout

##sorta of a seesaw thing going on. The flip isn't as smooth as last time. 
##Also first graph is wonk
##also flip is in the wrong direction. 
##found out why that graph was wonk. I put in the SE for the slope instead of the estimate.
##its not wonk anymore.
##Now that the slope is corrected, FSG and LSA are complimentary at low cos and competitive at high
##FSG slopes get stronger as LSA increases at low COS
##FSG gets weaker as LSA increases at higher cosines.
##this is the opposite of the pilot...

####single word norm models -- judgments####
##only using length since, length, syllables, and phonemes are so correlated.

overall.judge.sw.1 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2,                                                                         ##POS variables are fine individually
                         data = noout,                                                                    ##but the model freaks out when they are included together
                         method = "ML",                                                                   ##for now, I'm just going to focus on the target POS
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.1)

overall.judge.sw.2 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2 +
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.2)

overall.judge.sw.3 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2 +
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                           ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.3)

overall.judge.sw.4 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2 + 
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                           ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                           ZFSG * ZLSA * ZCOS,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.4) ##three way interaction is now significant after controlling for everything.

####looking just at info for target words####
overall.judge.sw.5 = lme(Judged.Value2 ~ Judgment +
                          ZSubtlex.2 + ZLength.2 + ZMorphemes.2 +
                           POS.2 + 
                           ZAOA.2 + ZFamiliarity.2 + ZValence.2 + ZImageability.2 + ZQCON.2 +
                           ZTSS.2 + ZFSS.2 + ZCOSC.2 + ZOrtho.2 + ZPhono.2 +
                           ZFSG * ZLSA * ZCOS,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.5) ##interaction is no longer signficant

##now looking just at cues
overall.judge.sw.6 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZLength.1 + ZMorphemes.1 +
                           POS.1 + 
                           ZAOA.1 + ZFamiliarity.1 + ZValence.1 + ZImageability.1 + ZQCON.1 +
                           ZQSS.1 + ZFSS.1 + ZCOSC.1 + ZOrtho.1 + ZPhono.1 +
                           ZFSG * ZLSA * ZCOS,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.6) ##interaction is still significant, sign has flipped to negative

####sw judgments moderation stuff####
lowcos3 = lme(Judged.Value2 ~ Judgment +
                ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                POS.2 + 
                ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                ZFSG * ZLSA * ZCOS_low,
              data = noout, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|Partno)
summary(lowcos3)

##high cosine
hicos3 = lme(Judged.Value2 ~ Judgment +
               ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
               POS.2 + 
               ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
               ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
               ZFSG * ZLSA * ZCOS_high,
             data = noout, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|Partno)
summary(hicos3)

##low cosine low lsa
lowcoslowlsa3 = lme(Judged.Value2 ~ Judgment +
                                ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                                POS.2 + 
                                ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                                ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                                ZFSG * ZLSA_low * ZCOS_low,
                              data = noout, 
                              method = "ML", 
                              na.action = "na.omit",
                              random = ~1|Partno)
summary(lowcoslowlsa3)

##low cosine high lsa
lowcoshighlsa3 = lme(Judged.Value2 ~ Judgment +
                       ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                       POS.2 + 
                       ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                       ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_high * ZCOS_low,
                     data = noout, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(lowcoshighlsa3)

##high low
highcoslowlsa3 = lme(Judged.Value2 ~ Judgment +
                       ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                       POS.2 + 
                       ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                       ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_low * ZCOS_high,
                     data = noout, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(highcoslowlsa3)

##high high
highcoshighlsa3 = lme(Judged.Value2 ~ Judgment +
                        ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                        POS.2 + 
                        ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                        ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                        ZFSG * ZLSA_high * ZCOS_high,
                      data = noout, 
                      method = "ML", 
                      na.action = "na.omit",
                      random = ~1|Partno)
summary(highcoshighlsa3)

avgcoslowlsa3 = lme(Judged.Value2 ~ Judgment +
                      ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                      POS.2 + 
                      ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                      ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                      ZFSG * ZLSA_low * ZCOS,
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(avgcoslowlsa3)

avgcoshighlsa3 = lme(Judged.Value2 ~ Judgment +
                       ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                       POS.2 + 
                       ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                       ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_high * ZCOS,
                     data = noout, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(avgcoshighlsa3)

####moderation graphs -- sw judgments####
##low cos
plot7 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.283, slope = 0.802, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.287, slope = 0.495, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.291, slope = 0.188, linetype = "+1SD ZLSA")) +
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
  geom_abline(aes(intercept = 0.321, slope = 0.445, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.339, slope = 0.293, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.358, slope = 0.141, linetype = "+1SD ZLSA")) +
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
  geom_abline(aes(intercept = 0.359, slope = 0.088, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.392, slope = 0.091, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.424, slope = 0.093, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot7)
sw.judge.plot <- plot_grid(plot7 + theme(legend.position="none"),
                           plot8 + theme(legend.position="none"),
                           plot9 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
sw.judge.plot

##the interaction flips this time, just as before. Competive at low Cos, but complimentary at high

####single word norm models -- recall####
overall.recall.sw.1 = glmer(Recall ~ (1|Partno) +
                            Judgment + Judged.Value2 +
                            #ZSubtlex.1 + + ZSubtlex.2 + 
                              ZLength.1 + 
                              ZLength.2 + 
                              ZMorphemes.1 + 
                              ZMorphemes.2 +
                            POS.2,
                            data = noout,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"),
                            nAGQ = 1)
summary(overall.recall.sw.1)

overall.recall.sw.2 = glmer(Recall ~ (1|Partno) +
                           Judgment +
                             Judged.Value2 +
                             #ZSubtlex.1 +  ZSubtlex.2 + 
                             ZLength.1 + 
                             ZLength.2 + 
                             ZMorphemes.1 + 
                             ZMorphemes.2 +
                             POS.2 +
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2,
                           data = noout,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 1)
summary(overall.recall.sw.2)

##the two models above run fine when subltex is not included (this fixed the previous warning)
##the two below still have problems, and I think its based on the size of the models.

overall.recall.sw.3 = glmer(Recall ~ (1|Partno) +
                              Judgment +
                              Judged.Value2 +
                              #ZSubtlex.1 +  ZSubtlex.2 + 
                              ZLength.1 + 
                              ZLength.2 + 
                              ZMorphemes.1 + 
                              ZMorphemes.2 +
                              POS.2 +
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + Valence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                           ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2,
                           data = noout,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 0) ##set to zero to make it run faster
summary(overall.recall.sw.3)

overall.recall.sw.4 = glmer(Recall ~ (1|Partno) +
                              Judgment +
                              Judged.Value2 +
                              #ZSubtlex.1 +  ZSubtlex.2 + 
                              ZLength.1 + 
                              ZLength.2 + 
                              ZMorphemes.1 + 
                              ZMorphemes.2 +
                              POS.2 + 
                            ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                            ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                            ZFSG * ZLSA * ZCOS,
                          data = noout,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 0)
summary(overall.recall.sw.4) ##3 way is significant, but still get warnings.
##fit warning is related to the network norms, since they only appear in model 4

####looking just at target word properties####
overall.recall.sw.5 = glmer(Recall ~ (1|Partno) +
                              Judgment +
                              Judged.Value2 +
                              ZSubtlex.2 + 
                              ZLength.2 + 
                              ZMorphemes.2 +
                              POS.2 + 
                              ZAOA.2 +  ZFamiliarity.2 + ZValence.2 + ZImageability.2 + ZQCON.2 +
                              ZTSS.2 + ZFSS.2 + ZCOSC.2 + ZOrtho.2 + ZPhono.2 +
                              ZFSG * ZLSA * ZCOS,
                            data = noout,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"),
                            nAGQ = 0)
summary(overall.recall.sw.5)

####moderation for sw recall will go here####
####sw judgments moderation stuff####
lowcos4 = glmer(Recall ~ (1|Partno) +
                  Judgment +
                  Judged.Value2 +
                  #ZSubtlex.1 +  ZSubtlex.2 + 
                  ZLength.1 + 
                  ZLength.2 + 
                  ZMorphemes.1 + 
                  ZMorphemes.2 +
                  POS.2 + 
                  ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                  ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                ZFSG * ZLSA * ZCOS_low,
              data = noout, 
              family = binomial,
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
summary(lowcos4)

##high cosine
hicos4 = glmer(Recall ~ (1|Partno) +
                 Judgment +
                 Judged.Value2 +
                 #ZSubtlex.1 +  ZSubtlex.2 + 
                 ZLength.1 + 
                 ZLength.2 + 
                 ZMorphemes.1 + 
                 ZMorphemes.2 +
                 POS.2 + 
                 ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                 ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
               ZFSG * ZLSA * ZCOS_high,
             data = noout, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 0)
summary(hicos4)

##low cosine low lsa
lowcoslowlsa4 = glmer(Recall ~ (1|Partno) +
                        Judgment +
                        Judged.Value2 +
                        #ZSubtlex.1 +  ZSubtlex.2 + 
                        ZLength.1 + 
                        ZLength.2 + 
                        ZMorphemes.1 + 
                        ZMorphemes.2 +
                        POS.2 + 
                        ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                        ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                      ZFSG * ZLSA_low * ZCOS_low,
                    data = noout, 
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 0)
summary(lowcoslowlsa4)

##low cosine high lsa
lowcoshighlsa4 = glmer(Recall ~ (1|Partno) +
                         Judgment +
                         Judged.Value2 +
                         #ZSubtlex.1 +  ZSubtlex.2 + 
                         ZLength.1 + 
                         ZLength.2 + 
                         ZMorphemes.1 + 
                         ZMorphemes.2 +
                         POS.2 + 
                         ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                         ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_high * ZCOS_low,
                     data = noout, 
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"),
                     nAGQ = 0)
summary(lowcoshighlsa4)

##high low
highcoslowlsa4 = glmer(Recall ~ (1|Partno) +
                         Judgment +
                         Judged.Value2 +
                         #ZSubtlex.1 +  ZSubtlex.2 + 
                         ZLength.1 + 
                         ZLength.2 + 
                         ZMorphemes.1 + 
                         ZMorphemes.2 +
                         POS.2 + 
                         ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                         ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_low * ZCOS_high,
                     data = noout, 
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"),
                     nAGQ = 0)
summary(highcoslowlsa4)

##high high
highcoshighlsa4 = glmer(Recall ~ (1|Partno) +
                          Judgment +
                          Judged.Value2 +
                          #ZSubtlex.1 +  ZSubtlex.2 + 
                          ZLength.1 + 
                          ZLength.2 + 
                          ZMorphemes.1 + 
                          ZMorphemes.2 +
                          POS.2 + 
                          ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                          ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                        ZFSG * ZLSA_high * ZCOS_high,
                      data = noout, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 0)
summary(highcoshighlsa4)

avgcoslowlsa4 = glmer(Recall ~ (1|Partno) +
                        Judgment +
                        Judged.Value2 +
                        #ZSubtlex.1 +  ZSubtlex.2 + 
                        ZLength.1 + 
                        ZLength.2 + 
                        ZMorphemes.1 + 
                        ZMorphemes.2 +
                        POS.2 + 
                        ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                        ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                      ZFSG * ZLSA_low * ZCOS,
                    data = noout, 
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 0)
summary(avgcoslowlsa4)

avgcoshighlsa4 = glmer(Recall ~ (1|Partno) +
                         Judgment +
                         Judged.Value2 +
                         #ZSubtlex.1 +  ZSubtlex.2 + 
                         ZLength.1 + 
                         ZLength.2 + 
                         ZMorphemes.1 + 
                         ZMorphemes.2 +
                         POS.2 + 
                         ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                         ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_high * ZCOS,
                     data = noout, 
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"),
                     nAGQ = 0)
summary(avgcoshighlsa4)

####moderation graphs -- sw judgments####
##low cos
plot13 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.659, slope = 1.141, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.726, slope = 1.349, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.793, slope = 1.557, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot14 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.622, slope = 2.657, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.754, slope = 1.956, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.886, slope = 1.254, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS") 

##high cos
plot15 = ggplot(noout, aes(x = ZCOS_low, y = Recall)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.585, slope = 4.173, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.782, slope = 2.563, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.980, slope = 0.952, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot13)
sw.recall.plot <- plot_grid(plot13 + theme(legend.position="none"),
                           plot14 + theme(legend.position="none"),
                           plot15 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
sw.recall.plot ##FSG is best at low lsa and high cos
##complimentary at low cos, competitive at high
##same results as when not controlling for sw norms

####combined sw####
##these analyses will look at the pilot and thesis data combined

##set up
pilot.dat = read.csv("Melted Data pilot.csv")

##datascreening
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

##get all the noout2 stuff on the right scaling
##network norms
noout2$ZCOS = scale(noout2$COS, scale = F)
noout2$ZLSA = scale(noout2$LSA, scale = F)
noout2$ZFSG = scale(noout2$FSG, scale = F)

##single word norms
noout2$ZQCON.1 = scale(noout2$QCON.1, scale = F)
noout2$ZQCON.2 = scale(noout2$QCON.2, scale = F )
noout2$ZQSS.1 = scale(noout2$QSS.1, scale = F)
noout2$ZTSS.2 = scale(noout2$TSS.2, scale = F)
noout2$ZOrtho.1 = scale(noout2$Ortho.1, scale = F)
noout2$ZOrtho.2 = scale(noout2$Ortho.2, scale = F)
noout2$ZPhono.1 = scale(noout2$Phono.1, scale = F)
noout2$ZPhono.2 = scale(noout2$Phono.2, scale = F)
noout2$ZPhonemes.1 = scale(noout2$Phonemes.1, scale = F)
noout2$ZPhonemes.2 = scale(noout2$Phonemes.2, scale = F)
noout2$ZImageability.1 = scale(noout2$Imageability.1, scale = F)
noout2$ZImageability.2 = scale(noout2$Imageability.2, scale = F)
noout2$ZFamiliarity.1 = scale(noout2$Familiarity.1, scale = F)
noout2$ZFamiliarity.2 = scale(noout2$Familiarity.2, scale = F)
noout2$ZSubtlex.1 = scale(noout2$Subtlex.1, scale = F)
noout2$ZSubtlex.2 = scale(noout2$Subtlex.2, scale = F)
noout2$ZLength.1 = scale(noout2$Length.1, scale = F)
noout2$ZLength.2 = scale(noout2$Length.2, scale = F)
noout2$ZValence.1 = scale(noout2$Valence.1, scale = F)
noout2$ZValence.2 = scale(noout2$Valence.2, scale = F)
noout2$ZSyllables.1 = scale(noout2$Syllables.1, scale = F)
noout2$ZSyllables.2 = scale(noout2$Syllables.2, scale = F)
noout2$ZMorphemes.1 = scale(noout2$Morphemes.1, scale = F)
noout2$ZMorphemes.2 = scale(noout2$Morphemes.2, scale = F)
noout2$ZAOA.1 = scale(noout2$AOA.1, scale = F)
noout2$ZAOA.2 = scale(noout2$AOA.2, scale = F)
noout2$ZFSS.1 = scale(noout2$FSS.1, scale = F)
noout2$ZFSS.2 = scale(noout2$FSS.2, scale = F)
noout2$ZCOSC.1 = scale(noout2$COSC.1, scale = F)
noout2$ZCOSC.2 = scale(noout2$COSC.2, scale = F)

##create the right scaling 
noout2$Judged.Value2 = noout2$Judged.Value/100

##add in variables for moderations
noout2$ZCOS_low = noout2$ZCOS + sd(noout2$ZCOS, na.rm = TRUE)
noout2$ZCOS_high = noout2$ZCOS - sd(noout2$ZCOS, na.rm = TRUE)
noout2$ZLSA_low = noout2$ZLSA + sd(noout2$ZLSA, na.rm = TRUE)
noout2$ZLSA_high = noout2$ZLSA - sd(noout2$ZLSA, na.rm = TRUE)

####combining pilot w/ part2
colnames(noout2)[2] = "Judgment"

combined = rbind(noout, noout2)

####judgment - sw combined data set####
combined.judge.sw.1 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2,                                                                         ##POS variables are fine individually
                         data = combined,                                                                    ##but the model freaks out when they are included together
                         method = "ML",                                                                   ##for now, I'm just going to focus on the target POS
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(combined.judge.sw.1)

combined.judge.sw.2 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2 +
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2,
                         data = combined, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(combined.judge.sw.2)

combined.judge.sw.3 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2 +
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                           ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2,
                         data = combined, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(combined.judge.sw.3)

combined.judge.sw.4 = lme(Judged.Value2 ~ Judgment +
                           ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                           POS.2 + 
                           ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                           ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                           ZFSG * ZLSA * ZCOS,
                         data = combined, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(combined.judge.sw.4) ##three way interaction is still significant

####combined judgments moderations####
lowcos5 = lme(Judged.Value2 ~ Judgment +
                ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                POS.2 + 
                ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                ZFSG * ZLSA * ZCOS_low,
              data = combined, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|Partno)
summary(lowcos5)

##high cosine
hicos5 = lme(Judged.Value2 ~ Judgment +
               ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
               POS.2 + 
               ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
               ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
               ZFSG * ZLSA * ZCOS_high,
             data = combined, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|Partno)
summary(hicos5)

##low cosine low lsa
lowcoslowlsa4 = lme(Judged.Value2 ~ Judgment +
                      ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                      POS.2 + 
                      ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                      ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                      ZFSG * ZLSA_low * ZCOS_low,
                    data = combined, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(lowcoslowlsa5)

##low cosine high lsa
lowcoshighlsa5 = lme(Judged.Value2 ~ Judgment +
                       ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                       POS.2 + 
                       ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                       ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_high * ZCOS_low,
                     data = combined, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(lowcoshighlsa5)

##high low
highcoslowlsa5 = lme(Judged.Value2 ~ Judgment +
                       ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                       POS.2 + 
                       ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                       ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA_low * ZCOS_high,
                     data = combined, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(highcoslowlsa5)

##high high
highcoshighlsa5 = lme(Judged.Value2 ~ Judgment +
                        ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                        POS.2 + 
                        ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                        ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                        ZFSG * ZLSA_high * ZCOS_high,
                      data = combined, 
                      method = "ML", 
                      na.action = "na.omit",
                      random = ~1|Partno)
summary(highcoshighlsa5)

##avg low
avgcoslowlsa5 = lme(Judged.Value2 ~ Judgment +
                      ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                      POS.2 + 
                      ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                      ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                      ZFSG * ZLSA_low * ZCOS,
                    data = combined, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(avgcoslowlsa5)

##avg high
avgcoshighlsa5 = lme(Judged.Value2 ~ Judgment +
                       ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                       POS.2 + 
                       ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                       ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                       ZFSG * ZLSA * ZCOS_high,
                     data = combined, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(avgcoshighlsa5)

####moderation graphs -- sw judgments####
##low cos
plot10 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.411, slope = 0.583, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.445, slope = 0.469, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.479, slope = 0.354, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot11 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.445, slope = 0.437, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.472, slope = 0.366, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.479, slope = 0.289, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS") 

##high cos
plot12 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgment Value") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.479, slope = 0.289, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.498, slope = 0.266, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.518, slope = 0.248, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

legend = get_legend(plot10)
combined.judge.plot <- plot_grid(plot10 + theme(legend.position="none"),
                           plot11 + theme(legend.position="none"),
                           plot12 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
combined.judge.plot

##interactions don't seesaw this time, but the difference between slopes becomes much smaller at high LSA
##basically when cos is high, high lsa hurts fsg less than it does at low cosine

####recall -- sw combined data set####
overall.recall.combined.1 = glmer(Recall ~ (1|Partno) + 
                                    Judgment +
                            POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2,
                          data = noout, 
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 0) ##currently set to zero, otherwise these models run forever.
summary(overall.recall.combined.1)

overall.recall.combined.2 = glmer(Recall ~ (1|Partno) + 
                                    Judgment +
                                  ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                                  POS.2 +
                                  ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2,
                                data = noout,
                                family = binomial,
                                control = glmerControl(optimizer = "bobyqa"),
                                nAGQ = 0)
summary(overall.recall.combined.2)

overall.recall.combined.3 = glmer(Recall ~ (1|Partno) + 
                                    Judgment +
                                  ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                                  POS.2 +
                                  ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                                  ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2,
                          data = noout, 
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 0)
summary(overall.recall.combined.3)

overall.recall.combined.4 = glmer(Recall ~ (1|Partno) + 
                            Judgment +
                            POS.1 + POS.2 + Subtlex.1 + Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                            AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                            QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2 +
                            FSG * LSA * COS,
                          data = noout, 
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 0)
summary(overall.recall.combined.4) ##three-way is significant

####moderations for combined recall####

