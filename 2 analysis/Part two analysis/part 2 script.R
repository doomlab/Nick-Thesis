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

##missing data
table("judge" = is.na(dat$Judged.Value), "recall" = is.na(dat$Recall))
##12451 data points being used, 1472 excluded

##outliers
mahal = mahalanobis(dat[ , c(4,5)], 
                    colMeans(dat[ , c(4,5)], na.rm = TRUE),
                    cov(dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(dat[ , c(4,5)]))
cutoff;ncol(dat[ , c(4,5)])
summary(mahal < cutoff) ##five outliers removed
noout = subset(dat, mahal < cutoff)


##additivity
cor(noout[ , c(4:15, 17:31, 33:46)], use = "pairwise.complete.obs")

##morphemes doesn't, so i may just use length and morphemes to cut down on correlations

####descriptive statistics will go below####

####replicating interactions from pilot -- judgments####
##mean center variables
##network norms
noout$ZCOS = scale(noout$COS, scale = F)
noout$ZLSA = scale(noout$LSA, scale = F)
noout$ZFSG = scale(noout$FSG, scale = F)

##single word norms
noout$ZQCON.1 = scale(noout$QCON.1, scale = F)
noout$ZQCON.2 = scale(noout$QCON.2, scale = F )
noout$ZQSS.1 = scale(noout$QSS.1, scale = F)
noout$ZTSS.2 = scale(noout$TSS.2, scale = F)
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
summary(overall.judge2) ##three way is still not significant

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
summary(overall.recall) ##three way interaction still exists! Thank the jesus!

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

avgcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS * ZLSA_high * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1) 
summary(avgcoshighlsa2)

####moderation graphs -- recall####
##low cos
plot4 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.149, slope = 0.070, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.105, slope = 1.213, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.061, slope = 0.580, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot5 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.152, slope = 1.981, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.289, slope = 1.796, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.426, slope = 1.612, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS") 

##high cos
plot6 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .155, slope = 3.892, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .473, slope = 2.380, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .792, slope = 0.868, linetype = "+1SD ZLSA")) +
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

##No see saw effect this go around. Also that first graph is wonk

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
                       ZFSG * ZLSA_low * ZCOS_high,
                     data = noout, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(avgcoshighlsa3)

####moderation graphs -- sw judgments####
##low cos
plot7 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.292, slope = 0.803, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.293, slope = 0.516, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.293, slope = 0.228, linetype = "+1SD ZLSA")) +
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
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.331, slope = 0.432, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.345, slope = 0.297, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.370, slope = 0.061, linetype = "+1SD ZLSA")) +
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
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.370, slope = 0.061, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.397, slope = 0.079, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.425, slope = 0.099, linetype = "+1SD ZLSA")) +
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
                            Judgment +
                            ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                            POS.2,
                            data = noout,
                            family = binomial,
                            control = glmerControl(optimizer = "bobyqa"),
                            nAGQ = 1)
summary(overall.recall.sw.1)

overall.recall.sw.2 = glmer(Recall ~ (1|Partno) +
                           Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2,
                           data = noout,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 1)
summary(overall.recall.sw.2)

overall.recall.sw.3 = glmer(Recall ~ (1|Partno) +
                           Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2,
                           data = noout,
                           family = binomial,
                           control = glmerControl(optimizer = "bobyqa"),
                           nAGQ = 1)
summary(overall.recall.sw.3)

overall.recall.sw.4 = glmer(Recall ~ (1|Partno) +
                            Judgment +
                            ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                            POS.2 + 
                            ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                            ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2 +
                            ZFSG * ZLSA * ZCOS,
                          data = noout,
                          family = binomial,
                          control = glmerControl(optimizer = "bobyqa"),
                          nAGQ = 1)
summary(overall.recall.sw.4)

####combined sw####
##these analyses will look at the pilot and thesis data combined

##set up
##combined = read.csv()

##judgment
overall.judge.combined.1 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.combined.1)

overall.judge.combined.2 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.combined.2)

overall.judge.combined.3 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.combined.3)

overall.judge.combined.4 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2 +
                           FSG * LSA * COS,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.combined.4)

##recall
overall.recall.combined.1 = lme(Recall ~ Judgment +
                            POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2,
                          data = noout, 
                          method = "ML", 
                          na.action = "na.omit",
                          random = ~1|Partno)
summary(overall.recall.combined.1)

overall.recall.combined.2 = lme(Recall ~ Judgment +
                                  ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                                  POS.2 +
                                  ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2,
                                data = noout,
                          method = "ML", 
                          na.action = "na.omit",
                          random = ~1|Partno)
summary(overall.recall.combined.2)

overall.recall.combined.3 = lme(Recall ~ Judgment +
                                  ZSubtlex.1 + ZSubtlex.2 + ZLength.1 + ZLength.2 + ZMorphemes.1 + ZMorphemes.2 +
                                  POS.2 +
                                  ZAOA.1 + ZAOA.2 + ZFamiliarity.1 + ZFamiliarity.2 + ZValence.1 + ZValence.2 + ZImageability.1 + ZImageability.2 + ZQCON.1 + ZQCON.2 +
                                  ZQSS.1 + ZTSS.2 + ZFSS.1 + ZFSS.2 + ZCOSC.1 + ZCOSC.2 + ZOrtho.1 + ZOrtho.2 + ZPhono.1 + ZPhono.2,
                          data = noout, 
                          method = "ML", 
                          na.action = "na.omit",
                          random = ~1|Partno)
summary(overall.recall.combined.3)

overall.recall.combined.4 = lme(Recall ~ Judgment +
                            POS.1 + POS.2 + Subtlex.1 + Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                            AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                            QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2 +
                            FSG * LSA * COS,
                          data = noout, 
                          method = "ML", 
                          na.action = "na.omit",
                          random = ~1|Partno)
summary(overall.recall.combined.4)