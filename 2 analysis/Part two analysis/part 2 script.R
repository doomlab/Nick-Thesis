####setup####
library(nlme)
library(lme4)
library(cowplot)
library(MOTE)
library(ggplot2)

options(scipen = 999)

##dat = read.csv()

####data screening will go below#####
##accuracy

##missing data

##outliers
mahal = mahalanobis(dat[ , c(4,5)], ##will need to adjust these columns
                    colMeans(dat[ , c(4,5)], na.rm = TRUE),
                    cov(dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(dat[ , c(4,5)]))
cutoff;ncol(dat[ , c(4,5)])
summary(mahal < cutoff)
noout = subset(dat, mahal < cutoff)

##additivity
cor(noout[ , 4:8], use = "pairwise.complete.obs") ##will need to adjust columns

####descriptive statistics will go below####

####replicating interactions from pilot -- judgments####
##mean center variables
noout$ZCOS = scale(noout$COS, scale = F)
noout$ZLSA = scale(noout$LSA, scale = F)
noout$ZFSG = scale(noout$FSG, scale = F)

##create the right scaling 
noout$Judged.Value2 = noout$Judged.Value/100

##overall model
overall.judge = lme(Judged.Value2 ~ Judgment + 
                  ZCOS * ZLSA * ZFSG, 
                data = noout, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|Partno)
summary(overall.judge)

####moderations will go below -- judgments####
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

##high cosine
highcos = lme(Judged.Value2 ~ Judgment +
                ZCOS_high * ZLSA * ZFSG, 
              data = noout, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|Partno)

summary(lowcos)
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
##low high
lowcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS_low  * ZLSA_high * ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
##avg low
avgcoslowlsa = lme(Judged.Value2 ~ Judgment +
                     ZCOS * ZLSA_low * ZFSG, 
                   data = noout, 
                   method = "ML", 
                   na.action = "na.omit",
                   random = ~1|Partno)
##avg high
avgcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS  * ZLSA_high * ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)

highcoslowlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS_high * ZLSA_low * ZFSG, 
                    data = noout, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
##high high
highcoshighlsa = lme(Judged.Value2 ~ Judgment +
                       ZCOS_high  * ZLSA_high * ZFSG, 
                     data = noout, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)

####judgment graphs####
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

##low cos
plot1 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgments") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .607, slope = .663, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .632, slope = .375, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .657, slope = .087, linetype = "+1SD ZLSA")) +
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
  labs(x = "ZFSG", y = "Judgments") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .586, slope = .381, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .603, slope = .271, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .621, slope = .161, linetype = "+1SD ZLSA")) +
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
  labs(x = "ZFSG", y = "Judgments") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .564, slope = .099, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .575, slope = .167, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .586, slope = .236, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="High ZCOS") 

# arrange plots together
legend = get_legend(plot1)
hyp2graphout <- plot_grid( plot1 + theme(legend.position="none"),
                           plot2 + theme(legend.position="none"),
                           plot3 + theme(legend.position="none"),
                           legend,
                           hjust = -1,
                           nrow = 2
)
hyp2graphout

####replicating interactions from pilot -- Recall####
overall.recall = glmer(Recall ~ (1|Partno) + Judgment + 
                    Judged.Value2 + ZCOS * ZLSA * ZFSG,
                  data = noout,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(overall.recall)

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
summary(highcos2)

##low cosine low lsa
lowcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                        Judgment + Judged.Value2 + ZCOS_low * ZLSA_low * ZFSG,
                      data = noout,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 1)

##low cosine high lsa
lowcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS_low * ZLSA_high * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)

##high low
highcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS_high * ZLSA_low * ZFSG,
                       data = noout,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)

##high high
highcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                          Judgment + Judged.Value2 + ZCOS_high * ZLSA_high * ZFSG,
                        data = noout,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)

####moderation graphs -- recall####
##low cos
plot4 = ggplot(noout, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.316, slope = 4.116, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.136, slope = 2.601, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = -0.044, slope = 1.086, linetype = "+1SD ZLSA")) +
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
  geom_abline(aes(intercept = 0.369, slope = 3.281, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.301, slope = 3.084, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.234, slope = 2.889, linetype = "+1SD ZLSA")) +
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
  geom_abline(aes(intercept = .421, slope = 2.44, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .466, slope = 3.569, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .511, slope = 4.692, linetype = "+1SD ZLSA")) +
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

####single word norm models -- judgments####
overall.judge.sw.1 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.1)

overall.judge.sw.2 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.2)

overall.judge.sw.3 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.3)

overall.judge.sw.4 = lme(Judged.Value2 ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2 +
                           FSG * LSA * COS,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.judge.sw.4)

####single word norm models -- recall####
overall.recall.sw.1 = lme(Recall ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.recall.sw.1)

overall.recall.sw.2 = lme(Recall ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(recall.judge.sw.2)

overall.recall.sw.3 = lme(Recall ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.recall.sw.3)

overall.recall.sw.4 = lme(Recall ~ Judgment +
                           POS.1 + POS.2 + Subtlex.1 +Subtlex.2 + Length.1 + Length.2 + Phonemes.1 + Phonemes.2 + Syllables.1 + Syllables.2 + Morphemes.1 + Morphemes.2 +
                           AOA.1 + AOA.2 + Familiarity.1 + Familiarity.2 + Valence.1 + Valence.2 + Imageability.1 + Imageability.2 + QCON.1 + QCON.2 +
                           QSS.1 + TSS.2 + FSS.1 + FSS.2 + COSC.1 + COSC.2 + Ortho.1 + Ortho.2 + Phono.1 + Phono.2 +
                           FSG * LSA * COS,
                         data = noout, 
                         method = "ML", 
                         na.action = "na.omit",
                         random = ~1|Partno)
summary(overall.recall.sw.4)