##setup
dat = read.csv("first block analysis.csv")
colnames(dat)[2] = "Judgment"

p.value = function(x){
  if (x < .001) { return("< .001")}
  else { return(apa(x, 3, F))}
}

##Libraries
library(cowplot)
library(MOTE)
library(nlme)
library(MuMIn)
library(lme4)

options(scipen = 999)

####data screening####
##accuracy
summary(dat)

##missing data
table("judge" = is.na(dat$Judged.Value), "recall" = is.na(dat$Recall))

##outliers for judgments and recall
mahal = mahalanobis(dat[ , c(4,5)],
                    colMeans(dat[ , c(4,5)], na.rm = TRUE),
                    cov(dat[ , c(4,5)], use = "pairwise.complete.obs"))
cutoff = qchisq(1-.001, ncol(dat[ , c(4,5)]))
cutoff;ncol(dat[ , c(4,5)])
summary(mahal < cutoff)
no.out = subset(dat, mahal < cutoff)

no.out$Judged.Value2 = no.out$Judged.Value/100

##additivity
cor(no.out[ , 4:8], use = "pairwise.complete.obs")

####descriptive statistics####
meanJno = tapply(no.out$Judged.Value, no.out$Judgment, mean, na.rm = T)
meanRno = tapply(no.out$Recall, no.out$Judgment, mean, na.rm = T)
sdJno = tapply(no.out$Judged.Value, no.out$Judgment, sd, na.rm = T)
sdRno = tapply(no.out$Recall, no.out$Judgment, sd, na.rm = T)

meanJno;meanRno
sdJno;sdRno

####hyp2 -- judgments####
no.out$ZCOS = scale(no.out$COS, scale = F)
no.out$ZLSA = scale(no.out$LSA, scale = F)
no.out$ZFSG = scale(no.out$FSG, scale = F)

##overall model
overallh2 = lme(Judged.Value2 ~ Judgment + 
                  ZCOS * ZLSA * ZFSG, 
                data = no.out, 
                method = "ML", 
                na.action = "na.omit",
                random = ~1|Partno)

summary(overallh2)

####moderation stuff judgments####
##setup
no.out$ZCOS_low = no.out$ZCOS + sd(no.out$ZCOS, na.rm = TRUE)
no.out$ZCOS_high = no.out$ZCOS - sd(no.out$ZCOS, na.rm = TRUE)

##low cosine
lowcos = lme(Judged.Value2 ~ Judgment +
               ZCOS_low * ZLSA * ZFSG, 
             data = no.out, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|Partno)
summary(lowcos)

##high cosine
highcos = lme(Judged.Value2 ~ Judgment +
                ZCOS_high * ZLSA * ZFSG, 
              data = no.out, 
              method = "ML", 
              na.action = "na.omit",
              random = ~1|Partno)
summary(highcos)

####examine the two way interactions by splitting LSA####
##setup
no.out$ZLSA_low = no.out$ZLSA + sd(no.out$ZLSA, na.rm = TRUE)
no.out$ZLSA_high = no.out$ZLSA - sd(no.out$ZLSA, na.rm = TRUE)

##low cosine, low lsa
lowcoslowlsa = lme(Judged.Value2 ~ Judgment +
                     ZCOS_low * ZLSA_low * ZFSG, 
                   data = no.out, 
                   method = "ML", 
                   na.action = "na.omit",
                   random = ~1|Partno)
summary(lowcoslowlsa)

##low cos, high lsa
lowcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS_low  * ZLSA_high * ZFSG, 
                    data = no.out, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(lowcoshighlsa)

##average cos, low lsa
agcoslowlsa = lme(Judged.Value2 ~ Judgment +
                    ZCOS * ZLSA_low * ZFSG, 
                  data = no.out, 
                  method = "ML", 
                  na.action = "na.omit",
                  random = ~1|Partno)
summary(agcoslowlsa)

##average cos, high lsa
avgcoshighlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS  * ZLSA_high * ZFSG, 
                    data = no.out, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(avgcoshighlsa)

##highcos low lsa
highcoslowlsa = lme(Judged.Value2 ~ Judgment +
                      ZCOS_high * ZLSA_low * ZFSG, 
                    data = no.out, 
                    method = "ML", 
                    na.action = "na.omit",
                    random = ~1|Partno)
summary(highcoslowlsa)

##high cos high lsa
highcoshighlsa = lme(Judged.Value2 ~ Judgment +
                       ZCOS_high  * ZLSA_high * ZFSG, 
                     data = no.out, 
                     method = "ML", 
                     na.action = "na.omit",
                     random = ~1|Partno)
summary(highcoshighlsa)

#####moderation graphs -- judgments####
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

dat.2 = no.out

##low cos
plot1 = ggplot(dat.2, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgments") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .558, slope = .901, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .562, slope = .417, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .568, slope = -.066, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot2 = ggplot(dat.2, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgments") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .521, slope = .596, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .534, slope = .330, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .548, slope = .064, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS") 

##high cos
plot3 = ggplot(dat.2, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Judgments") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .484, slope = .290, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .506, slope = .243, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .528, slope = .195, linetype = "+1SD ZLSA")) +
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

####hyp 3 -- recall####
overallh3 = glmer(Recall ~ (1|Partno) + Judgment + 
                    Judged.Value2 + ZCOS * ZLSA * ZFSG,
                  data = no.out,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(overallh3)

####moderation stuff -- recall####
#low cosine
lowcos2 = glmer(Recall ~ (1|Partno) +
                  Judgment + Judged.Value2 + ZCOS_low * ZLSA * ZFSG,
                data = no.out,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(lowcos2)

##high cosine
hicos2 = glmer(Recall ~ (1|Partno) + 
                 Judgment + Judged.Value2 + ZCOS_high * ZLSA * ZFSG,
               data = no.out,
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(hicos2)

####splitting lsa by cosine strength####
##low cosine low lsa
lowcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                        Judgment + Judged.Value2 + ZCOS_low * ZLSA_low * ZFSG,
                      data = no.out,
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"),
                      nAGQ = 1)
summary(lowcoslowlsa2)

##low cosine high lsa
lowcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS_low * ZLSA_high * ZFSG,
                       data = no.out,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(lowcoshighlsa2)

highcoslowlsa2 = glmer(Recall ~ (1|Partno) + 
                         Judgment + Judged.Value2 + ZCOS_high * ZLSA_low * ZFSG,
                       data = no.out,
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"),
                       nAGQ = 1)
summary(highcoslowlsa2)

highcoshighlsa2 = glmer(Recall ~ (1|Partno) + 
                          Judgment + Judged.Value2 + ZCOS_high * ZLSA_high * ZFSG,
                        data = no.out,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"),
                        nAGQ = 1)
summary(highcoshighlsa2)

##average cos, low lsa
agcoslowlsa2 = glmer(Recall ~ (1|Partno) +
                    Judgment + Judged.Value2 + ZCOS * ZLSA_low * ZFSG, 
                  data = no.out, 
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(agcoslowlsa2)

##average cos, high lsa
avgcoshighlsa2 = glmer(Recall ~ (1|Partno) +
                      Judgment + Judged.Value2 + ZCOS * ZLSA_high * ZFSG, 
                    data = no.out, 
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"),
                    nAGQ = 1)
summary(avgcoshighlsa2)

####moderation graphs -- recall####
##low cos
plot4 = ggplot(dat.2, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.1631, slope = 3.409, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = -0.015, slope = 2.678, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = -0.193, slope = 1.947, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Low ZCOS") 

##avg cos
plot5 = ggplot(dat.2, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = 0.261, slope = 2.317, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = 0.216, slope = 2.808, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = 0.171, slope = 3.300, linetype = "+1SD ZLSA")) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        breaks = c("-1SD ZLSA", "Average ZLSA", "+1SD ZLSA"),
                        name = "Simple Slope") +
  coord_cartesian(xlim = c(-.20, .60), ylim = c(.1, 1)) +
  geom_vline(xintercept = -.30) +
  geom_hline(yintercept = 0) +
  cleanup + 
  labs(title="Average ZCOS") 

##high cos
plot6 = ggplot(dat.2, aes(x = ZCOS_low, y = Judged.Value2)) +
  labs(x = "ZFSG", y = "Recall") +
  scale_size_continuous(guide = FALSE) +
  geom_abline(aes(intercept = .359, slope = 1.224, linetype = "-1SD ZLSA")) +
  geom_abline(aes(intercept = .447, slope = 2.938, linetype = "Average ZLSA")) +
  geom_abline(aes(intercept = .535, slope = 4.652, linetype = "+1SD ZLSA")) +
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