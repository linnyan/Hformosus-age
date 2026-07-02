
library(lme4)
library(ggplot2)
library(car)
library(factoextra)
library(MASS)
library(viridis) 
library(plotly)
library(plyr)
library(export)
library(emmeans)
library(MuMIn)

### receptivity and age ----
age=read.csv("age_trial.csv")
summary(age)
age$mature = ifelse(age$female_age < 0, 'imm', 'mat')
age$sp = as.factor(age$sp)
age_mat = subset(age, mature == "mat")
age_court = subset(age, court == 1)
age_court_mat = subset(age_mat, court == 1)
summary(age_court_mat)
ggplot(data = age_court_mat, aes(x = female_age, y = cop,color = sp)) + #geom_point()+
  geom_jitter(height = 0.05,alpha = 0.7,size = 2)+
  geom_smooth(method = "glm",
              method.args=list(family="binomial"))+
  theme_classic()+scale_color_manual(values=c( "#D55E00","#009E73"))
graph2ppt(file = "cp both sp mature.pptx",width = 10,height = 3)
graph2ppt(file = "cp both sp mature tall.pptx",width = 10,height = 6)

#consider all trials including non-courting ones
mod_f = glm(cop~female_age,data = age, family = binomial(link = 'logit'))
Anova(mod_f)
qqPlot(resid(mod_f))


#### (H.form) male courtship and female age ----
cp_male = read.csv("total duration.csv")
cp_male_imm = subset(cp_male, f_age<=0)
cp_male_10 = subset(cp_male, f_age<10 & f_age >0)
cp_male_20 = subset(cp_male, f_age<= 20 & f_age >=10)
cp_male_30 = subset(cp_male, f_age <=30 & f_age > 20)

cp_male_imm$age_group = "imm"
cp_male_10$age_group = "10"
cp_male_20$age_group = "20"
cp_male_30$age_group = "30"
cp_male = rbind(cp_male_imm,cp_male_10,cp_male_20,cp_male_30)
cp_male$age_group = as.factor(cp_male$age_group)
cp_male_court = subset(cp_male, intro_mean >0 & intro_mean < 150)
summary(cp_male_court)
cp_male_fagg = subset(cp_male, f_agg_count >0)
summary(cp_male_fagg)
cp_male_fagg$age_group = factor(cp_male_fagg$age_group, levels = c("imm","10","20","30"))
ggplot(data = cp_male_fagg, aes(x = age_group, y = f_agg_dur))+
  geom_boxplot()+theme_classic()
graph2ppt(file = "cp female aggression.pptx",width = 10,height = 6)

cp_male_court$age_group = factor(cp_male_court$age_group, levels = c("imm","10","20","30"))
ggplot(data = cp_male_court, aes(x = age_group, y = intro_mean))+
  geom_boxplot()+theme_classic()
graph2ppt(file = "cp intro mean all age group.pptx",width = 10,height = 6)

mod_age_intro = lm(intro_mean~age_group, data = cp_male_court)
summary(mod_age_intro)
Anova(mod_age_intro)
qqPlot(resid(mod_age_intro))
emmeans(mod_age_intro,pairwise~age_group,adjust = "tukey",type = "response")

cp_male_vib = subset(cp_male_court, vib_mean >0)
ggplot(data = cp_male_vib, aes(x = age_group, y = vib_mean))+
  geom_boxplot()+theme_classic()
graph2ppt(file = "cp vib mean all age group.pptx",width = 10,height = 6)

mod_age_vib = lm(vib_mean~age_group, data = cp_male_court)
summary(mod_age_vib)
Anova(mod_age_vib)
qqPlot(resid(mod_age_vib))
emmeans(mod_age_vib,pairwise~age_group,adjust = "tukey",type = "response")

#same plot but only 10-20 and 20-30
cp_male_mate = subset(cp_male, f_age>=10)
summary(cp_male_mate)

ggplot(data = cp_male_mate, aes(x = vib_mean, y = cop, color = age_group))+
  geom_jitter(height = 0.02)+
  geom_smooth(method = "glm",method.args=list(family="binomial"))+
  theme_classic()+scale_color_manual(values=c( "#0072B2", "#E69F00"))
graph2ppt(file = "cp 20 30 vib log.pptx",width = 10,height = 6)

ggplot(data = cp_male_mate, aes(x = intro_mean, y = cop, color = age_group))+
  geom_jitter(height = 0.02)+
  geom_smooth(method = "glm",method.args=list(family="binomial"))+
  theme_classic()+scale_color_manual(values=c( "#0072B2", "#E69F00"))
graph2ppt(file = "cp 20 30 intro log.pptx",width = 10,height = 6)

mod_cp0 = glm(cop~1,data = cp_male_mate,
              family = binomial(link = 'logit'))
mod_cpfull = glm(cop~age_group*vib_mean*intro_mean,data = cp_male_mate,
                 family = binomial(link = 'logit'))
summary(mod_cpfull)
Anova(mod_cpfull,type = "III")
mod_cp1 = glm(cop~age_group*vib_mean+age_group*intro_mean,data = cp_male_mate,
              family = binomial(link = 'logit'))
summary(mod_cp1)
Anova(mod_cp1)

mod_cp2 = glm(cop~vib_mean+age_group*intro_mean,data = cp_male_mate,
              family = binomial(link = 'logit'))
summary(mod_cp2)
Anova(mod_cp2,type = "III")

mod_cp3 = glm(cop~vib_mean+intro_mean+age_group,data = cp_male_mate,
              family = binomial(link = 'logit'))
summary(mod_cp3)

mod_cp4 = glm(cop~vib_mean,data = cp_male_mate,
              family = binomial(link = 'logit'))
summary(mod_cp4)
mod_cp5 = glm(cop~age_group,data = cp_male_mate,
              family = binomial(link = 'logit'))
summary(mod_cp5)

model.sel(mod_cp0,mod_cpfull,mod_cp1,mod_cp2,mod_cp3,mod_cp4,mod_cp5)#mod2
Anova(mod_cp2,type = "III")
