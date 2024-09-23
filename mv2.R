getwd()

library(tidyverse)
library(ISLR)



set.seed(18)

d<-Wage %>% 
  sample_n(1000) %>% 
  rename(salary=wage)

### multiple non linear polynomial model with interaction

m<-lm(log(salary)~poly(age,2)*health ,d)

library(effects)
plot(allEffects(m))

plot(
  allEffects(m),
  lines=list(multiline=T),
  confint=list(style="auto"))


library(sjPlot)

plot_model(m)

library(performance)

check_model(m)



### multiple non linear generalised additive models

library(gam)

gam1<-gam::gam(salary~s(year,df=4)+s(age,df=2)+education+jobclass,data = d)
summary(gam1)


par(mfrow=c(2,2))
plot.Gam(gam1,se=TRUE,col="green")



###  multiple logistics regression with interaction

m<-glm(health~jobclass*health_ins,d,family=binomial)
plot(allEffects(m))

plot_model(m,type="int")


library(emmeans)

emmeans(m,pairwise~jobclass|health_ins,adjust="fdr")$contrasts
emmeans(m,pairwise~health_ins|jobclass,adjust="fdr")$contrasts
emmeans(m,pairwise~jobclass*health_ins,adjust="fdr")$contrasts


##visualization

pwpp(emmeans(m,~health_ins*jobclass),type="response",adjust="fdr")+theme_minimal()





### multinomial logistic regression models with neural network


d<-foreign::read.dta("http://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
m<-nnet::multinom(prog~ses+write,data = d)
plot(allEffects(m),lines=list(multiline=TRUE),confint=list(style="auto"))





###multiple linear mixed effects models with interactions

library(lme4)
library(lmerTest)


set.seed(15)
d<-InstEval %>% 
  group_by(service,studage) %>% 
  sample_n(100) %>% 
  mutate(dept=as.numeric(dept))


m<-lm(y~service*studage,data = d)
m1<-lmer(y~service*studage+(1|s)+(1|d),data = d)


##compare models AIC BIC LOWER=BETTER
anova(m1,m)


plot(
  allEffects(m1),
  lines=list(multiline=TRUE),
  confint=list(style="auto")
)


##POST-HOC

emmeans(m1,pairwise~service|studage,adjust="none")$contrasts
pwpp(emmeans(m1,~service*studage),type="response",adjust="none")+theme_minimal()









###GAMs -generalised additive models (non-linear) |mixed effects models

library(mgcViz)

m<-gammV(y~s(as.numeric(d),k=3),random = list(s=~1),data = InstEval %>% slice(1:5000))

plot(m,allTerms = TRUE)




### kaplan meier survival model


library(survival)
library(survminer)


set.seed(90)
d<-lung %>% 
  filter(ph.ecog!=3) %>% 
  sample_n(100)

 
m<-survfit(surv(time,status) ~ ph.ecog, data=d)
ggsurvplot(m)



















###eponential parametric models

library(flexsurv)


ex<-flexsurvreg(surv(time,status)~factor(ph.ecog),data=d,dist="exponential")
ggsurvplot(ex)






###cox proportional hazard model


m<-coxph(surv(time,status)~age+sex+ph.ecog,data = d)


ggforest(m,d)
