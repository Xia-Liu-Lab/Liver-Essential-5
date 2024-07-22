
#2. Mediation analysis of sleep quality on the association of traditional healthy lifestyle with MAFLD
library(mediation)

SCC<-read.csv() #load data
SCC$lifepattern0<-NA
SCC<-within(SCC,{
  lifepattern0[lifepattern==0]<-1
  lifepattern0[lifepattern==1|lifepattern==2]<-0
})

SCC$lifepattern1<-NA
SCC<-within(SCC,{
  lifepattern1[lifepattern==1]<-1
  lifepattern1[lifepattern==0|lifepattern==2]<-0
})

#(1) Unadjusted for sleep quality
summary(glm(MAFLD~age+gender+education+marriage+income+lifepattern,family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(lifepattern,levels=c("2","1","0")),family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+lifepattern0+lifepattern1,family=binomial,data=SCC))

#(2) Adjusted for sleep quality
summary(glm(MAFLD~age+gender+education+marriage+income+lifepattern+sleep,family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(lifepattern,levels=c("2","1","0"))+sleep,family=binomial,data=SCC))
summary(glm(MAFLD~age+gender+education+marriage+income+lifepattern0+lifepattern1+sleep,family=binomial,data=SCC))

#(3) Mediation proportion
#total
med.xm <- glm(sleep~ lifepattern +age+gender+education+marriage+income, data = SCC)
med.xy <- glm(MAFLD ~sleep+lifepattern +age+gender+education+marriage+income, data = SCC,family=binomial)
set.seed(55555)
mod.med <- mediate(med.xm, med.xy, treat = 'lifepattern', mediator = 'sleep', sims = 100, boot = T)
summary(mod.med)
plot(mod.med)
#group
#---lifestyle pattern=1
med.xm <- glm(sleep~ lifepattern1+lifepattern0+age+gender+education+marriage+income, data = SCC)
med.xy <- glm(MAFLD ~sleep+lifepattern1+lifepattern0 +age+gender+education+marriage+income, data = SCC,family=binomial)
set.seed(55555)
mod.med <- mediate(med.xm, med.xy, treat = 'lifepattern1', mediator = 'sleep', sims = 100, boot = T)
summary(mod.med)
plot(mod.med)
#---lifestyle pattern=0
set.seed(55555)
mod.med <- mediate(med.xm, med.xy, treat = 'lifepattern0', mediator = 'sleep', sims = 100, boot = T)
summary(mod.med)
plot(mod.med)
