
#interaction and joint analysis
SCC<-read.csv() #load SCC data

#(1) interaction
library(epiR)
#multiplicative interaction
a<-glm(MAFLD~age+gender+education+marriage+income+lifepattern*pattern,data=SCC,family=binomial)
summary(a)
#additve interaction
int<-subset(SCC,lifepattern!=1)
int$add_life<-NA
int<-within(int,{
  add_life[lifepattern==2]<-0
  add_life[lifepattern==0]<-1
})
int$unhl_sleep<-NA
int<-within(int,{
  unhl_sleep[HL_sleep==1]<-0
  unhl_sleep[HL_sleep==0]<-1
})

inter<-glm(MAFLD~age+gender+education+marriage+income+add_life*unhl_sleep,data=int,family=binomial)
summary(inter)
b<-summary(inter)
epi.interaction(model=inter,param="product",coef=c(7,8,9),conf.level = 0.95)

#(2) Association of traditional healthy lifestyle with MAFLD according to sleep quality
SCC$lifescore01<-NA
SCC<-within(SCC,{
  lifescore01[lifescore==0]<-0
  lifescore01[lifescore==1]<-0
  lifescore01[lifescore==2]<-1
  lifescore01[lifescore==3]<-2
  lifescore01[lifescore==4]<-3
})
#poor sleep
poor<-subset(SCC,pattern==0)
summary(glm(MAFLD~age+gender+education+marriage+income+lifescore,data=poor,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+lifescore01,data=poor,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(lifescore01),data=poor,family=binomial()))

#medium sleep
medi<-subset(SCC,pattern==1)
summary(glm(MAFLD~age+gender+education+marriage+income+lifescore,data=medi,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+lifescore01,data=medi,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(lifescore01),data=medi,family=binomial()))
#good sleep
good<-subset(SCC,pattern==2)
summary(glm(MAFLD~age+gender+education+marriage+income+lifescore,data=good,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+lifescore01,data=good,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(lifescore01),data=good,family=binomial()))

#(3) Joint analysis
table(SCC$life_sleep) 
#participants were categorized into six groups according to traditional HLS (unfavorable, average, and favorable) and sleep quality (good, intermediate to poor)
summary(glm(MAFLD~age+gender+education+marriage+income+life_sleep,data=SCC,family=binomial()))
summary(glm(MAFLD~age+gender+education+marriage+income+factor(life_sleep),data=SCC,family=binomial()))
