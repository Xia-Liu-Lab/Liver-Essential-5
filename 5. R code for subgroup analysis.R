# Subgroup analysis
SCC<-read.csv()#load SCC data
subgroup<-SCC

#taking the subgroup analysis of age group for example
subgroup$age_group<-NA
subgroup<-within(subgroup,{
  age_group[age<60]<-0
  age_group[age>=60]<-1
})
age_0<-subset(subgroup,age_group==0)
age_1<-subset(subgroup,age_group==1)
#interaction and subgroup effect
summary(glm(MAFLD~gender+education+marriage+income+total_score*age_group,family=binomial,data=subgroup))
summary(glm(MAFLD~gender+education+marriage+income+total_score,family=binomial,data=age_0))
summary(glm(MAFLD~gender+education+marriage+income+total_score,family=binomial,data=age_1))
