install(tidyverse)  
install.packages("dplyr")
install.packages(c("ggplot2", "binom", "Barnard", "lattice", "DHARMa"))

#### Library####
library(tidyverse)
library(dplyr)
library(ggplot2) # load the package
library(binom)
library(ggpubr)
library(lattice)
library(lme4)
library(visreg)
library(DHARMa)
rm(list=ls())

#Enter data####
selection<-read.csv("selection1.6.csv")
selection
#look at headers
head(selection)
summary(selection)
names(selection)
str(selection)

selection$generation<-as.factor(selection$generation)

str(selection)


#subset wasp treatment#####
treatment<-as.factor(selection$treatment)
wasp<-subset(selection,treatment == "Wasp")
str(wasp)
wasp$generation<-as.factor(wasp$generation)
summary(wasp)

#subset from just wasp data into generations
Gen1<-subset(wasp, generation == "1")
Gen2<-subset(wasp, generation == "2")
Gen3<-subset(wasp, generation == "3")
Gen4<-subset(wasp, generation == "4")
Gen5<-subset(wasp, generation == "5")
Gen6<-subset(wasp, generation == "6")
#cIs gen 1####
gen1.CIs <- binom.confint(x=gen1.pooled$alive, n=gen1.pooled$nymph, methods="wilson")
gen1.CIs
gen1.plot<- cbind(gen1.pooled, gen1.CIs[1:20,4:6])
gen1.plot


gen1.pooled<- 
  Gen1%>%
  group_by (line) %>%
  summarise(across(c("nymph", "alive"), ~ sum(.x, na.rm = TRUE)))
summary(gen1.pooled)


  

summary (Gen6)

#plot gen 1 wasp data####

qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=gen1.plot, ylim=c(0,1))+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5, color = "blue") +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))


#Gen 2 wasp data 

gen2.CIs <- binom.confint(x=gen2.pooled$alive, n=gen2.pooled$nymph, methods="wilson")
gen2.CIs
gen2.plot<- cbind(gen2.pooled, gen2.CIs[1:20,4:6])
gen2.plot


gen2.pooled<- 
  Gen2%>%
  group_by (line) %>%
  summarise(across(c("nymph", "alive"), ~ sum(.x, na.rm = TRUE)))
summary(gen2.pooled)

qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=gen2.plot, ylim=c(0,1))+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5, color = "blue") +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))



#gen 3 data


gen3.pooled<- 
  Gen3%>%
  group_by (line) %>%
  summarise(across(c("nymph", "alive"), ~ sum(.x, na.rm = TRUE)))
summary(gen3.pooled)


gen3.CIs <- binom.confint(x=gen3.pooled$alive, n=gen3.pooled$nymph, methods="wilson")
gen3.CIs
gen3.plot<- cbind(gen3.pooled, gen3.CIs[1:20,4:6])
gen3.plot



qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=gen3.plot, ylim=c(0,1))+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5, color = "blue") +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))


#gen 4 data


gen4.pooled<- 
  Gen4%>%
  group_by (line) %>%
  summarise(across(c("nymph", "alive"), ~ sum(.x, na.rm = TRUE)))
summary(gen4.pooled)


gen4.CIs <- binom.confint(x=gen4.pooled$alive, n=gen4.pooled$nymph, methods="wilson")
gen4.CIs
gen4.plot<- cbind(gen4.pooled, gen4.CIs[1:20,4:6])
gen4.plot



qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=gen4.plot, ylim=c(0,1))+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5, color = "blue") +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))






#plots by generation gen1####
Tgen1<-subset(selection, generation == "1")
Tgen1.CIs <- binom.confint(x=Tgen1$alive, n=Tgen1$nymph, methods="wilson")
Tgen1.CIs
Tgen1.plot<- cbind(Tgen1, Tgen1.CIs[1:40,5:6])
Tgen1.plot


qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=Tgen1.plot, ylim=c(0,1), col=treatment, main= "generation 1")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))

#plots by gen 2####
Tgen2<-subset(selection, generation == "2")
Tgen2.CIs <- binom.confint(x=Tgen2$alive, n=Tgen2$nymph, methods="wilson")
Tgen2.CIs
Tgen2.plot<- cbind(Tgen2, Tgen2.CIs[1:40,5:6])
Tgen2.plot


qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=Tgen2.plot, ylim=c(0,1), col=treatment,main = "Generation 2")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))

#plots by gen 3#####
Tgen3<-subset(selection, generation == "3")
Tgen3.CIs <- binom.confint(x=Tgen3$alive, n=Tgen3$nymph, methods="wilson")
Tgen3.CIs
Tgen3.plot<- cbind(Tgen3, Tgen3.CIs[1:40,5:6])
Tgen3.plot


qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=Tgen3.plot, ylim=c(0,1), col=treatment,main = "Generation 3")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))


#plots by gen 4#####
Tgen4<-subset(selection, generation == "4")
Tgen4.CIs <- binom.confint(x=Tgen4$alive, n=Tgen4$nymph, methods="wilson")
Tgen4.CIs
Tgen4.plot<- cbind(Tgen4, Tgen4.CIs[1:40,5:6])
Tgen4.plot


qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=Tgen4.plot, ylim=c(0,1), col=treatment,main = "Generation 4")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 40, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))





#plots by gen 5#####

Tgen5<-subset(selection, generation == "5")
Tgen5.CIs <- binom.confint(x=Tgen5$alive, n=Tgen5$nymph, methods="wilson")
Tgen5.CIs
Tgen5.plot<- cbind(Tgen5, Tgen5.CIs[1:60,5:6])
Tgen5.plot


qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=Tgen5.plot, ylim=c(0,1), col=treatment,main = "Generation 5")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 60, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))


#plots gen 6#####

Tgen6<-subset(selection, generation == "6")
Tgen6.CIs <- binom.confint(x=Tgen6$alive, n=Tgen6$nymph, methods="wilson")
Tgen6.CIs
Tgen6.plot<- cbind(Tgen6, Tgen6.CIs[1:100,5:6])
Tgen6.plot


qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=Tgen6.plot, ylim=c(0,1), col=treatment,main = "Generation 6")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 100, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))
wasp6<-subset(Tgen6, treatment=="Wasp")
summary(wasp6)
c6<-subset(Tgen6, treatment=="Control")
summary(c6)

#plots by wasp treatment#####

wasp<-subset(selection, treatment =="Wasp")
wasp.CIs <- binom.confint(x=wasp$alive, n=wasp$nymph, methods="wilson")
wasp.CIs
wasp.plot<- cbind(wasp, wasp.CIs[1:120,5:6])
wasp.plot

generation<-as.factor(selection$generation)

qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=wasp.plot, ylim=c(0,1), col=factor(generation), main= "Wasp ")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 120, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Genotype"))))

#control treatment####
control<-subset(selection, treatment =="Control")
control.CIs <- binom.confint(x=control$alive, n=control$nymph, methods="wilson")
control.CIs
control.plot<- cbind(control, control.CIs[1:120,5:6])
control.plot
cw<-rbind(control.plot, wasp.plot)
cw<-subset(cw,generation=="6")
cw

qplot(x=line, y=alive/nymph, ymin=lower, ymax=upper, data=cw, ylim=c(0,1), col=factor(treatment), main= "Generation 6")+
  geom_pointrange(aes(ymin=lower, ymax=upper), size = 0.5) +
  scale_x_continuous(breaks = seq(from = 1, to = 60, by = 1)) + 
  ylab (substitute(paste(bold("Proportional survival")))) +
  xlab (substitute(paste(bold("Line"))))
#descriptive stats####
Gen1<-subset(selection, generation=="1")
Gen1
Gen2<-subset(selection, generation=="2")
Gen2
wgen1<-subset(Gen1, treatment=="Wasp")
summary(wgen1)
wgen2<-subset(Gen2, treatment=="Wasp")
summary(wgen2)
cgen2<-subset(Gen2, treatment=="Control")
summary(cgen2)


#models1-6####
#generation as fixed factor####
m1<-glmer(cbind(dead,alive)~generation+ (1|line), family="binomial", data=wasp ) 
m1
summary(m1)
attach(wasp)
visreg(m1)
visreg(m1, "generation", scale= "response", ylim = c(0.5,0.65), rug = FALSE, gg=TRUE) +
  labs(
       y = "Proportional mortality",
       x = "Generation")
  
#add in individual level random effect####
wasp$datapoint<- 1:nrow(wasp)

m1x<-glmer(cbind(dead,alive)~generation+ (1|line)
           +(1|datapoint), family="binomial", data=wasp )
m1x
summary(m1x)

# generation as a covariate####

wasp$generation_cont<-as.integer(wasp$generation)

str(wasp)
m2<-glmer(cbind(dead,alive)~generation_cont+ (1|line), family="binomial", data=wasp)
m2
summary(m2)
visreg(m2, "generation_cont", scale= "response", ylim = c(0.5,0.65), rug = FALSE, gg=TRUE) +
  labs(
    y = "Proportional mortality",
    x = "Generation")


#check random effects####
m3<-glmer(cbind(dead,alive)~1+ (1|line), family="binomial", data=wasp) 
summary(m3)

detach(wasp)

#check dispersion####
library(DHARMa)

testDispersion(m1)
plotResiduals(m1)
testDispersion(m2)
testDispersion(m3)
testDispersion(m1x)
library(blmeco)

dispersion_glmer(m1)
dispersion_glmer(m2)
                 
#Violin plot 1-6####
v<-ggplot(wasp, aes(x=generation, y=X.alive)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(x="Generation", y = "Proportional survival")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  theme_classic()
v

v+geom_boxplot(width=0.1)

v2<-ggplot(control, aes(x=generation, y=X.alive)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(x="Generation", y = "Proportional survival")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  theme_classic()
v2

v2+geom_boxplot(width=0.1)
wasp2<-subset(selection,treatment == "Wasp")
conwas<-rbind(control, wasp2)
str(conwas)
conwas$treatment<-as.factor(conwas$treatment)


v3 <- conwas |> 
  ggplot(aes(x = generation, y = X.alive, fill = treatment)) +
  geom_violin(width = 5, position = position_dodge(width = 0.75)) +  
  labs(x = "Generation", y = "Proportional Survival", fill = "Treatment") +
  theme_classic()
v3

maternal<-subset(Tgen6, treatment=="Control wasp"| treatment=="Relaxed wasp")
maternalv<-ggplot(maternal, aes(x=treatment, y=X.alive)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(x="Treatment", y = "Proportional survival")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  theme_classic()

maternalv
m4<-glmer(cbind(dead,alive)~treatment+ (1|line), family="binomial", data=maternal) 
summary(m4)
maternalc<-subset(Tgen5, treatment=="Control"| treatment=="Relaxed control")
maternalcv<-ggplot(maternalc, aes(x=treatment, y=X.alive)) + 
  geom_violin(trim=FALSE, fill="gray")+
  labs(x="Treatment", y = "Proportional survival")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
  theme_classic()

maternalcv
summary(maternalc)

m5<-glmer(cbind(dead,alive)~treatment+ (1|line), family="binomial", data=maternalc) 
summary(m5)
