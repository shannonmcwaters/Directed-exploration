rm(list=ls())

library(tidyverse)

#keeping the data sets seperate for ease and lack of confusion, downloads directly from csv
#DirectedData <- read_csv("https://docs.google.com/spreadsheets/d/1ovsN4S_WVcxDk0N7of0IAYOoADBa6VeDisBH5x6cBQw/gviz/tq?tqx=out:csv")
DirectedData <- read.csv("https://raw.githubusercontent.com/shannonmcwaters/Directed-exploration/main/Maze%20Data%20Raw%20-%20BeesRandom.csv")

######## Directed #######

#Remove first Trial
#DirectedData<-subset(DirectedData,Order!=1)

#Subset Directed by Horizon
HorizonOne<-subset(DirectedData,DirectedData$Horizon==1)
HorizonSix<-subset(DirectedData,DirectedData$Horizon==6)

#Combine The Two
DirectedData<-rbind(HorizonOne,HorizonSix)

#Check the two info and reward conditions
DirectedMatch<-subset(DirectedData,DirectedData$HighInfo==DirectedData$HighValue)
chisq.test(table(DirectedMatch$HighInfo))
DirectedMatchOne<-subset(HorizonOne,HighInfo==HighValue)
DirectedMatchSix<-subset(HorizonSix,HighInfo==HighValue)

DirectedDontMatch<-subset(DirectedData,DirectedData$HighInfo!=DirectedData$HighValue)
chisq.test(table(DirectedDontMatch$HighInfo))
DirectedDontMatchOne<-subset(HorizonOne,HighInfo!=HighValue)
DirectedDontMatchSix<-subset(HorizonSix,HighInfo!=HighValue)


PercentMatchInfoOne<-sum(DirectedMatchOne$HighInfo)/nrow(DirectedMatchOne)
PercentMatchInfoSix<-sum(DirectedMatchSix$HighInfo)/nrow(DirectedMatchSix)
PercentDontMatchInfoOne<-sum(DirectedDontMatchOne$HighInfo)/nrow(DirectedDontMatchOne)
PercentDontMatchInfoSix<-sum(DirectedDontMatchSix$HighInfo)/nrow(DirectedDontMatchSix)


PercentInfo<-sum(DirectedData$HighInfo)/nrow(DirectedData)

chisq.test(table(DirectedData$HighInfo))


vec<-c(PercentInfo)

df <- data.frame(trt = c("HighInfo"), outcome = vec)

df <- data.frame(trt = c("HighInfo"), outcome = vec)
ggplot(df, aes(trt, outcome)) + scale_fill_manual("darkblue") + 
  geom_col(fill="darkblue")+ggtitle("                Information-Seeking in Bumble Bees")+ylab("Informative Choice Probability")+ expand_limits(y=c(0,.5,1))+theme_classic() + geom_hline(yintercept=.5, linetype="dashed", color = "black", size=1.5)#+ geom_errorbar(aes(ymin=outcome-DirectedOnese, ymax=outcome+DirectedOnese), width=.2)



vec<-c(PercentDontMatchInfoSix,PercentMatchInfoSix,PercentDontMatchInfoOne,PercentMatchInfoOne)
cols<-c(pink,red,lightblue,navy)
df <- data.frame(trt = c("HighValue1", "HighValue6","LowValue1","LowValue6"), outcome = vec)
ggplot(df, aes(trt, outcome, fill=trt)) +
  geom_col()+ggtitle("                Directed Exploration in Bumble Bees")+xlab("Condition")+ylab("Informative Choice Probability")+ expand_limits(y=c(0,.5,1))+theme_classic() + geom_hline(yintercept=.5, linetype="dashed", color = "black", size=1.5)#+ geom_errorbar(aes(ymin=outcome-DirectedOnese, ymax=outcome+DirectedOnese), width=.2)






chisq.test(table(DirectedMatch$HighInfo))
chisq.test(table(DirectedMatchOne$HighInfo))
#need more
chisq.test(table(DirectedMatchSix$HighInfo))


#give us percents for graphing
PercentInfoOne<-sum(HorizonOne$HighInfo)/nrow(HorizonOne)
PercentInfoSix<-sum(HorizonSix$HighInfo)/nrow(HorizonSix)

#compare percents across Horizon
summary(chisq.test(table(DirectedData$Order,DirectedData$HighInfo)))


summary(chisq.test(table(DirectedData$Order)))

DirectedMatch<-subset(DirectedData,DirectedData$HighInfo==DirectedData$HighValue)
chisq.test(table(DirectedMatch$HighInfo))

DirectedDontMatch<-subset(DirectedData,DirectedData$HighInfo!=DirectedData$HighValue)
chisq.test(table(DirectedDontMatch$HighInfo))


DirectedOnese<-sd(HorizonOne$HighInfo)/sqrt(29) #.12
DirectedSixse<-sd(HorizonSix$HighInfo)/sqrt(15) #.11

#graph to compare info seeking(directed) between horizons
df <- data.frame(trt = c("1", "6"), outcome = c(PercentInfoOne,PercentInfoSix))
ggplot(df, aes(trt, outcome, fill=trt)) +
  geom_col()+ggtitle("                Directed Exploration in Bumble Bees")+xlab("Horizon")+ylab("Informative Choice Probability")+ expand_limits(y=c(0,.5,1))+theme_classic() + geom_hline(yintercept=.5, linetype="dashed", color = "black", size=1.5)+ geom_errorbar(aes(ymin=outcome-DirectedOnese, ymax=outcome+DirectedOnese), width=.2)


summary(chisq.test(table(HorizonSix$HighInfo)))




#Break down order in half to compare in Horizon One
HorizonOne$Half<-ifelse(HorizonOne$Order==1|HorizonOne$Order==2,"First","Second")

HorizonOneFirst<-subset(HorizonOne,HorizonOne$Half=="First")
HorizonOneSecond<-subset(HorizonOne,HorizonOne$Half=="Second")

PercentInfoFirst<-sum(HorizonOneFirst$HighInfo)/18
PercentInfoSecond<-sum(HorizonOneSecond$HighInfo)/11

chisq.test(table(HorizonOne$Half,HorizonOne$HighInfo))


####### Random ########

#Subset Random by Horizon
RHorizonOne<-subset(RandomData,RandomData$Horizon==1)
RHorizonSix<-subset(RandomData,RandomData$Horizon==6)


#Remove first Trial
#RHorizonOne<-subset(RHorizonOne,RHorizonOne$Order!=1)
#RHorizonSix<-subset(RHorizonSix,RHorizonSix$Order!=1)

#Combine The Two
RandomData<-rbind(RHorizonOne,RHorizonSix)


RPercentHighValueOne<-sum(RHorizonOne$HighValue)/nrow(RHorizonOne)
RPercentHighValueSix<-sum(RHorizonSix$HighValue)/nrow(RHorizonSix)




PercentHighValueOneFirst<-sum(HorizonOneFirstHalf$HighValue)/nrow(HorizonOneFirst)
PercentHighValueOneSecond<-sum(HorizonOneSecondHalf$HighValue)/nrow(HorizonSix)

####

#RandomData <- read.csv("https://docs.google.com/spreadsheets/d/1ooeEsMD_8FOR5TjEoRST7LMSvBqx59iZgH6opUNmvmU/gviz/tq?tqx=out:csv")
RandomData <- read.csv("https://docs.google.com/spreadsheets/d/1uErXpHRx3DQtmn1tGVxVdWCkXZxHlvbCZnNvm_h_pRw/gviz/tq?tqx=out:csv")


PercentLow<-sum(RandomData$HighValue)/nrow(RandomData)



chisq.test(table(RandomData$HighValue))


vec<-c(PercentLow)

df <- data.frame(trt = c("HighValue"), outcome = vec)

df <- data.frame(trt = c("HighValue"), outcome = vec)
ggplot(df, aes(trt, outcome)) + scale_fill_manual("red") + 
  geom_col(fill="red")+ggtitle("                Value-Seeking in Bumble Bees")+ylab("High Value Choice Probability")+ expand_limits(y=c(0,.5,1))+theme_classic() + geom_hline(yintercept=.5, linetype="dashed", color = "black", size=1.5)#+ geom_errorbar(aes(ymin=outcome-DirectedOnese, ymax=outcome+DirectedOnese), width=.2)



RandomOnese<-sd(RHorizonOne$HighValue)/nrow(RHorizonOne) #.12
RandomSixse<-sd(RHorizonSix$HighValue)/nrow(RHorizonSix) #.11

colors<-c("darkgreen","grey")
df <- data.frame(trt = c("1", "6"), outcome = c(RPercentHighValueOne, RPercentHighValueSix))
ggplot(df, aes(trt, outcome, fill=trt)) +
  geom_col(size=2)+ggtitle("                Random Exploration in Bumble Bees")+xlab("Horizon")+ylab("Error Probability") + scale_fill_manual(values=colors) + expand_limits(y=c(0,.5,1))+theme_classic()+ geom_hline(yintercept=.5, linetype="dashed", color = "black", size=1.5)+ geom_errorbar(aes(ymin=outcome-.11, ymax=outcome+.11), width=.2)


chisq.test(table(RHorizonOne$HighValue))
chisq.test(table(RHorizonSix$HighValue))

library(praise)
praise::praise()

library("beepr", lib.loc="~/R/win-library/3.4")
beep("fanfare")




