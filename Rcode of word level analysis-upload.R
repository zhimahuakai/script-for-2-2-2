
##Adult Age Differences in Parafoveal Preview Effects during Reading: Evidence from Chinese##

######word-level analysis

# Here we will look at the analysing a 2 x 2 x 2 design which has:
# 2 levels of group  (1y, 2o)
# 2 levels of N1  (1I, 2P)
# 2 levels of N2  (1I, 2P)


####Before running the code, some packages need to be installed and loaded####

library(languageR) #version 1.5.0
library(lme4) #version 1.1-23
library(lattice) #version 0.20-38
library(MASS) #version 7.3-51.5
library(plyr) #version 1.8.6
library(lmerTest) #version3.1-2


########################################################

rm(list=ls())

# Open "2x2x2Data"

datafile = read.csv(file.choose(), sep = ",", dec = ".")
colnames(datafile)

###################################################################################################################################
col.subject = 1      # specify which column your participant number is in
col.condition = 6   # specify which column contains your condition number (used for excluding outliers)
col.stim = 2         # specify which column your item number is in
crit = 3           # setting the criterium value you want, usually 3 or 2.5 standard deviations

# choose dependent variable - here I have several measures - I will use total reading times on word n

measure = "SFD"


# add to dataframe
datafile$depvar = datafile[,measure]
# Work out which colums the fixed and random factors are in
datafile$pp = datafile[,col.subject]
datafile$condition = datafile[,col.condition]
datafile$stim = datafile[,col.stim]

# Let's make sure all the variables are from the correct class
datafile$depvar = as.numeric(datafile$depvar)##numer
datafile$pp = as.factor(datafile$pp)
datafile$stim = as.factor(datafile$stim)
datafile $ N1 <- as.factor(datafile $ N1)
datafile $ N2 <- as.factor(datafile $ N2)
datafile $ group <- as.factor(datafile $ group)

str(datafile)
#############################
##############################
####?Calculate Means, Standard Deviations and Standard Errors####

#make a matrix with the means per subject and per condition for the dependent variable
mean.matrix = tapply(datafile$depvar, list(datafile$pp, datafile$condition), mean, na.rm = T)
mean.matrix

#make a matrix with the standard deviations per subject and per condition for the dependent variable
sd.matrix = tapply(datafile$depvar, list(datafile$pp, datafile$condition), sd, na.rm = T)
sd.matrix

#make a matrix with the means per subject and per condition for the dependent variable
mean.matrix = tapply(datafile$depvar, list(datafile$pp, datafile$condition), mean, na.rm = T)
mean.matrix

##  Calculate the grand mean, grand SD, and grand SE by condition
grand.mean=apply(mean.matrix, 2, mean, na.rm = T)

grand.mean
grand.sd=apply(mean.matrix, 2, sd, na.rm = T)

grand.sd
grand.se=grand.sd/sqrt(88)  ##The number of the participants is 88. 
grand.se


####################################################################################################

######## Remove outliers#######

##Remove data beyond 3 standard deviations

nrcolumns = ncol(datafile)   # We'll be using this for cleanup later

#let's first have a look

head(datafile)


# make a new datafile with fixation durations > 0 ms
between1 = datafile[datafile$depvar > 0, ]
head(between1)

between1 = datafile


#make a matrix with the means per subject and per condition for the dependent variable
mean.matrix = tapply(between1$depvar, list(between1$pp, between1$condition), mean, na.rm = T)
mean.matrix


#make a matrix with the standard deviations per subject and per condition for the dependent variable
sd.matrix = tapply(between1$depvar, list(between1$pp, between1$condition), sd, na.rm = T)
sd.matrix

#add this data to the actual dataframe
for(i in 1:nrow(between1)){between1$mean.sc[i] = mean.matrix[between1$pp[i],between1$condition[i]]}
for(i in 1:nrow(between1)){between1$sd.sc[i] = sd.matrix[between1$pp[i],between1$condition[i]]}


#calculate z-scores
for(i in 1:nrow(between1)) {between1$zscore[i] = (between1$depvar[i] - between1$mean.sc[i])/between1$sd.sc[i]}

# assign a zero value to cells with only 1 observation
between1$zscore[is.na(between1$zscore)] = 0

#make new matrix with only standard deviations below a certain criterium of z-score (in absolute value)

result = between1[abs(between1$zscore) < crit,]

#you might want to clean up this data file by removing the added columns
result = result[,1:nrcolumns]


tussen1 = result    # insert the name from the datafile coming from the cleanup on the right
datafile = tussen1
nrow(datafile) # check the observations after remove outlier


################################################################
###########

##table of means for group * N1 * N2
(table1 <- ddply(datafile, .(group, N1 , N2 ), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))
# table of means for N1, N2
(table2 <- ddply(datafile, .(N1, N2), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for group * N2
(table3 <- ddply(datafile, .(group, N2), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for group * N1
(table4 <- ddply(datafile, .(group, N1), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for N2
(table5 <- ddply(datafile, .(N2), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for N1
(table6 <- ddply(datafile, .(N1 ), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for group
(table7 <- ddply(datafile, .(group), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

	
########################################################################################################
# Do we need to log transform?
qqnorm(datafile$depvar)
qqnorm(log(datafile$depvar))
# If yes ...
datafile$depvar = log(datafile$depvar)

# How does it look at an individual level
qqmath(~depvar|pp, data = datafile)


####How does it look at an item level
qqmath(~depvar|stim, data = datafile)

head(datafile)
#####################################################################################################
##############################################

contrasts(datafile $ N1) <- contr.sdif(2)
contrasts(datafile $ N2) <- contr.sdif(2)
contrasts(datafile $ group) <- contr.sdif(2)

##################################
################################## 

##1. full model   

depvar.lmerfull = lmer(depvar ~ N1 * N2* group+ (1+ N1 * N2 |pp) + (1+ N1 * N2 |stim), datafile)
summary(depvar.lmerfull, corr = FALSE)

##############################################################
##############################################################

# 2. Remove correlations of item 

depvar.lmer2 = lmer(depvar ~ N1 * N2* group+ (1+ N1 * N2 |pp) + (0 + N1 * N2 |stim), datafile)
summary(depvar.lmer2, corr = FALSE)

#############################################################################################

## 3.remove interactions (with correlation back in) of item 

depvar.lmer3 = lmer(depvar ~ N1 * N2* group+ (1+ N1 * N2 |pp) + (1 + N1 + N2 |stim), datafile)
summary(depvar.lmer3, corr = FALSE)

#############################################################################################

### 4. Remove correlation and interactions of item 

depvar.lmer4 = lmer(depvar ~ N1 * N2* group+ (1+ N1 * N2 |pp) + (0 + N1 + N2 |stim), datafile)
summary(depvar.lmer4, corr = FALSE)


############################################################################################
### 5. remove one of the slopes of item 

#5a.
depvar.lmer5a = lmer(depvar ~ N1 * N2* group+ (1+ N1 * N2 |pp) + ( 1+ N1|stim), datafile)
summary(depvar.lmer5a, corr = FALSE)


#5b.
depvar.lmer5b = lmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 1+ N2 |stim), datafile)
summary(depvar.lmer5b, corr = FALSE)

###############################################################################################
# 6. Again, without correlation 
#6a.
depvar.lmer6a = lmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 0+ N1|stim), datafile)
summary(depvar.lmer6a, corr = FALSE)

#6b.
depvar.lmer6b = lmer(depvar ~ N1 * N2* group+ age+education+digit+vcabulary+acuityF+acuityN+accuracy+(1+ N1 * N2 |pp) + ( 0+ N2 |stim), datafile)
summary(depvar.lmer6b, corr = FALSE)

##################################################################################################
#7. Intercept only for items( 1|stim)

depvar.lmer7 = lmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 1|stim), datafile)
summary(depvar.lmer7, corr = FALSE)

##################################################################################################

## 8. Remove correlation for pp 

depvar.lmer8 = lmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (0 + N1 * N2 |pp) + ( 1|stim), datafile)
summary(depvar.lmer8, corr = FALSE)

###################################################################################################

# 9. Remove interaction (but with correlation back in) for pp

depvar.lmer9 = lmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+(1 + N1 + N2 |pp) + ( 1|stim), datafile)
summary(depvar.lmer9, corr = FALSE)

##################################################################################################

# 10. Remove interaction and correlation for pp

depvar.lmer10 = lmer(depvar ~ N1 * N2* group+ (0 + N1 + N2 |pp) + ( 1|stim), datafile)
summary(depvar.lmer10, corr = FALSE)

###############################################################################################

# 11. Remove one slope for pp


##11a.

depvar.lmer11a = lmer(depvar ~ N1 * N2* group+ (1 + N1|pp) + ( 1|stim), datafile)
summary(depvar.lmer11a, corr = FALSE)

##11b.
depvar.lmer11b = lmer(depvar ~ N1 * N2* group+(1 + N2|pp) + ( 1|stim), datafile)
summary(depvar.lmer11b, corr = FALSE)

###############################################################################################
# 12. Again without correlation for pp

##12a.

depvar.lmer12a = lmer(depvar ~ N1 * N2* group+ (0 + N1|pp) + ( 1|stim), datafile)
summary(depvar.lmer12a, corr = FALSE)

##12b.
depvar.lmer12b = lmer(depvar ~ N1 * N2* group+ (0 + N2 |pp) + ( 1|stim), datafile)
summary(depvar.lmer12b, corr = FALSE)

######################################################################################################

# 13. Intercepts only 

depvar.lmer13 = lmer(depvar ~ N1 * N2* group+ (1|pp) + (1|stim), datafile)
summary(depvar.lmer13, corr = FALSE)


##############################################################
######## SFD of Word N+1,run lmer5a
######## GD of Word N+2,run lmer5b
######## FFD and GD of WordN+1,SFD of Word N+2,run lmer11a
######## FFD and GD of Word N, run lmer11b
######## SFD of Word N,FFD of Word N+2 run lmer13

################################################################################
#####power analysis#########################################################
library(simr)

 Model = lmer(data=datafile, depvar ~  N1 * N2* group+ (1 |pp) + (1|stim))
 summary(Model)

###考察固定效应的power

ttest = simr::powerSim(fit = Model, # 要考察的模型
                       test = fixed('N1:N2:group', # 要考察的固定效应的名称
                                    method = 't'), # 选取检验方法，"t"or"f"
                       nsim=500) # 设置模拟次数，建议设置为500 (此时可以获取到较稳定的power)
ttest
#########考察主效应的power
PowerN2_ttest = simr::powerSim(fit = Model, test = fixed('N2', method = 't'), nsim=500)
PowerN2_ttest

#####################
#####confident analysis#########################################################

confint(depvar.lmer6a, method="Wald")### replace the name of the model with the one that run successfully. 


########################################

###simple analysis:two-way interaction###


##### 1. N1 & N2

depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , N1 =='1I'))
summary(depvar.lmer1, corr = FALSE)

depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , N1 =='2P'))
summary(depvar.lmer1, corr = FALSE)

#####2. N1 & group

depvar.lmer1 = lmer(depvar ~ N1 + (1 |pp) + (1 |stim ),subset ( datafile , group =='1Y'))
summary(depvar.lmer1, corr = FALSE)

depvar.lmer1 = lmer(depvar ~ N1 + (1 |pp) + (1 |stim ),subset ( datafile , group =='2O'))
summary(depvar.lmer1, corr = FALSE)

#####3. N2 & group

depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , group =='1Y'))
summary(depvar.lmer1, corr = FALSE)

depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , group =='2O'))
summary(depvar.lmer1, corr = FALSE)


################################################################################
#######simple analysis:three-way interaction#################

###1. older: n1 x n2

depvar.lmer1 = lmer(depvar ~ N1 * N2 + (1 |pp) + (1 |stim),subset(datafile,group =='2O'))
summary(depvar.lmer1, corr = FALSE)

##1.1 n1=I: n2 previews
depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim),subset(datafile,N1=='1I'& group =='2O'))
summary(depvar.lmer1, corr = FALSE)

##1.2 n1=P: n2previews
depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim),subset(datafile,N1=='2P'& group =='2O'))
summary(depvar.lmer1, corr = FALSE)

##################################
###2. younger: n1 x n2

depvar.lmer1 = lmer(depvar ~ N1*N2 + (1 |pp)+ (1 |stim) , subset(datafile,group =='1Y'))
summary(depvar.lmer1, corr = FALSE)

##2.1: n1=I: n2 previews
depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp)+ (1 |stim) ,subset(datafile,N1=='1I'& group =='1Y'))
summary(depvar.lmer1, corr = FALSE)

##2.2: n1=P: n2 previews
depvar.lmer1 = lmer(depvar ~ N2 + (1 |pp) + (1 |stim),subset(datafile,N1=='2P'& group =='1Y'))
summary(depvar.lmer1, corr = FALSE)



###########################################################################################################################################
###########################################################################################################################################################
###########The code for skipping
library(languageR)
library(lme4)
library(lattice)
library(MASS)
library(plyr)
library(lmerTest)


rm(list=ls())

# Open Data"
datafile = read.csv(file.choose(), sep = ",", dec = ".")
colnames(datafile)
######################################################################################################################################
#############################################################################################################################################
col.subject = 1      # specify which column your participant number is in
col.condition = 6  # specify which column contains your condition number (used for excluding outliers)
col.stim = 2         # specify which column your item number is in

# choose dependent variable
measure = "SP"

# add to dataframe
datafile$depvar = datafile[,measure]

# Work out which colums the fixed and random factors are in

datafile$pp = datafile[,col.subject]
datafile$condition = datafile[,col.condition]
datafile$stim = datafile[,col.stim]

# Let's make sure all the variables are from the correct class
datafile$depvar = as.numeric(datafile$depvar)
datafile$pp = as.factor(datafile$pp)
datafile$stim = as.factor(datafile$stim)
datafile$condition = as.factor(datafile$condition)
datafile $ N1 <- as.factor(datafile $ N1)
datafile $ N2 <- as.factor(datafile $ N2)
datafile $ group <- as.factor(datafile $ group)

######################################################################################################
######################################################################################################
##Code for calculating means between per pp and per condition

# table of means for group * N1 * N2
(table1 <- ddply(datafile, .(group, N1 , N2 ), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))
# table of means for N1, N2
(table2 <- ddply(datafile, .(N1, N2), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))


# table of means for group * N1
(table3 <- ddply(datafile, .(group, N1), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))


# table of means for group * N2
(table4 <- ddply(datafile, .(group, N2), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for N1
(table5 <- ddply(datafile, .(N1), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for N2
(table6 <- ddply(datafile, .(N2 ), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))

# table of means for group
(table7 <- ddply(datafile, .(group), summarise, N=length(depvar[!is.na(depvar)]), M=mean(depvar, na.rm = TRUE), SD=sd(depvar, na.rm = TRUE), SE=SD/sqrt(N) ))


########Add contrasts
contrasts(datafile$N1) <- contr.sdif(2)
contrasts(datafile$N2) <- contr.sdif(2)
contrasts(datafile$group) <- contr.sdif(2)
#########

##1. full model

depvar.glmerfull = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + (1+ N1 * N2 |stim), datafile, family = binomial)
summary(depvar.glmerfull, corr = FALSE)

##############################################################
##############################################################

# 2. Remove correlations of item 

depvar.glmer2 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + (0 + N1 * N2 |stim), control= glmerControl(optCtrl=list(maxfun = 2000)), datafile, family = binomial)
summary(depvar.glmer2, corr = FALSE)

#############################################################################################

## 3.remove interactions (with correlation back in) of item ??*??+??

depvar.glmer3 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + (1 + N1 + N2 |stim), datafile, family = binomial)
summary(depvar.glmer3, corr = FALSE)

#############################################################################################

### 4. Remove correlation and interactions of item

depvar.glmer4 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + (0 + N1 + N2 |stim), datafile, family = binomial)
summary(depvar.glmer4, corr = FALSE)


############################################################################################
### 5. remove one of the slopes of item 

#5a.
depvar.glmer5a = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 1+ N1|stim), datafile, family = binomial)
summary(depvar.glmer5a, corr = FALSE)


#5b.
depvar.glmer5b = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 1+ N2 |stim), datafile, family = binomial)
summary(depvar.glmer5b, corr = FALSE)

###############################################################################################
# 6. Again, without correlation 
#6a.
depvar.glmer6a = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 0+ N1|stim), datafile, family = binomial)
summary(depvar.glmer6a, corr = FALSE)

#6b.
depvar.glmer6b = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 0+ N2 |stim), datafile, family = binomial)
summary(depvar.glmer6b, corr = FALSE)

##################################################################################################
#7. Intercept only for items( 1|stim)

depvar.glmer7 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1+ N1 * N2 |pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer7, corr = FALSE)

##################################################################################################

## 8. Remove correlation for pp 

depvar.glmer8 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (0 + N1 * N2 |pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer8, corr = FALSE)

###################################################################################################

# 9. Remove interaction (but with correlation back in) for pp

depvar.glmer9 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1 + N1 + N2 |pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer9, corr = FALSE)

##################################################################################################

# 10. Remove interaction and correlation for pp

depvar.glmer10 = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (0 + N1 + N2 |pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer10, corr = FALSE)

###############################################################################################

# 11. Remove one slope for pp

##11a.

depvar.glmer11a = glmer(depvar ~ N1 * N2* group+ age+education+digit+vcabulary+acuityF+acuityN+accuracy+(1 + N1|pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer11a, corr = FALSE)

##11b.
depvar.glmer11b = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (1 + N2|pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer11b, corr = FALSE)

###############################################################################################
# 12. Again without correlation for pp

##12a.

depvar.glmer12a = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (0 + N1|pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer12a, corr = FALSE)

##12b.
depvar.glmer12b = glmer(depvar ~ N1 * N2* group+age+education+digit+vcabulary+acuityF+acuityN+accuracy+ (0 + N2 |pp) + ( 1|stim), datafile, family = binomial)
summary(depvar.glmer12b, corr = FALSE)

######################################################################################################

# 13. Intercepts only 

depvar.glmer13 = glmer(depvar ~ N1 * N2* group++age+education+digit+vcabulary+acuityF+acuityN+accuracy+(1|stim) +(1|pp), datafile, family = binomial)
summary(depvar.glmer13, corr = FALSE)



######## SP of Word N+2,run glmer10
######## SP of Word N+1, run glmer11b


################################################################################
###########################confident analysis###################################

confint(depvar.glmer13, method="Wald")### replace the name of the model with the one that run successfully. 


######################################################################################
############################simple analysis:two-way interaction#######################

##### 1. N1 & N2

depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , N1 =='1I'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , N1 =='2P'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

#####2. N1 & group

depvar.glmer1 = glmer(depvar ~ N1 + (1 |pp) + (1 |stim ),subset ( datafile , group =='1Y'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

depvar.glmer1 = glmer(depvar ~ N1 + (1 |pp) + (1 |stim ),subset ( datafile , group =='2O'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

#####3. N2 & group

depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp) + (1 |stim ),subset ( datafile , group =='1y'), family = binomial)
summary(depvar.glmer1, corr = FALSE)


depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp)+ (1 |stim ),subset ( datafile , group =='2o'), family = binomial)
summary(depvar.glmer1, corr = FALSE)


################################################################################
#######simple analysis:three-way interaction###

###1. older: n1 x n2

depvar.glmer1 = glmer(depvar ~ N1 * N2 + (1 |pp) + (1 |stim),subset(datafile,group =='2O'), family = binomial)
summary(depvar.glmer1, corr = FALSE)


####1.1 n1=I: n2 previews
depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp) + (1 |stim),subset(datafile,N1=='1I'& group =='2O'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

####1.2 n1=P: n2 previews
depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp) + (1 |stim),subset(datafile,N1=='2P'& group =='2O'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

##############
###2. younger: n1 x n2
depvar.glmer1 = glmer(depvar ~ N1*N2 + (1 |pp)+ (1 |stim) ,subset(datafile,group =='1Y'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

####2.1 n1=I: n2 previews
depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp)+ (1 |stim) ,subset(datafile,N1=='1I'& group =='1y'), family = binomial)
summary(depvar.glmer1, corr = FALSE)

##2.2 n1=P: n2 previews
depvar.glmer1 = glmer(depvar ~ N2 + (1 |pp) + (1 |stim),subset(datafile,N1=='2P'& group =='1y'), family = binomial)
summary(depvar.glmer1, corr = FALSE)
