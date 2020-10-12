# EDCA-Theme2
EDCA course at WUR, theme 2 group project 
#### THEME 2 PROJECT Data analyis 
#################################
rm(list = ls())

#potentially useful libraries to download:
library(readr)
library(reshape)
library(ggmap)
library(ggplot2)
library(oce)
library(effects)
library(stats)
library(stats4)
library(vegan)
library(car)
library(ggpubr)
library(reshape2)
library(foreign)
library(RColorBrewer)
library(sp)
library(rgdal)
library(maptools)
library(gstat)

###### DATA ANALYSIS 
#DATA IMPORT 
##Test for soil data - just fooling around and trying things!
# read data file:
#Data files: soiltransect, topsoilvariability, lake water and lake sediment 
soil = read.table("soiltransect_R.csv", sep=";", header=T)
#automatic data import 
topsoil <- topsoilvariability_R
names(topsoil)

##Sorting out data 
#subsets
west <- subset(topsoil, Area==c("W"))
east <- subset(topsoil, Area==c("E"))

#RQ 1 

# some info of data 
dim(soil) #dimentions 80 rown in 15 columns
class(soil) #type of data file - data frame 
names(soil) #column names 
summary(soil) #basic statistics of each column

#Basic statistics 
mean()
sd()

boxplot(topsoil$`Cu (mg/kg) 0.43 M HNO3`~topsoil$Area, ylab="Cu (mg/kg) 0.43 HNO3")

#Normality test to prove that our data is parametric: 
s <- glm(y~x)
shapiro.test(resid(s)) # parametric when p-value > 0.05

# test for homogeniety of variance:
bartlett.test(y~x) 
leveneTest(y~x) # parametric when p-value > 0.05

#visual check: 
hist(resid(s))
qqnorm(resid(s))
qqline(resid(s))

#t-test between areas
#--> use for lake sediment, water samples, soil samples to find out significant differences 

#the codes below are from another script modify to right dataset 
#t-test
t.test(ns_high$ability~ns_high$treatment, subset = ns_high$treatment %in% c("NP", "nanosilica"))
#2-way ANOVA   
#with interactions
res.aov2 <- aov(nd$ability~nd$treatment + nd$concentration + nd$treatment * nd$concentration)
plot(res.aov2)

summary(res.aov2)




