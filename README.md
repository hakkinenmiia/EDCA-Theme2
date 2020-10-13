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

#Q2
# loading the required packages
library(ggplot2)
library(ggmap)
library(qmplot)

#automatic data import 
topsoil <- topsoilvariability_R
names(topsoil)

top.var <- subset(topsoil, `Sample type` == c("topsoil site"))

##Mapping the measuring points 
#get map
#the map didn't work without API key from google maps

map <- get_map(location = c(lon = mean(top.var$Long), lat = mean(top.var$Lat)), zoom = 15,
               maptype = "satellite", scale = 2) 

area <- ggmap(map)  + geom_point(data=top.var, aes(top.var$Long, top.var$Lat), color="red")
area

#These are copied and modified from the lecture script

# define gstat object and compute variogram:
#id=identifier of the new variable
#formula=defines the dependent variable as a linear model of independent varaibles
#for ordinary and simple kirging formula is z~1
#for universal kriging where z is linearly dependen on x and y, use formula z~x+y
g = gstat(id = c("Cu (mg/kg) 0.43 M HNO3"), formula =top.var$`Cu (mg/kg) 0.43 M HNO3`~1, data=top.var)
#this line works but the next command vg doesn't work so maybe something is wrong here,
#I also found odd that g only gets one value. 
?gstat

vg = variogram(g) #<-- getting error message here and don't know how to proceed 

vg = variogram(g, width = 20000, cutoff = 600000)
plot(vg, plot.numbers = TRUE)

vgm = vgm(nugget=20000, psill=80000, range=2E5, model="Exp")
?vgm
plot(vg, vgm)

###################
##SEMIVARIOGRAM 
# loading the required packages
library(ggplot2)
library(ggmap)
library(qmplot)

# data import and views
topsoil <- read.csv("topsoilvariability_R_GH.csv", sep=";", header=TRUE)
names(topsoil)
top.var <- subset(topsoil, `Sample.type` == c("topsoil site"))
dim(top.var)
summary(top.var)
hist(top.var$Cu,col='Lightblue')  # looks pretty symmetric so no need for transformation
hist(top.var$Zn,col='Orange')  # looks pretty symmetric so no need for transformation

#data subsets into west and east
west <- subset(top.var, Area==c("W"))
east <- subset(top.var, Area==c("E"))

# variogram computation
library(sp)
library(gstat)

###West area, copper 
coordinates(west) =~Lat+Long
spplot(west, zcol="Cu")

gwCu = gstat(id = c("Cu"), formula = Cu~1, data = west)
vgwCu = variogram(gwCu, width=2E-4,cutoff=2.4E-1)
plot(vgwCu, plot.nu=TRUE)


vgm_wcu = vgm(nugget=2, psill=28, range=0.002, model="Exp")

plot(vgwCu, vgm_wcu)
vgm2 = fit.variogram(vgwCu, vgm_wcu)
plot(vgwCu, vgm2)
#parameter estimations: nugget=0, sill=30, range=0.003

var(west$Cu) #24.866 <-- lower than sill in the graph 

###East area, copper 
coordinates(east) =~Lat+Long
spplot(east, zcol="Cu")

geCu = gstat(id = c("Cu"), formula = Cu~1, data = east)
vgeCu = variogram(geCu, width=2E-4,cutoff=2.4E-2)
plot(vgeCu, plot.nu=TRUE)


vgm_ecu = vgm(nugget=30, psill=120, range=0.002, model="Exp")
plot(vgeCu, vgm_ecu)

vgm3 = fit.variogram(vgeCu, vgm_ecu)
plot(vgeCu, vgm3)
#parameter estimations: nugget=50, sill=150, range=0.003

var(east$Cu) #120.223 <-- lower than in the graph 


###West area, zinc
coordinates(west) =~Lat+Long
spplot(west, zcol="Zn")

gwZn = gstat(id = c("Zn"), formula = Zn~1, data = west)
vgwZn = variogram(gwZn, width=2E-4,cutoff=2.4E-2)
plot(vgwZn, plot.nu=TRUE)


vgm_wzn = vgm(nugget=10, psill=1000, range=0.004, model="Exp")

plot(vgwZn, vgm_wzn)
vgm4 = fit.variogram(vgwZn, vgm_wzn)
plot(vgwZn, vgm4)
#parameter estimations: nugget=0, sill=500, range=0.002

var(west$Zn) #513.866 <-- same as sill in the graph 

###East area, zinc
coordinates(east) =~Lat+Long
spplot(east, zcol="Zn")

geZn = gstat(id = c("Zn"), formula = Zn~1, data = east)
vgeZn = variogram(geZn, width=2E-4,cutoff=2.4E-2)
plot(vgeZn, plot.nu=TRUE)


vgm_ezn = vgm(nugget=500, psill=2000, range=0.003, model="Exp")
plot(vgeZn, vgm_ezn)

vgm5 = fit.variogram(vgeZn, vgm_ezn)
plot(vgeZn, vgm5)
#parameter estimations: nugget=0, sill=500, range=0.002

var(east$Zn) #1788.069 <-- lower as sill in the graph 

