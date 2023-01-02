#task 1 : import data 
Abalone<-read.table(file=file.choose(),header=TRUE,sep=",",dec=".")
View(Abalone)
summary(Abalone)
str(Abalone)
dim(Abalone)

#task 2 : Data Pre-Processing 
#Outliers
#Analyze all database variables and detect outliers
#/////////Length variable////////
#graphical method
boxplot(Abalone$Length)
plot(Abalone$Length)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
up<-0.615+1.5*(0.615-0.450)
up

#LOWER BOUND
#Q1-1.5*(Q3-Q1)
lo<-0.450-1.5*(0.615-0.450)
lo
out=which(Abalone$Length<lo)
out
Abalone$Length=replace(Abalone$Length,out,NA)
View(Abalone$Length)


#///////Diameter variable//////
#graphical method
boxplot(Abalone$Diameter)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upD<-0.4800+1.5*(0.4800-0.3500)
upD
#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loD<-0.3500-1.5*(0.4800-0.3500)
loD

#Impute outliers
outd=which(Abalone$Diameter<loD)
outd
Abalone$Diameter=replace(Abalone$Diameter,outd,NA)
View(Abalone$Diameter)
outd=which(Abalone$Diameter<loD)
outd
#to verify that we don't have outliers
boxplot(Abalone$Diameter)

#////////////Height variable/////////////
#graphical method
boxplot(Abalone$Height)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upH<-0.1650+1.5*(0.1650-0.1150)
upH

outh=which(Abalone$Height>upH)
outh
Abalone$Height=replace(Abalone$Height,outh,NA)
View(Abalone$Height)
#to verify that we don't have outliers 
outh=which(Abalone$Height>upH)
outh

#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loH<-0.1150-1.5*(0.1650-0.1150)
loH

outLh=which(Abalone$Height<loH)
outLh
Abalone$Height=replace(Abalone$Height,outLh,NA)
View(Abalone$Height)
#to verify that we don't have outliers 
outLh=which(Abalone$Height<loH)
outLh


#//////////Whole_Weight variable///////////
#graphical method
boxplot(Abalone$Whole_Weight)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upW<-1.1530+1.5*(1.1530-0.4415)
upW

outw=which(Abalone$Whole_Weight>upW)
outw
Abalone$Whole_Weight=replace(Abalone$Whole_Weight,outw,NA)
View(Abalone$Whole_Weight)
#to verify that we don't have outliers 
outw=which(Abalone$Whole_Weight>upW)
outw


#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loW<-0.4415-1.5*(1.1530-0.4415)
loW


#////////////Shucked_Weight variable/////////
#graphical method
boxplot(Abalone$Shucked_Weight)
plot(Abalone$Shucked_Weight)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upSW<-0.4955+1.5*(0.4955-0.1840)
upSW

outSw=which(Abalone$Shucked_Weight>upSW)
outSw
Abalone$Shucked_Weight=replace(Abalone$Shucked_Weight,outSw,NA)
View(Abalone$Shucked_Weight)
#to verify that we don't have outliers 
outSw=which(Abalone$Shucked_Weight>upSW)
outSw

#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loSW<-0.1840-1.5*(0.4955-0.1840)
loSW

#///////////Viscera_Weight variable////////
#graphical method
boxplot(Abalone$Viscera_Weight)
plot(Abalone$Viscera_Weight)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upV<-0.2530+1.5*(0.2530-0.0935)
upV

outV=which(Abalone$Viscera_Weight>upV)
outV
Abalone$Viscera_Weight=replace(Abalone$Viscera_Weight,outV,NA)
View(Abalone$Viscera_Weight)
outV=which(Abalone$Viscera_Weight>upV)
outV
boxplot(Abalone$Viscera_Weight)

#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loV<-0.0935-1.5*(0.2530-0.0935)
loV



#///////////SHell_Weight variable////////
#graphical method
boxplot(Abalone$SHell_Weight)
plot(Abalone$SHell_Weight)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upSh<-0.3290+1.5*(0.3290-0.1300)
upSh

outSH=which(Abalone$SHell_Weight>upSh)
outSH
Abalone$SHell_Weight=replace(Abalone$SHell_Weight,outSH,NA)
View(Abalone$SHell_Weight)
outSH=which(Abalone$SHell_Weight>upSh)
outSH

#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loSh<-0.1300-1.5*(0.3290-0.1300)
loSh


#//////////Rings variable////////
#graphical method
boxplot(Abalone$Rings)
plot(Abalone$Rings)
#IQR
#UPPER BOUND 
#Q3+1.5*(Q3-Q1)
upR<-11.000+1.5*(11.000-8.000)
upR

outR=which(Abalone$Rings>upR)
outR
Abalone$Rings=replace(Abalone$Rings,outR,NA)
View(Abalone$Rings)
outR=which(Abalone$Rings>upR)
outR

#LOWER BOUND
#Q1-1.5*(Q3-Q1)
loR<-8.000-1.5*(11.000-8.000)
loR

outLR=which(Abalone$Rings<loR)
outLR
Abalone$Rings=replace(Abalone$Rings,outLR,NA)
View(Abalone$Rings)
outLR=which(Abalone$Rings<loR)
outLR

#////////Sex variable/////////

boxplot(Abalone$Length~Abalone$Sex)
plot(Abalone$Length~Abalone$Sex)

boxplot(Abalone$Diameter~Abalone$Sex)
plot(Abalone$Diameter~Abalone$Sex)

boxplot(Abalone$Height~Abalone$Sex)
plot(Abalone$Height~Abalone$Sex)

boxplot(Abalone$Whole_Weight~Abalone$Sex)
plot(Abalone$Whole_Weight~Abalone$Sex)

boxplot(Abalone$Shucked_Weight~Abalone$Sex)
plot(Abalone$Shucked_Weight~Abalone$Sex)


boxplot(Abalone$Viscera_Weight~Abalone$Sex)
plot(Abalone$Viscera_Weight~Abalone$Sex)


boxplot(Abalone$SHell_Weight~Abalone$Sex)
plot(Abalone$SHell_Weight~Abalone$Sex)

boxplot(Abalone$Rings~Abalone$Sex)
plot(Abalone$Rings~Abalone$Sex)

Abalone$Sex<-replace(Abalone$Sex,Abalone$Sex=="", NA)
Abalone$Sex
View(Abalone$Sex)     
#//////////
#======>We can notice points that are far from the extremities (MAX,Min) in all the boxplots
#We graphically detect the existence of outliers in all the different variables



#Missing values :
#Study the rate of missing values :

library(VIM)
aggr(Abalone)

##rate of NA values
rate=sum(is.na(Abalone))/prod(dim(Abalone))*100 
rate
str(Abalone)
#Imputation of missing values 
# Generalized imputation

install.packages("Hmisc")
library(Hmisc)
install.packages("stringi")
library(stringi)
library(Hmisc)
#Imputation of missing values 
# Generalized imputation

install.packages("Hmisc")
library(Hmisc)
install.packages("stringi")
library(stringi)
library(Hmisc)

#imputation of missing values by the KNN 
library(VIM)
df_knn <- kNN(Abalone)
df_knn <- df_knn[1:9]
View (df_knn)



#imputation of missing values by the HotDeck
df_hotdeck <- hotdeck(Abalone)
View (df_hotdeck)
View(Abalone)
#task3 : test the normality
#1st Method : 
#Quantitive variables 

# H0: Quantitative variable follow the the normal distriblution 
# H1: Quantitative variable does not follow the the normal distriblution

shapiro.test(df_knn $Length)
shapiro.test(df_knn $Diameter)
shapiro.test(df_knn $Height)
shapiro.test(df_knn $Whole_Weight)
shapiro.test(df_knn $Shucked_Weight)
shapiro.test(df_knn $Viscera_Weight)
shapiro.test(df_knn $SHell_Weight)
shapiro.test(df_knn $Rings)

# all p-values of each feature are less than 0.05 ,H1 is accepted
=> not a single feature follows the normal law

#Qualitative Variable
unique(df_knn$Sex)

#2nd Method(graphical method) 
#Quantitive variables 

hist(df_knn$Length, col='steelblue')
hist(df_knn$Diameter, col='steelblue')
hist(df_knn$Height, col='steelblue')
hist(df_knn$Whole_Weight, col='steelblue')
hist(df_knn$Shucked_Weight, col='steelblue')
hist(df_knn$Viscera_Weight, col='steelblue')
hist(df_knn$SHell_Weight, col='steelblue')
hist(df_knn$Rings, col='steelblue')

summary(df_knn)

#Qualitative Variable

pie(table(df_knn$Sex))
#test the variability within a group
#H1:varience are equals
#H2:variance not equals
bartlett.test(df_knn$Length~df_knn$Sex)
bartlett.test(df_knn$Diameter~df_knn$Sex)
bartlett.test(df_knn$Height~df_knn$Sex)
bartlett.test(df_knn$Whole_Weight~df_knn$Sex)
bartlett.test(df_knn$Shucked_Weight~df_knn$Sex)
bartlett.test(df_knn$Viscera_Weight~df_knn$Sex)
bartlett.test(df_knn$SHell_Weight~df_knn$Sex)
bartlett.test(df_knn$Rings~df_knn$Sex)

# All p-value < 0.05, we accept H1 = ” variances are not equals”.
#there a difference between the variences within a group.

# We have 3 modalities so we should work with kruskal test (to verify the dependency between variables)
#H1:there is an effect between variables
#H0:there is no effect between variables
kruskal.test(df_knn$Length~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Length on sex 
#Median not equals 

kruskal.test(df_knn$Diameter~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Diameteron sex 
#Median not equals 

kruskal.test(df_knn$Height~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Heighton sex 
#Median not equals 

kruskal.test(df_knn$Whole_Weight~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Whole_Weight on sex 
#Median not equals 

kruskal.test(df_knn$Shucked_Weight~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Shucked_Weighon sex 
#Median not equals

kruskal.test(df_knn$Viscera_Weight~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Viscera_Weighton sex 
#Median not equals

kruskal.test(df_knn$SHell_Weight~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of SHell_Weight on sex 
#Median not equals

kruskal.test(df_knn$Rings~df_knn$Sex)
#p-value<0.05
#weaccept h1 : There is an effect of Ringson sex 
#Median not equals


#////////////////////////////////////////////task 4//////////////////////////////////
#Since they don't follow the normal distribution we will use "spearman" for the correlation test 
## H0: r=0, No significant dependence between Variables
# H1: r=/0, There is a significant dependence between Variables
# we accept H1

cor.test(df_knn$Diameter,df_knn$Rings, method ="spearman")# rho=0.6192438 normal correlation
cor.test(df_knn$Diameter,df_knn$SHell_Weight, method ="spearman")# rho = 0.9545252 strong correlation
cor.test(df_knn$Diameter,df_knn$Viscera_Weight, method ="spearman")#0.9486862 strong correlation
cor.test(df_knn$Diameter,df_knn$Shucked_Weight, method ="spearman")#0.9508008 strong correlation
cor.test(df_knn$Diameter,df_knn$Whole_Weight, method ="spearman")#0.9716968 strong correlation
cor.test(df_knn$Diameter,df_knn$Height , method ="spearman")# 0.8977451 strong correlation
cor.test(df_knn$Diameter,df_knn$Length , method ="spearman")#0.9832713 strong correlation

#graphical method
par(mfrow=c(1,5))
plot(df_knn$Diameter,df_knn$Viscera_Weight,col="red")
plot(df_knn$Diameter,df_knn$SHell_Weight,col="green")
plot(df_knn$Diameter,df_knn$Shucked_Weight,col="hotpink")
plot(df_knn$Diameter,df_knn$Length,col="pink")
plot(df_knn$Diameter,df_knn$Whole_Weight,col="yellow")
#there is a highly linear correlation between Diameter ,..
#////////////////

par(mfrow=c(1,2))
plot(df_knn$Diameter,df_knn$Height,col="purple")
plot(df_knn$Diameter,df_knn$Rings,col="blue")
#there is no linear correlation between  Diameter and Rings
#there is a linear correlation between  Diameter and .... 

#/////////////////////
# correlation tests for whole dataset
install.packages("corrplot")
library(corrplot)
df = subset(df_knn, select = -c(Sex) )
my_data <- df[, c(1,2,3,4,5,6,7,8)]
res <- cor(my_data)
round(res, 2)
#graphical method
install.packages("ggcorrplot")
install.packages("GGally")
library(GGally)

library(ggcorrplot)
ggcorr( df_knn,palette="RdBu", label= TRUE)


#//////////////////////////// 
#task5
#quetion 1
RM1=lm(df$Diameter~.,data =df)
RM1
summary(RM1) # R-squared:0.98> 0.7:risk 
then we can conclude that 98% of the variability of Diameter is explained by the other variables => this model is a good model

#we will drop the  less significant  variables with the highest p-value

RM2=lm(df_knn$Diameter~.-Whole_Weight ,data =df)
RM2
summary(RM2) # R-squared:0.98> 0.7 
  
R=residuals(RM1)
plot(R,col="red")
qqnorm(R)
qqline(R)

#///////////quetion 2
AIC(RM1)#-23178.07
AIC(RM2)#-23179.41
#RM2 has the lowest AIC so it's the most sutable

#///////////////////// PCA/////question 3

install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
library("dplyr")
To_Remove_Columns2=c("Diameter","Sex")
test <- df_knn%>% select(- one_of(To_Remove_Columns2))
View(test)
x1=PCA(test, scale.unit = TRUE, ncp = 3, graph = TRUE)
library("FactoMineR")
res.pca <- PCA(test, graph = FALSE)
print(res.pca)
#table of dimenssions
eig.val <- get_eigenvalue(res.pca)
eig.val

##we determine the number of pca that have the highest variance.percent 
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0,100))

var <- get_pca_var(res.pca)
var

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#8B0000", #color of variables
                col.ind = "#696969"  #color of individuals
)
summary(res.pca)
head(var$contrib, 7)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# cos2 =cossinus carré
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)
#pc1 is highly corolated with Whole_Weight and pc2 is highly corolated with rings
ind <- get_pca_ind(res.pca)
ind
RM = lm(Diameter ~ Rings + Whole_Weight, data=df)
summary(RM)
#88% of diameter variability is explained by rings and Whole_Weight

#//////////Task6
GLM1 <- glm(df$Diameter  ~ . , data=df, family=gaussian(link="identity")) #AIC: -23178
summary(GLM1 )
#////////
GLM2 <- glm(df$Diameter  ~ . , data=df, family=Gamma(link="inverse")) #AIC: -17472
summary(GLM2)
#/////////no because for binary data
pMod1 <- glm(df$Diameter ~ . , family = binomial, data = df)#AIC: 4324.2
summary(pMod1)
#/////no because for positive integer or small natural number like count, individual number, frequency.
pMod2 <- glm(df$Diameter ~ . , family = poisson, data = df)#AIC: Inf
summary(pMod2)
#/////
GLM1 <- glm(df$Diameter  ~ . , data=df, family=gaussian(link="identity")) #AIC: -23178
summary(GLM1 )
#//////////
AIC(GLM1 ,GLM2 )



