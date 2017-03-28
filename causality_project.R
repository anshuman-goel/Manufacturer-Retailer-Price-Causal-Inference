# Load the libraries 
# To install pcalg library you may first need to execute the following commands:
# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
#install.packages('pcalg')
#install.packages('vars')
#install.packages('urca')
library(pcalg)
library(graph)
library(RBGL)
library(vars)
library(urca)

# Read the input data 
data<-read.csv("E:\\NCSU\\Semester 2\\Algorithms for Data Guided Business Intelligence\\Projects\\Manufacturer Retailer Price Causal Inference\\causality\\data.csv",header = TRUE,sep = ',')

# Build a VAR model 
# Select the lag order using the Schwarz Information Criterion with a maximum lag of 10
# see ?VARselect to find the optimal number of lags and use it as input to VAR()
VARselect(data)
model<-VAR(data,lag.max = 1,ic="SC")

# Extract the residuals from the VAR model 
# see ?residuals
res<-residuals(model)

# Check for stationarity using the Augmented Dickey-Fuller test 
# see ?ur.df
summary(ur.df(res[,1],selectlags = "BIC"))
summary(ur.df(res[,2],selectlags = "BIC"))
summary(ur.df(res[,3],selectlags = "BIC"))
#Ref :https://en.wikipedia.org/wiki/Augmented_Dickey%E2%80%93Fuller_test
#Stationary

# Check whether the variables follow a Gaussian distribution  
# see ?ks.test
ks.test(res[,1],"pnorm",exact = FALSE)
ks.test(res[,2],"pnorm",exact = FALSE)
ks.test(res[,3],"pnorm",exact = FALSE)

# Write the residuals to a csv file to build causal graphs using Tetrad software


# OR Run the PC and LiNGAM algorithm in R as follows,
# see ?pc and ?LINGAM 

# PC Algorithm

# LiNGAM Algorithm
