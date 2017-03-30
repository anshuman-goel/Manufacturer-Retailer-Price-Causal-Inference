# Load the libraries 
# To install pcalg library you may first need to execute the following commands:
# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")
# biocLite("RBGL")
# biocLite("Rgraphviz")
#install.packages('pcalg')
#install.packages('vars')
#install.packages('urca')
library(pcalg)
library(graph)
library(RBGL)
library(vars)
library(urca)

# Read the input data 
data<-read.csv("./data.csv",header = TRUE,sep = ',')

# Build a VAR model 
# Select the lag order using the Schwarz Information Criterion with a maximum lag of 10
# see ?VARselect to find the optimal number of lags and use it as input to VAR()
model<-VAR(data,lag.max = VARselect(data)$selection[3],ic="SC")

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
#Ref:
#https://arxiv.org/pdf/1603.00784.pdf
#https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test
ks.test(res[,1],pnorm,exact = FALSE)
ks.test(res[,2],pnorm,exact = FALSE)
ks.test(res[,3],pnorm,exact = FALSE)

# Write the residuals to a csv file to build causal graphs using Tetrad software
write.table(res,file = "./residual.csv",sep=",",row.names = FALSE)

# OR Run the PC and LiNGAM algorithm in R as follows,
# see ?pc and ?LINGAM 

# PC Algorithm
suffStat<-list(C=cor(res), n=1000)
pc_algo<-pc(suffStat,indepTest=gaussCItest,alpha=0.1,labels=colnames(res),skel.method="original",verbose=TRUE)
plot(pc_algo,main="PC Graph")

# LiNGAM Algorithm
show(lingam(res,verbose =1))
