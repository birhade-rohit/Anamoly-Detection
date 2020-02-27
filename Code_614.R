library(readxl)
Project_dataset <- read_excel("C:/Users/birha/OneDrive/Desktop/New folder/Data/Project_dataset.xlsx")
#str(Project_dataset)
#summary(Project_dataset)
PR<-prcomp(Project_dataset,scale. = F)
names(PR)
#PR$sdev^2 #eigen values
#PR$center 
#PR$scale
#PR$x[,1:4]
plot(PR,scale = 0,xlab="Principle Components") #scree plot
#PR$rotation
#PR$rotation <- -PR$rotation
#PR$rotation
plot(PR, type = "l") 
VE <- PR$sdev^2
pve <- VE*100 / sum(VE)
pve <- round(pve, 2) #proportion contribution of each eigen corresponding to PCs
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,100),type='b')

PC1<-as.data.frame(PR$x[,1])
PC2<-as.data.frame(PR$x[,2])
PC3<-as.data.frame(PR$x[,3])
PC4<-as.data.frame(PR$x[,4])

library(MSQC)
library(spc)
PC_old<-cbind(PC1,PC2,PC3,PC4)
PC_new <- PC_old

#Results by using on T-square
for (i in seq(1, 13)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_new,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  outliers = tsquare$t2 > tsquare$ucl
  print(sum(outliers))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_new[inctrl_index,]
  
  PC_new <- PC_1
  nrow(PC_new)
}

#Results by using only EWMA

for (i in seq(1,7)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  mw<-mult.chart(type = "mewma", PC_new, lambda=0.5, phase=1)
  critical_val = mewma.crit(l=0.5,L0=200,p=4)
  
  outliers = mw$t2 > critical_val
  print(sum(outliers))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_new[inctrl_index,]
  
  PC_new <- PC_1
  nrow(PC_new)
}

#Results by using on m-CUSUM

for (i in seq(1,7)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  mcusum3 <-mult.chart(type = "mcusum", PC_new,k=1.5, h=6, phase=1)
  
  outliers = mcusum3$t2 > mcusum3$ucl
  print(sum(outliers))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_new[inctrl_index,]
  
  PC_new <- PC_1
  nrow(PC_new)
}

#Results using T-square and mCUSUM combined
for (i in seq(1,7 )) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_new,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  mcusum <-mult.chart(type = "mcusum", PC_new,k=1.5, h=6, phase=1)
  outliers = (tsquare$t2 > tsquare$ucl | mcusum$t2 > mcusum$ucl)
  #print(sum(outliers))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_new[inctrl_index,]
  
  PC_new <- PC_1
  nrow(PC_new)
}
#46, 31, 22, 12, 8, 3, 3, 1, 1, 1, 1


#Results using mewma and T-square combined
for (i in seq(1,7)) {
  print("----------------")
  print(paste("****Results for ", i, " Iteration****"))
  
  
  tsquare<-mult.chart(PC_new,type="t2",phase=1,alpha=0.005, keep.source = FALSE)
  mw<-mult.chart(type = "mewma", PC_new, lambda=0.5, phase=1)
  critical_val = mewma.crit(l=0.5,L0=200,p=4)
  outliers = (tsquare$t2 > tsquare$ucl | mw$t2 > critical_val)
  print(sum(outliers))
  
  inctrl_index = which(outliers==FALSE)
  
  #removing ooc from data 1st it
  
  PC_1<-PC_new[inctrl_index,]
  
  PC_new <- PC_1
  nrow(PC_new)
}
#46, 31, 22, 12, 8, 3, 3, 1, 1, 1, 1

