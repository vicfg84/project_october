
# rm(list=ls())
######################################################
#
#   READING OF THE RESULTS

getwd()

result=read.table(file="res_essai_night4.csv",header=TRUE,sep=";")

nrow(result)

head(result)

names(result)=gsub("_onetime", "_OT", names(result))
names(result)=gsub("_probability", "_PR", names(result))
names(result)=gsub("_stability", "_ST", names(result))


rest_division= (1:dim(result)[2])%%6          
col_RMSE=which(rest_division==1)
col_RMSEsd=which(rest_division==2)
col_MAD=which(rest_division==3)
col_MADsd=which(rest_division==4)
col_sensitivity=which(rest_division==5)
col_specificity=which(rest_division==0)


ind_selec_1=c(1,4,7,10,13,16,19,21,23,25,27,29) # OT
ind_selec_2=c(2,5,8,11,14,17)                   # ST
ind_selec_3=c(3,6,9,12,15,18)                   # PR
ind_selec_4=c(20,22,24,26,28)                   # GAP

ind_selec=c(ind_selec_1,ind_selec_2,ind_selec_3,ind_selec_4)

names(result[col_RMSE[ind_selec]])


par(mfrow=c(1,1))
par(mar=c(6,3,3,1)+0.1,mgp=c(5,1,0))
boxplot(result[,col_RMSE[ind_selec ]],main="RMSE",names=unlist(strsplit(names(result[ ,col_RMSE[ind_selec]]),"_RMSE")),las=2, cex.axis=0.7,boxwex=0.7)
abline(v = c(12.5,18.5,24.5), col = "red", lty = 3)
boxplot(result[,col_RMSEsd[ind_selec ]],main="RMSE sd",names=unlist(strsplit(names(result[,col_RMSEsd[ind_selec ]]),"_RMSESD")),las=2, cex.axis=0.7)
abline(v = c(12.5,18.5,24.5), col = "red", lty = 3)
boxplot(result[,col_MAD[ind_selec ]],main="MAD",names=unlist(strsplit(names(result[,col_MAD[ind_selec ]]),"_MAD")),las=2, cex.axis=0.7)
abline(v = c(12.5,18.5,24.5), col = "red", lty = 3)
boxplot(result[,col_MADsd[ind_selec ]],main="MAD sd",names=unlist(strsplit(names(result[,col_MADsd[ind_selec ]]),"_MADSD")),las=2, cex.axis=0.7)
abline(v = c(12.5,18.5,24.5), col = "red", lty = 3)
boxplot(result[,col_sensitivity[ind_selec ]],main="Sensitivity",names=unlist(strsplit(names(result[,col_sensitivity[ind_selec ]]),"_sensitivity")),las=2, cex.axis=0.7)
abline(v = c(12.5,18.5,24.5), col = "red", lty = 3)
boxplot(result[,col_specificity[ind_selec ]],main="Specificity",names=unlist(strsplit(names(result[,col_specificity[ind_selec ]]),"_specificity")),las=2, cex.axis=0.7)
abline(v = c(12.5,18.5,24.5), col = "red", lty = 3)

# result[,which(grepl(pattern="RMSE$",x=names(result))&!grepl(pattern="SD",x=names(result)))]



