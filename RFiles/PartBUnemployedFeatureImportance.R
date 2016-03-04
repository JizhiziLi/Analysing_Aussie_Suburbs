library(XLConnect)
library(ecodist)
library(labdsv)
library(ape)
library(ade4)
library(smacof)
library(mlbench)
library(caret)
data= list()
fileNames=c('Ascot-Vale-Suburb - XLSX.xlsx',
            'Braybrook-Suburb - XLSX.xlsx', 'Craigieburn-Suburb - XLSX.xlsx',
            'Croydon-Suburb - XLSX.xlsx', 'Fawkner-Suburb - XLSX.xlsx',
            'Footscray-Suburb - XLSX.xlsx','Glenroy-Suburb - XLSX.xlsx', 
            'Malvern-East-Suburb - XLSX.xlsx', 'Malvern-Suburb - XLSX.xlsx',
            'Melbourne-Airport-Suburb - XLSX.xlsx', 'Mentone-Suburb - XLSX.xlsx',
            'Moorabbin-Suburb - XLSX.xlsx', 'Mordialloc-Suburb - XLSX.xlsx', 
            'Murrumbeena-Suburb - XLSX.xlsx', 'Noble-Park-Suburb - XLSX.xlsx',
            'North-Melbourne-Suburb - XLSX.xlsx', 'Northcote-Suburb - XLSX.xlsx',
            'Parkville-Suburb - XLSX.xlsx', 'Pascoe-Vale-South-Suburb - XLSX.xlsx',
            'Port-Melbourne-Suburb - XLSX.xlsx','Prahran-Suburb - XLSX.xlsx',
            'Somerville-Suburb - XLSX.xlsx','Sorrento-Suburb - XLSX.xlsx', 
            'South-Melbourne-Suburb - XLSX.xlsx','South-Yarra-Suburb - XLSX.xlsx',
            'Springvale-Suburb - XLSX.xlsx','St-Andrews-Beach-Suburb - XLSX.xlsx',
            'St-Kilda-East-Suburb - XLSX.xlsx','St-Kilda-Suburb - XLSX.xlsx',
            'St-Kilda-West-Suburb - XLSX.xlsx','Toorak-Suburb - XLSX.xlsx',
            'Tyabb-Suburb - XLSX.xlsx','Waterways-Suburb - XLSX.xlsx',
            'Windsor-Suburb - XLSX.xlsx')
suburbs=gsub("-Suburb - XLSX.xlsx","",fileNames)
path="/Users/yunwang/myFile/unimelb/machinelearning/assignment2/project2_data/"
for(i in 1:34){
  data[[i]]=readWorksheet(loadWorkbook(paste(path,fileNames[i],sep=""))
                          ,header=F,sheet=2)
}
feature=c(seq(123,129,2),132,134,136:138,144,146,seq(150,158,2),
          seq(172,180,2))
featureNames=data[[1]]$Col2[feature]
matrixData=data[[1]]$Col3[feature]
for(i in 2:34){
  matrixData=rbind(matrixData, data[[i]]$Col3[feature])
}
class(matrixData)="numeric"
colNames="v1"
for(i in 2:21){
  colNames=c(colNames,paste("v",as.character(i),sep=""))
}
colnames(matrixData)=colNames


label=data[[1]]$Col3[148]
for(i in 2:34){
  label=c(label,data[[i]]$Col3[148])
}
label=as.numeric(label)
matrixData=cbind(matrixData,label)
for(i in 1:ncol(matrixData)){
  matrixData[is.na(matrixData[,i]), i] <- mean(matrixData[,i], na.rm = TRUE)
}

matrixData=data.frame(matrixData)



# ensure the results are repeatable
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(matrixData[,1:21])
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(featureNames[highlyCorrelated])



names(matrixData)=c(featureNames,"label")
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(label~., data=matrixData, method="lm", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# plot importance
plot(importance,main="The importance of features on predicting unemployed rate")

fit1=lm(matrixData[,22]~matrixData[,19])
plot(matrixData[,19],matrixData[,22],pch=16,
     main="Unemployed rate vs percentage born in non-English speaking country",
     xlab="Born in non-English speaking country, %",ylab="unemployed rate")
lines(matrixData[,19],fitted(fit1),col="green",lwd="4")

fit2=lm(matrixData[,22]~matrixData[,14])
plot(matrixData[,14],matrixData[,22],pch=16,
     main="Unemployed rate vs percentage aged 75+ and lives alone",
     xlab="Aged 75+ and lives alone, %",ylab="unemployed rate")
lines(matrixData[,14],fitted(fit2),col="green",lwd="4")


  
  
  
  
  








