### R function
library(Hmisc)

rm(testdata)
 install.packages("mlbench")
data(BreastCancer, package="mlbench")
D= BreastCancer

#importing csv file to R
testdata=read.csv("insuranceData.csv",header=TRUE)
testdata=read.csv("sample.csv",header=TRUE)
#testdata$bmi[c(1:20)]=NA
#testdata=dt



#identifying quantitative and qualitative variables

data_analysis=function(testdata,y){

dimension=dim(testdata)
var_names=colnames(testdata)

variable= vector()
data_type= vector()
missing_value_count= vector()
missing_value_count_after=vector()
for (i in 1:dimension[2]){
  a[i]= class(testdata[,i])
  b[i]= sum(is.na(testdata[,i]))
  #cat(var_names[i],"is",a[i],"\n")
  variable[i]= var_names[i]
  data_type[i]= a[i]
  missing_value_count[i]=b[i]
  #if (i== dimension[2]) {
  cat(variable[i],data_type[i],"missing value count:",missing_value_count[i],"\n","\n")
  #}
  #if (a[i]=="numeric") {
  require(Hmisc)
  testdata[,i]=impute(testdata[,i], fun = mean) 
  testdata[,i][is.na(testdata[,i])] = round(mean(testdata[,i], na.rm = TRUE),digits = 2)
  c[i]= sum(is.na(testdata[,i]))
  missing_value_count_after[i]=b[i]
  cat(variable[i],"missing value count after imputation:",missing_value_count_after[i],"\n","\n")
  #}
  if (a[i]=="numeric" | a[i]=="integer"){
    cat(var_names[i], "outliers :",boxplot.stats(testdata[,i])$out,"\n")
    hist(testdata[,i],col="peachpuff",border="black",main =(paste("Histogrm of" ,var_names[i])),xlab=var_names[i])
    boxplot(testdata[,i],col="peachpuff",border="black",main =(paste("Boxplot of" ,var_names[i])),xlab=var_names[i])
    
    
    } else {
      if (a[i]=="factor") {
        cat(var_names[i], "outliers :",boxplot.stats(testdata[,i])$out,"\n")
        
        
        
        ftbl=data.frame(table(testdata[i])) #creating a data frame
        ftbl$pct <- round(ftbl$Freq/sum(ftbl$Freq)*100) #calculating %
        lbls <- paste(ftbl$Var1, ftbl$pct) # add percents to labels
        lbls <- paste(lbls,"%",sep="") # ad % to labels
      }
        
        if (nlevels(a[i])<5) {
        pie(ftbl$Freq,labels=lbls, main=(paste("pie chart of" ,var_names[i])))
        } else {
        barplot(ftbl$Freq,xlab = ftbl$Var1, main=(paste("barplot of" ,var_names[i])))
        }

      
    }
  
 }
  


if (class(y)=="numeric"){
  full.model <- lm(y~ .-y, data=testdata)
  reduced.model <- step(full.model, direction="backward")
  print(summary(reduced.model))
} else { 
  
  full.model1 = glm(y~ .-y, family=binomial(), data=testdata)
  reduced.model <- step(full.model, direction="backward")
  summary(reduced.model)
  print(summary(reduced.model))}

histogram_residuals=hist(reduced.model$residuals)
pdf("histogram_residuals.pdf")#normality of the resuduals
#graphics.on()
#par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(reduced.model) 
#par(mfrow=c(1,1))
print(shapiro.test(reduced.model$residuals))


#return(any(is.na(testdata)))
  
  
  
  
}

#return(any(is.na(testdata)))
#return(missing_value_count)




#df1= data.frame(variable,data_type,missing_value_count)
#return(df1)
#}

#Identify_variables(testdata,testdata$premium)

data_analysis(D,as.factor(D$Class))

data_analysis(testdata,testdata$premium)
data_analysis(testdata,testdata$premium)
#data_analysis(dt,dt$Sex
#table(testdata[,3])


nlevels(testdata$gender)
