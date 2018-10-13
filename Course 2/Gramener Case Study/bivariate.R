

mydata <- loan[,c(3,4,5,7,8,14,25,34,33)] 
corr_matx <- round(cor(mydata,use="complete.obs"),2)  
#corr_matx 
library(reshape2) 
melted_corr_matx <- melt(corr_matx) 
head(melted_corr_matx)  
library(ggplot2) 
ggplot(data = melted_corr_matx, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

