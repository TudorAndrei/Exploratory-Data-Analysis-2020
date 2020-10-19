cat("\f")

### Default Options 

options(scipen=999)
options(stringsAsFactors = FALSE)

### Read Data

help(iris)
Data = iris # iris is a default database in R 

str(Data)
dim(Data)

########################################################
### visualisation of the data

plot(Data[,1:4], pch=16, cex=0.6)

#correlation matrix
cor(Data[,1:4])

# not easy to interpret

##########################################################################################################

### set the colours for the three classes
cols = c("steelblue1", "hotpink", "mediumpurple")
### set the symbols for the three classes
pchs = c(1,2,3)


pairs(Data[,1:4], pch = pchs[Data$Species],  cex = 1,
      col = cols[Data$Species],
      lower.panel=NULL)

### create a legend for the plot
par(xpd = TRUE)
legend("bottomleft",legend=c("setosa","versicolor","virginica"),
       col=cols,pch=pchs,cex=1,text.font=2)


############################################################################################
# use corrplot

library(corrplot)
corrplot(cor(Data[,1:4]), method = "ellipse")

# it is an abstract version of the previous plot, but makes things "easy" to interpret
############################################################################################
