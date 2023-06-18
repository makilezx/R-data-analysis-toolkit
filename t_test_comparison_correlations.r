install.packages("haven", dependencies = T)
library(haven)

      
dataset<- read_csv(file.choose())
colnames(dataset)

varijable_dataset<- subset(dataset, select=c(374:378))
colnames(varijable_dataset)
 
library(psych)
describe(varijable_dataset)


#correlations
install.packages("Hmisc", dependencies = T)
library(Hmisc)

var_dataset2 <- as.matrix(varijable_dataset)
rcorr(var_dataset2, type="pearson")


#visualization
install.packages("corrplot", dependencies = T)
library(corrplot)

options(max.print=10000) 
varijable_dataset

kor_viz <- cor(varijable_dataset)
kor_viz
round(kor_viz, digits = 2)

corrplot(kor_viz, method="circle")
korelogram <- corrplot(kor_viz, method="circle")
korelogram 

#1 visualization
corrplot(kor_viz, type = "full", method="number", bg = "gray55", order ="AOE")
#2 visualization
corrplot(kor_viz, type = "upper", method="circle", bg = "gray55", order ="AOE")
#3 visualization
corrplot.mixed(kor_viz)

# t-test
colnames(dataset)
t1.ind <- t.test(Zmalo_verovatne_vrline~Pol, alternative='two.sided', conf.level=.95,  var.equal=FALSE, data=dataset)
t1.ind

#plot
install.packages("ggplot2")
library(ggplot2)

boxplot(Zmalo_verovatne_vrline ~ Pol, data=dataset, xlab="Pol", ylab="MVV")

        
        
