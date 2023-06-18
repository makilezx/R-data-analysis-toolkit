install.packages("lavaan")
install.packages("psych")
install.packages("semPlot")
install.packages("haven", dependencies = T)
library(lavaan)
library(psych)  
library(semPlot)
library(haven)


dataset<- read_spss(file.choose())
colnames(dataset)

describe(dataset)
        
#mod1 - 1 faktor, sve stavke ga opterecuju
        PANAS_mod1<-'F=~ PANAS_1 + PANAS_2 + PANAS_3 + PANAS_4 + PANAS_5 + PANAS_6 + PANAS_7 + PANAS_8 + PANAS_9 + PANAS_10 + PANAS_11 + PANAS_12 + PANAS_13 + PANAS_14 + PANAS_15 + PANAS_16 + PANAS_17 + PANAS_18 + PANAS_19 + PANAS_20'
                     
        PANAS_mod1_fit <- cfa(PANAS_mod1,        
                            data = dataset,   
                            std.lv = TRUE,    
                            orthogonal = F,   
                            estimator = "ML")
        
        summary(PANAS_mod1_fit, standardized = TRUE, fit.measures = TRUE)
        
 
        PANAS_viz<- semPaths(PANAS_mod1_fit, 
                               what = "std",              
                               layout = "tree",           
                               residuals = T,             
                               title = TRUE,              
                               style = "lisrel",                 
                               color = list (lat= "gray94",       
                                             man = "royalblue1")) 
        
#mod2 - 2 faktora, ortogonalni
        PANAS_mod2<-'F1=~ PANAS_1 + PANAS_3 + PANAS_5 + PANAS_9 + PANAS_10 + PANAS_12 + PANAS_14 + PANAS_16 + PANAS_17 + PANAS_19
                     F2=~ PANAS_2 + PANAS_4 + PANAS_6 + PANAS_7 + PANAS_8 + PANAS_11 + PANAS_13 + PANAS_15 + PANAS_18 + PANAS_20'
        
        PANAS_mod2_fit <- cfa(PANAS_mod2,        
                              data = dataset,   
                              std.lv = TRUE,    
                              orthogonal = TRUE, #achtung achtung 
                              estimator = "ML")
        
        summary(PANAS_mod2_fit, standardized = TRUE, fit.measures = TRUE)
        
        
        PANAS_viz<- semPaths(PANAS_mod2_fit, 
                             what = "std",              
                             layout = "tree",           
                             residuals = T,             
                             title = TRUE,              
                             style = "lisrel",                 
                             color = list (lat= "gray94",       
                                           man = "royalblue1")) 

#mod3 - 2 faktora, korelirani
        PANAS_mod3<-'F1=~ PANAS_1 + PANAS_3 + PANAS_5 + PANAS_9 + PANAS_10 + PANAS_12 + PANAS_14 + PANAS_16 + PANAS_17 + PANAS_19
                     F2=~ PANAS_2 + PANAS_4 + PANAS_6 + PANAS_7 + PANAS_8 + PANAS_11 + PANAS_13 + PANAS_15 + PANAS_18 + PANAS_20'
        
        PANAS_mod3_fit <- cfa(PANAS_mod3,        
                              data = dataset,   
                              std.lv = TRUE,    
                              orthogonal = F, #achtung achtung 
                              estimator = "ML")
        
        summary(PANAS_mod3_fit, standardized = TRUE, fit.measures = TRUE)
        
       
        PANAS_viz<- semPaths(PANAS_mod3_fit, 
                             what = "std",              
                             layout = "tree",           
                             residuals = T,             
                             title = TRUE,              
                             style = "lisrel",                 
                             color = list (lat= "gray94",       
                                           man = "royalblue1")) 


#mod4 - bifaktorski
        PANAS_mod4<-'F1=~ PANAS_1 + PANAS_3 + PANAS_5 + PANAS_9 + PANAS_10 + PANAS_12 + PANAS_14 + PANAS_16 + PANAS_17 + PANAS_19
                     G=~  PANAS_1 + PANAS_2 + PANAS_3 + PANAS_4 + PANAS_5 + PANAS_6 + PANAS_7 + PANAS_8 + PANAS_9 + PANAS_10 + PANAS_11 + PANAS_12 + PANAS_13 + PANAS_14 + PANAS_15 + PANAS_16 + PANAS_17 + PANAS_18 + PANAS_19 + PANAS_20
                     F2=~ PANAS_2 + PANAS_4 + PANAS_6 + PANAS_7 + PANAS_8 + PANAS_11 + PANAS_13 + PANAS_15 + PANAS_18 + PANAS_20'
        
        PANAS_mod4_fit <- cfa(PANAS_mod4,        
                              data = dataset,   
                              std.lv = TRUE,    
                              orthogonal = T, #achtung achtung 
                              estimator = "ML")
        
        summary(PANAS_mod4_fit, standardized = TRUE, fit.measures = TRUE)
        
     
        PANAS_viz<- semPaths(PANAS_mod4_fit, 
                             what = "std",              
                             layout = "tree",           
                             residuals = T,             
                             title = TRUE,              
                             style = "lisrel",                 
                             color = list (lat= "gray94",       
                                           man = "royalblue1")) 
        
        
