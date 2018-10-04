# Data Read
setwd("E:/Health/SetA/set-a/")
sml_dat <-list()
x_sml_dat <- list()
urine_dat <- list()
listtxt <- dir(pattern = "*.txt") # creates the list of all the csv files in the directory
library(sqldf)
library(lubridate)


# Grouping the parameters and finding the minimum values of each parameter for each patient
for (k in 1: 4000){
  ldf <- read.csv(listtxt[k])
  ldf$newtime <- strptime(x  = as.character(ldf$Time), format = "%H:%M")
  print(k)
  z <- with(ldf, ldf[hours(ldf$newtime) <24,])
  z<- z[, c("Value", "Parameter", "Time")]
  print(k)
  sml_dat[[k]] <- sqldf("select Value, min(Parameter ), Time from 'z' group by Parameter ")
  x_sml_dat[[k]] <- sqldf("select Value, max(Parameter ), Time from 'z' group by Parameter ")
  print(k)
  urine_dat[[k]] <- sqldf("select value, sum(Parameter)from z where Parameter= 'Urine'")
}

big_list <- do.call(rbind, sml_dat)
big_df <- as.data.frame(big_list)

#Creating the Saps score function for each variable

#1 Age
Age <- function(x){tryCatch({
  if(x > 75){
    return(4)
  }
  else if (x>65){
    return(3)
  }
  else if(x> 55){
    return(2)
  }
  else if(x>45){
    return(1)
  }
  else if(x>=0){
    return(0)
  }
  else return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
}
#2 Heart Rate

HR <- function(x){tryCatch({
  if(x>= 180){
    return(4)
  }
  else if(x>= 140){
    return(3)
  }
  else if(x>= 110){
    return(2)
  }
  else if(x>= 70){
    return(0)
  }
  else if (x>= 55){
    return(2)
  }
  else if(x >= 40){
    return(3)
  }
  else if(x >= 10){
    return(4)
  }
  else return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
}

#3 Systolic Blood Presure
SysABP <- function(x){tryCatch({
  if (x>= 190){
    return(4)
  }
  else if(x>= 150){
    return(2)
  }
  else if(x >= 80){
    return(0)
  }
  else if(x>= 55){
    return(2)
  }
  else if(x>= 20){
    return(4)
  }
  else return(0)}
  ,
  error= function(e){
    return(0)
  }
)

}

#4 Temp
Temp <- function(x){tryCatch({
  if(x>= 41){
    return(4)
  }
  else if(x>= 39){
    return(3)
  }
  else if(x>= 38.5){
    return(2)
  }
  else if (x>= 36){
    return(0)
  }
  else if(x >= 34){
    return(1)
  }
  else if(x >= 32){
    return(2)
  }
  else if(x>= 30){
    return(3)
  }
  else if(x>= 15){
    return(4)
  }
  else return(0)}
,

  error= function(e){
    return(0)
  } 
)
}

#5 Respiration Rate

RespRate <- function(x){tryCatch({
 
  if (x >= 50){
    return(4)
  }
  else if (x >= 35){
    return(3)
  }
  else if (x >= 25){
    return(1)
  }
  else if (x >= 12){
    return(0)
  }
  else if (x >= 10){
    return(1)
  }
  else if (x >= 6){
    return(2)
  }
  else if (x>=2){
    return(4)
  }
  else return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
}

#6 BUN

BUN <- function(x){ tryCatch({
  if (bun >=  55) return(4)
  else if (bun >=  36) return(3)
  else if (bun >=  29) return(2)
  else if (bun >= 7.5) return(1)
  else if (bun >= 3.5) return(0)
  else if (bun >=   1) return(1)
  else	 	 return (0)}
  ,
  
  error= function(e){
    return(0)
  } 
)
}


#7 urine

Urine <- function(x){tryCatch({
  if (x >= 5.0) return(2)
  else if (x >= 3.5) return(1)
  else if (x >= 0.7) return(0)
  else if (x >= 0.5) return(2)
  else if (x >= 0.2) return(3)
  else if (x >=   0) return(4)
  else	        return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
  
}



#8 HCT

HCT <- function(x){tryCatch({
  if(x >= 60) return(4)
  else if (x >= 50) return(2)
  else if (x >= 46) return(1)
  else if (x >= 30) return(0)
  else if (x >= 20) return(2)
  else if (x >=  5) return(4)
  else	 	return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
}


#9 WBC

WBC <- function(x){tryCatch({
  if (x >= 40) return(4)
  else if (x >= 20) return(2)
  else if (x >= 15) return(1)
  else if (x >=  3) return(0)
  else if (x >=  1) return(2)
  else if (x >=0.1) return(4)
  else	        return (0)}
  ,
  error= function(e){
    return(0)
  } 
)
}


#10 Glucose

Glucose <- function(x){tryCatch({
  if (x >= 44.5) return(4)
  else if (x >= 27.8) return(3)
  else if (x >= 14.0) return(1)
  else if (x >=  3.9) return(0)
  else if (x >=  2.8) return(2)
  else if (x >=  1.6) return(3)
  else if (x >=  0.5) return(4)
  else	  	  return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
}

#11 Potassium

K <- function(x){tryCatch({
  if (x >= 7.0) return(4)
  else if (x >= 6.0) return(3)
  else if (x >= 5.5) return(2)
  else if (x >= 3.5) return (0)
  else if (x >= 3.0) return(1)
  else if (x >= 2.5) return(2)
  else if (x >= 0.5) return(4)
  else	       return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
} 


#12 Sodium

Na <- function(x){tryCatch({
  if (x >= 180) return(4)
  else if (x >= 161) return(3)
  else if (x >= 156) return(2)
  else if (x >= 151) return(1)
  else if (x >= 130) return(0)
  else if (x >= 120) return(2)
  else if (x >= 110) return(3)
  else if (x >=  50) return(4)
  else	 	return (0)}
  ,
  error= function(e){
    return(0)
  } 
)
}

#13 HCO3

HCO3 <- function(x){tryCatch({
  if (x >= 40) return(4)
  else if (x >= 30) return(1)
  else if (x >= 20) return(0)
  else if (x >= 10) return(1)
  else if (x >=  5) return(2)
  else if (x >=  2) return(4)
  else	 	 return (0)}
  ,
  error= function(e){
    return(0)
  } 
  )
}

#14 Glucose Coma Score
GCS <- function(x){tryCatch({
  if (x >  15) return(0)
  else if (x >= 13) return(0)
  else if (x >= 10) return(1)
  else if (x >=  7) return(2)
  else if (x >=  4) return(3)
  else if (x >=  3) return(4)
  else	 	return(0)}
  ,
  error= function(e){
    return(0)
  } 
)
}

SAPS = list()
Age_score= list()
HR_score = list()
Temp_score = list()
WBC_score = list()
HCO3_score = list()
K_score = list()
Na_score = list()
Glucose_score = list()
GCS_score = list()
HCT_score = list ()
BUN_score = list()
Urine_score = list()
RespRate_score = list()
SysABP_score = list()

# Calculate the SAPS score for each patient:

for (i in 1:4000){
  print(i)
  
  Age_score[[i]] = Age(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Age", sml_dat[[i]]$Value)) 
    
  HR_score[[i]] =  HR(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "HR", sml_dat[[i]]$Value)) 
    
  Temp_score[[i]] =  Temp(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Temp", sml_dat[[i]]$Value))   
    
  WBC_score[[i]]  = WBC(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "WBC", sml_dat[[i]]$Value)) 
    
  HCO3_score[[i]] =  HCO3(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "HCO3", sml_dat[[i]]$Value))   
    
  K_score[[i]] =  K(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "K", sml_dat[[i]]$Value)) 
    
  Na_score[[i]] =  Na(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Na", sml_dat[[i]]$Value))  
    
  Glucose_score[[i]] =  Glucose(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Glucose", sml_dat[[i]]$Value)/ 18.0) 
    
  GCS_score[[i]]  = GCS(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "GCS", sml_dat[[i]]$Value))
    
  HCT_score[[i]] =  HCT(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "HCT", sml_dat[[i]]$Value)) 
    
  BUN_score[[i]] =  BUN(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "BUN", sml_dat[[i]]$Value)/ 2.8) 
    
  Urine_score[[i]]=  Urine(urine_dat[[i]]$Value/1000)         
  
  RespRate_score[[i]] = RespRate(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "RespRate", sml_dat[[i]]$Value)) 
  
  SysABP_score[[i]] = SysABP(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "SysABP", sml_dat[[i]]$Value)) 
  
  
  
  
  
   SAPS[[i]] = Age_score[[i]] +   HR_score[[i]] + Temp_score[[i]] + WBC_score[[i]] + HCO3_score[[i]] +
     K_score[[i]] + Na_score[[i]] + Glucose_score[[i]] + GCS_score[[i]] + HCT_score[[i]] +
   BUN_score[[i]] + Urine_score[[i]] + RespRate_score[[i]] + SysABP_score[[i]]
}

#Combining list of each variables

# Sap_list
sap_list <- as.data.frame(do.call(rbind, SAPS))

#Age list
age_list <- as.data.frame(do.call(rbind, Age_score))

#HR List
HR_list <- as.data.frame(do.call(rbind, HR_score))

#Temp List
Temp_list <-as.data.frame(do.call(rbind, Temp_score))

#WBC List
WBC_list <- as.data.frame(do.call(rbind, WBC_score))

#HCO3 List
HCO3_list <-as.data.frame(do.call(rbind, HCO3_score))

#K List
K_list <- as.data.frame(do.call(rbind, K_score))

#Na List
Na_list <- as.data.frame(do.call(rbind, Na_score))

#Glucose ist
Glucose_list <- as.data.frame(do.call(rbind, Glucose_score))

#GCS List
GCS_list <- as.data.frame(do.call(rbind, GCS_score))

#HCT list
HCT_list <- as.data.frame(do.call(rbind, HCT_score))

#Urine List
Urine_list <- as.data.frame(do.call(rbind, Urine_score))

#RespRate List
RespRate_list <- as.data.frame(do.call(rbind, RespRate_score))

#SysABP List
SysABP_list <- as.data.frame(do.call(rbind, SysABP_score))

#BUN List
BUN_list <- as.data.frame(do.call(rbind, BUN_score))

#Convert the sap list into data frame
sap_df <- as.data.frame(sap_list)
library(data.table)

#Loeading the output data:
survivor <- fread("E:/Health/Outcomes-a.txt")
survivor <- survivor[,-c(1:5)]
survivor

# Variable scores {combined list of all SAPS variables with output variable and SAPs score}
variable_scores <- data.table("Age_s" = age_list, "HR_s"= HR_list , Temp_s = Temp_list , WBC_s = WBC_list,
                              HCO3_s= HCO3_list , K_s = K_list, Na_s = Na_list, Glucose_s = Glucose_list,
                              GCS_s = GCS_list, HCT_s= HCT_list , Urine_s= Urine_list,
                              RespRate_s = RespRate_list, SysABP_s = SysABP_list, BUN_s = BUN_list, 
                              "survivor" = survivor, "saps_score" = sap_df)




# Write CSV file for Tableau:
write.csv(file= "E:/Health/SAPS.csv", x= variable_scores, row.names = FALSE)

#Get RecordId of patients
patient_id <- subset(big_df$Value, big_df$`min(Parameter )`== "RecordID")

#Combining patient scores and id
patient_scores <- data.frame(patient_ids = patient_id, "sap_scors" = sap_list)


# Logistic Model


Log_mdl <- glm(variable_scores$`survivor.In-hospital_death` ~., data = variable_scores, family = "binomial")
summary(Log_mdl)
# Varibales Selected: Age, HR, WBC, HCO3

#Predicted Probabilities
pred_prob <- as.data.frame(predict(Log_mdl, variable_scores[,-15], type = "response"))


x <- data.frame("acutal_survivor"= survivor, "predicted_survivor" = pred_prob)
colnames(x) <- c("Actual_survivors", "probability_survivor")
pred <- as.data.frame(ifelse(pred_prob>0.5,1,0))
y<- data.frame("actual_survivor" = x$Actual_survivors, "pred_survors"=pred)


library(caret)
confusionMatrix(table(y$actual_survivor, y$predict.Log_mdl..variable_scores....15...type....response..))
x$probability_survivor

#Accuracy ~ 85%


##################################################PART 1 ########################################################


################################################PART 2###########################################################

variable_scores$sap_scores <- sap_df


# Histogram of SAPS Score:
 
library(ggplot2)

ggplot(variable_scores, aes(variable_scores$sap_scores)) + geom_histogram(breaks= seq(0,26, by = 1), 
                                                                          col = "blue", 
                                                                          aes(fill= ..count..)) +
  scale_fill_gradient("Count", low = "black", high = "red")+ geom_density(col=2) +
  labs(title= "Histogram for SAPS score") + 
  labs(x= "sap_scores", y= "Count") +
    xlim(c(0,28)) +
    ylim(c(0,475))




df_melt <- melt(variable_scores[,1:14])

library(sqldf)
sum_var <- sqldf("select Variable, sum(value) from 'df_melt' group by variable")
pct <- round(sum_var$`sum(value)`/ sum(sum_var$`sum(value)`)* 100)
sum_var$pct <- pct



#Pie Chart to show the contribution of each variable in the SAPS score
pie(sum_var$`sum(value)`, labels = paste0(as.character(sum_var$variable), "", sum_var$pct, "%")  
    ,radius = 1,    col = rainbow(length(sum_var$variable)), main = "Pie chart of SAPS variables")


# Box plots of survior and saps score with all variables as factor

fill <- "#4271AE"
line <- "#1F3552"
z  <-  theme_bw() +theme(panel.grid.major = element_line(colour = "#d3d3d3"),
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(), panel.background = element_blank(),
                         plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
                         text=element_text(family="Tahoma"),
                         axis.title = element_text(face="bold"),
                         axis.text.x=element_text(colour="black", size = 11),
                         axis.text.y=element_text(colour="black", size = 9),
                         axis.line = element_line(size=0.5, colour = "black"))



#1.  Plot fo SAP Score vs Survivor Class and Age as variable
age_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$Age_s.V1))) +labs(title ="Sap score v/s Survivor and Age") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("Age score as Factor")) + z

#2. HR factor
HR_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$HR_s.V1))) +labs(title ="Sap score v/s survivor and HR") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("HR score as Factor")) + z

#3. Temp factor
Temp_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$Temp_s.V1))) +labs(title ="Sap score v/s Age and Temp") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("Temp score as Factor")) + z



#4. WBC factor
WBC_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$WBC_s.V1))) +labs(title ="Sap score v/s Age and WBC") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("WBC score as Factor")) + z

#5. HCO3 factor
HCO3_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$HCO3_s.V1))) +labs(title ="Sap score v/s Age and HCO3") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("HCO3 score as Factor")) + z

#6. K factor
K_box <-ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$K_s.V1))) +labs(title ="Sap score v/s Age and K") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("K score as Factor")) + z

#7. Na factor
Na_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$Na_s.V1))) +labs(title ="Sap score v/s Age and Na") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("Na score as Factor")) +z

#8. Glucose factor
Glucose_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$Glucose_s.V1))) +labs(title ="Sap score v/s Age and Glucose") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("Glucose score as Factor")) + z


#9. GCS factor
GCS_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$GCS_s.V1))) +labs(title ="Sap score v/s Age and GCS") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("GCS score as Factor")) + z


#10. HCT factor
HCT_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$HCT_s.V1))) +labs(title ="Sap score v/s Age and HCT") + labs(x= " Survivor_class", y= "Sap_score") +  guides(color = guide_legend("HCT score as Factor")) + z


#11.Urine factor
Urine_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$Urine_s.V1))) +labs(title ="Sap score v/s Age and Urine") + labs(x= " Survivor_class", y= "Sap_score") +  guides(color = guide_legend("Urine score as Factor")) + z

#12. RespRate factor
RespRate_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$RespRate_s.V1))) +labs(title ="Sap score v/s Age and RespRate") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("RespRate score as Factor")) + z


#13. SysABP factor
SysABP_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$SysABP_s.V1))) +labs(title ="Sap score v/s Age and SysABP") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("SysABP score as Factor")) + z


#14.BUN factor
BUN_box <- ggplot(variable_scores, aes(as.factor(variable_scores$`survivor.In-hospital_death`), variable_scores$sap_scores)) + geom_boxplot(fill = fill , colour= line, alpha = 0.6, outlier.colour = "#1F3552", outlier.shape = 20, size = 0.5, notch = TRUE) +
  geom_jitter(alpha = 0.3, aes(color= factor(variable_scores$BUN_s.V1))) +labs(title ="Sap score v/s Age and BUN") + labs(x= " Survivor_class", y= "Sap_score") + guides(color = guide_legend("BUN score as Factor"))

install.packages("gridExtra")
library(gridExtra)
grid.arrange(age_box, HR_box, Temp_box, WBC_box, HCO3_box, K_box, Na_box, Glucose_box, HCT_box, nrow = 3, ncol = 3)
grid.arrange(Urine_box, RespRate_box, SysABP_box, BUN_box, nrow = 2, ncol =3)
# 
ggplot(variable_scores, aes(y = variable_scores$sap_scores, x =as.factor(variable_scores$`survivor.In-hospital_death`),
                            col= as.factor(variable_scores$Age_s.V1), shape = as.factor(variable_scores$HR_s.V1),
                            size= variable_scores$Temp_s.V1)) + geom_point()


# Plot for each variable count with Survivor as factor in one plot 

library(miscset) 
ggplotGrid(ncol = 3,
           lapply(c("Age_s.V1","HR_s.V1", "Temp_s.V1","WBC_s.V1", "HCO3_s.V1", "K_s.V1",                    
 "Na_s.V1", "Glucose_s.V1", "GCS_s.V1", "HCT_s.V1", "Urine_s.V1", "RespRate_s.V1", "SysABP_s.V1", "BUN_s.V1" ),
                  function(col) {
                    ggplot(variable_scores, aes_string(col)) + 
                      geom_bar(position = 'stack', aes(fill = interaction(`survivor.In-hospital_death`))) + coord_flip()
                  }))

# Correlation diagram for all variables
corre_scores <-round(cor(variable_scores), 2)

melted_scores <- melt(corre_scores)



# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(corre_scores)

melted_scores <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_scores, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
################################################################################################################

########################################### Part III############################################################

################################################################################################################

x_sml_dat = list()
x_urine_dat = list()
for (k in 1: 4000){
  ldf <- read.csv(listtxt[k])
  
  x_sml_dat[[k]] <- sqldf("select Value, max(Parameter ) from 'ldf' group by Parameter")
  x_urine_dat[[k]] <- sqldf("select value, sum(Parameter)from ldf where Parameter= 'Urine' ")
}


#Calculate the SAPS score for all variables when maximum:

max_Age_score= list()
max_HR_score = list()
max_Temp_score = list()
max_WBC_score = list()
max_HCO3_score = list()
max_K_score = list()
max_Na_score = list()
max_Glucose_score = list()
max_GCS_score = list()
max_HCT_score = list ()
max_BUN_score = list()
max_Urine_score = list()
max_RespRate_score = list()
max_SysABP_score = list()

for (i in 1:4000){
  print(i)
  
  max_Age_score[[i]] = Age(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Age", x_sml_dat[[i]]$Value)) 
  
  max_HR_score[[i]] =  HR(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "HR", x_sml_dat[[i]]$Value)) 
  
  max_Temp_score[[i]] =  Temp(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Temp", x_sml_dat[[i]]$Value))   
  
  max_WBC_score[[i]]  = WBC(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "WBC", x_sml_dat[[i]]$Value)) 
  
  max_HCO3_score[[i]] =  HCO3(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "HCO3", x_sml_dat[[i]]$Value))   
  
  max_K_score[[i]] =  K(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "K", x_sml_dat[[i]]$Value)) 
  
  max_Na_score[[i]] =  Na(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Na", x_sml_dat[[i]]$Value))  
  
  max_Glucose_score[[i]] =  Glucose(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Glucose", x_sml_dat[[i]]$Value)/18) 
  
  max_GCS_score[[i]]  = GCS(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "GCS", x_sml_dat[[i]]$Value))
  
  max_HCT_score[[i]] =  HCT(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "HCT", x_sml_dat[[i]]$Value)) 
  
  max_BUN_score[[i]] =  BUN(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "BUN", x_sml_dat[[i]]$Value)/2.8) 
  
  max_Urine_score[[i]]=  Urine(x_urine_dat[[i]]$Value/1000)         
  
  max_RespRate_score[[i]] = RespRate(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "RespRate", x_sml_dat[[i]]$Value)) 
  
  max_SysABP_score[[i]] = SysABP(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "SysABP", x_sml_dat[[i]]$Value)) 
  
}

#Combining list of each variables
#Age list
max_age_list <- as.data.frame(do.call(rbind, max_Age_score))

#HR List
max_HR_list <- as.data.frame(do.call(rbind, max_HR_score))

#Temp List
max_Temp_list <-as.data.frame(do.call(rbind, max_Temp_score))

#WBC List
max_WBC_list <- as.data.frame(do.call(rbind, max_WBC_score))

#HCO3 List
max_HCO3_list <-as.data.frame(do.call(rbind, max_HCO3_score))

#K List
max_K_list <- as.data.frame(do.call(rbind, max_K_score))

#Na List
max_Na_list <- as.data.frame(do.call(rbind, max_Na_score))

#Glucose ist
max_Glucose_list <- as.data.frame(do.call(rbind, max_Glucose_score))

#GCS List
max_GCS_list <- as.data.frame(do.call(rbind, max_GCS_score))

#HCT list
max_HCT_list <- as.data.frame(do.call(rbind, max_HCT_score))

#Urine List
max_Urine_list <- as.data.frame(do.call(rbind, max_Urine_score))

#RespRate List
max_RespRate_list <- as.data.frame(do.call(rbind, max_RespRate_score))

#SysABP List
max_SysABP_list <- as.data.frame(do.call(rbind, max_SysABP_score))

#BUN List
max_BUN_list <- as.data.frame(do.call(rbind, max_BUN_score))

### Max Values of variables of SAPS-I, between min and max value:

optimum_age <- pmax(age_list, max_age_list)
optimum_HR <- pmax(HR_list, max_HR_list)
optimum_Temp <- pmax(Temp_list, max_Temp_list)
optimum_WBC <- pmax(WBC_list, max_WBC_list)
optimum_HCO3 <- pmax(HCO3_list, max_HCO3_list)
optimum_K <- pmax(K_list, max_K_list)
optimum_Na <- pmax(Na_list, max_Na_list)
optimum_Glucose <- pmax(Glucose_list, max_Glucose_list)
optimum_GCS <- pmax(GCS_list, max_GCS_list)
optimum_HCT <- pmax(HCT_list, max_HCT_list)
optimum_Urine <- pmax(Urine_list, max_Urine_list)
optimum_RespRate <- pmax(RespRate_list, max_RespRate_list)
optimum_SysABP <- pmax(SysABP_list, max_RespRate_list)
optimum_BUN <- pmax(BUN_list, max_BUN_list)

# Optimum SAPS Score:
optimum_SAPS <- rowSums(optimum_age + optimum_HR + optimum_Temp + optimum_WBC + optimum_HCO3 + optimum_K + 
          optimum_Na + optimum_Glucose + optimum_GCS + optimum_HCT + optimum_Urine + optimum_RespRate + optimum_SysABP +optimum_BUN )

#Combining patient ids:
max_patient_scores <- data.frame("patient_id" = patient_id, "opt_saps_score" = optimum_SAPS)
comb_patient_scores <- data.frame("patient_id" = patient_id, "opt_saps_score" = optimum_SAPS, "min_Scores"= sap_list)
colnames(comb_patient_scores) <- c("patientID", "opt_saps_score", "min_score")


# max_Variable scores {combined list of all SAPS variables with output variable and SAPs score}
max_variable_scores <- data.table("Age_s" = max_age_list, "HR_s"= max_HR_list , "Temp_s" = max_Temp_list , "WBC_s" = max_WBC_list,
                              "HCO3_s"= max_HCO3_list , "K_s" = max_K_list, "Na_s" = max_Na_list, "Glucose_s" = max_Glucose_list,
                              "GCS_s" = max_GCS_list, "HCT_s"= max_HCT_list , "Urine_s"= max_Urine_list,
                              "RespRate_s" = max_RespRate_list, "SysABP_s" = max_SysABP_list, "BUN_s" = max_BUN_list, 
                              "survivor" = survivor)


#Optimium Model: 
max_model <- glm(max_variable_scores$`survivor.In-hospital_death`~., data = max_variable_scores, family = 'binomial' )
summary(max_model)
max_prob <- as.data.frame(predict(max_model, max_variable_scores[,-15], type = 'response'))
class_model <- as.data.frame(ifelse(max_prob>0.5, 1,0))

max_x <- data.frame("pred" = class_model, "actual" = max_variable_scores$`survivor.In-hospital_death`)
colnames(max_x) <- c("pred", "actual")
confusionMatrix(table(max_x$pred, max_x$actual))

#
k <- data.frame("opt"= class_model, "min" = pred)
colnames(k) <- c("opt", "min")
######Acuuracy = 85.92%


###################################################################################################################
#############################################SAPS -II #############################################################
##################################################################################################################

# Creating functions for each variable :
#1 AGE
sap2_age <- function(x){tryCatch({
  if (x<40) return(0)
  else if(x>=40 & x <=59) return(0)
  else if(x>= 60 & x<= 69) return(12)
  else if(x>= 70 & x <= 74) return(15)
  else if(x>=75 & x <= 80) return(16)
  else if(x> 80) return(18)
  else return(0)}
  ,
  error = function(e){
    return(0)
  }
)
}

#2 Heart Rate

sap2_HR <- function(x){tryCatch({
  if (x<40) return(11)
  else if(x>= 40 & x <= 69) return(2)
  else if(x>= 70 & x <= 119) return(0)
  else if(x>=120 & x<= 159) return(4)
  else if(x>= 160) return(7)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}


#3 Sys ABP
sap2_sysABP <- function(x){tryCatch({
  if (x<70) return(13)
  else if (x >= 70 & x<= 99) return(5)
  else if (x>= 100 & x<= 199) return(0)
  else if(x> 200) return(2)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}


#4 Temperature

sap2_temp <- function(x){tryCatch({
  if(x < 39) return(0)
  else if(x> 39) return(3)
  else return(0)}
  ,
  error = function(e){
    return(0)
  }
)}

#5 PAO2/ FIO2
sap2_ratio_PAO2_FiO2 <- function(x){tryCatch({
  if(x< 100) return(11)
  else if(x>=100 & x<= 199) return(9)
  else if(x >=200) return(6)
  else return(0)
  }
  ,
  error = function(e){
    return(0)
  }
  )}

#5 GCS
sap2_GCS <- function(x){tryCatch({
  if (x< 6) return(26)
  else if (x>= 6 & x <=8) return(13)
  else if (x>= 9 & x <= 10) return(7)
  else if ( x>=11 & x <= 13) return(5)
  else if (x>=14 & x<=15) return(0)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}

#6 Potassium
sap2_K <- function(x){tryCatch({
  if (x<3) return(3)
  else if (x>= 3 & x <5) return(0)
  else if (x>=5) return(3)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}  

#7 Sodium
sap2_Na <- function(x){tryCatch({
  if (x< 125) return(5)
  else if (x>=125 & x<= 144) return(0)
  else if (x> 145) return(1)
  else return(0)}
  ,
  error = function(e){
    return(0)
  }
)}  

#8 Urea
sap2_urea <- function(x){tryCatch({
  if (x< 10) return(0)
  else if (x>=10 & x<30) return(6)
  else if (x>= 30) return(10)
  else return(0)}
  ,
  error = function(e){
    return(0)
  }
)}

#9 WBC
sap2_WBC <- function(x){tryCatch({
  if (x<1) return(12)
  else if (x>=1 & x< 20) return(0)
  else if (x >= 20) return(3)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}

#10 Urine
sap2_urine <- function(x){tryCatch({
  if (x <0.5) return(11)
  else if(x >= 0.5 & x<= 0.99) return(4)
  else if(x>= 1) return(0)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}

#11 HCO3

sap2_HCO3 <- function(x){tryCatch({
  if (x< 15) return(6)
  else if (x>= 15 & x <= 19) return(3)
  else if (x>=20) return(0)
  else return(0)}
  ,
  error = function(e){
    return(0)
  }
)}

#12 Bilirubin

sap2_bilirubin <- function(x){tryCatch({
  if (x < 4) return(0)
  else if(x>=4 & x<= 5.99) return(4)
  else if(x>=6) return(9)
  else return(0)}
  , 
  error = function(e){
    return(0)
  }
)}

# Initializing empty list

sap2_score = list()
sap2_age_score = list()
sap2_HR_score = list()
sap2_sysABP_score = list()
sap2_temp_score= list()
sap2_bilirubin_score =list()
sap2_GCS_score = list()
sap2_HCO3_score =list()
sap2_K_score = list()
sap2_Na_score =list()
sap2_urea_score =list()
sap2_urine_score = list()
sap2_WBC_score =list()
sap2_ratio_PAO2_FiO2_score = list()

# Creating list for individual variables and calculating SAP2_score:

for (i in 1:4000){
  print(i)
sap2_age_score[[i]] = sap2_age(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == 'Age'))
sap2_HR_score[[i]] = sap2_HR(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "HR"))
sap2_sysABP_score[[i]] = sap2_sysABP(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "SysABP"))
sap2_temp_score[[i]] = sap2_temp(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "Temp"))
sap2_bilirubin_score[[i]] = sap2_bilirubin(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "Bilirubin"))
sap2_GCS_score[[i]] = sap2_GCS(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "GCS"))
sap2_HCO3_score[[i]] = sap2_HCO3(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "HCO3"))
sap2_K_score[[i]] = sap2_K(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "K"))
sap2_Na_score[[i]] = sap2_Na(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "Na"))
sap2_urea_score[[i]] = sap2_urea(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "BUN"))
sap2_urine_score[[i]] = sap2_urine(urine_dat[[i]])
sap2_WBC_score[[i]] = sap2_WBC(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )`== "WBC"))
sap2_ratio_PAO2_FiO2_score[[i]] = sap2_ratio_PAO2_FiO2(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "PaO2")/ subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "FiO2"))


sap2_score [[i]] = sap2_age_score[[i]] + sap2_HR_score[[i]] + sap2_sysABP_score[[i]] + sap2_temp_score[[i]]+
  sap2_bilirubin_score[[i]] + sap2_GCS_score[[i]] + sap2_HCO3_score[[i]] + sap2_K_score[[i]]+ sap2_Na_score[[i]] +
  sap2_urea_score[[i]] + sap2_urine_score[[i]] + sap2_WBC_score[[i]] + sap2_ratio_PAO2_FiO2_score[[i]]
}
library(data.table)

# Creating dataframe of the list of lists
sap2_age_df <- as.data.frame(do.call(rbind, sap2_age_score))
sap2_HR_df <- as.data.frame(do.call(rbind, sap2_HR_score))
sap2_sysABP_df <- as.data.frame(do.call(rbind, sap2_sysABP_score))
sap2_temp_df <- as.data.frame(do.call(rbind, sap2_temp_score))
sap2_bilirubin_df <- as.data.frame(do.call(rbind, sap2_bilirubin_score))
sap2_GCS_df <- as.data.frame(do.call(rbind, sap2_GCS_score))
sap2_HCO3_df <- as.data.frame(do.call(rbind, sap2_HCO3_score))
sap2_K_df <- as.data.frame(do.call(rbind, sap2_K_score))
sap2_Na_df <- as.data.frame(do.call(rbind, sap2_Na_score))
sap2_Urea_df <- as.data.frame(do.call(rbind, sap2_urea_score))
sap2_Urine_df <- as.data.frame(do.call(rbind, sap2_urine_score))
sap2_WBC_df <- as.data.frame(do.call(rbind, sap2_WBC_score))
sap2_ratio_PAO2_FiO2_df <- as.data.frame(do.call(rbind, sap2_ratio_PAO2_FiO2_score))

sap2_variable_scores = data.table("age" = sap2_age_df, "HR"= sap2_HR_df, "SysABP" = sap2_sysABP_df,"Temp" = sap2_temp_df, 
                                  "Bilirubin" = sap2_bilirubin_df, "GCS" = sap2_GCS_df, "HCO3" = sap2_HCO3_df, 
                                  "K" = sap2_K_df, "Na" = sap2_Na_df, "Urea" = sap2_Urea_df, 
                                  "Urine" = sap2_Urine_df, "WBC" =sap2_WBC_df, "ratio_PaO2_FiO2" = sap2_ratio_PAO2_FiO2_df,
                                  "survivor" = survivor)



#SaP2 Model {minimum values}: 
sap2_model <- glm(sap2_variable_scores$`survivor.In-hospital_death` ~., data = sap2_variable_scores, family = "binomial")
summary(sap2_model)

# Predicting the probabilities:
sap2_prob <-    predict(sap2_model, sap2_variable_scores, type = "response")

#Categorizing the classes:

sap2_class <- ifelse(sap2_prob> 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(table(sap2_class, sap2_variable_scores$`survivor.In-hospital_death`))


### Calcutate the score for maximum absolute values:

osap2_sml_dat <-list()
osap2_listtxt <- dir(pattern = "*.txt") # creates the list of all the csv files in the directory
library(sqldf)

# Grouping the parameters and finding the minimum values of each parameter for each patient
for (k in 1: 4000){
  osap2_ldf <- read.csv(osap2_listtxt[k])
  
  osap2_sml_dat[[k]] <- sqldf("select Value, max(Parameter ),Time from 'osap2_ldf' group by Parameter")

}

#Creating the Saps score function for each variable
osap2_score = list()
osap2_age_score = list()
osap2_HR_score = list()
osap2_sysABP_score = list()
osap2_temp_score= list()
osap2_bilirubin_score =list()
osap2_GCS_score = list()
osap2_HCO3_score =list()
osap2_K_score = list()
osap2_Na_score =list()
osap2_urea_score =list()
osap2_urine_score = list()
osap2_WBC_score =list()


for (i in 1:4000){
  print(i)
  osap2_age_score[[i]] = sap2_age(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )` == 'Age'))
  osap2_HR_score[[i]] = sap2_HR(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )`== "HR"))
  osap2_sysABP_score[[i]] = sap2_sysABP(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )`== "SysABP"))
  osap2_temp_score[[i]] = sap2_temp(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )`== "Temp"))
  osap2_bilirubin_score[[i]] = sap2_bilirubin(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )` == "Bilirubin"))
  osap2_GCS_score[[i]] = sap2_GCS(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )`== "GCS"))
  osap2_HCO3_score[[i]] = sap2_HCO3(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )` == "HCO3"))
  osap2_K_score[[i]] = sap2_K(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )` == "K"))
  osap2_Na_score[[i]] = sap2_Na(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )` == "Na"))
  osap2_urea_score[[i]] = sap2_urea(subset(osap2_sml_dat[[i]]$Value, osap2_sml_dat[[i]]$`max(Parameter )` == "BUN"))
  osap2_urine_score[[i]] = sap2_urine(urine_dat[[i]])
  osap2_WBC_score[[i]] = sap2_WBC(subset(osap2_sml_dat[[i]]$Value,osap2_sml_dat[[i]]$`max(Parameter )`  == "WBC"))
  
}
library(data.table)


#Comparing the list values of SAP II:



# Creating dataframe of the list of lists
osap2_age_df <- as.data.frame(do.call(rbind, osap2_age_score))
osap2_HR_df <- as.data.frame(do.call(rbind, osap2_HR_score))
osap2_sysABP_df <- as.data.frame(do.call(rbind, osap2_sysABP_score))
osap2_temp_df <- as.data.frame(do.call(rbind, osap2_temp_score))
osap2_bilirubin_df <- as.data.frame(do.call(rbind, osap2_bilirubin_score))
osap2_GCS_df <- as.data.frame(do.call(rbind, osap2_GCS_score))
osap2_HCO3_df <- as.data.frame(do.call(rbind, osap2_HCO3_score))
osap2_K_df <- as.data.frame(do.call(rbind, osap2_K_score))
osap2_Na_df <- as.data.frame(do.call(rbind, osap2_Na_score))
osap2_Urea_df <- as.data.frame(do.call(rbind, osap2_urea_score))
osap2_Urine_df <- as.data.frame(do.call(rbind, osap2_urine_score))
osap2_WBC_df <- as.data.frame(do.call(rbind, osap2_WBC_score))

# Comparing the maximum SAP_II values based on paper:
optimum_sap2_age <- pmax(osap2_age_df, sap2_age_df)
optimum_sap2_HR <- pmax(osap2_HR_df, sap2_HR_df)
optimum_sap2_SysABP <- pmax(osap2_sysABP_df, sap2_sysABP_df )
optimum_sap2_temp <- osap2_temp_df
optimum_sap2_bilirubin <- osap2_bilirubin_df
optimum_sap2_GCS <- sap2_GCS_df #using the lowest
optimum_sap2_HCO3 <- sap2_HCO3_df
optimum_sap2_K <- pmax(osap2_K_df, sap2_K_df)
optimum_sap2_Na <- pmax(osap2_Na_df, sap2_Na_df)
optimum_sap2_Urea <- osap2_Urea_df
optimum_sap2_Urine <- sap2_Urine_df
optimum_sap2_WBC <- pmax(osap2_WBC_df, sap2_WBC_df)
optimum_sap2_ratio_PaO2_FiO2 <- sap2_ratio_PAO2_FiO2_df

opt_sap2_variable_scores = data.table("age" = optimum_sap2_age, "HR"= optimum_sap2_HR, "SysABP" = optimum_sap2_SysABP,
                                      "Temp" = optimum_sap2_temp, "Bilirubin" = optimum_sap2_bilirubin, "GCS" = optimum_sap2_GCS,
                                      "HCO3" = optimum_sap2_HCO3, "K" = optimum_sap2_K, "Na" = optimum_sap2_Na, "Urea" = optimum_sap2_Urea,
                                      "Urine" = optimum_sap2_Urea, "WBC" =optimum_sap2_WBC, "ratio_PaO2_FiO2" = optimum_sap2_ratio_PaO2_FiO2, 
                                      "survivor" = survivor)



#SaP2 Model: 
opt_sap2_model <- glm(opt_sap2_variable_scores$`survivor.In-hospital_death` ~., data = opt_sap2_variable_scores, family = "binomial")
summary(opt_sap2_model)

# Predicting the probabilities:
opt_sap2_prob <-    predict(opt_sap2_model, opt_sap2_variable_scores, type = "response")

#Categorizing the classes:

opt_sap2_class <- ifelse(opt_sap2_prob> 0.5, 1, 0)

# Confusion Matrix
confusionMatrix(table(opt_sap2_class, opt_sap2_variable_scores$`survivor.In-hospital_death`))

###############################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ APACHE II SCORE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################################################
# APACHE 

#1. Heart Rate Score: (bpm)

ap_HR <- function(x){tryCatch(
  if (x>= 180  || x<=39) return(4)
  else if ((x >= 140 && x <= 179) || (x >= 40 &&  x<=54)) return(3)
  else if ((x >= 55 && x<= 69) || (x>= 110 && x<= 139)) return(2)
  else if (x>= 70 &&  x<= 109) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#2. Temperature Score (Celcius)
ap_Temp <- function(x){tryCatch(
  if (x>= 41  || x <= 29.9) return(4)
  else if ((x >= 39 && x < 40.9) || (x >= 30 &&  x<=31.9)) return(3)
  else if (x >= 32 && x<= 33.9) return(2)
  else if ((x>= 34 && x <=35.9) || (x>= 38.5 && x<= 38.9)) return(1)
  else if (x>= 36 &&  x<= 38.4) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
  )}

#3. Respiratory Rate  (bpm)

ap_RespRate <- function(x){tryCatch(
  if (x>=50  || x<5) return(4)
  else if (x >= 35 && x < 49) return(3)
  else if (x >= 6 && x<= 9) return(2)
  else if ((x>= 25 &&  x<=34) || (x >= 10 && x<= 11))  return(1)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#4 PAO2 Score ############

ap_PAO2 <- function(x){tryCatch(
  if (x> 500  || x < 55) return(4)
  else if ((x >= 350 && x <= 499) || (x >= 55 &&  x<=60)) return(3)
  else if (x >= 200 && x<= 349)  return(2)
  else if (x >= 61 && x<= 70) return(1)
  else if (x>= 70 &&  x<= 200) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#5 PH score [0-14]

ap_PH <- function(x){tryCatch(
  if (x>= 7.7  || x<7.15) return(4)
  else if ((x >= 7.15 && x < 7.24) || (x >= 7.6 &&  x<= 7.69)) return(3)
  else if (x >= 7.25 && x<= 7.32) return(2)
  else if(x>= 7.5 &&  x<= 7.59) return(1)
  else if (x>= 7.33 &&  x<= 7.49) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}


#6. HCO3 score (mmol/L)
ap_HCO3 <- function(x){tryCatch(
  if (x>= 52  || x<15) return(4)
  else if ((x >= 41 && x < 51.9) || (x >= 15 &&  x <= 17.9)) return(3)
  else if (x >= 18 && x<= 21.9) return(2)
  else if (x>= 32 && x<= 40.9) return(1)
  else if (x>= 22 &&  x<= 31.9) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}


#7. GCS Score [3-15]

ap_GCS <- function(x){tryCatch(
  if (x== 15) return(0)
  else if (x == 14) return(1)
  else if (x == 13) return(2)
  else if (x == 12) return(3)
  else if (x == 11) return(4)
  else if (x == 10) return(5)
  else if (x == 9) return(6)
  else if (x == 8) return(7)
  else if (x == 7) return(8)
  else if (x == 6) return(9)
  else if (x == 5) return(10)
  else if (x == 4) return(11)
  else if (x <=3) return(12)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#8. WBC Score (cells/nL)

ap_WBC <- function(x){tryCatch(
  if (x>= 40  || x<1) return(4)
  else if ((x >= 20 && x < 39.9) || (x >= 1 &&  x <= 2.9)) return(2)
  else if (x>= 15 && x<= 19.9) return(1)
  else if (x>= 3 &&  x<= 14.9) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#9.Age 

ap_Age <- function(x){tryCatch(
  if (x>= 75) return(6)
  else if (x >= 65 && x <= 74) return(5)
  else if (x >= 55 && x<= 64) return(3)
  else if (x>= 45  && x<= 54) return(2)
  else if (x<= 44) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}


#10. Haematocrit (HCT) (in %)

ap_HCT <- function(x){tryCatch(
  if (x >= 60 || x<20 ) return(4)
  else if ((x >= 50 && x < 59.9) || (x >= 20 &&  x <= 29.9)) return(2)
  else if (x>= 46 && x<= 49.9) return(1)
  else if (x>= 30 &&  x<= 45.9) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#11.  Serum Creatinine (mg/ 100ml) ##########

ap_Creatinine <- function(x){tryCatch(
  if (x>= 3.5) return(4)
  else if (x >= 2 && x < 3.4) return(3)
  else if (x >= 2 && x < 3.4) return(6)
  else if ((x<= 0.6) || (x>= 1.5 && x<= 1.9)) return(2)
  else if (x>= 0.6 && x<= 1.4) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}


#12. Serum Potassium (mMol/L)

ap_K_ <- function(x){tryCatch(
  if (x> 7  || x<  2.5) return(4)
  else if (x >= 6 && x < 6.9) return(3)
  else if (x >= 2.5 && x<= 2.9) return(2)
  else if ((x>= 3 && x<= 3.4) || (x>= 5 && x<= 5.9)) return(1)
  else if (x>= 3.5 &&  x<= 5.4) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#13. Serum Sodium Score

ap_Na <- function(x){tryCatch(
  if (x>= 180  || x<= 110) return(4)
  else if ((x >= 160 && x < 179) || (x >= 111 &&  x <= 119)) return(3)
  else if ((x >= 155 && x<= 159) || (x>= 120 && x<= 129)) return(2)
  else if (x>= 150 && x<= 154) return(1)
  else if (x>= 130 &&  x<= 149) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}

#14. Sys ABP

ap_SysABP <- function(x){tryCatch(
  if (x>= 160  || x<= 49) return(4)
  else if (x >= 130 && x < 159) return(3)
  else if ((x >= 110 && x<= 129) || (x>= 50 && x<=69)) return(2)
  else if (x>= 70 &&  x<= 109) return(0)
  else return(0)
  , 
  error = function(e){
    return(0)
  }
)}


# Initializing empty list

ap_sap_score = list()
ap_Age_score = list()
ap_HR_score = list()
ap_SysABP_score = list()
ap_Temp_score= list()
ap_GCS_score = list()
ap_HCO3_score =list()
ap_K_score = list()
ap_Na_score =list()
ap_WBC_score =list()
ap_PAO2_score = list()
ap_creatinine_score = list()
ap_HCT_score = list()
ap_PH_score = list()
ap_resprate_score = list()


# Calculate the APACHE II score for each patient:

for (i in 1:4000){
  print(i)
  
  ap_Age_score[[i]] = ap_Age(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Age", sml_dat[[i]]$Value)) 
  
  ap_HR_score[[i]] =  ap_HR(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "HR", sml_dat[[i]]$Value)) 
  
  ap_Temp_score[[i]] =  ap_Temp(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Temp", sml_dat[[i]]$Value))   
  
  ap_WBC_score[[i]]  = ap_WBC(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "WBC", sml_dat[[i]]$Value)) 
  
  ap_HCO3_score[[i]] =  ap_HCO3(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "HCO3", sml_dat[[i]]$Value))   
  
  ap_K_score[[i]] =  ap_K(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "K", sml_dat[[i]]$Value)) 
  
  ap_Na_score[[i]] =  ap_Na(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "Na", sml_dat[[i]]$Value))  
  
  ap_GCS_score[[i]]  = ap_GCS(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "GCS", sml_dat[[i]]$Value))
  
  ap_HCT_score[[i]] =  ap_HCT(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "HCT", sml_dat[[i]]$Value)) 
  
  ap_RespRate_score[[i]] = ap_RespRate(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "RespRate", sml_dat[[i]]$Value)) 
  
  ap_SysABP_score[[i]] = ap_SysABP(subset(sml_dat[[i]]$Value, sml_dat[[i]]$`min(Parameter )` == "SysABP", sml_dat[[i]]$Value)) 
  
  ap_Creatinine_score[[i]] = ap_Creatinine(subset(sml_dat[[1]]$`min(Parameter )` == "Creatinine", sml_dat[[1]]$Value ))
  
  ap_PH_score[[i]] = ap_PH(subset(sml_dat[[1]]$`min(Parameter )` == "pH", sml_dat[[1]]$Value ))
  
  ap_PAO2_score[[i]] = ap_PH(subset(sml_dat[[1]]$`min(Parameter )` == "PAO2", sml_dat[[1]]$Value ))
  
  
  
  max_ap_Age_score[[i]] = ap_Age(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Age", x_sml_dat[[i]]$Value)) 
  
  max_ap_HR_score[[i]] =  ap_HR(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "HR", sml_dat[[i]]$Value)) 
  
  max_ap_Temp_score[[i]] =  ap_Temp(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Temp", x_sml_dat[[i]]$Value))   
  
  max_ap_WBC_score[[i]]  = ap_WBC(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "WBC", x_sml_dat[[i]]$Value)) 
  
  max_ap_HCO3_score[[i]] =  ap_HCO3(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "HCO3", x_sml_dat[[i]]$Value))   
  
  max_ap_K_score[[i]] =  ap_K(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "K", x_sml_dat[[i]]$Value)) 
  
  max_ap_Na_score[[i]] =  ap_Na(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "Na", x_sml_dat[[i]]$Value))  
  
  max_ap_GCS_score[[i]]  = ap_GCS(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "GCS", x_sml_dat[[i]]$Value))
  
  max_ap_HCT_score[[i]] =  ap_HCT(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "HCT", x_sml_dat[[i]]$Value)) 
  
  max_ap_RespRate_score[[i]] = ap_RespRate(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "RespRate", x_sml_dat[[i]]$Value)) 
  
  max_ap_SysABP_score[[i]] = ap_SysABP(subset(x_sml_dat[[i]]$Value, x_sml_dat[[i]]$`max(Parameter )` == "SysABP", x_sml_dat[[i]]$Value)) 
  
  max_ap_Creatinine_score[[i]] = ap_Creatinine(subset(x_sml_dat[[1]]$`max(Parameter )` == "Creatinine", x_sml_dat[[1]]$Value ))
  
  max_ap_PH_score[[i]] = ap_PH(subset(x_sml_dat[[1]]$`max(Parameter )` == "pH", x_sml_dat[[1]]$Value ))
  
  max_ap_PAO2_score[[i]] = max_ap_PH_score(subset(sml_dat[[1]]$`min(Parameter )` == "PAO2", sml_dat[[1]]$Value ))
}


# Creating dataframe of the list of lists
ap_Age_df <- as.data.frame(do.call(rbind, ap_Age_score))
ap_HR_df <- as.data.frame(do.call(rbind, ap_HR_score))
ap_SysABP_df <- as.data.frame(do.call(rbind, ap_SysABP_score))
ap_Temp_df <- as.data.frame(do.call(rbind, ap_Temp_score))
ap_GCS_df <- as.data.frame(do.call(rbind, ap_GCS_score))
ap_HCO3_df <- as.data.frame(do.call(rbind, ap_HCO3_score))
ap_K_df <- as.data.frame(do.call(rbind, ap_K_score))
ap_Na_df <- as.data.frame(do.call(rbind, ap_Na_score))
ap_WBC_df <- as.data.frame(do.call(rbind, ap_WBC_score))
ap_Creatinine_df <- as.data.frame(do.call(rbind, ap_Creatinine_score))
ap_PH_df <- as.data.frame(do.call(rbind, ap_PH_score))
ap_RespRate_df <- as.data.frame(do.call(rbind, ap_RespRate_score))
ap_PAO2_df <- as.data.frame(do.call(rbind, ap_PAO2_score))
ap_HCT_df <- as.data.frame(do.call(rbind, ap_HCT_score))

# Creating dataframe of the list of lists
max_ap_Age_df <- as.data.frame(do.call(rbind, max_ap_Age_score))
max_ap_HR_df <- as.data.frame(do.call(rbind, max_ap_HR_score))
max_ap_SysABP_df <- as.data.frame(do.call(rbind, max_ap_SysABP_score))
max_ap_Temp_df <- as.data.frame(do.call(rbind, max_ap_Temp_score))
max_ap_GCS_df <- as.data.frame(do.call(rbind, max_ap_GCS_score))
max_ap_HCO3_df <- as.data.frame(do.call(rbind, max_ap_HCO3_score))
max_ap_K_df <- as.data.frame(do.call(rbind, max_ap_K_score))
max_ap_Na_df <- as.data.frame(do.call(rbind, max_ap_Na_score))
max_ap_WBC_df <- as.data.frame(do.call(rbind, max_ap_WBC_score))
max_ap_Creatinine_df <- as.data.frame(do.call(rbind, max_ap_Creatinine_score))
max_ap_PAO2_df <- as.data.frame(do.call(rbind, max_ap_PAO2_score))
max_ap_HCT_df <- as.data.frame(do.call(rbind, max_ap_HCT_score))
max_ap_PH_df <- as.data.frame(do.call(rbind, max_ap_PH_score))
max_ap_RespRate_df <- as.data.frame(do.call(rbind, max_ap_resprate_score))


### Max Values of variables of APACHE- II, between min and max value:

optimum_Age <- pmax(ap_Age_df, max_ap_Age_df)
optimum_HR <- pmax(ap_HR_df, max_ap_HR_df)
optimum_Temp <- pmax(ap_Temp_df, max_ap_Temp_df)
optimum_WBC <- pmax(ap_WBC_df, max_ap_WBC_df)
optimum_HCO3 <- pmax(ap_HCO3_df, ap_max_HCO3_df)
optimum_K <- pmax(ap_K_df, ap_max_K_df)
optimum_Na <- pmax(ap_Na_df, max_ap_Na_df)
optimum_GCS <- pmax(ap_GCS_df, max_ap_GCS_df)
optimum_HCT <- pmax(ap_HCT_df, max_ap_HCT_df)
optimum_RespRate <- pmax(ap_RespRate_df, max_ap_RespRate_df)
optimum_SysABP <- pmax(ap_SysABP_df, max_ap_RespRate_df)
optimum_PH <- pmax(ap_PH_df, max_ap_PH_df)
optimum_PAO2 <- pmax(ap_PAO2_df, max_ap_PAO2_df)
optimuum_Creatinine <- pmax(ap_Creatinine_df, max_ap_Creatinine_df)

# Optimum APACHE - II Score:
optimum_SAPS <- rowSums(optimum_age + optimum_HR + optimum_Temp + optimum_WBC + optimum_HCO3 + optimum_K + 
                          optimum_Na + optimum_Glucose + optimum_GCS + optimum_HCT + optimum_Urine + optimum_RespRate + optimum_SysABP +optimum_BUN )

 
