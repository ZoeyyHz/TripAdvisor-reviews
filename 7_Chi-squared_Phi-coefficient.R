# install.packages("readr")
# install.packages("psych")
# install.packages("stats")
# install.packages("gmodels")
library(readr)
library(psych)
library(stats)
library(gmodels)

###################################################################
####################   Quarantine dataset     #####################
###################################################################


#import dataset file
Quan <- read_csv("../output/data_Quarantine_labelled.csv")

#view dataset
#View(Quan)

#see all columns name
colnames(Quan)

###################################################
####### Chi-square test & Phi coefficient  ########
###################################################

#label_air
CrossTable(Quan$label_air, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_air, Quan$`hotel reply yes or no`) , digits = 3)

#label_emotional
CrossTable(Quan$label_emotional, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_emotional, Quan$`hotel reply yes or no`) , digits = 3)

#label_facilities
CrossTable(Quan$label_facilities, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_emotional, Quan$`hotel reply yes or no`) , digits = 3)

#label_food
CrossTable(Quan$label_food, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_food, Quan$`hotel reply yes or no`) , digits = 3)

#label_hygiene
CrossTable(Quan$label_hygiene, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_hygiene, Quan$`hotel reply yes or no`) , digits = 3)

#label_infoSeeking
CrossTable(Quan$label_infoSeeking, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_infoSeeking, Quan$`hotel reply yes or no`) , digits = 3)

#label_location
CrossTable(Quan$label_location, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_location, Quan$`hotel reply yes or no`) , digits = 3)

#label_price
CrossTable(Quan$label_price, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_price, Quan$`hotel reply yes or no`) , digits = 3)

#label_room
CrossTable(Quan$label_room, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_room, Quan$`hotel reply yes or no`) , digits = 3)

#label_service
CrossTable(Quan$label_service, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_service, Quan$`hotel reply yes or no`) , digits = 3)

#label_wifi
CrossTable(Quan$label_wifi, Quan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(Quan$label_wifi, Quan$`hotel reply yes or no`) , digits = 3)


###################################################################
#################   Non-Quarantine dataset     ##################
###################################################################


#import dataset file
NonQuan <- read_csv("../output/data_NonQuarantine_labelled.csv")

#view dataset
#View(NonQuan)

#see all columns name
colnames(NonQuan)

###################################################
####### Chi-square test & Phi coefficient  ########
###################################################



#label_air
CrossTable(NonQuan$label_air, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
fisher.test(NonQuan$label_air, NonQuan$`hotel reply yes or no`)
phi(table(NonQuan$label_air, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_emotional
CrossTable(NonQuan$label_emotional, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_emotional, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_facilities
CrossTable(NonQuan$label_facilities, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_emotional, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_food
CrossTable(NonQuan$label_food, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_food, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_hygiene
CrossTable(NonQuan$label_hygiene, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_hygiene, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_infoSeeking
CrossTable(NonQuan$label_infoSeeking, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_infoSeeking, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_location
CrossTable(NonQuan$label_location, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_location, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_price
CrossTable(NonQuan$label_price, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_price, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_room
CrossTable(NonQuan$label_room, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_room, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_service
CrossTable(NonQuan$label_service, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
phi(table(NonQuan$label_service, NonQuan$`hotel reply yes or no`) , digits = 3)

#label_wifi
CrossTable(NonQuan$label_wifi, NonQuan$`hotel reply yes or no`, expected = TRUE, format = "SPSS")
fisher.test(NonQuan$label_wifi, NonQuan$`hotel reply yes or no`)
phi(table(NonQuan$label_wifi, NonQuan$`hotel reply yes or no`) , digits = 3)

##########################################################
# Below just an attempt, haven't use in the final report
##########################################################
######### logistic regression - Quanrantine #############
##########################################################

df_Quan = data.frame(Quan$`hotel reply yes or no`, Quan$label_air, Quan$label_emotional, Quan$label_facilities, Quan$label_food, Quan$label_hygiene, Quan$label_infoSeeking, Quan$label_location, Quan$label_price, Quan$label_room, Quan$label_service, Quan$label_wifi)

#see all columns name
colnames(df_Quan)

logistic <- glm(`Quan..hotel.reply.yes.or.no.` ~. , data=df_Quan, family="binomial")
summary(logistic)


##########################################################
######### logistic regression - Non-Quanrantine ##########
##########################################################

df_NonQuan = data.frame(NonQuan$`hotel reply yes or no`, NonQuan$label_air, NonQuan$label_emotional, NonQuan$label_facilities, NonQuan$label_food, NonQuan$label_hygiene, NonQuan$label_infoSeeking, NonQuan$label_location, NonQuan$label_price, NonQuan$label_room, NonQuan$label_service, NonQuan$label_wifi)

#see all columns name
colnames(df_NonQuan)

logistic_NQ <- glm(`NonQuan..hotel.reply.yes.or.no.` ~. , data=df_NonQuan, family="binomial")
summary(logistic_NQ)
