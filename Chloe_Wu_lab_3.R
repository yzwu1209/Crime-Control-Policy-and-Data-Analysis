getwd()
setwd("/Users/cati/Documents/Study/UCB/W203-Statistics_for_Data_Science/HomeWork/lab_3")

crime_all = read.csv("crime_v2.csv")

### 1. View histogram ###
hist(crime_all$wcon)
hist(crime_all$wtuc)
hist(crime_all$wtrd)
hist(crime_all$wfir)
hist(crime_all$wser) ### There seems to be an outlier
hist(crime_all$wmfg)
hist(crime_all$wfed)
hist(crime_all$wsta)
hist(crime_all$wloc)

### 2. Basic data check ###
summary(crime_all$wcon)
summary(crime_all$wtuc)
summary(crime_all$wtrd)
summary(crime_all$wfir)
summary(crime_all$wser)
summary(crime_all$wmfg)
summary(crime_all$wfed)
summary(crime_all$wsta)
summary(crime_all$wloc)

### 3. Scatterplot between crime rate and weekly wage by industry ###
plot(crime_all$wcon,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

plot(crime_all$wtuc,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

### seems linear ###
plot(crime_all$wtrd,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

plot(crime_all$wfir,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

### interesting pattern ###
plot(crime_all$wser,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

plot(crime_all$wmfg,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

### seems linear ###
plot(crime_all$wfed,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

plot(crime_all$wsta,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

plot(crime_all$wloc,crime_all$crmrte, 
     ylab = "Crimes committed per person", main = "Weekly Wage vs. Crime Rate")

###################
### 4. Data transformation ###
df <- crime_all[is.na(crime_all$wcon),]

crime_1 <- crime_all[!is.na(crime_all$wcon), ]

# add an average weekly wage across all industry sectors for each county
crime_1$wavg <- (crime_1$wcon + crime_1$wtuc + crime_1$wtrd + crime_1$wfir + 
                crime_1$wser + crime_1$wmfg + crime_1$wfed + crime_1$wsta + 
                crime_1$wloc) / 9

# add a median weekly wage across all industry sectors for each county
?median
crime_1$wmed <- median(c(crime_1$wcon, crime_1$wtuc, crime_1$wtrd, crime_1$wfir, 
                         crime_1$wser, crime_1$wmfg, crime_1$wfed, crime_1$wsta, 
                         crime_1$wloc))
crime_1$wmed <- apply(crime_1[,c("wcon","wtuc","wtrd","wfir","wser","wmfg",
                                 "wfed","wsta","wloc")],1,median)

# add a log of (median weekly wage) across all industry sectors for each county
crime_1$wlog <- log(crime_1$wmed)

plot(crime_1$wavg,crime_1$crmrte, 
     ylab = "Crimes committed per person", 
     xlab = "Average weekly wage across industry sectors", 
     main = "Weekly Wage vs. Crime Rate")

plot(crime_1$wmed,crime_1$crmrte, 
     ylab = "Crimes committed per person", 
     xlab = "Median weekly wage across industry sectors", 
     main = "Weekly Wage vs. Crime Rate")

plot(crime_1$wlog,crime_1$crmrte, 
     ylab = "Crimes committed per person", 
     xlab = "ln of Median weekly wage across industry sectors", 
     main = "Weekly Wage vs. Crime Rate")

###################
### 5. Fit a linear model ###
mwage <- lm(crmrte ~ wmed, data = crime_1)                
abline(mwage)
summary(mwage)$r.square
AIC(mwage)
