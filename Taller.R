rm(list=ls())

##Set the Working Directory##
setwd('/Users/Diana/Dropbox/Taller CID/Code')


girardot.raw<-read.csv('Girardot_update.csv', sep=',', header=TRUE, as.is=TRUE)
colnames(girardot.raw)<-c('date_report_char', 'EW', 'year', 'age', 'uni_med', 'sex', 'pregnant', 
                         'date_clinic_char', 'date_onset_char', 'case_type', 'date_birth_char')
girardot.raw$date_report<-as.Date(girardot.raw$date_report_char, '%m/%d/%Y')
girardot.raw$date_clinic<-as.Date(girardot.raw$date_clinic_char, '%m/%d/%Y')
girardot.raw$date_onset<-as.Date(girardot.raw$date_onset_char, '%m/%d/%Y')
girardot.raw$date_birth<-as.Date(girardot.raw$date_birth_char, '%m/%d/%Y')
girardot.raw$date_report_char<-girardot.raw$date_clinic_char<-girardot.raw$date_onset_char<-girardot.raw$date_birth_char<-NULL
girardot.raw$gender <- as.numeric(girardot.raw$sex=='F')
girardot.raw$age_grp <- 1
girardot.raw$age_grp[girardot.raw$age>20 & girardot.raw$age<=50] <- 2
girardot.raw$age_grp[girardot.raw$age>50] <- 3
girardot.raw$child <- as.numeric(girardot.raw$age_grp==1)
girardot.raw$senior <- as.numeric(girardot.raw$age_grp==3)
girardot.raw$day <- as.numeric(girardot.raw$date_onset - min(girardot.raw$date_onset) + 1)
girardot.raw <- girardot.raw[order(girardot.raw$date_onset, girardot.raw$gender, girardot.raw$age_grp),]
girardot.raw$count <- 1
girardot <- aggregate(count~date_onset, data=girardot.raw, FUN='sum')
girardot$day <- as.numeric(girardot$date_onset - min(girardot$date_onset) + 1)

girardot.raw$age_grp <- factor(girardot.raw$age_grp,
                               levels = c(1,2,3),
                               labels = c("Children", "Adults", "Senior"))

#girardot.raw$sex <- factor(girardot.raw$sex,
                               #levels = c(1,2),
                               #labels = c("M", "F"))
#Descriptive 
#Epidemic curve
counts <- table(girardot.raw$date_onset)
barplot(counts, main="Epidemic curve Zika, Girardot", 
        xlab="Date of onset of Symptoms")

#Genero
sex.table <- table(girardot.raw$sex)
sex.table
a<-prop.table(sex.table)
rbind(sex.table, a)

#Plot
counts <- table(girardot.raw$sex)
barplot(counts, main="Casos de Zika por genero, Girardot", 
        xlab="Genero")

#Grupo edad
counts <- table(girardot.raw$age)
barplot(counts, main="Casos de Zika por Edad, Girardot", 
        xlab="Edad", col=c("red"))

summary(girardot.raw$age)

counts <- table(girardot.raw$age_grp)
barplot(counts, main="Casos de Zika por Grupo edad, Girardot", 
        xlab="Grupo de Edad",  col=c("orange"))

# Agrupado por grupo edad & genero
counts <- table(girardot.raw$sex, girardot.raw$age_grp)
barplot(counts, main="Casos de Zika por edad y genero, Girardot",
        xlab="Age group", col=c("red", "darkblue"),
        legend = rownames(counts), beside=TRUE)

#plot.epicurve(girardot, 'Girardot, Colombia')
girardot.strata <- aggregate(count~day+gender+child+senior, data=girardot.raw, FUN='sum')
demo.girardot.raw<-read.csv('pop_Girardot.csv', sep=',', header=TRUE, as.is=TRUE)
demo.girardot.raw$child<-as.numeric(demo.girardot.raw$agegrp %in% c('0-4', '5-9', '10-14', '15-19'))
demo.girardot.raw$senior<-as.numeric(demo.girardot.raw$agegrp %in% c('50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80'))
demo.male <- aggregate(male~child+senior, data=demo.girardot.raw, FUN='sum')
colnames(demo.male) <- c('child', 'senior', 'total')
demo.male$gender <- 0
demo.female <- aggregate(female~child+senior, data=demo.girardot.raw, FUN='sum')
colnames(demo.female) <- c('child', 'senior', 'total')
demo.female$gender <- 1
demo.girardot <- rbind(demo.male, demo.female)

save(girardot, girardot.strata, demo.girardot, 'girardot.RData')
load('girardot.RData')

#San Andres

sanandres.raw<-read.csv('Daily_cases_SanAndres.csv', sep=',', header=TRUE, as.is=TRUE)
colnames(sanandres.raw)<-c('date_report_char', 'EW', 'year', 'age', 'uni_med', 'sex', 'area', 'pregnant', 
                         'date_clinic_char', 'date_onset_char', 'case_type', 'date_birth_char', 'municipality')
sanandres.raw$date_report<-as.Date(sanandres.raw$date_report_char, '%m/%d/%Y')
sanandres.raw$date_clinic<-as.Date(sanandres.raw$date_clinic_char, '%m/%d/%Y')
sanandres.raw$date_onset<-as.Date(sanandres.raw$date_onset_char, '%m/%d/%Y')
sanandres.raw$date_birth<-as.Date(sanandres.raw$date_birth_char, '%m/%d/%Y')
sanandres.raw$date_report_char<-sanandres.raw$date_clinic_char<-sanandres.raw$date_onset_char<-sanandres.raw$date_birth_char<-NULL
sanandres.raw$gender <- as.numeric(sanandres.raw$sex=='F')
sanandres.raw$age_grp <- 1
sanandres.raw$age_grp[sanandres.raw$age>20 & sanandres.raw$age<=50] <- 2
sanandres.raw$age_grp[sanandres.raw$age>50] <- 3
sanandres.raw$child <- as.numeric(sanandres.raw$age_grp==1)
sanandres.raw$senior <- as.numeric(sanandres.raw$age_grp==3)
sanandres.raw$day <- as.numeric(sanandres.raw$date_onset - min(sanandres.raw$date_onset) + 1)

sanandres.raw$count <- 1
sanandres <- aggregate(count~date_onset, data=sanandres.raw, FUN='sum')
sanandres$day <- as.numeric(sanandres$date_onset - min(sanandres$date_onset) + 1)
sanandres.raw$age_grp <- factor(sanandres.raw$age_grp,
                                levels = c(1,2,3),
                               labels = c("Children", "Adults", "Senior"))
#Descriptive 
#Epidemic curve
counts <- table(sanandres.raw$date_onset)
barplot(counts, main="Epidemic Curve Zika, San Andres", 
        xlab="Date of onset of Symptoms")
#Genero

s.table <- table(sanandres.raw$sex)
s.table
a<-prop.table(s.table)
rbind(s.table, a)

counts <- table(sanandres.raw$sex)
barplot(counts, main="Casos de Zika por genero, San Andres", 
        xlab="Genero")

#Grupo edad

counts <- table(sanandres.raw$age)
barplot(counts, main="Casos de Zika por Edad, San Andres", 
        xlab="Edad", col=c("green"))


summary(sanandres.raw$age)

counts <- table(sanandres.raw$age_grp)
barplot(counts, main="Casos de Zika por Grupo edad, San Andres", 
        xlab="Grupo de Edad",  col=c("orange"))

# Agrupado por grupo edad & genero
counts <- table(sanandres.raw$sex, sanandres.raw$age_grp)
barplot(counts, main="Zika Cases by gender and age, San Andres",
        xlab="Age group", col=c("red","darkblue"),
        legend = rownames(counts), beside=TRUE)


#plot.epicurve(sanandres, 'San Andres')
sanandres.strata <- aggregate(count~day+gender+child+senior, data=sanandres.raw, FUN='sum')

demo.sanandres.raw<-read.csv('pop_SanAndres.csv', sep=',', header=TRUE, as.is=TRUE)
demo.sanandres.raw$child<-as.numeric(demo.sanandres.raw$agegrp %in% c('0-4', '5-9', '10-14', '15-19'))
demo.sanandres.raw$senior<-as.numeric(demo.sanandres.raw$agegrp %in% c('50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '>80'))
demo.male <- aggregate(male~child+senior, data=demo.sanandres.raw, FUN='sum')
colnames(demo.male) <- c('child', 'senior', 'total')
demo.male$gender <- 0
demo.female <- aggregate(female~child+senior, data=demo.sanandres.raw, FUN='sum')
colnames(demo.female) <- c('child', 'senior', 'total')
demo.female$gender <- 1
demo.sanandres <- rbind(demo.male, demo.female)

save(sanandres, sanandres.strata, demo.sanandres, file='sanandres.RData')
load('sanandres.RData')

# EpiEstim
source('EstimationR.R')
fill.dates <- function(cases)
{ 
   cases.sort <- cases[order(cases$date_onset), ]
   tmp.date<-tmp.day<-NULL
   for(i in 2:nrow(cases))
   {
      d <- as.numeric(cases.sort$date_onset[i]-cases.sort$date_onset[i-1])
      if(d>1)
      {
         for(j in 1:(d-1))
         {
            tmp.date <- c(tmp.date, cases.sort$date_onset[i-1]+j)
            tmp.day <- c(tmp.day, cases.sort$day[i-1]+j)
         }
      }
   }
   if(length(tmp.date) > 0)
   {
      data.tmp<-data.frame(tmp.date, rep(0,length(tmp.date)), tmp.day)
      colnames(data.tmp) <- c('date_onset', 'count', 'day')
      data.tmp$date_onset <- as.Date(data.tmp$date_onset, origin = "1970-01-01")
      data.new <- rbind(cases, data.tmp)
      data.new <- data.new[order(data.new$date_onset),]
      return(data.new)
   }  else  return(cases.sort)
}


fit.for.plot <- function(cases, start.time, stop.time, SI.mean, SI.sd, main.text)
{
   #cases<-girardot; start.time<-8:67; stop.time<-14:73; SI.mean<-c(10,15,20); SI.sd<-c(3,3,3); main.text<-'Girardot, Columbia'
   R.mean <- R.lower <- R.upper <- NULL
   for(i in 1:length(SI.mean))
   {
      fit<-EstimateR(cases$count, T.Start=start.time, T.End=stop.time, method=c("ParametricSI"),
                     Mean.SI=SI.mean[i], Std.SI=SI.sd[i], plot=TRUE)
      R.mean <- rbind(R.mean, fit$R[,3])
      R.lower <- rbind(R.lower, fit$R[,5])
      R.upper <- rbind(R.upper, fit$R[,11])
   }
   R.max <- max(R.upper, na.rm=TRUE)
   #par(mfrow=c(2,1))
   plot(stop.time, R.mean[1,], type='n', xlim=c(0,nrow(cases)), ylim=c(0, R.max), xaxt='n', xlab='', ylab='R', main=main.text)
   k<-which(!is.na(R.upper[1,]) & !is.na(R.lower[1,]))
   polygon(c(rev(stop.time[k]), stop.time[k]), c(rev(R.upper[1,k]), R.lower[1,k]), col = 'grey80', border = NA)
   k<-which(!is.na(R.upper[2,]) & !is.na(R.lower[2,]))
   polygon(c(rev(stop.time[k]), stop.time[k]), c(rev(R.upper[2,k]), R.lower[2,k]), col = 'grey80', border = NA)
   k<-which(!is.na(R.upper[3,]) & !is.na(R.lower[3,]))
   polygon(c(rev(stop.time[k]), stop.time[k]), c(rev(R.upper[3,k]), R.lower[3,k]), col = 'grey80', border = NA)
   lines(stop.time, R.mean[1,], lwd=1.5, lty=1, col='green')
   lines(stop.time, R.mean[2,], lwd=1.5, lty=2, col='red')
   lines(stop.time, R.mean[3,], lwd=1.5, lty=3, col='blue')
   legend("topright", legend=paste('Mean SI: ', SI.mean, ' days', sep=''),
          lty=c(1,2,3), lwd=c(2,2,2), col=c('green', 'red', 'blue'), text.width=30)
   x<-1:nrow(cases)
   x<-c(x-0.5, x+0.5)
   k<-order(x)
   x<-x[k]
   y<-rep(cases$count,each=2)
   plot(x,y, ylim=c(0, max(y)), type='l', lty=1, lwd=1.5, xlab='Day', ylab='Case Number')
}   

# fitting the model using EpiEstim
par(mfcol=c(2,2))  
tmp <- fill.dates(girardot) 
fit.for.plot(tmp, 8:84, 14:90, c(10,15,20), c(3,3,3), 'Girardot, Colombia')  
tmp <- fill.dates(sanandres) 
fit.for.plot(tmp, 8:134, 14:140, c(10,15,20), c(3,3,3), 'San Andres, Colombia')  
 

