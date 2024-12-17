library(SPEI)
library(drought)
setwd("/Users/prof.ogolo/Desktop/Rstudioresults")
droughtdat<- 

### Compute the standardize precipitation index
spi24<-spi(droughtdat$Prec.,24)
spi12<-spi(droughtdat$Prec.,12)
spi6<-spi(droughtdat$Prec.,6)
spi3<-spi(droughtdat$Prec.,3)

library(e1071)
kurtosis(droughtdat$Prec.)
skewness(droughtdat$Prec.)
shapiro.test(droughtdat$Prec.)
library(dplyr)
View(droughtdat$Prec.)
     
seasonal_precip <- aggregate(droughtdat$Prec., ~ year + month, data = droughtdat$Prec., FUN = sum)
seasonal_precip <- subset(seasonal_precip, month >= 4 & month <= 10)     
     
precip_wetseason<-droughtdat$Prec.%>%
                filter(month >= 4 & month <= 10)

spidrought<-data.frame(spi3=spi3$fitted,
                       spi6=spi6$fitted,
                       spi12=spi12$fitted,
                       spi24=spi24$fitted,droughtdat)

view(spidrought)
getwd()
spidroughts<-write.csv(spidrought,"spidrought.csv")

### Generate the plots of the time scales
plot(spi12, ylab = "SPI 12", type = "l", col="blue", lwd=2)
plot(spi24)
plot(spi9)
plot(spi6)
plot(spi3)

spi_pivot<-pivot_longer(spidrought,
                        cols = c("spi24","spi12","spi6","spi3"),
                        names_to = "index",
                        values_to = "Intensity")
  view(spi_pivot)                     
library(ggplot2)
ggplot(spi_pivot,aes(x = Month, y = Intensity))+geom_col()+
  facet_wrap(~index,nrow = 3)

###### plotting with ggplot
ggplot(spidrought,aes(Year,spi24))+geom_bar(stat = "identity",fill="skyblue")+
  labs(x="Year",y="Drought Intensity")

#### Compute drought categories
library(tidyverse)
spi3_cat <- cut(spidrought$spi3,
                 breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))

spi6_cat <- cut(spidrought$spi6,
                  breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))

spi12_cat <- cut(spidrought$spi12, 
                 breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))

spi24_cat <- cut(spidrought$spi24,
                  breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))
spicat<-data.frame(spi3_cat,spi6_cat,spi12_cat,spi24_cat,droughtdat)
# Rename variables
spicat<-spicat %>% 
          rename("spi3c"="spi3_cat",
                 "spi6c"="spi6_cat",
                 "spi12c"="spi12_cat",
                 "spi24c"="spi24_cat")
view(spicat)
# combine all files as dataframe

spi12new <- data.frame(spi12$fitted,spi9$fitted, spi6$fitted, spi3$fitted, spi12_cat,spi9_cat,
                       spi6_cat, spi3_cat, droughtdat)
is.na(spi12new$spi3)
View(is.na(spi12new$spi3))
library(RcppRoll)
cumsum(droughtdat, Prec.)

ggplot(sp_cat) + geom_boxplot(mapping = aes(x=spi12_cat))
View(spi12new)
head(droughtdat)
mutate(droughtdat, Tmean = (Tmax + Tmin)/2)
head(droughtdat)
cumsum(droughtdat, Prec.)
head(drou)
### get the number of drought category annually
library(tidyverse)
spi12new$monthcat <- month(spi12new$Month)
spi12new$monthcat <- ifelse(spi12new$monthcat>3&spi12new$monthcat<11,"Wet","Dry")
# breakup into dry and wet season
spidata_Dry <- spi12new %>%
  filter(monthcat=="Dry")

spidata_Wet <- spi12new %>%
  filter(monthcat=="Wet")

summrr <- spi12new %>%
  group_by(Year, spi12_cat) %>%
  summarise(number_cat = length(spi12_cat))
summrr <- na.omit(summrr)
summrrdata <- pivot_wider(summrr, names_from = spi12_cat, values_from = number_cat, values_fill = 0)
summrrdata$total <- summrrdata$Moderate +summrrdata$Mild+ summrrdata$Severe + summrrdata$Extreme

ggplot(summrrdata, aes(x=Year, y=total))+
  geom_col()

ggplot(summrr, aes(x=as.character(Year), y=number, label = number))+
  facet_wrap(~spi12_cat)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  geom_text(position = position_stack(vjust = 0.5), color="#ffffff")
  scale_x_discrete(labels=rev(year), breaks=year)

ggplot(summrr, aes(x=as.character(Year), y=number, fill=spi12_cat, label = number))+
    geom_col()+
  geom_text(position = position_stack(vjust = 0.5), color="#ffffff")+
  theme(axis.text.x = element_text(angle = 90))
  
  
?geom_col
tail(spi12new,n=6)
ggplot(droughtdat)
library(tidyr)
library(ggplot2)
ggplot(spi12new)+geom_boxplot(mapping = aes(x=spi3_cat)) + 
  ggtitle("evolution of drought categories for spi12")

y<-seq(2, 10, lenght.out =3)
 ### plot
 head(droughtdat)
library(ggplot2)
library(tidyr)  
 P<-ggplot(droughtdat)
 P+geom_line(mapping=aes(x = Year, y= Tmax,color = Month), size=1.0) 
 
 
 + 
   geom_smooth(mapping=aes(x = Year, y= Tmax), color="green")
 
 rlang::last_trace( geom_smooth(mapping=aes(x = Prec.[24,], y= Tmax[24,]), color="green"))`

#### count the number of each severity cases
table(spi12new$spi12_cat)
table(spi12new$spi9_cat)
table(spi12new$spi6_cat)
table(spi12new$spi3_cat)

#### Generate tables by year and month
AnnualCategory_count<- table(spi12new$spi12_cat,spi12new$Year)
write.csv(AnnualCategory_count,'Annualdroughtcat',row.names = TRUE)
MonthlyCategory_count<-table(spi12new$spi12_cat,spi12new$Month)
write.csv(MonthlyCategory_count,'Monthlydroughtcat',row.names = TRUE)


#threshold <- -0.5
#spi12new$drought_event <- ifelse(spi12$fitted < threshold,1,0)
#View(spi12new)
#table(spi12new$drought_event)
#drought_freq <- sum(diff(drought_event)==1)
#drought_freq


library(tidyverse)
library(dplyr)

#### Computing drought frequency
threshold <- -2

 drought_event <- spi12new %>% dplyr::filter(spi12.fitted < threshold)
total_years <- length(unique((drought_event$Year)))
num_drought_event <- nrow(drought_event)
summarise(num_drought_event)
drought_freq <- num_drought_event/total_years
cat("Drought frequency:", drought_freq)
cat("Total years:", total_years)
dim(spi12new)
hist(spi12new$spi12.fitted)
hist(spi12new$spi3.fitted)
 hist(Lokodrought$prec.) 
library(readxl)
 droughtdat <- read_excel("Desktop/Rstudioresults/droughtdat.xlsx")
 
 view(droughtdat)
 droughtdat <- read_excel("Desktop/Rstudioresults/droughtdat.xlsx", 
                          +     sheet = "Osogbo")
 
 head(droughtdat,24)
 tail(droughtdat$Prec.,6)
 summary(droughtdat$Prec.)
 hist(droughtdat$Tmax)
str(droughtdat)
precip<-read_csv("Desktop/precip.csv")
library(readr)
precip<-read.csv("Desktop/precip.csv" 
 view (precip,4)                
 view(precip$Tmax)                
  str(precip)               
  precip[1:12,3:5]               
   summary(precip[1:12,3:5] )              
   precip[,c("Tmin","Tmax")]              
   class(precip$Month)              
 library(RcmdrMisc)
   numSummary(spi12new[ ,c("spi3.fitted", "spei6.fitted"), drop=FALSE], 
              statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))  
   library(RcmdrMisc)
   library(Rcmdr)
   library(car)
   library(carData)
   library(sandwich)
   View(spi12new)
   read.table(spi12new)
   numSummary(data.frame()[,c("spi12new$spi3.fitted","spi12new$spi6.fitted"), drop=FALSE], 
              statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1)) 
   numSummary(spi12new[, drop=FALSE], 
              statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1)) 
   selected_columns <- spi12new[, c("spi3fitted", "spi6fitted")]
   View(spi12new)
   class(spi12new)
   setwd(spi12new)
   read.csv(spi12new.csv)
   droughtdat <- read_excel("Desktop/Rstudioresults/droughtdat.xlsx") 
  View(spi12new) 
  numSummary(spi12new[ ,c("Prec.", "spi3.fitted","spi6.fitted", "spi12.fitted"), drop=FALSE], 
                          statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))   
   
  cor(spi12new[ ,c("Prec.", "spi3.fitted","spi6.fitted", "spi12.fitted", "Tmin", "Tmax")], method="spearman", use="complete")
   View(spi12_cat) 
   Hist(spi12new$spi12.fitted, main = "spi12 drought evolution", xlab="spi12.fitted", ylab="frequency", col="red")
   table(spi12new$spi12_cat)
  
   barplot(table(spi12new$spi3_cat), main="drought categories for spi3", xlab= "drought categories", ylab = "frequency", names.arg = c("Extreme", "severe", "Moderate", "Mild"), col="red")
   table(spi12new$spi12_cat,spi12new$spi3_cat)
   barplot(table(spi12new$spi3_cat), main="drought categories for spi3", xlab= "drought categories", ylab = "frequency", names.arg = c("Extreme", "severe", "Moderate", "Mild"), col="red")
   barplot(table(spi12new$spi12_cat,spi12new$spi3_cat), main= "comparison of drought categories",xlab= "drought categories", ylab = "frequency", names.arg = c("Extreme", "severe", "Moderate", "Mild"), beside = TRUE,  col=c("red", "Blue"),legend = rownames(table(spi12new$spi12_cat, spi12new$spi3_cat)))
    boxplot(spi12new$spi3.fitted ~ spi12new$spi12.fitted, main="Annual variation of drought for Spi3 and spi6 ", xlab=c(spi12new$spi3.fitted,spi12new$spi6.fitted), ylab="Year")
   scatter.smooth(spi12new$spi12.fitted, spi12new$spi9.fitted, main="Relationship between spi9 and spi12", xlab="spi12", ylab="spi9")           
    library(ggplot2) 
   ggplot(spi12new)+geom_point(mapping = aes(x = spi12.fitted, y=spi3.fitted), position = "jitter") + 
     geom_smoot(mapping = aes(x = spi12.fitted, y=spi3.fitted))
  
  

ggplot(spi12new)+geom_point(mapping = aes(x=Year, y=spi12.fitted, color="spei12.fitted"), position = "jitter")

ggplot(spi12new,aes(spi12)+geom_histogram(bins = 10)

  library(sp) 
library(rgdal)   
library(raster)   
library(ggplot2)   
library(viridis)             
library(rasterVis)              
   install.packages(rgdal)           
              

