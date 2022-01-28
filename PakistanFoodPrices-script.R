library(readxl)
library(dplyr)
library(dlookr)
library(pastecs)
library(summarytools)

data <- read_excel("Pakifood.xlsx")
summary(data)

# Creación, modificación y observación de las variables --------------------
data1 <- data

str(data1)
names(data1)
head(data1)
summary(data1)

unique(data1$date)
unique(data$admname)

data1 <- data1 %>% mutate(year = case_when(
  endsWith(date, "2004")~"2004",
  endsWith(date, "2005")~"2005",
  endsWith(date, "2006")~"2006",
  endsWith(date, "2007")~"2007",
  endsWith(date, "2008")~"2008",
  endsWith(date, "2009")~"2009",
  endsWith(date, "2010")~"2010",
  endsWith(date, "2011")~"2011",
  endsWith(date, "2012")~"2012",
  endsWith(date, "2013")~"2013",
  endsWith(date, "2014")~"2014",
  endsWith(date, "2015")~"2015",
  endsWith(date, "2016")~"2016",
  endsWith(date, "2017")~"2017",
  endsWith(date, "2018")~"2018",
  endsWith(date, "2019")~"2019",
  endsWith(date, "2020")~"2020",
  endsWith(date, "2021")~"2021",
  endsWith(date, "2022")~"2022",))

datamon <- data %>% mutate(month = case_when(
  startsWith(date, "1/15")~"enero",
  startsWith(date, "2/15")~"febrero",
  startsWith(date, "3")~"marzo",
  startsWith(date, "4")~"abril",
  startsWith(date, "5")~"mayo",
  startsWith(date, "6")~"junio",
  startsWith(date, "7")~"julio",
  startsWith(date, "8")~"agosto",
  startsWith(date, "9")~"septiembre",
  startsWith(date, "10")~"octubre",
  startsWith(date, "11")~"noviembre",
  startsWith(date, "12")~"diciembre",))


data1$month <- datamon$month
monthord <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre",
              "noviembre","diciembre")
data1$month <- factor(data1$month, levels = monthord) 

data1$date = as.factor(data1$date)
data1$year = as.factor(data1$year)
data1$month = as.factor(data1$month)
data1$cmname = as.factor(data1$cmname)
data1$unit = as.factor(data1$unit)
data1$category = as.factor(data1$category)
data1$currency = as.factor(data1$currency)
data1$country = as.factor(data1$country)
data1$admname = as.factor(data1$admname)
data1$adm1id = as.numeric(data1$adm1id)
data1$mktname = as.factor(data1$mktname)
data1$mktid = as.numeric(data1$mktid)
data1$cmid = as.numeric(data1$cmid)
data1$cmidf = as.factor(data1$cmid)
data1$ptid = as.numeric(data1$ptid)
data1$ptidf = as.factor(data1$ptid)
data1$umid = as.numeric(data1$umid)
data1$umidf = as.factor(data1$umid)
data1$catid = as.numeric(data1$catid)
data1$catidf = as.factor(data1$catid)
data1$sn = as.factor(data1$sn)
str(data1)

# Cada uno de los niveles de las variables
levels(data1$cmname)
levels(data1$admname)
levels(data1$mktname)
levels(data1$category)
levels(data1$catidf)
levels(data1$cmidf)
levels(data1$ptidf)
levels(data1$umidf)
levels(data1$month)
levels(data1$year)

# Identificación y tratamientos de NA -------------------------------------
summary(data1)
apply(data1,2,function(x)sum(is.na(x)))
apply(data1,2,function(x)which(is.na(x)))

# Creación de sub-bases de datos ------------------------------------------
datanum <- data1 %>% select(date,month,year,country,admname,mktname,category,cmname,price,catid)

# Filtros por estado
databalo <- datanum %>% filter(admname=="Balochistan")
datakhy <- datanum %>% filter(admname=="Khyber Pakhtunkhwa")
datapun <- datanum %>% filter(admname=="Punjab")
datasin <- datanum %>% filter(admname=="Sindh")

# Filtro por producto
eggs <- datanum %>% filter(cmname=="Eggs - Retail")

# Filtros por categoría
certub <- datanum %>% filter(category=="cereals and tubers")
animals <- datanum %>% filter(category=="meat, fish and eggs")
dairy <- datanum %>% filter(category=="milk and dairy")
miscellaneous <- datanum %>% filter(category=="miscellaneous food")
nonfood <- datanum %>% filter(category=="non-food")
fats <- datanum %>% filter(category=="oil and fats")
pulses <- datanum %>% filter(category=="pulses and nuts")

# Identificación y tratamiento de Outliers --------------------------------

# Para Datanum (General)
diagnose_outlier(datanum)
datanum %>% plot_outlier(price)
boxplot(datanum$price)$out
outliers <- boxplot(datanum$price)$out

# Para observar el comportamiento de los Outliers en Datanumout
datanumout <- datanum[datanum$price %in% outliers,]
nrow(datanumout)
summary(datanumout)
boxplot(datanumout$price)

diagnose_outlier(datanumout)
datanumout %>% plot_outlier(price)
boxplot(datanumout$price)$out
outliers1 <- boxplot(datanumout$price)$out

# Base Eggs
diagnose_outlier(eggs)
eggs %>% plot_outlier(price)
boxplot(eggs$price)$out

levels(eggs$admname)
eggsoutbalo <- eggs %>% filter(admname=="Balochistan")
diagnose_outlier(eggsoutbalo)

eggsoutsin <- eggs %>% filter(admname=="Sindh")
diagnose_outlier(eggsoutsin)

eggsoutkhy <- eggs %>% filter(admname=="Khyber Pakhtunkhwa")
diagnose_outlier(eggsoutkhy)

eggsoutpun <- eggs %>% filter(admname=="Punjab")
diagnose_outlier(eggsoutpun)

# Estadística descriptiva e inferencial -----------------------------------

# ANOVA para diferencias entre estados
anovaeggs <- aov(price~admname, data=eggs)
summary(anovaeggs)

# ANOVA para diferencias entre años
anovaeggs1 <- aov(price~year, data=eggs)
summary(anovaeggs1)

# Extra -------------------------------------------------------------------
eggsbalo <- eggs %>% filter(admname=="Balochistan")
eggskhy <- eggs %>% filter(admname=="Khyber Pakhtunkhwa")
eggspun <- eggs %>% filter(admname=="Punjab")
eggssin <- eggs %>% filter(admname=="Sindh")

levels(eggs$admname)
levels(eggs$year)

summary(eggs)
str(eggs)

save(eggs,datanum,data1,eggsbalo,eggskhy,eggspun,eggssin,anovaeggs,anovaeggs1, file="final.rda")

