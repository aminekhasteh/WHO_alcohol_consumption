# Libraries
library(haven) # to read .dta files
library(readxl) # to read .xslx
library(stringr) # string manipulation
library(readstata13) # to read .dta files (better!)

# Reading the country code data file
country_dat <- read_excel("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/GENACIS-GENAHTO (new)/country_code_genacis.xlsx")
country_dat <- as.data.frame(as.matrix(country_dat))
country_code_dat <- read_excel('C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/all countries_iso_regions170427.xlsx')

# setting the target column names for the final dataset
targets <- c('country','iso3a','iso3n','data','method',
             'populex','resprate','ref','year','sex','agemin',
             'agemax','N1','cdtl','cda','cdase','fdtl',
             'fda','fdase','laa','laase','N2','heda','hedase','heda365','hedase365',
             'hedtl','hedtlm', 'N2_12' , 'heda_12' , 'hedase_12' , 
             'heda365_12', 'hedase365_12', 'hedtlm_12',
             'hedalc','hedact','N3','ddla','ddlase')

####################################################
############### Main Functions #####################
####################################################

row_creator_365 <- function(agemin = age_min, increment,
                            country_code_dat = country_code_dat, 
                            tmp_dat = tmp_dat, gender, country = country,
                            method = method){
  iso3a <- country_code_dat[which(country_code_dat$country == country),]$iso3a
  print(country)
  iso3n <- as.numeric(country_code_dat[which(country_code_dat$iso3a == iso3a),]$iso3n)
  data <- 'GENACIS-GENAHTO team’'
  populex <- 'unknown'
  if(country == 'Chile'){
    resprate <- 0.72
  } else if (country == 'Nigeria'){
    resprate <- 0.99
  } else if (country == 'India'){
    resprate <- 0.97
  } else if (country == 'Sri Lanka'){
    resprate <- 0.93
  } else if (country == 'Thailand'){
    resprate <- 0.94
  } else if (country == 'Vietnam'){
    resprate <- 0.99
  } else if (country == 'Laos'){
    resprate <- 0.99
  } else if (country == 'Australia'){
    resprate <- 0.35
  } else if (country == 'New Zealand'){
    resprate <- 0.64
  } else {
    resprate <- 'unknown'
  }
  ref <- 'CAMH/GMEL'
  year <- as.vector(country_dat[which(country_dat$Country == country),]$Year)
  print(agemin)
  agemax <- agemin + increment
  if(gender == 'women'){
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "women")),]
    sex <- 'female'
  }
  
  if(gender == 'men'){
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "men")),]
    sex <- 'male'
  }
  
  if(gender == 'all'){
    tmp_tmp_dat <- tmp_dat
    sex <- 'total'
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$age >=agemin) & 
                                      (tmp_tmp_dat$age <=agemax)),]
  ######################################################
  ####################CDTL = 365#########################
  ######################################################
  # cdtl, cda, cdase: Those who have reported drinking at least 1x in the last year
  N <- length(tmp_tmp_dat1$age)
  if(N > 0){
    ## babs (equivalent to definition of bfdb) : Abstainers (past year or more)
    # 0 No
    # 1 Yes
    N1 <- sum(tmp_tmp_dat1$babs==0,na.rm=TRUE)
    print(paste0('Number of those who have reported drinking at least 1x in the last year is ',
                 N1))
    cdtl <- 365
    cda <- 100*(N1/N)
    cdase <- 100 * sqrt(((N1/N)*(1-N1/N))/N)
    # No data available for : fdtl, fda,fdase (Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then)
    fdtl <- NA
    fda <- NA
    fdase <- NA
    
    # No data available for : laa, lase (Those who have never consumed any alcohol in their lives (not just abstaining in the last year))
    laa <- NA
    laase <- NA
    
    ## bf60: How often have you drunk at least 60g on a single day in the last 12 months value
    # 0 Never in last 12months
    # 1 Less than once a month
    # 2      1-3 times a month
    # 3       1-2 times a week
    # 4       3-4 times a week
    # 5       5-6 times a week
    # 6               Everyday
    
    ## bf60_146: How often have you drunk at least 60g on a single day in the last 12 months for New Zealand
    # 0                Never
    # 1    Less than monthly
    # 2  About 1 day a month
    # 3     2-3 days a month
    # 4      1-2 days a week
    # 5      3-4 days a week
    # 6      5-6 days a week
    # 7             Everyday
    
    if (country == "New Zealand"){
      N2 <- sum(tmp_tmp_dat1$bf60_146 != 0,na.rm=TRUE) # at least one heavy episodic drinking in the past 12 months
      N2_12 <- sum(tmp_tmp_dat1$bf60_146 > 1,na.rm=TRUE) # at least monthly heavy episodic drinking in the past 12 months
      
      # Denominator is the total people
      heda <- 100*(N2/N)
      hedase <- 100 * sqrt(((N2/N)*(1-N2/N))/N)

      heda_12 <- 100*(N2_12/N)
      hedase_12 <- 100 * sqrt(((N2_12/N)*(1-N2_12/N))/N)
     
      # Denominator is people who have had any drink in the past year
      heda365 <- 100*(N2/N1)
      hedase365 <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
      
      heda365_12 <- 100*(N2_12/N1)
      hedase365_12 <- 100 * sqrt(((N2_12/N1)*(1-N2_12/N1))/N1)
      
      hedtl <- 365
      hedtlm <- 1
      hedtlm_12 <- 12
      hedalc <- 60
      hedact <- 'total'
    } else {
      N2 <- sum(tmp_tmp_dat1$bf60 != 0,na.rm=TRUE) # at least one heavy episodic drinking in the past 12 months
      N2_12 <- sum(tmp_tmp_dat1$bf60 > 1,na.rm=TRUE) # at least monthly heavy episodic drinking in the past 12 months
      
      # Denominator is the total people
      heda <- 100*(N2/N)
      hedase <- 100 * sqrt(((N2/N)*(1-N2/N))/N)
      
      heda_12 <- 100*(N2_12/N)
      hedase_12 <- 100 * sqrt(((N2_12/N)*(1-N2_12/N))/N)
      
      if(N1>0){
        # Denominator is people who hav had any drink in the past year
        heda365 <- 100*(N2/N1)
        hedase365 <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
        heda365_12 <- 100*(N2_12/N1)
        hedase365_12 <- 100 * sqrt(((N2_12/N1)*(1-N2_12/N1))/N1)
      } else{
        heda365 <- NA
        hedase365 <- NA
        heda365_12 <- NA
        hedase365_12 <- NA
      }
      hedtl <- 365
      hedtlm <- 1
      hedtlm_12 <- 12
      hedalc <- 60
      hedact <- 'total'
    }
    
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    ## btyv: Total volume: average number of drinks yearly (DDL = btyv/365)
    # Finding the total number of people who have ever had a drink
    tmp_tmp_dat2 <- tmp_tmp_dat1[which(!is.na(tmp_tmp_dat1$btyv)),]
    if(length(tmp_tmp_dat2$age) > 0){
      a <- 10 * ((as.numeric(tmp_tmp_dat2$btyv))/365)
      b <- a[which(a!=0)]
      if(length(b)>1){
        N3 <- length(b)
        ddla <- mean(b)
        ddlase <- sd(b)/sqrt(N3)
      } else {
        N3 = NA
        ddla = NA
        ddlase = NA
      }
    } else {
      N3 = NA
      ddla = NA
      ddlase = NA
    }
    row <- c( country , iso3a , iso3n , data , method ,
              populex , resprate , ref , year , sex , agemin ,
              agemax , N1 , cdtl , cda , cdase , fdtl ,
              fda , fdase , laa , laase , N2 , heda , hedase , heda365, hedase365, 
              hedtl , hedtlm ,N2_12 , heda_12 , hedase_12 , heda365_12, hedase365_12, hedtlm_12,
              hedalc , hedact , N3 , ddla , ddlase )
    return(row)
  } 
  
} # end of function

#######################################################
#######################################################
#######################################################

dat <- read_dta("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/GENACIS-GENAHTO (new)/GENACIS-GENAHTO_data.dta")
dat <- as.data.frame(as.matrix(dat))
# Changing country codes to the country name:
dat[which(dat$country==120),]$country <- as.character(country_dat[which(country_dat$Code == 120),1])
dat[which(dat$country==121),]$country <- as.character(country_dat[which(country_dat$Code == 121),1])
dat[which(dat$country==130),]$country <- as.character(country_dat[which(country_dat$Code == 130),1])
dat[which(dat$country==144),]$country <- as.character(country_dat[which(country_dat$Code == 144),1])
dat[which(dat$country==146),]$country <- as.character(country_dat[which(country_dat$Code == 146),1])
dat[which(dat$country==147),]$country <- as.character(country_dat[which(country_dat$Code == 147),1])
dat[which(dat$country==148),]$country <- as.character(country_dat[which(country_dat$Code == 148),1])
dat[which(dat$country==149),]$country <- as.character(country_dat[which(country_dat$Code == 149),1])
dat[which(dat$country==150),]$country <- as.character(country_dat[which(country_dat$Code == 150),1])
names(dat)[3] <- 'age'
names(dat)[4] <- 'sex'
dat[which(dat$sex==1),]$sex= 'men'
dat[which(dat$sex==2),]$sex= 'women'

countries <- c("Australia","Chile","India","Laos","New Zealand","Nigeria","Sri Lanka","Thailand","Vietnam")

# Setting the age intervals:
summary(dat[which(dat$sex=='men'),]$age)   # Both sexes have the same max
summary(dat[which(dat$sex=='women'),]$age) # and min values.
 # max = 98
age_lb <- seq(15, max(as.numeric(dat$age),na.rm=TRUE), by=5)


# Setting the main dataset as a null data frame
genacis_dat<-NULL
for (country in countries){
  tmp_dat <- dat[dat$country == country,]
  if((country == "Australia")|
  (country == "New Zealand")){
    method = "Computer Assisted Telephone Interviewing"
  } else {
    method = "Face to face"
  }
  total_increment <- 98 - 15
  # list of age based on each country
  
  # sex = female
  for (age_min in age_lb){
    row <- row_creator_365(agemin = age_min, increment=4,
                           country_code_dat = country_code_dat, 
                           tmp_dat = tmp_dat, gender = 'women', country = country,
                           method = method)
    genacis_dat = rbind(genacis_dat,row)
  }
  row <- row_creator_365(agemin = 15, increment=total_increment,
                         country_code_dat = country_code_dat, 
                         tmp_dat = tmp_dat, gender = 'women', country = country,
                         method = method)
  genacis_dat = rbind(genacis_dat,row)
  
  # sex = male
  for (age_min in age_lb){
    row <- row_creator_365(agemin = age_min, increment=4,
                           country_code_dat = country_code_dat, 
                           tmp_dat = tmp_dat, gender = 'men', country = country,
                           method = method)
    genacis_dat = rbind(genacis_dat,row)
  }
  row <- row_creator_365(agemin = 15, increment=total_increment,
                         country_code_dat = country_code_dat, 
                         tmp_dat = tmp_dat, gender = 'men', country = country,
                         method = method)
  genacis_dat = rbind(genacis_dat,row)
  # sex =  total
  for (age_min in age_lb){
    row <- row_creator_365(agemin = age_min, increment=4,
                           country_code_dat = country_code_dat, 
                           tmp_dat = tmp_dat, gender = 'all', country = country,
                           method = method)
    genacis_dat = rbind(genacis_dat,row)
  }
  row <- row_creator_365(agemin = 15, increment=total_increment,
                         country_code_dat = country_code_dat, 
                         tmp_dat = tmp_dat, gender = 'all', country = country,
                         method = method)
  genacis_dat = rbind(genacis_dat,row)
}

genacis_dat <- as.data.frame(genacis_dat)
names(genacis_dat)=targets

# Saving the final dataset:
write.csv(genacis_dat,
          "C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Final_data/GENACIS.csv", row.names = FALSE)

