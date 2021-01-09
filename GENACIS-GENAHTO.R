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
             'fda','fdase','laa','laase','N2','heda','hedase',
             'hedtl','hedtlm','hedalc','hedact','N3','ddla','ddlase')

####################################################
############### Main Functions #####################
####################################################

row_creator_365 <- function(age_min = age_min, increment,
                            country_code_dat = country_code_dat, 
                            tmp_dat = tmp_dat, gender){
  iso3a <- toupper(str_sub(tmp_file_name,,-5)) # Making the string to upper case
  country <- country_code_dat[which(country_code_dat$iso3a == iso3a),]$country
  print(country)
  iso3n <- as.numeric(country_code_dat[which(country_code_dat$iso3a == iso3a),]$iso3n)
  data <- 'STEPS'
  method <- 'face-to-face'
  populex <- 'unknown'
  resprate <- 'unknwon'
  ref <- 'WHO STEPS - CAMH'
  year <- str_sub(tmp_file_name,-4)
  if(gender == 'women'){
    sex <- 'female'
  }
  if(gender == 'men'){
    sex <- 'male'
  }
  if(gender == 'all'){
    sex <- 'total'
  }
  agemin <- age_min
  print(agemin)
  agemax <- age_min + increment
  #cdtl: tmp_dat$a2 365 days and tmp_dat$a5 30 days
  if(gender == 'women'){
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "women")|
                                   (tmp_dat$sex == "Women")|
                                   (tmp_dat$sex == "Female")),]
  }
  
  if(gender == 'men'){
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "men")|
                                   (tmp_dat$sex == "Men")|
                                   (tmp_dat$sex == "Male")),]
  }
  if(gender == 'men'){
    tmp_tmp_dat <- tmp_dat[which(tmp_dat$sex == 1),]
  }
  if(gender == 'all'){
    tmp_tmp_dat <- tmp_dat
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$age >=agemin) & 
                                      (tmp_tmp_dat$age <=agemax)),]
  if (length(tmp_tmp_dat1$age) > 0){
    tmp_tmp_dat1[tmp_tmp_dat1 == 77] <- NA
  }
  ######################################################
  ####################CDTL = 365#########################
  ######################################################
  # cdtl, cda, cdase: Those who have reported drinking at least 1x in the last year
  N <- length(tmp_tmp_dat$age)
  tmp_tmp_dat2 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a2 == 'yes')),]
  N_365 <- length(tmp_tmp_dat2$age) 
  N1 <- length(tmp_tmp_dat1[which(tmp_tmp_dat1$a1 == 'yes'),]$age) # The sample size based on which "cda" (and "fda" and "laa") is calculated
  print(paste0('Number of those who have reported drinking at least 1x in the last year is ',length(tmp_tmp_dat2$age)))
  if(N1 > 0){
    cdtl <- 365
    cda <- 100*(N_365/N1)
    cdase <- 100 * sqrt(((N_365/N1)*(1-N_365/N))/N1)
    #fdtl, fda,fdase: Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then
    tmp_tmp_dat3 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a2 == 'no' & tmp_tmp_dat1$a1 == 'yes'),]
    if ((N_drinker) > 0){
      fdtl <- 365 # last 365 days
      M <- length(tmp_tmp_dat3$age)
      fda <- 100*(M/N1)
      fdase <- 100 * sqrt(((M/N1)*(1-M/N1))/N1)
    } else {
      fdtl <- NA
      fda <- NA
      fdase <- NA
    }
    #laa, lase : Those who have never consumed any alcohol in their lives (not just abstaining in the last year)
    tmp_tmp_dat4 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a1 == 'no'),]
    M1 <- length(tmp_tmp_dat4$age)
    if(N >0){
      laa <- 100*(M1/N)
      laase <- 100 * sqrt(((M1/N)*(1-M1/N))/N)
    } else {
      laa <- NA
      laase <- NA
    }
    # The STEPS dataset doesn't have any information on heavy drinking for the past 365 days (only 30 days), so we will leave hed variables as NA for cdtl = 365
    N2 <- NA
    heda <- NA
    hedase <- NA
    hedtl <- NA
    hedtlm <- NA
    hedalc <- NA
    hedact <- NA
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    # Finding the total number of people who have ever had a drink
    tmp_tmp_dat6 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a1 == 'yes'),]
    if(length(tmp_tmp_dat6$age) > 0){
      # Using a7 : During the past 30 days, when you drank alcohol, how many standard drinks on average did you have during one drinking occasion
      tmp_tmp_dat6[is.na(tmp_tmp_dat6)] <- 0
      a <- 10 * ((as.numeric(tmp_tmp_dat6$a7))/30)
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
              fda , fdase , laa , laase , N2 , heda , hedase ,
              hedtl , hedtlm , hedalc , hedact , N3 , ddla , ddlase )
    return(row)
  } 
  
} # end of function 1


####################

row_creator_30 <- function(age_min = age_min, increment,
                           country_code_dat = country_code_dat, 
                           tmp_dat = tmp_dat, gender){
  iso3a <- toupper(str_sub(tmp_file_name,,-5)) # Making the string to upper case
  country <- country_code_dat[which(country_code_dat$iso3a == iso3a),]$country
  print(country)
  iso3n <- as.numeric(country_code_dat[which(country_code_dat$iso3a == iso3a),]$iso3n)
  data <- 'STEPS'
  method <- 'face-to-face'
  populex <- 'unknown'
  resprate <- 'unknwon'
  ref <- 'WHO STEPS - CAMH'
  year <- str_sub(tmp_file_name,-4)
  if(gender == 'women'){
    sex <- 'female'
  }
  if(gender == 'men'){
    sex <- 'male'
  }
  if(gender == 'all'){
    sex <- 'total'
  }
  agemin <- age_min
  print(agemin)
  agemax <- age_min + increment
  if(gender == 'women'){
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "women")|
                                   (tmp_dat$sex == "Women")|
                                   (tmp_dat$sex == "Female")),]
  }
  if(gender == 'men'){
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "men")|
                                   (tmp_dat$sex == "Men")|
                                   (tmp_dat$sex == "Male")),]
  }
  if(gender == 'all'){
    tmp_tmp_dat <- tmp_dat
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$age >=agemin) & 
                                      (tmp_tmp_dat$age <=agemax)),]
  if (length(tmp_tmp_dat1$age) > 0){
    tmp_tmp_dat1[tmp_tmp_dat1 == 77] <- NA
  }
  ######################################################
  ####################CDTL = 30#########################
  ######################################################
  # cdtl, cda, cdase: Those who have reported drinking at least 1x in the last year
  N <- length(tmp_tmp_dat$age)
  tmp_tmp_dat2 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a5 == 'yes')),]
  N_30 <- length(tmp_tmp_dat2$age) 
  N1 <- length(tmp_tmp_dat1[which(tmp_tmp_dat1$a1 == 'yes'),]$age) # The sample size based on which "cda" (and "fda" and "laa") is calculated
  print(paste0('Number of those who have reported drinking at least 1x in the last year is ',length(tmp_tmp_dat2$age)))
  if(N1 > 0){
    cdtl <- 30 # last 30 days
    cda <- 100*(N_30/N1)
    cdase <- 100 * sqrt(((N_30/N1)*(1-N_30/N))/N1)
    #fdtl, fda,fdase: Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then
    tmp_tmp_dat3 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a5 == 'no' & tmp_tmp_dat1$a1 == 'yes'),]
    if ((N_drinker) > 0){
      fdtl <- 30 # last 30 days
      M <- length(tmp_tmp_dat3$age)
      fda <- 100*(M/N1)
      fdase <- 100 * sqrt(((M/N1)*(1-M/N1))/N1)
    } else {
      fdtl <- NA
      fda <- NA
      fdase <- NA
    }
    #laa, lase : Those who have never consumed any alcohol in their lives (not just abstaining in the last year)
    tmp_tmp_dat4 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a1 == 'no'),]
    M1 <- length(tmp_tmp_dat4$age)
    if(N >0){
      laa <- 100*(M1/N)
      laase <- 100 * sqrt(((M1/N)*(1-M1/N))/N)
    } else {
      laa <- NA
      laase <- NA
    }
    # The STEPS dataset doesn't have any information on heavy drinking for the past 365 days (only 30 days), so we will leave hed variables as NA for cdtl = 365
    N2 <- NA
    heda <- NA
    hedase <- NA
    hedtl <- NA
    hedtlm <- NA
    hedalc <- NA
    hedact <- NA
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    # Finding the total number of people who have ever had a drink
    tmp_tmp_dat6 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a1 == 'yes'),]
    if(length(tmp_tmp_dat6$age) > 0){
      # Using a7 : During the past 30 days, when you drank alcohol, how many standard drinks on average did you have during one drinking occasion
      tmp_tmp_dat6[is.na(tmp_tmp_dat6)] <- 0
      a <- 10 * ((as.numeric(tmp_tmp_dat6$a7))/30)
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
              fda , fdase , laa , laase , N2 , heda , hedase ,
              hedtl , hedtlm , hedalc , hedact , N3 , ddla , ddlase )
    return(row)
  } 
} # end of function 2

#######################################################
#######################################################
#######################################################

# Setting the main dataset as a null data frame
genacis_dat<-NULL
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
table(dat$country)
names(dat)[3] <- 'age'
names(dat)[4] <- 'sex'
dat[which(dat$sex==1),]$sex= 'men'
dat[which(dat$sex==2),]$sex= 'women'


# Other variables:

## babs : Abstainers (past year or more)
# 0 No
# 1 Yes

## bf60: How often have you drunk at least 60g on a single day in the last 12 months value                  label
# 0 Never in last 12months
# 1 Less than once a month
# 2      1-3 times a month
# 3       1-2 times a week
# 4       3-4 times a week
# 5       5-6 times a week
# 6               Everyday

## bf60_146: How often have you drunk at least 60g on a single day in the last 12 months
# 0                Never
# 1    Less than monthly
# 2  About 1 day a month
# 3     2-3 days a month
# 4      1-2 days a week
# 5      3-4 days a week
# 6      5-6 days a week
# 7             Everyday

## btyv: Total volume: average number of drinks yearly (DDL = btyv/365)

