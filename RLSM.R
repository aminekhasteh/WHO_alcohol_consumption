# Libraries
library(haven) # to read .dta files
library(readxl) # to read .xslx
library(stringr) # string manipulation
library(readstata13) # to read .dta files (better!)
library(foreign)

# setting the target column names for the final dataset
targets <- c('country','iso3a','iso3n','data','method',
             'populex','resprate','ref','year','sex','agemin',
             'agemax','N1','cdtl','cda','cdase','fdtl',
             'fda','fdase','laa','laase','N2','heda','hedase','heda365','hedase365',
             'hedtl','hedtlm','hedalc','hedact','N3','ddla','ddlase')

####################################################
############### Main Functions #####################
####################################################

row_creator_365 <- function(age_min = age_min, increment, 
                            tmp_dat = tmp_dat, gender, year){
  iso3a <- 'RUS'
  country <- 'Russian Federation'
  iso3n <- 643
  data <- 'RLMS-HSE'
  method <- 'CATI interview'
  populex <- 'non-households'
  resprate <- 'unknwon'
  ref <- 'UNC Dataverse Download'
  year <- year
  agemin <- age_min
  print(agemin)
  agemax <- age_min + increment
  if(gender == 'women'){
    sex <- 'female'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$H5 == 2)),]
  }
  if(gender == 'men'){
    sex <- 'male'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$H5 == 1)),]
  }
  if(gender == 'all'){
    sex <- 'total'
    tmp_tmp_dat <- tmp_dat
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$age >=agemin) & 
                                      (tmp_tmp_dat$age <=agemax)),]
  
  ######################################################
  ####################CDTL = 30#########################
  ######################################################
  N <- length(tmp_tmp_dat1$age)
  # M80_0 CONSUME ALCOHOLIC BEVERAGES? 
  N_all <- length(tmp_tmp_dat1[which(tmp_tmp_dat1$M80_0 == 1),]$age) # The sample size based on which "cda" (and "fda" and "laa") is calculated
  # M80 DRANK ALCOHOL IN LAST 30 DAYS? 
  tmp_tmp_dat2 <- tmp_tmp_dat1[which((tmp_tmp_dat1$M80 == 1)),]
  N1 <- length(tmp_tmp_dat2$age)
  print(paste0('Number of those who have reported drinking at least 1x in the last month is ',
               N1))
  if(N_all > 0){ # Changing N_all-->N since 11904 individuals selected 6 (valid skip) for ALC_Q20
    cdtl <- 30 # last 30 days
    cda <- 100*(N1/N_all)
    cdase <- 100 * sqrt(((N1/N_all)*(1-N1/N_all))/N_all)
    #fdtl, fda,fdase: Those who have not consumed alcohol in the last month but have consumed alcohol before then
    tmp_tmp_dat3 <- tmp_tmp_dat1[which((tmp_tmp_dat1$M80_0 == 1) # Have had a drink
                                       & (tmp_tmp_dat1$M80 == 2)) ,] # Didn'drink last month
    
    fdtl <- 30 # last 30 days
    M <- length(tmp_tmp_dat3$age)
    fda <- 100*(M/N_all)
    fdase <- 100 * sqrt(((M/N_all)*(1-M/N_all))/N_all)
    
    #laa, lase : Those who have never consumed any alcohol in their lives (not just abstaining in the last month)
    tmp_tmp_dat4 <- tmp_tmp_dat1[which((tmp_tmp_dat1$M80_0 == 2)),]
    M1 <- length(tmp_tmp_dat4$age)
    
    laa <- 100*(M1/N)
    laase <- 100 * sqrt(((M1/N)*(1-M1/N))/N)
  
           ## HEAVY EPISODIC DRINKING ##
  # M84.3B GRAMS OF FORTIFIED WINE *18% ## EMPTY
  # M84.2B GRAMS OF DRY WINE *14.2% ## EMPTY
  # M84.11B GRAMS OF BEER? *5.4% ## EMPTY
  # M84.1B GRAMS OF BEER, BRAGA *5.4% ## EMPTY
    
    # M84.8B HOMEMADE WINE PER DAY - GRAMS *14.2%
    # M84.12B BRAGA PER DAY - GRAMS *5.4%
    # M84112B HOMEMADE BEER PER DAY - GRAMS  *5.4%
    # M84111B THE INDUSTRIAL PRODUCTION OF BEER PER DAY - GRAMS *5.4%
    # M84.6B GRAMS OTHER ALCOHOL *20%
    # M84.5B GRAMS OF VODKA_HARD LIQUOR *40%
    # M84.4B GRAMS OF HOMEMADE LIQUOR *50%
  
    tmp_tmp_dat1[(tmp_tmp_dat1==99999997)|
                   (tmp_tmp_dat1==99999998)|
                   (tmp_tmp_dat1==99999999)|
                   (is.na(tmp_tmp_dat1))] <- 0
    # Adding up the amount of alochol (in Grams) consumed by each individual in the past month
    tmp_tmp_dat1[,'total_alc_gram'] <- as.numeric(tmp_tmp_dat1$M84_8B)*0.142 +
                                           as.numeric(tmp_tmp_dat1$M84_12B)*0.054 +
                                           as.numeric(tmp_tmp_dat1$M84112B)*0.054 +
                                           as.numeric(tmp_tmp_dat1$M84111B)*0.054 +
                                           as.numeric(tmp_tmp_dat1$M84_6B)*0.20 +
                                           as.numeric(tmp_tmp_dat1$M84_5B)*0.40 +
                                           as.numeric(tmp_tmp_dat1$M84_4B)*0.50
                                           
    if(sex=="female"){
      tmp_tmp_dat5 <- tmp_tmp_dat1[which(tmp_tmp_dat1$total_alc_gram>40),]
      N2 <- length(tmp_tmp_dat5$age)
      # Denominator is the total people
      heda <- 100*(N2/N_all)
      hedase <- 100 * sqrt(((N2/N_all)*(1-N2/N_all))/N_all)
      
      if(N1>0){
        # Denominator is people who hav had any drink in the past year
        heda365 <- 100*(N2/N1)
        hedase365 <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
        hedtl <- 30
        hedtlm <- 1
        hedalc <- 40
        hedact <- 'total'
      } else{
        heda365 <- NA
        hedase365 <- NA
        hedtl <- NA
        hedtlm <- NA
        hedalc <- NA
        hedact <- NA
      }
    } else if ((sex=="male")|
               (sex=="total")){
      tmp_tmp_dat5 <- tmp_tmp_dat1[which(tmp_tmp_dat1$total_alc_gram>60),]
      N2 <- length(tmp_tmp_dat5$age)
      # Denominator is the total people
      heda <- 100*(N2/N_all)
      hedase <- 100 * sqrt(((N2/N_all)*(1-N2/N_all))/N_all)
      
      if(N1>0){
        # Denominator is people who hav had any drink in the past year
        heda365 <- 100*(N2/N1)
        hedase365 <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
        hedtl <- 30
        hedtlm <- 1
        hedalc <- 60
        hedact <- 'total'
      } else{
        heda365 <- NA
        hedase365 <- NA
        hedtl <- NA
        hedtlm <- NA
        hedalc <- NA
        hedact <- NA
      }
    } 
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    # M81 ALCOHOL-USE FREQUENCY LAST 30D 
      
    a1 <- (as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==1),]$total_alc_gram)*
             as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==1),]$M81))/30 # Drank alcoholic bevrages Daily or almost daily in the last month
    a2 <- (as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==2),]$total_alc_gram)*
             as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==2),]$M81))/20 # Drank alcoholic bevrages 4 to 6 times a week in the last month
    a3 <- (as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==3),]$total_alc_gram)*
             as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==3),]$M81))/10 # Drank alcoholic bevrages 2 to 3 times a week in the last month
    a4 <- (as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==4),]$total_alc_gram)*
             as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==4),]$M81))/4 # Drank alcoholic bevrages Once a week in the last month
    a5 <- (as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==5),]$total_alc_gram)*
             as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==5),]$M81))/2.5 # Drank alcoholic bevrages 2 to 3 times in the last 12 month
    a6 <- (as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==6),]$total_alc_gram)*
             as.numeric(tmp_tmp_dat1[which(tmp_tmp_dat1$M81==6),]$M81))/1 # Drank alcoholic bevrages Once in the last month
      
    b <- c(a1,a2,a3,a4,a5,a6)
    if(length(b)>1){
      N3 <- length(b)
      ddla <- mean(b)
      ddlase <- sd(b)/sqrt(N3)
    } else {
      N3 = NA
      ddla = NA
      ddlase = NA
    }
    row <- c( country , iso3a , iso3n , data , method ,
              populex , resprate , ref , year , sex , agemin ,
              agemax , N1 , cdtl , cda , cdase , fdtl ,
              fda , fdase , laa , laase , N2 , heda , hedase , heda365, hedase365,
              hedtl , hedtlm , hedalc , hedact , N3 , ddla , ddlase )
    return(row)
  } 
  
} # end of function

#######################################################
#######################################################
#######################################################

# Importing the RLMS dataset:

dat <- read.csv("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/RLMS (new)/USER_RLMS-HSE_IND_2016_2019_v2_eng.csv")
summary(dat$age) # removing individual with age = 99999997
dat <- dat[which(dat$age!=99999997),]

# Setting the main dataset as a null data frame
rlsm_dat<-NULL

years <- c(2016,2017,2018,2019)
age_lb <- seq(15, max(as.numeric(dat$age),na.rm=TRUE), by=5)
total_increment <- max(as.numeric(dat$age), na.rm = TRUE) - 15

for(year in years){
  tmp_dat <- dat[which(dat$year==year),]
  # sex = female
  for (age_min in age_lb){
    row <- row_creator_365(age_min = age_min, increment=4,
                           tmp_dat = tmp_dat, gender='women', year=year)
    rlsm_dat = rbind(rlsm_dat,row)
  }
  # All female
  row <- row_creator_365(age_min = age_min, increment=total_increment,
                         tmp_dat = tmp_dat, gender='women', year=year)
  rlsm_dat = rbind(rlsm_dat,row)
  # sex = male
  for (age_min in age_lb){
    row <- row_creator_365(age_min = age_min, increment=4,
                           tmp_dat = tmp_dat, gender='men', year=year)
    rlsm_dat = rbind(rlsm_dat,row)
  }
  # All male
  row <- row_creator_365(age_min = age_min, increment=total_increment,
                         tmp_dat = tmp_dat, gender='men', year=year)
  rlsm_dat = rbind(rlsm_dat,row)
  # sex = total
  for (age_min in age_lb){
    row <- row_creator_365(age_min = age_min, increment=4,
                           tmp_dat = tmp_dat, gender='all', year=year)
    rlsm_dat = rbind(rlsm_dat,row)
  }
  
  # ALL
  row <- row_creator_365(age_min = age_min, increment=total_increment,
                         tmp_dat = tmp_dat, gender='all', year=year)
  rlsm_dat = rbind(rlsm_dat,row)
}

rlsm_dat <- as.data.frame(rlsm_dat)
names(rlsm_dat)=targets

# Saving the final dataset:
write.csv(rlsm_dat,
          "C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Final_data/RLSM.csv", row.names = FALSE)

