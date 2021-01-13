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
             'fda','fdase','laa','laase','N2','heda','hedase','hedaever','hedaseever',
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
  resprate <- '44.9' # ask about this!
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
    
    if(sex=="female"){
      tmp_tmp_dat5 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_50 != 8)&
                                           (tmp_tmp_dat1$ALC_50 != 96)&
                                           (tmp_tmp_dat1$ALC_50 != 97)&
                                           (tmp_tmp_dat1$ALC_50 != 98)&
                                           (tmp_tmp_dat1$ALC_50 != 99)),]
      N2 <- length(tmp_tmp_dat5$age)
      # Denominator is the total people
      heda <- 100*(N2/N_all)
      hedase <- 100 * sqrt(((N2/N_all)*(1-N2/N_all))/N_all)
      
      if(N1>0){
        # Denominator is people who hav had any drink in the past year
        hedaever <- 100*(N2/N1)
        hedaseever <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
        hedtl <- 30
        hedtlm <- 1
        hedalc <- 40
        hedact <- 'total'
      } else{
        hedaever <- NA
        hedaseever <- NA
        hedtl <- NA
        hedtlm <- NA
        hedalc <- NA
        hedact <- NA
      }
    } else if (sex=="male"){
      tmp_tmp_dat5 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_60 != 8)&
                                           (tmp_tmp_dat1$ALC_60 != 96)&
                                           (tmp_tmp_dat1$ALC_60 != 97)&
                                           (tmp_tmp_dat1$ALC_60 != 98)&
                                           (tmp_tmp_dat1$ALC_60 != 99)),]
      N2 <- length(tmp_tmp_dat5$DVAGE)
      # Denominator is the total people
      heda <- 100*(N2/N_all)
      hedase <- 100 * sqrt(((N2/N_all)*(1-N2/N_all))/N_all)
      
      if(N1>0){
        # Denominator is people who hav had any drink in the past year
        hedaever <- 100*(N2/N1)
        hedaseever <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
        hedtl <- 30
        hedtlm <- 1
        hedalc <- 60
        hedact <- 'total'
      } else{
        hedaever <- NA
        hedaseever <- NA
        hedtl <- NA
        hedtlm <- NA
        hedalc <- NA
        hedact <- NA
      }
    }
    
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    tmp_tmp_dat6 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_60 != 96)&
                                         (tmp_tmp_dat1$ALC_60 != 97)&
                                         (tmp_tmp_dat1$ALC_60 != 98)&
                                         (tmp_tmp_dat1$ALC_60 != 99)),]
    if(length(tmp_tmp_dat6$DVAGE) > 0){
      #use ALC_40*ALC_50
      a <- 13.6 * ((as.numeric(tmp_tmp_dat6$ALC_60))/30) # Converting from standard drinks to grams (1 Canadian standard drink = 13.6g)
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
              fda , fdase , laa , laase , N2 , heda , hedase , hedaever, hedaseever,
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

years <- c(2016,2017,2018,2019)
age_lb <- seq(15, max(as.numeric(dat$age),na.rm=TRUE), by=5)

for(year in years){
  tmp_dat <- dat[which(dat$year==year),]
  
}
