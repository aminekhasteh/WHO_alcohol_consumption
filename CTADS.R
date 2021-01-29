# Libraries
library(haven) # to read .dta files
library(readxl) # to read .xslx
library(stringr) # string manipulation
library(readstata13) # to read .dta files (better!)

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

row_creator_365 <- function(age_min = age_min, increment, 
                            tmp_dat = tmp_dat, gender, alc=68){ #alc = 68 or 54.4 grams
  iso3a <- 'CAN'
  country <- 'Canada'
  print(country)
  iso3n <- 124
  data <- 'CTADS'
  method <- 'CATI interview'
  populex <- 'non-households'
  resprate <- 'unknwon'
  ref <- 'SDA@CHASS - UofT'
  year <- 2017
  agemin <- age_min
  print(agemin)
  agemax <- age_min + increment
  if(gender == 'women'){
    sex <- 'female'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$SEX == 2)),]
  }
  if(gender == 'men'){
    sex <- 'male'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$SEX == 1)),]
  }
  if(gender == 'all'){
    sex <- 'total'
    tmp_tmp_dat <- tmp_dat
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$DVAGE >=agemin) & 
                                      (tmp_tmp_dat$DVAGE <=agemax)),]
  
  ######################################################
  ####################CDTL = 365#########################
  ######################################################
  N <- length(tmp_tmp_dat1$DVAGE)
  # a1==ALC_20: Have had a drink
  N_all <- length(tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_20 == 1)),]$DVAGE) # The sample size based on which "cda" (and "fda" and "laa") is calculated
  # a2==ALC_10: Drank alcoholic beverages - 12 mo
  tmp_tmp_dat2 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_10 != 8)&
                                       (tmp_tmp_dat1$ALC_10 != 96)&
                                       (tmp_tmp_dat1$ALC_10 != 97)&
                                       (tmp_tmp_dat1$ALC_10 != 98)&
                                       (tmp_tmp_dat1$ALC_10 != 99)),]
  N1 <- length(tmp_tmp_dat2$DVAGE)
  print(paste0('Number of those who have reported drinking at least 1x in the last year is ',
               N1))
  if(N > 0){ # Changing N_all-->N since 11904 individuals selected 6 (valid skip) for ALC_Q20
    cdtl <- 365 # last 365 days
    cda <- 100*(N1/N)
    cdase <- 100 * sqrt(((N1/N)*(1-N1/N))/N)
    #fdtl, fda,fdase: Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then
    tmp_tmp_dat3 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_20 == 1) # Have had a drink
                                        & (tmp_tmp_dat1$ALC_10 == 8)) ,] # Didn'drink last year
    
    fdtl <- 365 # last 365 days
    M <- length(tmp_tmp_dat3$DVAGE)
    fda <- 100*(M/N)
    fdase <- 100 * sqrt(((M/N)*(1-M/N))/N)
    
    #laa, lase : Those who have never consumed any alcohol in their lives (not just abstaining in the last year)
    tmp_tmp_dat4 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_20 == 2)),]
    M1 <- length(tmp_tmp_dat4$DVAGE)
    
    laa <- 100*(M1/N)
    laase <- 100 * sqrt(((M1/N)*(1-M1/N))/N)
    
    # ALC_60: Frequency of drinking five or more drinks on one occasion -12 mo. Using this for males.
    # ALC_50: Frequency of drinking four or more drinks on one occasion -12 mo. Using this for females.
    # During the past 12 months, how often have you had four or more drinks on one occasion?
      
    # 01: Daily or almost daily
    # 02: 4 to 5 times a week
    # 03: 2 to 3 times a week
    # 04: Once a week
    # 05: 2 to 3 times a month
    # 06: Once a month
    # 07: Less than once a month
    # 08: Never
    # 98: RF
    # 99: DK
    
    if((sex=="female") & (alc==54.4)){
      tmp_tmp_dat5 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_50 != 8)&
                                           (tmp_tmp_dat1$ALC_50 != 96)&
                                           (tmp_tmp_dat1$ALC_50 != 97)&
                                           (tmp_tmp_dat1$ALC_50 != 98)&
                                           (tmp_tmp_dat1$ALC_50 != 99)),]
      
      tmp_tmp_dat5_12 <- tmp_tmp_dat5[which(tmp_tmp_dat5$ALC_50 != 7),] # People who drank at least monthly in the past 12 months
      N2 <- length(tmp_tmp_dat5$DVAGE)
      N2_12 <- length(tmp_tmp_dat5_12$DVAGE)
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
      hedalc <- 54.4
      hedact <- 'total'
    } else if (((sex=="female") & (alc==68))|
               ((sex=="male") & (alc==68))|
               (sex=="total")){
      tmp_tmp_dat5 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_60 != 8)&
                                           (tmp_tmp_dat1$ALC_60 != 96)&
                                           (tmp_tmp_dat1$ALC_60 != 97)&
                                           (tmp_tmp_dat1$ALC_60 != 98)&
                                           (tmp_tmp_dat1$ALC_60 != 99)),]
      tmp_tmp_dat5_12 <- tmp_tmp_dat5[which(tmp_tmp_dat5$ALC_60 != 7),] # People who drank at least monthly in the past 12 months
      N2 <- length(tmp_tmp_dat5$DVAGE)
      N2_12 <- length(tmp_tmp_dat5_12$DVAGE)
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
      hedalc <- 68
      hedact <- 'total'
    } 

    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    tmp_tmp_dat6 <- tmp_tmp_dat1[which((tmp_tmp_dat1$ALC_40 != 96)&
                                         (tmp_tmp_dat1$ALC_40 != 97)&
                                         (tmp_tmp_dat1$ALC_40 != 98)&
                                         (tmp_tmp_dat1$ALC_40 != 99)),]
    if(length(tmp_tmp_dat6$DVAGE) > 0){
      # use ALC_40 : Number of drinks consumed on those days - 12 mo, and ALC_10
      # Converting from standard drinks to grams (1 Canadian standard drink = 13.6g)
      a1 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==1),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==1),]$ALC_10))/365 # Drank alcoholic beverages Daily or almost daily in the last 12 month
      a2 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==2),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==2),]$ALC_10))/365 # Drank alcoholic beverages 4 to 5 times a week in the last 12 month
      a3 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==3),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==3),]$ALC_10))/365 # Drank alcoholic beverages 2 to 3 times a week in the last 12 month
      a4 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==4),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==4),]$ALC_10))/365 # Drank alcoholic beverages Once a week in the last 12 month
      a5 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==5),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==5),]$ALC_10))/365 # Drank alcoholic beverages 2 to 3 times a month  in the last 12 month
      a6 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==6),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==6),]$ALC_10))/365 # Drank alcoholic beverages Once a month  in the last 12 month
      a7 <- 13.6 * (as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==7),]$ALC_40)*
                      as.numeric(tmp_tmp_dat6[which(tmp_tmp_dat6$ALC_10==7),]$ALC_10))/365 # Drank alcoholic beverages Less than once a month  in the last 12 month
      
      b <- c(a1,a2,a3,a4,a5,a6,a7)
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

# Importing the CTADS dataset:

dat <- read.csv('C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Datasets/CTADS 2017 (new)/CTADS 2017 data.csv')

# Setting the main dataset as a null data frame
ctads_dat<-NULL

# list of age based on each country
age_lb <- seq(15, max(as.numeric(dat$DVAGE),na.rm=TRUE), by=5)
total_increment <- max(as.numeric(dat$DVAGE), na.rm = TRUE) - 15

# sex = female
for (age_min in age_lb){
  row <- row_creator_365(age_min = age_min, increment=4,
                         tmp_dat = dat, gender='women', 
                         alc = 54.4)
  ctads_dat = rbind(ctads_dat,row)
  row <- row_creator_365(age_min = age_min, increment=4,
                         tmp_dat = dat, gender='women', 
                         alc = 68)
  ctads_dat = rbind(ctads_dat,row)
}
# All female
row <- row_creator_365(age_min = 15, increment = total_increment,
                       tmp_dat = dat, gender='women', 
                       alc = 54.4)
ctads_dat = rbind(ctads_dat,row)
row <- row_creator_365(age_min = 15, increment = total_increment,
                       tmp_dat = dat, gender='women', 
                       alc = 68)
ctads_dat = rbind(ctads_dat,row)
# sex = male
for (age_min in age_lb){
  row <- row_creator_365(age_min = age_min, increment=4,
                         tmp_dat = dat, gender='men')
  ctads_dat = rbind(ctads_dat,row)
}
# All male
row <- row_creator_365(age_min = 15, increment = total_increment,
                       tmp_dat = dat, gender='men')
ctads_dat = rbind(ctads_dat,row)

# sex = total
for (age_min in age_lb){
  row <- row_creator_365(age_min = age_min, increment=4,
                         tmp_dat = dat, gender='all')
  ctads_dat = rbind(ctads_dat,row)
}

# ALL
row <- row_creator_365(age_min = 15, increment = total_increment,
                       tmp_dat = dat, gender='all')
ctads_dat = rbind(ctads_dat,row)

ctads_dat <- as.data.frame(ctads_dat)
names(ctads_dat)=targets

# Saving the final dataset:
write.csv(ctads_dat,
          "C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Aggregated_Datasets/CTADS.csv", row.names = FALSE)

##################################################################################################
##################################################################################################
############################################# WEIGHT CHECK #######################################
##################################################################################################
##################################################################################################
library(Hmisc)
library(survey)

##	outcome and weights:
a1 	<- numeric(nrow(dat))
a1[dat$ALC_10==1] <- 1
a1[is.na(dat$ALC_10)] <- NA
table(a1)

dat$a1 <- a1
wght <- dat$WTPP

##	trial and error:
mean(a1, na.rm=TRUE) ## 3.046058%
weighted.mean(a1,wght, na.rm=TRUE) ## 6.680718% --> b?m!

##	confidence interval??
#wtd.mean(a1, weights=wght, normwt="ignored", na.rm=TRUE)
m <- 	wtd.mean(a1, weights=wght, normwt=FALSE, na.rm=TRUE)
v <- 	wtd.var(a1, weights=wght, normwt=FALSE, na.rm=TRUE)
se <- 	sqrt(v/length(a1[complete.cases(a1)]))
m - 1.96*se ## 0.06297975#
m + 1.96*se ## 0.07063462#
##########################  CI_95% = (0.06297975,0.07063462)

##	via svydesign?
svy <- svydesign(~1, data=dat, weights=dat$wtpp)
svymean(~ a1, svy, na.rm=TRUE) ##  mean = 3.0461%, SE=0.0013
svyciprop(~ a1, svy, na.rm=TRUE, method = c("logit")) ##  mean = 3.05,CI_95% = (0.0279,0.03)
dat$WTPP
