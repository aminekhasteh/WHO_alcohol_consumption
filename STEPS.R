# Libraries
library(haven) # to read .dta files
library(readxl) # to read .xslx
library(stringr) # string manipulation
library(readstata13) # to read .dta files (better!)

# Reading the country code data file
country_code_dat <- read_excel('C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/all countries_iso_regions170427.xlsx')

# Reading all file names for each country
file_names <- list.files(path = "C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data", 
                         pattern = NULL, all.files = FALSE,
                         full.names = FALSE, recursive = FALSE,
                         ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# setting the target column names for the final dataset
targets <- c('country','iso3a','iso3n','data','method',
             'populex','resprate','ref','year','sex','agemin',
             'agemax','N1','cdtl','cda','cdase','fdtl',
             'fda','fdase','laa','laase','N2','heda','hedase','hedaever','hedaseever',
             'hedtl','hedtlm','hedalc','hedact','N3','ddla','ddlase')

# They used 77 as 'Don't know' in their questionnaire

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
  agemin <- age_min
  print(agemin)
  agemax <- age_min + increment
  if(gender == 'women'){
    sex <- 'female'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "women")|
                                   (tmp_dat$sex == "Women")|
                                   (tmp_dat$sex == "Female")|
                                   (tmp_dat$sex == "female")),]
  }
  if(gender == 'men'){
    sex <- 'male'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "men")|
                                   (tmp_dat$sex == "Men")|
                                   (tmp_dat$sex == "Male")|
                                   (tmp_dat$sex == "male")),]
  }
  if(gender == 'all'){
    sex <- 'total'
    tmp_tmp_dat <- tmp_dat
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$age >=agemin) & 
                                      (tmp_tmp_dat$age <=agemax)),]
  if (length(tmp_tmp_dat1$age) > 0){
    tmp_tmp_dat1[,-which(names(tmp_tmp_dat1)=='age')][tmp_tmp_dat1[,-which(names(tmp_tmp_dat1)=='age')] == 77] <- NA
  }
  ######################################################
  ####################CDTL = 365#########################
  ######################################################
  N <- length(tmp_tmp_dat1$age)
  # N_all is the total number of individuals who have ever had a drink in their life time 
  N_all <- length(tmp_tmp_dat1[which((tmp_tmp_dat1$a1 == 'yes')|
                                       (tmp_tmp_dat1$a1 == 'Yes')),]$age) # The sample size based on which "cda" (and "fda" and "laa") is calculated
  tmp_tmp_dat2 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a2 == 'yes')|
                                       (tmp_tmp_dat1$a2 == 'Yes')),]
  N1 <- length(tmp_tmp_dat2$age)
  print(paste0('Number of those who have reported drinking at least 1x in the last year is ',
               N1))
  if(N_all > 0){
    cdtl <- 365 # last 365 days
    cda <- 100*(N1/N_all)
    cdase <- 100 * sqrt(((N1/N_all)*(1-N1/N_all))/N_all)
    #fdtl, fda,fdase: Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then
    tmp_tmp_dat3 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a2 == 'no' 
                                        & tmp_tmp_dat1$a1 == 'yes')|
                                         (tmp_tmp_dat1$a2 == 'No' 
                                          & tmp_tmp_dat1$a1 == 'Yes')) ,]
    if (N > 0){
      fdtl <- 365 # last 30 days
      M <- length(tmp_tmp_dat3$age)
      fda <- 100*(M/N)
      fdase <- 100 * sqrt(((M/N)*(1-M/N))/N)
    } else {
      fdtl <- NA
      fda <- NA
      fdase <- NA
    }
    #laa, lase : Those who have never consumed any alcohol in their lives (not just abstaining in the last year)
    tmp_tmp_dat4 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a1 == 'no')|
                                         (tmp_tmp_dat1$a1 == 'No')),]
    M1 <- length(tmp_tmp_dat4$age)
    if (N > 0){
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
    hedaever <- NA
    hedaseever <- NA
    hedtl <- NA
    hedtlm <- NA
    hedalc <- NA
    hedact <- NA
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    # Finding the total number of people who have ever had a drink
    tmp_tmp_dat6 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a1 == 'yes')|
                                         (tmp_tmp_dat1$a1 == 'Yes')),]
    if(length(tmp_tmp_dat6$age) > 0){
      # Using a7 : During the past 30 days, when you drank alcohol, how many standard drinks on average did you have during one drinking occasion
      # non-zero A7 (drink quantity) multiplied by A6 (drink frequency) divided by 30 days (time frame) x 10, take the mean.
      tmp_tmp_dat6[is.na(tmp_tmp_dat6)] <- 0
      a <- 10 * ((as.numeric(tmp_tmp_dat6$a7) * as.numeric(tmp_tmp_dat6$a6))/30)
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
  agemin <- age_min
  print(agemin)
  agemax <- age_min + increment
  if(gender == 'women'){
    sex <- 'female'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "women")|
                                   (tmp_dat$sex == "Women")|
                                   (tmp_dat$sex == "Female")|
                                   (tmp_dat$sex == "female")),]
  }
  if(gender == 'men'){
    sex <- 'male'
    tmp_tmp_dat <- tmp_dat[which((tmp_dat$sex == "men")|
                                   (tmp_dat$sex == "Men")|
                                   (tmp_dat$sex == "Male")|
                                   (tmp_dat$sex == "male")),]
  }
  if(gender == 'all'){
    sex <- 'total'
    tmp_tmp_dat <- tmp_dat
  }
  tmp_tmp_dat1 <- tmp_tmp_dat[which((tmp_tmp_dat$age >=agemin) & 
                                      (tmp_tmp_dat$age <=agemax)),]
  if (length(tmp_tmp_dat1$age) > 0){
    tmp_tmp_dat1[,-which(names(tmp_tmp_dat1)=='age')][tmp_tmp_dat1[,-which(names(tmp_tmp_dat1)=='age')] == 77] <- NA
  }
  ######################################################
  ####################CDTL = 30#########################
  ######################################################
  # cdtl, cda, cdase: Those who have reported drinking at least 1x in the last year
  N <- length(tmp_tmp_dat1$age)
  N_all <- length(tmp_tmp_dat1[which((tmp_tmp_dat1$a1 == 'yes')|
                                       (tmp_tmp_dat1$a1 == 'Yes')),]$age) # The sample size based on which "cda" (and "fda" and "laa") is calculated
  tmp_tmp_dat2 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a5 == 'yes')|
                                       (tmp_tmp_dat1$a5 == 'Yes')),]
  N1 <- length(tmp_tmp_dat2$age)
  print(paste0('Number of those who have reported drinking at least 1x in the last month is ',
               N1))
  if(N_all > 0){
    cdtl <- 30 # last 30 days
    cda <- 100*(N1/N_all)
    cdase <- 100 * sqrt(((N1/N_all)*(1-N1/N_all))/N_all)
    #fdtl, fda,fdase: Those who have not consumed alcohol in the last 1 year but have consumed alcohol before then
    tmp_tmp_dat3 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a2 == 'no' 
                                        & tmp_tmp_dat1$a1 == 'yes')|
                                         (tmp_tmp_dat1$a2 == 'No' 
                                          & tmp_tmp_dat1$a1 == 'Yes')),]
    if (N > 0){
      fdtl <- 30 # last 30 days
      M <- length(tmp_tmp_dat3$age)
      fda <- 100*(M/N)
      fdase <- 100 * sqrt(((M/N)*(1-M/N))/N)
    } else {
      fdtl <- NA
      fda <- NA
      fdase <- NA
    }
    #laa, lase : Those who have never consumed any alcohol in their lives (not just abstaining in the last year)
    tmp_tmp_dat4 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a1 == 'no')|
                                         (tmp_tmp_dat1$a1 == 'No')),]
    M1 <- length(tmp_tmp_dat4$age)
    if (N > 0){
      laa <- 100*(M1/N)
      laase <- 100 * sqrt(((M1/N)*(1-M1/N))/N)
    } else {
      laa <- NA
      laase <- NA
    }
    # Those who have reported binge drinking at least 60 grams or more of pure alcohol on at least one occasion in the past 30 days
    tmp_tmp_dat5 <- tmp_tmp_dat1[which(tmp_tmp_dat1$a9 >0),]
    N2 <- length(tmp_tmp_dat5$age)
    # Denominator is people who had any drink in the past month
    if (N1>0){
      hedaever <- 100*(N2/N1)
      hedaseever <- 100 * sqrt(((N2/N1)*(1-N2/N1))/N1)
    } else{
      hedaever <- NA
      hedaseever <- NA
    }
    
    # Denominator is people who hav had any drink
    heda <- 100*(N2/N_all)
    hedase <- 100 * sqrt(((N2/N_all)*(1-N2/N_all))/N_all)
    
    hedtl <- 30
    # minimum amount of binge drinking occasions per time frame
    hedtlm <- 1
    # minimum amount of pure alcohol consumed per binge drinking occasion
    hedalc <- 60
    hedact <- 'total'
    # ddla,ddlase : The average daily intake of alcohol (in grams) among drinkers (not total sample)
    # Finding the total number of people who have ever had a drink
    tmp_tmp_dat6 <- tmp_tmp_dat1[which((tmp_tmp_dat1$a1 == 'yes')|
                                         (tmp_tmp_dat1$a1 == 'Yes')),]
    if(length(tmp_tmp_dat6$age) > 0){
      # Using a7 : During the past 30 days, when you drank alcohol, how many standard drinks on average did you have during one drinking occasion
      tmp_tmp_dat6[is.na(tmp_tmp_dat6)] <- 0
      a <- 10 * ((as.numeric(tmp_tmp_dat6$a7) * as.numeric(tmp_tmp_dat6$a6))/30)
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
} # end of function 2

#######################################################
#######################################################
#######################################################

# Setting the main dataset as a null data frame
steps_dat<-NULL

for (dat in file_names){
  if(dat == "eth2015.dta"){
    tmp_dat <- read_dta(paste0("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data/",dat))
    tmp_dat <- as.matrix(tmp_dat)
    tmp_dat <- as.data.frame(tmp_dat)
    names(tmp_dat)[3] = 'sex'
    names(tmp_dat)[4] = 'age'
    tmp_dat[which(tmp_dat$sex==1),]$sex= 'men'
    tmp_dat[which(tmp_dat$sex==2),]$sex= 'women'
    tmp_dat[which(tmp_dat$a1==1),]$a1= 'yes'
    tmp_dat[which(tmp_dat$a1==2),]$a1= 'no'
    tmp_dat[which(tmp_dat$a2==1),]$a2= 'yes'
    tmp_dat[which(tmp_dat$a2==2),]$a2= 'no'
    tmp_dat[which(tmp_dat$a5==1),]$a5= 'yes'
    tmp_dat[which(tmp_dat$a5==2),]$a5= 'no'
    tmp_dat <- tmp_dat[!is.na(tmp_dat$age),]
    
    # removing the .dta extension
    tmp_file_name <- str_sub(dat,,-5)
    
  } else if (dat=="geo2016.dta") {
    tmp_dat <- read.dta13(paste0("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data/",dat))
    names(tmp_dat)[2] <- 'sex'
    tmp_dat <- tmp_dat[!is.na(tmp_dat$age),]
    
    # removing the .dta extension
    tmp_file_name <- str_sub(dat,,-5)
  } else if (dat=="qat2012.dta") { next # Qatar doesn't have any of Alcohol questionnaire
  } else if (dat=="SYC2013.xlsx"){
    # this file has very different varibales, doesn't have all questions from the questionnaire
    tmp_dat <- read_excel(paste0("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data/",
                                 dat), sheet = "Dataall-nonames")
    tmp_dat <- as.matrix(tmp_dat)
    tmp_dat <- as.data.frame(tmp_dat)
    tmp_dat$a1 <- tmp_dat$ohever
    tmp_dat[which(tmp_dat$a1==1),]$a1= 'yes'
    tmp_dat[which(tmp_dat$a1==2),]$a1= 'no'
    
    # ever had a drink in the last 365 days?
    # drinkmax : In the past 12 months, what was the largest amount of drinks you had on one single day, accounting for all types of drinks alltogether?
    tmp_dat$a2 <- ''
    tmp_dat[which(!is.na(tmp_dat$drinkmax)),]$a2= 'yes'
    tmp_dat[which(is.na(tmp_dat$drinkmax)),]$a2= 'no'
    
    # ever had a drink in the past 30 days?
    # ohnow : Do you currently drink alcohol beverages such as beer, wine, spirit or other other alcoholic drinks at least once per month?
    tmp_dat$a5 <- ''
    tmp_dat[which(!is.na(tmp_dat$ohnow)),]$a5= 'yes'
    tmp_dat[which(is.na(tmp_dat$ohnow)),]$a5= 'no'
    
    # During the past 30 days, how many times did you have six or more standard drinks in a single drinking occasion?
    # In a typical month (30 days), on how many days do you drink more than 5 (men)/4 (women) standard drinks per day, on average? (card: 1beer=1glass wine/liquor=1peg whisky)
    tmp_dat$a9 <- as.numeric(tmp_dat$drink5d)
    
    # During the past 30 days, when you drank alcohol, how many standard drinks on average did you have during one drinking occasion?
    # On average, how many alcohol drinks (beer, wine, spirit, etc) do you have per day between Monday and Thursday, Friday and Sat, Sun, think of any type of alcohol drinks?
    tmp_dat$a7 <- 30 * ((as.numeric(tmp_dat$drinkworkday) + # multiplying it by 30 since the function divides it by 30
                           as.numeric(tmp_dat$drinkfriday) +
                           as.numeric(tmp_dat$drinksat) +
                           as.numeric(tmp_dat$drinksun))/5) # taking the weekly average drinking (only for the 5 days)
    
    tmp_dat <- tmp_dat[!is.na(tmp_dat$age),]
    
    # removing the .dta extension
    tmp_file_name <- str_sub(dat,,-6)
  } else if(dat=="ury2013.dta") { # tis file has different variables
    tmp_dat <- read_dta(paste0("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data/",dat))
    tmp_dat <- as.matrix(tmp_dat)
    tmp_dat <- as.data.frame(tmp_dat)
    # changing varibale names:
    tmp_dat$sex <- as.numeric(tmp_dat$c1)
    tmp_dat[which(tmp_dat$sex==1),]$sex= 'men'
    tmp_dat[which(tmp_dat$sex==2),]$sex= 'women'
    tmp_dat$age <- as.numeric(tmp_dat$age)
    # assuming a1 is yes for everyone, since this variable is in this dataset
    tmp_dat$a1 <- 'yes' # need to find better a1, otherwise all will be fda = 0
    tmp_dat$a5 <- as.numeric(tmp_dat$x33)
    tmp_dat[which(tmp_dat$a5==1),]$a5= 'yes'
    tmp_dat[which(tmp_dat$a5==2),]$a5= 'no'
    tmp_dat$a2 <- as.numeric(tmp_dat$a1b)
    tmp_dat[which(tmp_dat$a2==1),]$a2= 'yes'
    tmp_dat[which(tmp_dat$a2==2),]$a2= 'no'
    tmp_dat$a9 <- as.numeric(tmp_dat$x25)
    tmp_dat$a7 <- 30 * ((as.numeric(tmp_dat$a9a) + # multiplying it by 30 since the function divides it by 30
                           as.numeric(tmp_dat$a9b) +
                           as.numeric(tmp_dat$a9c) +
                           as.numeric(tmp_dat$a9d)+
                           as.numeric(tmp_dat$a9e)+
                           as.numeric(tmp_dat$a9f)+
                           as.numeric(tmp_dat$a9g))/7) # taking the weekly average drinking 
    
    tmp_dat <- tmp_dat[!is.na(tmp_dat$age),]
    
    # removing the .dta extension
    tmp_file_name <- str_sub(dat,,-5)
  } else if((dat=="irq2015.dta")|
            (dat=="jor2019.dta")|
            (dat=="ukr2019.dta")|
            (dat=="mng2019.dta")){
    tmp_dat <- read.dta13(paste0("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data/",dat))
    tmp_dat <- tmp_dat[!is.na(tmp_dat$age),]
    # changing 1 --> yes and 2 --> no
    tmp_dat[which(tmp_dat$a1==1),]$a1= 'yes'
    tmp_dat[which(tmp_dat$a1==2),]$a1= 'no'
    tmp_dat[which(tmp_dat$a2==1),]$a2= 'yes'
    tmp_dat[which(tmp_dat$a2==2),]$a2= 'no'
    tmp_dat[which(tmp_dat$a5==1),]$a5= 'yes'
    tmp_dat[which(tmp_dat$a5==2),]$a5= 'no'
    
    # removing the .dta extension
    tmp_file_name <- str_sub(dat,,-5)
  } else {
    tmp_dat <- read.dta13(paste0("C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Project/Individual Survey datasets/New data_obtainined 2020/STEPS (new)/new STEPS data/",dat))
    tmp_dat <- tmp_dat[!is.na(tmp_dat$age),]
    
    # removing the .dta extension
    tmp_file_name <- str_sub(dat,,-5)
  }
  total_increment <- max(as.numeric(tmp_dat$age), na.rm = TRUE) - 15
  # list of age based on each country
  age_lb <- c(15,20,25,30,35,40,45,50,55,60)
  
  # sex = female
  for (age_min in age_lb){
    row <- row_creator_365(age_min = age_min, increment = 4,
                           country_code_dat = country_code_dat,
                           tmp_dat = tmp_dat, gender='women')
    steps_dat = rbind(steps_dat,row)
    row <- row_creator_30(age_min = age_min, increment = 4,
                          country_code_dat = country_code_dat,
                          tmp_dat = tmp_dat, gender='women')
    steps_dat = rbind(steps_dat,row)
  }
  
  # sex = all female
  row <- row_creator_365(age_min = 15, increment = total_increment,
                         country_code_dat = country_code_dat,
                         tmp_dat = tmp_dat, gender='women')
  steps_dat = rbind(steps_dat,row)
  row <- row_creator_30(age_min = 15, increment = total_increment,
                        country_code_dat = country_code_dat,
                        tmp_dat = tmp_dat, gender='women')
  steps_dat = rbind(steps_dat,row)
  
  # sex = male
  for (age_min in age_lb){
    row <- row_creator_365(age_min = age_min, increment = 4,
                           country_code_dat = country_code_dat,
                           tmp_dat = tmp_dat, gender='men')
    steps_dat = rbind(steps_dat,row)
    row <- row_creator_30(age_min = age_min, increment = 4,
                          country_code_dat = country_code_dat,
                          tmp_dat = tmp_dat, gender='men')
    steps_dat = rbind(steps_dat,row)
  }
  
  # sex = all male
  row <- row_creator_365(age_min = 15, increment = total_increment,
                         country_code_dat = country_code_dat,
                         tmp_dat = tmp_dat, gender='men')
  steps_dat = rbind(steps_dat,row)
  row <- row_creator_30(age_min = 15, increment = total_increment,
                        country_code_dat = country_code_dat,
                        tmp_dat = tmp_dat, gender='men')
  steps_dat = rbind(steps_dat,row)
  
  # sex = all
  for (age_min in age_lb){
    row <- row_creator_365(age_min = age_min, increment = 4,
                           country_code_dat = country_code_dat,
                           tmp_dat = tmp_dat, gender='all')
    steps_dat = rbind(steps_dat,row)
    row <- row_creator_30(age_min = age_min, increment = 4,
                          country_code_dat = country_code_dat,
                          tmp_dat = tmp_dat, gender='all')
    steps_dat = rbind(steps_dat,row)
  }
  
  # sex = all 
  row <- row_creator_365(age_min = 15, increment = total_increment,
                         country_code_dat = country_code_dat,
                         tmp_dat = tmp_dat, gender='all')
  steps_dat = rbind(steps_dat,row)
  row <- row_creator_30(age_min = 15, increment = total_increment,
                        country_code_dat = country_code_dat,
                        tmp_dat = tmp_dat, gender='all')
  steps_dat = rbind(steps_dat,row)
}
steps_dat <- as.data.frame(steps_dat)
names(steps_dat)=targets

###############################
# Changing Lebanon --> Lebanese gen pop
#          Syria --> Lebanon- Syrian Refugees (iso3a and iso3n are changing too)

steps_dat[,1] <- gsub('Lebanon', 'Lebanese gen pop', steps_dat[,1])
steps_dat[,1] <- gsub('Syria', 'Lebanon- Syrian Refugees', steps_dat[,1])
steps_dat[,2] <- gsub('SYR', 'LBN', steps_dat[,2])
steps_dat[,3] <- gsub('760', '422', steps_dat[,3])


# Saving the final dataset:
write.csv(steps_dat,
          "C:/Users/amink/OneDrive/Documents/Current Jobs/WHO project/Final_data/STEPS.csv", row.names = FALSE)

