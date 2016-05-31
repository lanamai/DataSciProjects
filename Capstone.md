#### CKME136    - Data Analytics, Capstone Course
#### Instructor - Dr. Atakan Erdem
#### Author     - Sue Lana Mai     

######--------------------------------------------------------------------#

##### Animal Shelter Output Dataset

##### Import csv file train dataset into Rstudio

##### Note: only the training dataset is used in this project.  This is due to 
#####       fact that test dataset does not include the OutcomeType 
#####       and this project is to predict whether the animal is adopted or not.

aso <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

######--------------------------------------------------------------------#

##### Data Exploration and Cleaning

class(aso)    # "data.frame"
dim(aso)      # 26729    10
  
colnames(aso)
###### [1] "AnimalID"       "Name"           "DateTime"       "OutcomeType"   
###### [5] "OutcomeSubtype" "AnimalType"     "SexuponOutcome" "AgeuponOutcome"
###### [9] "Breed"          "Color"         

head(aso)
###### AnimalID    Name         DateTime     OutcomeType OutcomeSubtype AnimalType
###### 1  A671945 Hambone 12/02/2014 18:22 Return_to_owner                       Dog
###### 2  A656520   Emily 13/10/2013 12:44      Euthanasia      Suffering        Cat
###### 3  A686464  Pearce 31/01/2015 12:28        Adoption         Foster        Dog
###### 4  A683430         11/07/2014 19:09        Transfer        Partner        Cat
###### 5  A667013         15/11/2013 12:52        Transfer        Partner        Dog
###### 6  A677334    Elsa 25/04/2014 13:04        Transfer        Partner        Dog
###### SexuponOutcome AgeuponOutcome                             Breed       Color
###### 1  Neutered Male         1 year             Shetland Sheepdog Mix Brown/White
###### 2  Spayed Female         1 year            Domestic Shorthair Mix Cream Tabby
###### 3  Neutered Male        2 years                      Pit Bull Mix  Blue/White
###### 4    Intact Male        3 weeks            Domestic Shorthair Mix  Blue Cream
###### 5  Neutered Male        2 years       Lhasa Apso/Miniature Poodle         Tan
###### 6  Intact Female        1 month Cairn Terrier/Chihuahua Shorthair   Black/Tan

str(aso)
###### 'data.frame':	26729 obs. of  10 variables:
###### $ AnimalID      : chr  "A671945" "A656520" "A686464" "A683430" ...
###### $ Name          : chr  "Hambone" "Emily" "Pearce" "" ...
###### $ DateTime      : chr  "12/02/2014 18:22" "13/10/2013 12:44" "31/01/2015 12:28" "11/07/2014 19:09" ...
###### $ OutcomeType   : chr  "Return_to_owner" "Euthanasia" "Adoption" "Transfer" ...
###### $ OutcomeSubtype: chr  "" "Suffering" "Foster" "Partner" ...
###### $ AnimalType    : chr  "Dog" "Cat" "Dog" "Cat" ...
###### $ SexuponOutcome: chr  "Neutered Male" "Spayed Female" "Neutered Male" "Intact Male" ...
###### $ AgeuponOutcome: chr  "1 year" "1 year" "2 years" "3 weeks" ...
###### $ Breed         : chr  "Shetland Sheepdog Mix" "Domestic Shorthair Mix" "Pit Bull Mix" "Domestic Shorthair Mix" ...
###### $ Color         : chr  "Brown/White" "Cream Tabby" "Blue/White" "Blue Cream" ... 

##### count blanks in each column
sum((nchar(aso$AnimalID)       == 0) == TRUE)
sum((nchar(aso$Name)           == 0) == TRUE)    ###### 7691  (curate to not named)
sum((nchar(aso$DateTime)       == 0) == TRUE)
sum((nchar(aso$OutcomeType)    == 0) == TRUE)
sum((nchar(aso$OutcomeSubtype) == 0) == TRUE)    ###### 13612 (not used)
sum((nchar(aso$AnimalType)     == 0) == TRUE)
sum((nchar(aso$SexuponOutcome) == 0) == TRUE)    ###### 1     (drop)
sum((nchar(aso$AgeuponOutcome) == 0) == TRUE)    ###### 18    (drop)
sum((nchar(aso$Breed)          == 0) == TRUE)
sum((nchar(aso$Color)          == 0) == TRUE)

##### Drop rows with missing values for attributes to be used for analysis
aso <- subset(aso, (nchar(aso$SexuponOutcome) != 0))
aso <- subset(aso, (nchar(aso$AgeuponOutcome) != 0))

##### identify distinct values
factor(aso$OutcomeType)[1:10]
###### [1] Return_to_owner Euthanasia      Adoption        Transfer        Transfer       
###### [6] Transfer        Transfer        Transfer        Adoption        Adoption       
###### Levels: Adoption Died Euthanasia Return_to_owner Transfer
factor(aso$AnimalType)[1:10]
###### [1] Dog Cat Dog Cat Dog Dog Cat Cat Dog Dog
###### Levels: Cat Dog
factor(aso$SexuponOutcome)[1:10]
###### [1] Neutered Male Spayed Female Neutered Male Intact Male   Neutered Male
###### [6] Intact Female Intact Male   Unknown       Spayed Female Spayed Female
###### Levels:  Intact Female Intact Male Neutered Male Spayed Female Unknown
factor(aso$AgeuponOutcome)[1:10]
###### [1] 1 year   1 year   2 years  3 weeks  2 years  1 month  3 weeks  3 weeks 
###### [9] 5 months 1 year  
###### 45 Levels:  0 years 1 day 1 month 1 week 1 weeks 1 year 10 months ... 9 years
factor(aso$Breed)[1:10]
###### [1] Shetland Sheepdog Mix             Domestic Shorthair Mix           
###### [3] Pit Bull Mix                      Domestic Shorthair Mix           
###### [5] Lhasa Apso/Miniature Poodle       Cairn Terrier/Chihuahua Shorthair
###### [7] Domestic Shorthair Mix            Domestic Shorthair Mix           
###### [9] American Pit Bull Terrier Mix     Cairn Terrier                    
###### 1380 Levels: Abyssinian Mix Affenpinscher Mix Afghan Hound Mix ... Yorkshire Terrier/Toy Poodle
factor(aso$Color)[1:10]
###### [1] Brown/White Cream Tabby Blue/White  Blue Cream  Tan         Black/Tan  
###### [7] Blue Tabby  Brown Tabby Red/White   White      
###### 366 Levels: Agouti Agouti/Brown Tabby Apricot Apricot/Brown Apricot/White ... Yellow/Yellow

##### count of SexuponOUtcome with value 'unknown'
sum((aso$SexuponOutcome == 'Unknown') == TRUE)   ###### 1089 

##### Drop rows with 'Unknown' for SexuponOutcome
aso <- subset(aso, (aso$SexuponOutcome != 'Unknown'))

##### ensure no unknown values for SexuponOutcome 
factor(aso$SexuponOutcome)[1:10]
###### [1] Neutered Male Spayed Female Neutered Male Intact Male   Neutered Male
###### [6] Intact Female Intact Male   Spayed Female Spayed Female Spayed Female
###### Levels: Intact Female Intact Male Neutered Male Spayed Female

######--------------------------------------------------------------------#

##### Clean and Curate data

##### Named: yes, no
aso$Named[nchar(aso$Name) != 0] <- 'yes'
aso$Named[nchar(aso$Name) == 0] <- 'no'


num.Named     = sum(aso$Named=='yes')        ###### 19000
num.NotNamed  = sum(aso$Named=='no')         ###### 6621
num.TotNamed  = num.Named  + num.NotNamed    ###### 25621
prob.Named    = num.Named  / num.TotNamed    ###### 0.7415792
prob.NotNamed = num.NotNamed / num.TotNamed  ###### 0.2584208

##### Adopted: 1-yes, 0-no
aso$Adopted <- ifelse(grepl('Adoption', aso$OutcomeType), 1, 0)
                     
num.Adopted     = sum(aso$Adopted==1)       ###### 10769
num.NotAdopted  = sum(aso$Adopted==0)        ###### 14852
num.TotAdopted  = num.Adopted  + num.NotAdopted ###### 25621
prob.Adopted    = num.Adopted  / (num.Adopted  + num.NotAdopted)   ###### 0.4203193
prob.NotAdopted = num.NotAdopted / (num.Adopted  + num.NotAdopted) ###### 0.5796807

##### Sex: Male, Female
aso$Sex <- ifelse(grepl('Male', aso$SexuponOutcome), 'Male', 'Female')
                   
num.Male     = sum(aso$Sex == 'Male')       ###### 13298
num.Female   = sum(aso$Sex == 'Female')     ###### 12323
num.TotSex   = num.Male + num.Female        ###### 25621
prob.Male    = num.Male  / num.TotSex       ###### 0.5190274
prob.Female  = num.Female / num.TotSex      ###### 0.4809726


##### Intact: Intact, Sterile, 2-unknown
aso$Intact <- ifelse(grepl('Intact', aso$SexuponOutcome), 'Intact', 'Sterile')
                    
num.Intact     = sum(aso$Intact == 'Intact')         ###### 7023
num.NotIntact  = sum(aso$Intact == 'Sterile')        ###### 18598
num.TotIntact  = num.Intact  + num.NotIntact       ###### 25621
prob.Intact    = num.Intact  / num.TotIntact       ###### 0.2741111
prob.NotIntact   = num.NotIntact / num.TotIntact   ###### 0.7258889

##### PureBred: yes, no
aso$PureBred <- ifelse(grepl('Mix', aso$Breed), 'no', 'yes')

num.PureBred     = sum(aso$PureBred == 'yes')         ###### 4407
num.NotPureBred  = sum(aso$PureBred=='no')            ###### 22214
num.TotPureBred  = num.PureBred  + num.NotPureBred    ###### 25621
prob.PureBred    = num.PureBred  / num.TotPureBred     ###### 0.1720073
prob.NotPureBred = num.NotPureBred / num.TotPureBred   ###### 0.8279927

##### Age - Convert to common scales of days
##### Get the time value:
aso$timeval <- sapply(aso$AgeuponOutcome,  
                      function(x) strsplit(x, split = ' ')[[1]][1])
##### Get the unit of time:
aso$timeunit <- sapply(aso$AgeuponOutcome,  
                       function(x) strsplit(x, split = ' ')[[1]][2])
##### Remove 's' from end of time unit
aso$timeunit <- gsub('s', '', aso$timeunit)

##### convert to required data type
aso$timeval  <- as.numeric(aso$timeval)
aso$timeunit <- as.factor(aso$timeunit)

##### Make a multiplier vector to convert to common scale of months
multiplier <- ifelse(aso$timeunit == 'day', 1/30,
                     ifelse(aso$timeunit == 'week', 1/4,
                            ifelse(aso$timeunit == 'month', 1, 
                                   ifelse(aso$timeunit == 'year', 12, NA))))

##### Convert age to common scale of months
aso$AgeinMths <- aso$timeval * multiplier

##### do summary
summary(aso$AgeinMths)
###### Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
###### 0.00    2.00   12.00   26.13   36.00  240.00      18 

##### Replace missing values with mean
mean.age <- mean(aso$AgeinMths,na.rm=TRUE)
aso$AgeinMths[is.na(aso$AgeinMths)] <- mean.age

##### do summary again 
summary(aso$AgeinMths)
###### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###### 0.00    3.00   12.00   26.93   36.00  240.00 

##### Group age into lifestage
aso$LifeStage <- ifelse(aso$AgeinMths <= 3, 'Baby',
                     ifelse((aso$AgeinMths > 3 & aso$AgeinMths <= 12), 'Adolescent',
                            ifelse((aso$AgeinMths > 12 & aso$AgeinMths <= 96), 'Adult', 'Senior'))) 
                                   
##### count in each lifestage category
sum((aso$LifeStage=='Baby')==TRUE)        ###### 7487
sum((aso$LifeStage=='Adolescent')==TRUE)  ###### 7423
sum((aso$LifeStage=='Adult')==TRUE)       ###### 9233
sum((aso$LifeStage=='Senior')==TRUE)      ###### 1478

##### Convert LifeStage groups to binary attributes
aso$Baby <- ifelse(aso$LifeStage == 'Baby', 'yes', 'no')
aso$Adolescent <- ifelse(aso$LifeStage == 'Adolescent', 'yes', 'no')
aso$Adult <- ifelse(aso$LifeStage == 'Adult', 'yes', 'no')
aso$Senior <- ifelse(aso$LifeStage == 'Senior', 'yes', 'no')

##### Determine best representation of Age
##### Model on LifeStage groups only
model <- glm(Adopted ~ Baby + Adolescent + Adult + Senior, 
             family=binomial(link='logit'), data=aso)

##### Results of model
summary(model,corr=T)
###### Call:
######   glm(formula = Adopted ~ Baby + Adolescent + Adult + Senior, family = binomial(link = "logit"), 
######       data = aso)
###### 
###### Deviance Residuals: 
######   Min       1Q   Median       3Q      Max  
###### -1.1320  -1.1015  -0.8926   1.2553   1.7475  
###### 
###### Coefficients: (1 not defined because of singularities)
######                  Estimate Std. Error z value Pr(>|z|)    
###### (Intercept)     -1.28197    0.06289 -20.384   <2e-16 ***
######   Babyyes        1.17414    0.06665  17.615   <2e-16 ***
######  Adolescentyes   1.10070    0.06696  16.439   <2e-16 ***
######   Adultyes       0.56732    0.06662   8.516   <2e-16 ***
######   Senioryes           NA         NA      NA       NA    
###### ---
######   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
###### 
###### (Dispersion parameter for binomial family taken to be 1)
###### 
###### Null deviance: 36040  on 26728  degrees of freedom
###### Residual deviance: 35337  on 26725  degrees of freedom
###### AIC: 35345
###### 
###### Number of Fisher Scoring iterations: 4
###### 
###### Correlation of Coefficients:
######   (Intercept) Babyyes Adolescentyes
###### Babyyes       -0.94                            
###### Adolescentyes -0.94        0.89                
###### Adultyes      -0.94        0.89    0.89

##### Interpretation of Results
##### p-vals
##### - Small p-vals indicate a statistically significance to adoption.  
##### - The smaller the p-val, the stronger the association.
##### - statiscally significant is:
#####   Baby, Adolescent
##### - less statistically significant is:
#####   Adult
##### - statistically insignificant is:
#####   Senior
#####
##### Coefficients
##### - Logit model is:
#####      logit(p) = ln(odds) = ln(p/(1-p)) = 
#####        -1.28197 + (1.17414*Baby) + (1.10070*Adolescent) + 
#####        (-0.56732*Adult) where p = log[p/(1-p)]
##### - A positive coefficient means that if all other variables are equal,
#####   the attribute of value indicated is more likely to be adopted.  
##### - The larger the value, the more likely.
##### - eg. being baby and adolescent increases the log odds 
#####       by 1.17 and 1.10 respectively of being adopted whereas
#####       being an adult decreases the log odds by 0.56 of being adopted.
#####  

anova(model, test='Chisq')
###### Analysis of Deviance Table
###### 
###### Model: binomial, link: logit
###### 
###### Response: Adopted
###### 
###### Terms added sequentially (first to last)
###### 
###### 
######            Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
###### NULL                       25620      34865              
###### Baby        1   425.64     25619      34439 < 2.2e-16 ***
###### Adolescent  1   415.90     25618      34023 < 2.2e-16 ***
###### Adult       1    82.85     25617      33940 < 2.2e-16 ***
###### Senior      0     0.00     25617      33940              
###### ---
######   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##### Interpretation: Analysis of deviance table 
##### - Deviance col is diff between models starting with null, then
#####   adding each variable. The larger the deviance, the better. 
##### - Deviances approx chisquare distributed with stated degrees of freedom.
##### - Variables Baby and Adolescent have significant effect on model
##### - Adult and Senior are insignificant

# Simplify model by making one attribute to indicate Child (baby, adolescent)
# or not (adult or senior)
aso$Child <- ifelse((aso$AgeinMths <= 3 & aso$AgeinMths <= 12), 'yes', 'no')

######--------------------------------------------------------------------#
######--------------------------------------------------------------------#

## Logistic Modelling dataset
# aso2 <- subset(aso, select=c(AnimalID, Named, Adopted, AnimalType,
#               Sex, Intact, PureBred, AgeinMths, LifeStage, Child))

# Export cleaned aso2 file to csv file for further analysis
# write.csv(aso2, file='aso2.csv')

# from saved data of previous 2 statements (commented out after 1st run) 
aso2 <- read.csv("aso2.csv", header = TRUE, stringsAsFactors = FALSE)

######--------------------------------------------------------------------#
str(aso2)
###### 'data.frame':	25621 obs. of  10 variables:
######   $ AnimalID  : chr  "A671945" "A656520" "A686464" "A683430" ...
###### $ Named     : chr  "yes" "yes" "yes" "no" ...
###### $ Adopted   : num  0 0 1 0 0 0 0 1 1 1 ...
###### $ AnimalType: chr  "Dog" "Cat" "Dog" "Cat" ...
###### $ Sex       : chr  "Male" "Female" "Male" "Male" ...
###### $ Intact    : chr  "Sterile" "Sterile" "Sterile" "Intact" ...
###### $ PureBred  : chr  "no" "no" "no" "no" ...
###### $ AgeinMths : num  12 12 24 0.75 24 1 0.75 5 12 24 ...
###### $ LifeStage : chr  "Adolescent" "Adolescent" "Adult" "Baby" ...
###### $ Child     : chr  "no" "no" "no" "yes" ...

# identify unique values
factor(aso$Adopted)[1:10]
###### [1] 0 0 1 0 0 0 0 1 1 1
###### Levels: 0 1
 factor(aso$AnimalType)[1:10]
###### [1] Dog Cat Dog Cat Dog Dog Cat Dog Dog Dog
###### Levels: Cat Dog
factor(aso$Sex)[1:10]
###### [1] Male   Female Male   Male   Male   Female Male   Female Female Female
###### Levels: Female Male
factor(aso$Intact)[1:10]
###### [1] Sterile Sterile Sterile Intact  Sterile Intact  Intact  Sterile Sterile Sterile
###### Levels: Intact Sterile
factor(aso$PureBred)[1:10]
###### [1] no  no  no  no  yes yes no  no  yes no 
###### Levels: no yes
factor(aso$LifeStage)[1:10]
###### [1] Adolescent Adolescent Adult      Baby       Adult      Baby       Baby      
###### [8] Adolescent Adolescent Adult  
factor(aso$Child)[1:10]
###### [1] no  no  no  yes no  yes yes no  no  no 
###### Levels: no yes

# Check to ensure no missing values
sum(nchar(aso2$AnimalID) == 0)    
sum(nchar(aso2$Named) == 0)       
summary(aso2$Adopted)  
###### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###### 0.0000  0.0000  0.0000  0.4029  1.0000  1.0000 
sum(nchar(aso2$AnimalType) == 0)  
sum(nchar(aso2$sex) == 0)         
sum(nchar(aso2$Intact) == 0)      
sum(nchar(aso2$PureBred) == 0)    
summary(aso2$AgeinMths)
###### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###### 0.00    2.00   12.00   26.13   36.00  240.00 
summary(aso2$LifeStage)
###### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
###### 1.000   1.000   2.000   2.155   3.000   4.000 
sum(nchar(aso2$Child) == 0)         # 0

######--------------------------------------------------------------------#

##### Basic Visualizations
##### Animal Adoption 
barplot(table(aso2$Adopted), main='Animal Adoption',
                             names= c('Adopted', 'Not Adopted'),
                             col=c('blue','red'))

##### Adoption by Animal Type
counts <- table(aso2$Adopted, aso2$AnimalType)
barplot(counts, xlab = "Animal Type", ylab = "Number Adopted", 
        main = "Adopted or Not Adopted by Dogs and Cats",
        col=c('blue','red'),
        legend = c('Adopted','Not Adopted'))

##### Named 
barplot(table(aso2$Named), main='Animal Named',
        names= c('Not Named', 'Named'),
        col=c('brown','orange'))

##### Sex 
barplot(table(aso2$Sex), main='Animal Sex',
        names= c('Female', 'Male'),
        col=c('pink', 'purple'))

##### Intact
barplot(table(aso2$Intact), main='Animal Intact',
        names= c('Intact', 'Sterile'),
        col=c('dark red', 'dark blue'))

##### PureBred
barplot(table(aso2$PureBred), main='Animal PureBred',
        names= c('Mix', 'PureBred'),
        col=c('grey', 'black'))

##### Animal age 
hist(aso2$AgeinMths, main='Animal Age', xlab = 'Age in Months', 
     col='light green')

##### Age in Months Density
plot(density(aso2$AgeinMths, na.rm = TRUE))

##### Animal LifeStage
aso2$LifeStage_fac <- factor(aso2$LifeStage, 
                             levels = c('Baby','Adolescent','Adult','Senior'))
tt <- table(aso2$LifeStage_fac)
tt
###### Baby Adolescent      Adult     Senior 
###### 7487       7423       9233       1478 

barplot(tt, main='Animal LifeStage', 
        col=c('red','blue','green','yellow'))

######--------------------------------------------------------------------#
######--------------------------------------------------------------------#

##### Logistic Modelling

##### Divide data into training (80%) and testing (20%) sets
rn_train    <- sample(nrow(aso2), floor(nrow(aso2)*0.8))
aso2_train <- aso2[rn_train,]
aso2_test  <- aso2[-rn_train,]

######--------------------------------------------------------------------#

##### Model Fitting 1
model1 <- glm(Adopted ~ Named + AnimalType + Sex + Intact + PureBred + Child, 
             family=binomial(link='logit'), data=aso2_train)

##### Results of model1
summary(model1,corr=T)
###### Call:
######   glm(formula = Adopted ~ Named + AnimalType + Sex + Intact + PureBred + 
######         Child, family = binomial(link = "logit"), data = aso2_train)
###### 
###### Deviance Residuals: 
######   Min       1Q   Median       3Q      Max  
###### -2.1803  -1.0786  -0.1603   1.1186   3.0770  
###### 
###### Coefficients:
######                 Estimate Std. Error z value Pr(>|z|)    
###### (Intercept)     -4.34849    0.08658 -50.225  < 2e-16 ***
######   Namedyes       0.37692    0.04977   7.573    3.65e-14 ***
######   AnimalTypeDog -0.15436    0.04079  -3.785    0.000154 ***
######   SexMale       -0.22230    0.03393  -6.552    5.69e-11 ***
######  IntactSterile   4.11139    0.07722  53.242  < 2e-16 ***
######   PureBredyes    0.10083    0.04440   2.271    0.023151 *  
######   Childyes       2.03869    0.05574  36.576  < 2e-16 ***
######   ---
######   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
######
###### (Dispersion parameter for binomial family taken to be 1)
###### 
###### Null deviance: 27886  on 20495  degrees of freedom
###### Residual deviance: 20492  on 20489  degrees of freedom
###### AIC: 20506
###### 
###### Number of Fisher Scoring iterations: 5
###### 
###### Correlation of Coefficients:
######   (Intercept) Namedyes AnimalTypeDog SexMale IntactSterile PureBredyes
###### Namedyes      -0.33                                                               
###### AnimalTypeDog -0.24       -0.05                                                   
###### SexMale       -0.16        0.00    -0.03                                          
###### IntactSterile -0.75       -0.19    -0.07         -0.04                            
###### PureBredyes    0.00       -0.01    -0.27         -0.02   -0.01                    
###### Childyes      -0.62        0.17     0.25         -0.03    0.43         -0.01

##### Interpretation of Results
##### p-vals
##### - Small p-vals indicate a statistically significance to adoption.  
##### - The smaller the p-val, the stronger the association.
##### - statiscally significant are:
#####   Intact, Child, Named 
##### - less statistically significant are:
#####   Sex, AnimalType, PureBred
##### - Order of significance based on z-val is:
#####   Intact, Child, Named, PureBred, AnimalType, Sex
#####
##### Coefficients
##### - Logit model is:
#####      logit(p) = ln(odds) = ln(p/(1-p)) = 
#####        -4.34849 + (0.37692*Named) + (-0.15436*AnimalType) + 
#####        (-0.22230*Sex) + (4.11139*Intact) + (0.10083*PureBred) +
#####        (2.03869*Child)  where p = log[p/(1-p)] 
##### - A positive coefficient means that if all other variables are equal,
#####   the attribute of value indicated id more likely to be adopted.  
##### - The larger the value, the more likely.
##### - eg. being sterile increases the log odds by 4.11 of being adopted
#####  
    
##### Deviance table analysis
anova(model1, test='Chisq')
# Analysis of Deviance Table
# 
# Model: binomial, link: logit
# 
# Response: Adopted
# 
# Terms added sequentially (first to last)
# 
# 
#            Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                       20495      27886              
# Named       1    854.5     20494      27032 < 2.2e-16 ***
# AnimalType  1     66.5     20493      26965 3.431e-16 ***
# Sex         1     23.2     20492      26942 1.473e-06 ***
# Intact      1   4663.7     20491      22279 < 2.2e-16 ***
# PureBred    1     10.5     20490      22268  0.001203 ** 
# Child       1   1776.0     20489      20492 < 2.2e-16 ***
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##### Interpretation: Analysis of deviance table 
##### - Deviance col is diff between models starting with null, then
#####   adding each variable. The bigger the gap, the better. 
##### - Deviances approx chisquare distributed with stated degrees of freedom.
##### - Variables Intact, Child and Named have significant effect on model
##### - AnimalType, Sex and PureBred are insignificant
##### - deviances confirms conclusions from analysis of model1
##### - order of significance based on deviance is:
#####   Intact, Child, Named, AnimalType, Sex, PureBred

##### Assessing Predictive Ability of Model1
##### Evaluate model1 in adoption prediction on a new set of data, aso2_test.
##### Use predict function with type='response'. 
##### R outputs probabilities in P(y=1/x) form.
##### Decision boundary is 0.5.  ie. if P(y=1/x) > 0.5, then y=1 (adopt), else y=0.
fit1 <- predict(model1, newdata=aso2_test, type='response')
fit1 <- ifelse(fit1 > 0.5, 1, 0)
ClassifyErr <- mean(fit1 != aso2_test$Adopted)
print(paste('Accuracy ',1-ClassifyErr))
###### "Accuracy  0.710243902439024"

######--------------------------------------------------------------------#

##### Run model again with Named + Intact + Child only

##### Model Fitting 2
model2 <- glm(Adopted ~ Named + Intact + Child, 
              family=binomial(link='logit'), data=aso2_train)

##### Results of model2
summary(model2,corr=T)
###### Call:
######   glm(formula = Adopted ~ Named + Intact + Child, family = binomial(link = "logit"), 
######       data = aso2_train)
###### 
###### Deviance Residuals: 
######   Min       1Q   Median       3Q      Max  
###### -2.0675  -1.1458  -0.1759   1.2093   3.0141  
###### 
###### Coefficients:
######   Estimate Std. Error z value Pr(>|z|)    
###### (Intercept)     -4.53157    0.08241 -54.985  < 2e-16 ***
######   Namedyes       0.37099    0.04970   7.465 8.33e-14 ***
######   IntactSterile  4.08576    0.07691  53.123  < 2e-16 ***
######   Childyes       2.08666    0.05380  38.787  < 2e-16 ***
######   ---
######   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
###### 
###### (Dispersion parameter for binomial family taken to be 1)
###### 
###### Null deviance: 27886  on 20495  degrees of freedom
###### Residual deviance: 20552  on 20492  degrees of freedom
###### AIC: 20560
###### 
###### Number of Fisher Scoring iterations: 5
###### 
###### Correlation of Coefficients:
######   (Intercept) Namedyes IntactSterile
###### Namedyes      -0.36                             
###### IntactSterile -0.82       -0.19                 
###### Childyes      -0.61        0.19     0.46        

##### Interpretation of Results
##### p-vals
##### - Small p-vals indicate a statistically significant association to adoption.  
##### - The smaller the p-val, the stronger the association.
##### - statiscally significant is:
#####   Intact, Child
##### - less statistically significant is:
#####   Named
#####
##### Coefficients
##### - Logit model (Best Fit Model) is:
#####      logit(p) = ln(odds) = ln(p/(1-p)) = 
#####        -4.53157 +0.37099*Named + 4.08576*Intact + 2.08666*Child
#####        where p = log[p/(1-p)]
##### - A positive coefficient means that if all other variables are equal,
#####   the attribute of value indicated id more likely to be adopted.  
##### - The larger the value, the more likely.
##### - eg. being sterile increases the log odds by 4.09 of being adopted,
#####       being a child ie <= 12 months old increases the log odds by 2.09

##### Deviance table analysis using ANOVA
anova(model2, test='Chisq')
###### Analysis of Deviance Table
###### 
###### Model: binomial, link: logit
###### 
###### Response: Adopted
###### 
###### Terms added sequentially (first to last)
###### 
###### 
###### Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
###### NULL                   20495      27886              
###### Named   1    854.5     20494      27032 < 2.2e-16 ***
###### Intact  1   4385.8     20493      22646 < 2.2e-16 ***
###### Child   1   2093.9     20492      20552 < 2.2e-16 ***
######   ---
###### Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##### Interpretation: Analysis of deviance table 
#####   adding each variable. The larger the deviance, the better.
##### - Deviances approx chisquare distributed with stated degrees of freedom.
##### - Deviances confirms conclusions from analysis of model2:
##### - statiscally significant on adoption are:
#####   Intact, Child
##### - less statistically significant is:
#####   Named 

######--------------------------------------------------------------------#
  
##### Assessing Predictive Ability of Model2

##### Evaluate model2 in adoption prediction on a new set of data, aso2_test.
##### Use predict function with type='response'. 
##### R outputs probabilities in P(y=1/x) form.
##### Decision boundary is 0.5.  ie. if P(y=1/x) > 0.5, then y=1 (adopt), else y=0.
fit2 <- predict(model2, newdata=aso2_test, type='response')
fit2 <- ifelse(fit2 > 0.5, 1, 0)
ClassifyErr <- mean(fit2 != aso2_test$Adopted)
print(paste('Accuracy ',1-ClassifyErr))
###### "Accuracy  0.704"

##### profile plot for adoption
library(MASS)
plot(profile(model3))

##### Odds ratio estimates
exp(cbind(OR=coef(model3), confint(model3)))
###### Waiting for profiling to be done...
###### OR       2.5 %      97.5 %
######   (Intercept)    0.01198538  0.01015064  0.01409824
###### IntactSterile 60.21687971 51.87513654 70.16788013
###### Childyes       8.07881900  7.27731443  8.98761098
###### Namedyes       1.44754061  1.31312812  1.59596591
###### SexMale        0.79864309  0.74729492  0.85347377

##### Performance measurements for binary classification 
##### 1. Plot ROC (Receiver Operating Characteristics) curve
##### 2. Calculate AUC (area under curve).  
##### The closer AUC is to 1 (1 being perfect), the better the 
##### predictive ability of the model.
library(ROCR)
p <- predict(model2, newdata=aso2_test, type='response')
pr <- prediction(p, aso2_test$Adopted)
prf <- performance(pr, measure='tpr', x.measure='fpr')
plot(prf)

auc <- performance(pr, measure='auc')
auc <- auc@y.values[[1]]
auc






