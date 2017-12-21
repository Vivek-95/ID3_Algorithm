require(dplyr)
require(readr)
require(car)
require(RODBC)

#Loading it into MySQL database. Can be ignored by loading the dataset directly into R. 
#odbcChannel <- odbcConnect("DataMining")
#train <- sqlQuery(odbcChannel,"select * from house")
#close(odbcChannel)
train <- read_csv("train.csv")

View(train)

#Get rid of the first attribute ID
train$Id <- NULL

#Find out the log transform of the saleprice variable
train$SalePrice <- log(train$SalePrice)

#Round it off to the nearest decimal
train$SalePrice <- round(train$SalePrice,1)

hist(train$SalePrice)

#Boxplot of Saleprice 
boxplot(train$SalePrice,main="Boxplot of logarithmic transformation of saleprice")
#Boxplot shows that the values lying outside of the range between 11 -13 are outliers.

#Next we shall check whether the values lying in the range of 11 to 13 are normally distributed.
hist(train$SalePrice[train$SalePrice>=11 & train$SalePrice <=13],main="Histogram of values in the range of 11 to 13"
     ,xlab="Log transformation of Sale Price")

#Plotting density function
plot(density(train$SalePrice[train$SalePrice >=11 & train$SalePrice <= 13]),main="Density plot")

#Q-Q plot-Comparing the distribution against a standard normal distribution.
qqnorm(train$SalePrice[train$SalePrice >=11 & train$SalePrice <=13])
qqline(train$SalePrice[train$SalePrice >=11 & train$SalePrice <= 13])

#The three plots above show that the values from 11 to 13 are almost normally distributed. We shall bin the values
#according to their normal distributions. The mean is at 12 and the standard deviation is almost 0.4. We shall bin them 
#as follows : <=11.2;>11.2 to 11.6; >11.6 to 12; >12 to 12.4; >12.4 to 12.8; >12.8. So totally there are 6 bins.
#Shall represent them with factor levels 1,2,3,4,5 and 6 respectively.
train$SalePrice <- as.numeric(cut(train$SalePrice,c(10.5,11.2,11.6,12,12.4,12.8,13.6),labels=c(1:6),right=FALSE))


#MS SubClass Attribute
#Finding out correlation between current attribute and class
cor(as.numeric(train$MSSubClass),train$SalePrice)
# -0.075
summary(train$MSSubClass)
train$MSSubClass <- factor(train$MSSubClass)
table(train$MSSubClass)
#150 level not present, if present in testing set remove.

#Finding out the mean of the Saleprice factor level for every level in MSSubClass.
by(train$SalePrice,train$MSSubClass,mean)
#Too many factor levels, shall remove to prevent it from affecting information gain.
train$MSSubClass <- NULL

#MSZoning Attribute
#I and RP levels not present.
summary(as.factor(train$MSZoning))
train$MSZoning <- as.factor(train$MSZoning)
#finding correlation with MS Subclass attribute
#0.036 not a strong co-relation with MSSubclass
cor(as.numeric(train$MSZoning),train$SalePrice)
#-0.19 strong correlation with SalePrice

by(train$SalePrice,train$MSZoning,mean)
#Why is the mean of commercial lots low?
#View(train[train$MSZoning=="C (all)",])

#LotFrontage Property
summary(train$LotFrontage)
train$LotFrontage[is.na(train$LotFrontage)] <- 0
#Continuous Attribute, depicts area of street present in front of the house. Can be removed
train$LotFrontage <- NULL

#LotArea Attribute
summary(train$LotArea)
#Take log transform of lot area property since some of the values are highly skewed.
#train$LotArea <- log

#Street Attribute
train$Street <- factor(train$Street)
summary(train$Street)
#Data has only 6 instances of Gravel factor with the rest being pavement. Hence attribute can be removed
train$Street <- NULL

#Alley Attribute
train$Alley <- factor(train$Alley)
summary(train$Alley)
#Lots of missing attributes(1369). Hence attribute is removed.
train$Alley <- NULL

#LotShape Attribute
train$LotShape <- factor(train$LotShape)
summary(train$LotShape)
#Finding out the mean of SalePrice in every factor level of Lot Shape.
train %>% group_by(LotShape) %>% summarise(mean=mean(SalePrice))
#Shall Recode the factor levels in the LotShape attribute as either irregular or regular.
train$LotShape <- Recode(train$LotShape,'c("IR1","IR2","IR3")="IR"')

#LandContour Attribute
summary(train$LandContour)
train$LandContour <- factor(train$LandContour)
#90 percent of the values in the land contour attribute seem to have the same value(Level) while the remaining is divided
#amongst three factor levels.
cor(as.numeric(train$LandContour),train$SalePrice)
#Does not show a strong correlation with the SalePrice variable. Hence best to get rid of it.
train$LandContour <- NULL 

#Utilities Attribute
train$Utilities <- factor(train$Utilities)
summary(train$Utilities)
#Highly dominated by All public utilities( only 1 observation contains NoSewa(electricity and gas only) value).
#Makes sense to get rid of it.
train$Utilities <- NULL

#LotConfig Attribute
train$LotConfig <- factor(train$LotConfig)
summary(train$LotConfig)


#Land Slope Attribute
train$LandSlope <- factor(train$LandSlope)
summary(train$LandSlope)
cor(train$SalePrice,as.numeric(train$LandSlope))
#Not highly correlated with the SalePrice. Makes sense to remove the attribute.
train$LandSlope <- NULL

#Neighbourhood Atrtibute
train$Neighborhood <- factor(train$Neighborhood)

summary(train$Neighborhood)

#Divide neighbourhood into posh,normal,below par. Look at count and mean of each neighbourhood.
View(train %>% group_by(Neighborhood) %>% summarise(count=n(),mean=mean(SalePrice)))

#We shall bin them as follows, the neighbourhoods that have a SalePrice of >4 and have a decent count value(say >10), 
#can be classified as posh,similarly those which lie in the range between >3 and <=4 can be classified as normal and 
#those below <=3 can be classifed as sub par.

#If the count is not ten then we can't be sure of the neighbourhood belonging to the particular class. Hence to
#be on the safer side we shall downgrade it to the previous class.
neigh_partition <- train %>% group_by(Neighborhood) %>% summarise(count=n(),mean=mean(SalePrice)) 

posh <- neigh_partition %>% filter(count >=10 & mean >=4)
# Gives the posh neighbourhoods along with their count and mean values

standard <- neigh_partition %>% filter(count>=10 & mean >=3 & mean< 4)
# Gives the standard neighbourhoods along with their count and mean values

standard <- rbind(standard,neigh_partition %>% filter(count<10 & mean >4))
# The houses of the neighbourhoods whose count was less than 10 and their mean values were greater than 4 are 
#placed in the class of standard neighbourhoods. In this dataset there are no such houses.

subpar <- neigh_partition %>% filter(count >=10 & mean <3)
# Gives the subpar neighbourhoods along with their count and mean values

subpar <- rbind(subpar,neigh_partition %>% filter(count<10 & mean <4))
# The houses of the neighbourhoods whose count was less than 10 and their mean values are lesser than 3 are 
#placed in the class of standard neighbourhoods.

#Binning the neighbourhoods into posh,standard and subpar neighbourhoods
train$Neighborhood <- Recode(train$Neighborhood, 'unique(posh$Neighborhood)="posh"')
train$Neighborhood <- Recode(train$Neighborhood, 'unique(standard$Neighborhood)="normal"')
train$Neighborhood <- Recode(train$Neighborhood, 'unique(subpar$Neighborhood)="subpar"')


#condiion 1 Attribute
train$Condition1 <- factor(train$Condition1)
summary(train$Condition1)
#We shall get rid of this attribute as the conditions given do not make a strong influence on the price of the house
train$Condition1 <- NULL

#Condition 2 Attribute
train$Condition2 <- factor(train$Condition2)
#Getting rid of this attribute for the same reason as that of the previous attribute.
train$Condition2 <- NULL


#Building Type Attribute
train$BldgType <- factor(train$BldgType)
summary(train$BldgType)


#House Style Attribute
train$HouseStyle <- factor(train$HouseStyle)


#overall Quality Attribute
train$OverallQual <- factor(train$OverallQual)
summary(train$OverallQual)

#OverallCond Attribute
train$OverallCond <- factor(train$OverallCond)
summary(train$OverallCond)
#Meaning of the attrbute is pretty much same as Overall Quality. Overall Quality has more information hence
#this attribute is gotten rid of.
train$OverallCond <- NULL


#Year Built Attribute
summary(train$YearBuilt)
#Binning the values.
train$YearBuilt <- as.numeric(cut(train$YearBuilt,breaks=c(1800,1900,1920,1940,1960,1980,2000,2011),labels=c(1:7),right=FALSE))
train$YearBuilt <- factor(train$YearBuilt)

#Year Remodeled ttribute
#We're keeping the year built attribute. This Attribute can be removed.
train$YearRemodAdd <- NULL

#Roof Style Attribute
train$RoofStyle <- factor(train$RoofStyle)
#Style of the roof should not affect the price of the house by too much.

train %>% group_by(RoofStyle) %>% summarise(mean=mean(SalePrice))
#mean values of the sales price for each roof style ranges between 3.2 to 4.5 . There are no extreme values, pretty 
#much close to the actual mean of the sales price. This proves that the attribute is not interesting enough.
train$RoofStyle <- NULL


#Roof MAterial Attribute
summary(train$RoofMatl)
#Dominated by Standard Composite Shingle values.
#Does not offer much information, can be removed.
train$RoofMatl <- NULL


#Exterior 1st attribute
train$Exterior1st <- factor(train$Exterior1st)
summary(train$Exterior1st)
#Too many factor level, shall remove it to prevent it from affecting the information gain.
train$Exterior1st <- NULL

#Exterior 2nd Attribute
#Remove attribute as Exterior 1 attribute offers enough information
train$Exterior2nd <- NULL

#Exterior Quality Attribute
train$ExterQual <- factor(train$ExterQual)

train %>% group_by(ExterQual) %>% summarise(count =n(),mean=mean(SalePrice))
#The mean of the saleprice are lined up according to the exterior quality of the house.We shall keep the attribute 
#unchanged

#Exterior Condition Attribute
train$ExterCond <- factor(train$ExterCond)

train %>% group_by(ExterCond) %>% summarise(count =n(),mean=mean(SalePrice))
#The SalePrice mean of Good exterior condition is lesser than that of the average exterior condition despite
#both of the levels having sizable counts.
#The counts for Exterior Quality attribute are more ditributed when compared to the exterir condition and 
#the SalePrice mean are ordered according to the levels of the exterior quality qttribute as well. There is no 
#need for this attribute.
train$ExterCond <- NULL

#Mason Veneer Type
#converting missing values to 0.
train$MasVnrType <- as.character(train$MasVnrType)
train$MasVnrType[is.na(train$MasVnrType)] <- 0

train$MasVnrType <- factor(train$MasVnrType)

train %>% group_by(MasVnrType) %>% summarise(count =n(),mean=mean(SalePrice))
#masvnrarea and masvnrtype have the same missing values. Hence it is most likely that there is no masonry
#veneer present
train$MasVnrType <- Recode(train$MasVnrType,'c("0") = "None"')

#Mason Veneer Area
summary(train$MasVnrArea)
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
#continuous attribute, not important.
train$MasVnrArea <- NULL


#foundation Attribute
train$Foundation <- factor(train$Foundation)

summary(train$Foundation)
#Keep the variable as it is.


#Basement Quality and Condition
#There are 37 missing values for each of the basement attributes. This means that there are 37 houses that do not 
#have basements.
#Recoded missing values as No
train$BsmtQual <- as.character(train$BsmtQual)
train$BsmtQual[is.na(train$BsmtQual)] <- "No"
train$BsmtQual <- factor(train$BsmtQual)
summary(train$BsmtQual)

train %>% group_by(as.factor(BsmtCond)) %>% summarise(count=n(),mean(SalePrice))

train %>% group_by(BsmtQual) %>% summarise(count=n(),mean(SalePrice))
#There are 1311 instances in the basement condition attribute that says they are average. The values in the basement
#quality attribute are more distributed. Hence remove basement condition.

train$BsmtCond <- NULL

#Basement Exposure Attribute
train$BsmtExposure[is.na(train$BsmtExposure)] <- "No"
train$BsmtExposure <- factor(train$BsmtExposure)
summary(train$BsmtExposure)
#Basement Quality provides sufficient information. There is no need for this attribute
train$BsmtExposure <- NULL

#Basement Finished Type 1  type 2 attributes
train$BsmtFinType1 <- as.character(train$BsmtFinType1)
summary(as.factor(train$BsmtFinType1))
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "No" 
train$BsmtFinType1 <- factor(train$BsmtFinType1)
#Houses of unfinished basements seem to have a higher mean values than all of the other levels except Good living
#quarters. This is because half of the unfinished basements are categorized as having good quality basements. Hence 
#the importantness of the Basement Quality attribute overshadows the effect of this attribute. Since Type 1 depicts
#more information than Type 2, Type 2 attribute has no usefulness.
train$BsmtFinType1 <- NULL
train$BsmtFinType2 <- NULL


#Basement Areas
#The total basement area is just the sum of all the other basement areas.Hence the other areas can be removed.
train$BsmtFinSF1 <- NULL
train$BsmtFinSF2 <- NULL
train$BsmtUnfSF <- NULL
summary(train$TotalBsmt)
#Notice that the maximum basement area is 6110. This might be thought of an outlier at first glance but furthur analysis
#proves that the value is most likely true. We use the Lot Area to verify that the value is true or not.
temp <- train
#Remove all instances in which basement do not exist in the temporary dataframe.
temp <- temp[temp$TotalBsmt>0,]
#find the mean ratio of lot area and basement area.
mean(temp$LotArea/temp$TotalBsmt)

train[train$TotalBsmt ==6110,]
#This value is almost equal to 10.2 . Now we compare this value for our so called basement are outlier value by
#applying the same set of operations. ie 6110/63887 = 10.45 which is almost same as the mean. Hence the value is 
#most likely not an outlier.

#Heating Attribute
summary(as.factor(train$Heating))
#Most of the values are gas.
train %>% group_by(Heating) %>% summarise(count=n(),mean(SalePrice))
#Houses that have gas have a higher mean saleprice than houses that don't. This attribute can be re-encoded as houses
#that have gas or do not have gas for heating.
train$Heating <- factor(train$Heating)
train$Heating <- Recode(train$Heating,'c("GasA","GasW")="Gas"')
train$Heating <- Recode(train$Heating,'c("Wall","OthW","Floor","Grav")="Other"')
# Renamed factor levels GasA and GasW as Gas; renamed factor levels Wall,OthW,Floor and Grav as Other

#Heating Quality Attribute
train$HeatingQC <- factor(train$HeatingQC)

train %>% group_by(HeatingQC) %>% summarise(count=n(),mean(SalePrice))
#Attribute is good enough to be used as it is.

#Central Air conditioning Attribute
summary(as.factor(train$CentralAir))
train$CentralAir <- factor(train$CentralAir)
train %>% group_by(CentralAir) %>% summarise(count=n(),mean=mean(SalePrice))
#It can be clearly seen that houses that have central AC have a much higher mean SalePRice value than houses that dont

#Electrical Attribute
summary(as.factor(train$Electrical))
train %>% group_by(Electrical) %>% summarise(count=n(),mean=mean(SalePrice))
#It is seen that the mean SalePrice value for Standard circuit breakers is much greater than the mean saleprice values
#of the rest of the levels. Also the mean of the missing value is 4 which is closer to the standard circuit breaker 
#level than any other levels. Thus this missing value can be plugged as the standard circuit breaker value. Then
#the attribute can be made binary as either having standard breaker level or not.
train$Electrical <- as.character(train$Electrical)
train$Electrical[is.na(train$Electrical)] <- "missing"
train$Electrical <- factor(train$Electrical)
#Recode the missing value as SBrkr
train$Electrical <- Recode(train$Electrical,'c("missing")="SBrkr"')
#Mutate the attribute into a binary atribute as either having the standard circuit board or not
train$Electrical <- factor(ifelse(train$Electrical=="SBrkr","SBrkr","Other"))


#Living Area Attribute
table(train[,21]+train[,22]+train$LowQualFinSF == train$GrLivArea)
#Hence the GrLiveArea is the total sum of 1stfloor,2ndfloor and low quality finished area. Hence we can get rid of 
#those attributes.
train$`1stFlrSF` <- NULL
train$`2ndFlrSF` <- NULL
train$LowQualFinSF <- NULL

#Creating a new total are attribute from lot area,basement area and living area
train$LotArea <- train$LotArea + train$TotalBsmt + train$GrLivArea
#Renaming lot area as total area
names(train)[2] <- "TotalArea"
train$TotalBsmt <- NULL
train$GrLivArea <- NULL

#Binning Total Area
train$TotalArea <- log(train$TotalArea)
train$TotalArea <- as.numeric(cut(train$TotalArea,c(7,8,9,10,11,12,13),labels=c(1:6),right=FALSE))
train$TotalArea <- factor(train$TotalArea)
#Basement Bathrooms
#Consider half a bathroom as half of a full bathroom.
train$BsmtFullBath <- train$BsmtFullBath + train$BsmtHalfBath*0.5
names(train)[names(train)=="BsmtFullBath"] <- "BasementBath"
train$BsmtHalfBath <- NULL

#Living floor bathrooms
#Do the same for living floor bathrooms
train$FullBath <- train$FullBath + train$HalfBath*0.5
names(train)[names(train)=="FullBath"] <- "LivingAreaBath"
train$HalfBath <- NULL

#Creation of a new attribute that stores the total number of bathrooms in the house(ie basement + living area)
train$LivingAreaBath <- train$LivingAreaBath + train$BasementBath
names(train)[names(train)=="LivingAreaBath"] <- "TotalBath"
train$TotalBath <- factor(train$TotalBath)
train$BasementBath <- NULL

#total rooms and bedrooms attributes
train %>% group_by(BedroomAbvGr) %>% summarise(count=n(),mean=mean(SalePrice))
#A linear increase with the price and bedroom is to be expected but that is not the case over here. 

train %>% group_by(TotRmsAbvGrd) %>% summarise(count=n(),mean=mean(SalePrice))
#Over here a linear increase is present. Since the total number of rooms include the bedrooms we can just take 
#this attribute and ignore the number of bedrooms.
train$TotRmsAbvGrd <- factor(train$TotRmsAbvGrd)
train$BedroomAbvGr <- NULL


#Functional Attribute
#Shall convert it into a binary attribute. Houses which either have typical functionality or have some minor errors
train$Functional <- factor(ifelse(train$Functional=="Typ","Typ","MinErr"))


#Kitchen Attribute
summary(as.factor(train$KitchenAbvGr))
train %>% group_by(KitchenAbvGr) %>% summarise(count=n(),mean=mean(SalePrice))
#Does not seem to give too much information apart from having a single kitchen is the ideal value.
train$KitchenAbvGr <- NULL


#Kitchen Quality Attribute
train %>% group_by(KitchenQual) %>% summarise(count=n(),mean=mean(SalePrice))
#It is good as it is.
train$KitchenQual <- factor(train$KitchenQual)


#Fireplaces Attribute
train %>% group_by(Fireplaces) %>% summarise(count=n(),mean=mean(SalePrice))
#Good as it is. Provides more information than the Fireplaces Quality attribute.
train$Fireplaces <- factor(train$Fireplaces)
train$FireplaceQu <- NULL


#Garage Type Attribute
#81 instances with no Garages are present 
train$GarageType <- as.character(train$GarageType)
train$GarageType[is.na(train$GarageType)] <- "None"
train$GarageType <- factor(train$GarageType)
summary(train$GarageType)
train %>% group_by(GarageType) %>% summarise(count=n(),mean=mean(SalePrice))


#Garage Finish Attribute
#81 instances with no Garages are present 
train$GarageFinish <-as.character(train$GarageFinish)
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
train$GarageFinish <- factor(train$GarageFinish)
summary(train$GarageFinish)
train %>% group_by(GarageFinish) %>% summarise(count=n(),mean=mean(SalePrice))


#Garage Year Built
table(is.na(train$GarageYrBlt))
#Endode missing instances as 0.
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
#The garage year built does not offer much information as all that is required is the usefulness of the garage
#(such as number of cars it can hold, type etc) which is denoted by the other garage attributes. Since the other
#garage attributes would store the latest functionality of the garage(ie after the garage gets remodeled), the 
#year at which the garage got remodeled is not necessary.
train$GarageYrBlt <- NULL

#Garage Cars Attribute
summary(as.factor(train$GarageCars))
train$GarageCars <- factor(train$GarageCars)
train %>% group_by(GarageCars) %>% summarise(count=n(),mean=mean(SalePrice))
#More the number of cars more is the mean of the house SalePrice.

#Garage  Area Attribute
plot(train$GarageArea)
summary(train$GarageArea)

train %>% group_by(GarageCars) %>% summarise(count=n(),mean=mean(GarageArea))
#More the number of cars greater is the area. Attribute is redundant. HEnce can be removed.
train$GarageArea <- NULL


#Garage Quality Attribute
summary(as.factor(train$GarageQual))
train$GarageQual <- as.character(train$GarageQual)
#Encoding missing values.
train$GarageQual[is.na(train$GarageQual)] <- "None"
train %>% group_by(GarageQual) %>% summarise(count=n(),mean(SalePrice))
#Even though excellent quality is supposed to denote the best, the mean of house saleprices seem to be higher for
#those that are in good condition. This is because this attribute just denotes the quality of the garage, rather the
#number of cars the garage can hold determines the effectiveness of this attribute. Since it is dependant on the 
#number of cars attribute it can be eliminated.
train %>% group_by(GarageQual,GarageCars) %>% summarise(count=n(),mean(SalePrice))
train$GarageQual <- NULL

#Garage Condition Attribute
#Same analysis as the previous attribute.
train %>% group_by(GarageCond,GarageCars) %>% summarise(count=n(),mean(SalePrice))
#Atrtibute can be eliminated
train$GarageCond <- NULL


#Paved Driveway atribute
summary(as.factor(train$PavedDrive))
#Most of the driveways are paved(1340). Attribute can be removed.
train$PavedDrive <- NULL

#Wood Deck  Area Attribute
summary(train$WoodDeckSF)
#We shall bin the wood deck in multiples of 100. ie 0-100;100-200;200-300;300-400;400-500;500-600;
#600-700;700-800;800-900;
train$WoodDeckSF <- cut(train$WoodDeckSF,c(0,100,200,300,400,500,600,700,800,900),labels=c(1:9),right=FALSE)
#Now it is seen that the wood deck areas over 100 sq ft seem to aprroximately have the same value.
#Can be converted into a binary attribute with wood sqft being over 100 or lesser than/equal to 100.
train$WoodDeckSF <- as.integer(train$WoodDeckSF)
train$WoodDeckSF <- factor(ifelse(train$WoodDeckSF>1,1,0))


#Open Porch, Enclosed porch , three season porch and screen porch Area
#Can add up the open and enclosed porch areas and create a new attribute representing the total area of porch.
#Then the attribute can be binned in multiples of 100.
train$OpenPorchSF <- train$OpenPorchSF + train$EnclosedPorch + train$`3SsnPorch` + train$ScreenPorch
train$OpenPorchSF <- cut(train$OpenPorchSF,c(0,100,200,300,400,500,600,700,800,900,1100),labels=c(1:10),right=FALSE)
#Analyze the bins
train %>% group_by(OpenPorchSF) %>% summarise(count=n(),mean(SalePrice))
#Saleprice mean of different porches of a sizable count all lie within one standard deviation of the SalePRice mean.
#Not much variability hence all the attributes can be removed.
train$OpenPorchSF <- NULL
train$EnclosedPorch <- NULL
train$`3SsnPorch` <- NULL
train$ScreenPorch <- NULL


#Pool Area
table(train$PoolArea)
#1453 houses do not have a pool. 
train %>% group_by(PoolArea) %>% summarise(count=n(),mean(SalePrice))
#The house that do have a pool have SalePrices greater than that of the SalePrice mean. It is safe to assume that
#since the house has a pool it probably has other features that would have increased the SalePrice of the house.
#Since there are only 7 houses that have a pool, it is better to get rid of it.
train$PoolArea <- NULL
train$PoolQC <- NULL

#Fence Attribute
table(train$Fence)
train %>% group_by(Fence) %>% summarise(count=n(),mean(SalePrice))
#Quality of the fence shouldn't really have a drastic impact on the saleprice of the house
train$Fence <- NULL

#Miscellaneous features
table(train$MiscFeature)
#count too less. Remove
train$MiscFeature <- NULL

#Miscellaneous Values
table(train$MiscVal)
#Count too less, remove.
train$MiscVal <- NULL

#Year Sold
table(train$YrSold)
train %>% group_by(YrSold) %>% summarise(count=n(),mean(SalePrice))
abc <- train %>% group_by(YrSold,MoSold) %>% filter(YrSold==2010) %>% summarise(count=n(),mean(SalePrice))
train$YrSold <- factor(train$YrSold)

#Month Sold
table(train$MoSold)
#train$MoSold <- factor(train$MoSold)
#train %>% group_by(MoSold) %>% summarise(count=n(),mean(SalePrice))
#The SALEPRICe mean of different months seems to be a standard deviation away from the actual Saleprice mean.
#Only thing that can be inferenced is that during the months from march to august a lot more houses are sold when
#compared to the rest of the months. Since it does not affect the SalePRice the attribute can be removed.
train$MoSold <- NULL



#SaleType Attribute
table(train$SaleType)
train %>% group_by(SaleType) %>% summarise(count=n(),mean(SalePrice))
#The mean SalePrice value for new homes that just got constructed and sold are much higher than the others. This is
#even backed up by a sizable count(122). Hence this can be encoded as a binary attribute indicating whether it was a 
#new home or not.
train$SaleType <- factor(train$SaleType)
train$SaleType <- factor(ifelse(train$SaleType == "New",1,0))

#Sale Condition Attribute
train %>% group_by(SaleCondition) %>% summarise(count=n(),mean(SalePrice))
#Since the partial completed homes are associated with new homes, they have a higher salesprice. Apart from that there
# is nothing interesting about the rest of the factor levels.
train$SaleCondition <- NULL



train2 <- data.frame(train)
#Recoding the levels in SalePrice as type1,type2...type6
train2$SalePrice <- as.numeric(train2$SalePrice)
train2$SalePrice <- Recode(train2$SalePrice,'1=c("Type1")')
train2$SalePrice <- Recode(train2$SalePrice,"c(2)=c('Type2')")
train2$SalePrice <- Recode(train2$SalePrice,"c(3)=c('Type3')")
train2$SalePrice <- Recode(train2$SalePrice,"c('4')=c('Type4')")
train2$SalePrice <- Recode(train2$SalePrice,"c('5')=c('Type5')")
train2$SalePrice <- Recode(train2$SalePrice,"c('6')=c('Type6')")
train2$SalePrice <- factor(train2$SalePrice)




