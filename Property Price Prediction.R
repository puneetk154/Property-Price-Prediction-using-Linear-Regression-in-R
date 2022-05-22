#Set the Working Directory
setwd("C:/Users/Puneet Kishore/OneDrive/Desktop/Imarticus/R/Project 1 - Property Price Prediction")

House_df<-read.csv("Property_Price_Train.csv",stringsAsFactors = T)
House_df=as.data.frame(unclass(House_df))
View(House_df)

str(House_df)

summary(House_df)

table(House_df$Road_Type) #Since 99.58% of the values in this variable are of same category. 
House_df=House_df[-6] #So we'll drop this column

table(House_df$Lane_Type) #Since 99.58% of the values in this variable are of same category. 
House_df=House_df[-6] #So we'll drop this column

table(House_df$Condition2) #Since 99.58% of the values in this variable are of same category. 
House_df=House_df[-13] #So we'll drop this column

table(House_df$Fireplace_Quality) #Since 99.58% of the values in this variable are of same category. 
House_df=House_df[-55] #So we'll drop this column

table(House_df$Utility_Type) #Since 99.99% of the values in this variable are of same category. 
House_df=House_df[-8] #So we'll drop this column

table(House_df$Roof_Quality) #Since 98.2% of the values in this variable are of same category. 
House_df=House_df[-19] #So we'll drop this column

table(House_df$Heating_Type) #Since 95.2% of the values in this variable are of same category. 
House_df=House_df[-35] #So we'll drop this column

table(House_df$Garage_Condition) #Since 90% of the values in this variable are of same category. 
House_df=House_df[-58] #So we'll drop this column

summary(House_df$Pool_Quality)
#Since 95% of the values in this variable are missing 
House_df=House_df[-65] #So we'll drop this column

summary(House_df$Fence_Quality)
#Since 95% of the values in this variable are missing 
House_df=House_df[-65] #So we'll drop this column


summary(House_df$Miscellaneous_Feature)
#Since 95% of the values in this variable are missing 
House_df=House_df[-65] #So we'll drop this column

summary(House_df$Miscellaneous_Value)#Since 95% of the values in this variable are missing 
House_df=House_df[-65] #So we'll drop this column
names(House_df)

library(Hmisc)
Hmisc::describe(House_df)

options(scipen = 420)
House_df$Building_Class[House_df$Building_Class== 20]="1-STORY 1946 & NEWER ALL STYLES"
House_df$Building_Class[House_df$Building_Class== 30]="1-STORY 1945 & OLDER"
House_df$Building_Class[House_df$Building_Class== 40]="1-STORY W/FINISHED ATTIC ALL AGES"
House_df$Building_Class[House_df$Building_Class== 45]="1-1/2 STORY - UNFINISHED ALL AGES"
House_df$Building_Class[House_df$Building_Class== 50]="1-1/2 STORY FINISHED ALL AGES"
House_df$Building_Class[House_df$Building_Class== 60]="2-STORY 1946 & NEWER"
House_df$Building_Class[House_df$Building_Class== 70]="2-STORY 1945 & OLDER"
House_df$Building_Class[House_df$Building_Class== 75]="2-1/2 STORY ALL AGES"
House_df$Building_Class[House_df$Building_Class== 80]="SPLIT OR MULTI-LEVEL"
House_df$Building_Class[House_df$Building_Class== 85]="SPLIT FOYER"
House_df$Building_Class[House_df$Building_Class== 90]="DUPLEX - ALL STYLES AND AGES"
House_df$Building_Class[House_df$Building_Class== 120]="1-STORY PUD (Planned Unit Development) - 1946 & NEWER"
House_df$Building_Class[House_df$Building_Class== 150]="1-1/2 STORY PUD - ALL AGES"
House_df$Building_Class[House_df$Building_Class== 160]="2-STORY PUD - 1946 & NEWER"
House_df$Building_Class[House_df$Building_Class== 180]="PUD - MULTILEVEL - INCL SPLIT LEV/FOYER"
House_df$Building_Class[House_df$Building_Class== 190]="2 FAMILY CONVERSION - ALL STYLES AND AGES"

House_df$Building_Class=as.factor(House_df$Building_Class)
table(House_df$Building_Class)
class(House_df$Building_Class)
House_df$Overall_Material=as.factor(House_df$Overall_Material)
table(House_df$Overall_Material)

House_df$House_Condition=as.factor(House_df$House_Condition)
table(House_df$House_Condition)

House_df$Construction_Year=as.integer(House_df$Construction_Year)
range(House_df$Construction_Year)

House_df$Construction_Year<-ifelse(House_df$Construction_Year>=1870 & House_df$Construction_Year<=1880,"Between_1870_1880",
                                   ifelse(House_df$Construction_Year>=1881 & House_df$Construction_Year<=1890,"Between_1881_1890",
                                          ifelse(House_df$Construction_Year>=1891 & House_df$Construction_Year<=1900,"Between_1891_1900",
                                                 ifelse(House_df$Construction_Year>=1901 & House_df$Construction_Year<=1910,"Between_1901_1910",
                                                        ifelse(House_df$Construction_Year>=1911 & House_df$Construction_Year<=1920,"Between_1911_1920",
                                                               ifelse(House_df$Construction_Year>=1921 & House_df$Construction_Year<=1930,"Between_1921_1930",
                                                                      ifelse(House_df$Construction_Year>=1931 & House_df$Construction_Year<=1940,"Between_1931_1940",
                                                                             ifelse(House_df$Construction_Year>=1931 & House_df$Construction_Year<=1940,"Between_1931_1940",
                                                                                    ifelse(House_df$Construction_Year>=1941 & House_df$Construction_Year<=1950,"Between_1941_1950",
                                                                                           ifelse(House_df$Construction_Year>=1951 & House_df$Construction_Year<=1960,"Between_1951_1960",
                                                                                                  ifelse(House_df$Construction_Year>=1961 & House_df$Construction_Year<=1970,"Between_1961_1970",
                                                                                                         ifelse(House_df$Construction_Year>=1971 & House_df$Construction_Year<=1980,"Between_1971_1980",
                                                                                                                ifelse(House_df$Construction_Year>=1981 & House_df$Construction_Year<=1990,"Between_1981_1990",
                                                                                                                       ifelse(House_df$Construction_Year>=1991 & House_df$Construction_Year<=2000,"Between_1991_2000","2000_2010"))))))))))))))		

House_df$Construction_Year=as.factor(House_df$Construction_Year)

table(House_df$Construction_Year)


range(House_df$Remodel_Year)
House_df$Remodel_Year<-ifelse(House_df$Remodel_Year>=1950 & House_df$Remodel_Year<=1960,"Between_1950_1960",
                              ifelse(House_df$Remodel_Year>=1961 & House_df$Remodel_Year<=1970,"Between_1961_1970",
                                     ifelse(House_df$Remodel_Year>=1971 & House_df$Remodel_Year<=1980,"Between_1971_1980",
                                            ifelse(House_df$Remodel_Year>=1981 & House_df$Remodel_Year<=1990,"Between_1981_1990",
                                                   ifelse(House_df$Remodel_Year>=1991 & House_df$Remodel_Year<=2000,"Between_1991_2000","Between_2000_2010")))))
House_df$Remodel_Year=as.factor(House_df$Remodel_Year)
table(House_df$Remodel_Year)

House_df$Underground_Full_Bathroom[House_df$Underground_Full_Bathroom== 1]="Basement full bathrooms"
unique(House_df$Underground_Full_Bathroom)
House_df$Underground_Full_Bathroom=as.factor(House_df$Underground_Full_Bathroom)
table(House_df$Underground_Full_Bathroom)

House_df$Underground_Half_Bathroom
unique(House_df$Underground_Half_Bathroom)
House_df$Underground_Half_Bathroom=as.factor(House_df$Underground_Half_Bathroom)
table(House_df$Underground_Half_Bathroom)

House_df$Full_Bathroom_Above_Grade
unique(House_df$Full_Bathroom_Above_Grade)
House_df$Full_Bathroom_Above_Grade=as.factor(House_df$Full_Bathroom_Above_Grade)
table(House_df$Full_Bathroom_Above_Grade)

House_df$Half_Bathroom_Above_Grade
unique(House_df$Half_Bathroom_Above_Grade)
House_df$Half_Bathroom_Above_Grade=as.factor(House_df$Half_Bathroom_Above_Grade)
table(House_df$Half_Bathroom_Above_Grade)

House_df$Bedroom_Above_Grade
unique(House_df$Bedroom_Above_Grade)
House_df$Bedroom_Above_Grade=as.factor(House_df$Bedroom_Above_Grade)
table(House_df$Bedroom_Above_Grade)

House_df$Kitchen_Above_Grade
unique(House_df$Kitchen_Above_Grade)
House_df$Kitchen_Above_Grade=as.factor(House_df$Kitchen_Above_Grade)
table(House_df$Kitchen_Above_Grade)

House_df$Rooms_Above_Grade
unique(House_df$Rooms_Above_Grade)
House_df$Rooms_Above_Grade=as.factor(House_df$Rooms_Above_Grade)
table(House_df$Rooms_Above_Grade)

House_df$Fireplaces
unique(House_df$Fireplaces)
House_df$Fireplaces=as.factor(House_df$Fireplaces)
table(House_df$Fireplaces)

range(House_df$Garage_Built_Year,na.rm = T)
House_df$Garage_Built_Year<-ifelse(House_df$Garage_Built_Year>=1900 & House_df$Garage_Built_Year<=1910,"Between_1900_1910",
                                   ifelse(House_df$Garage_Built_Year>=1911 & House_df$Garage_Built_Year<=1920,"Between_1911_1920",
                                          ifelse(House_df$Garage_Built_Year>=1921 & House_df$Garage_Built_Year<=1930,"Between_1921_1930",
                                                 ifelse(House_df$Garage_Built_Year>=1931 & House_df$Garage_Built_Year<=1940,"Between_1931_1940",
                                                        ifelse(House_df$Garage_Built_Year>=1931 & House_df$Garage_Built_Year<=1940,"Between_1931_1940",
                                                               ifelse(House_df$Garage_Built_Year>=1941 & House_df$Garage_Built_Year<=1950,"Between_1941_1950",
                                                                      ifelse(House_df$Garage_Built_Year>=1951 & House_df$Garage_Built_Year<=1960,"Between_1951_1960",
                                                                             ifelse(House_df$Garage_Built_Year>=1961 & House_df$Garage_Built_Year<=1970,"Between_1961_1970",
                                                                                    ifelse(House_df$Garage_Built_Year>=1971 & House_df$Garage_Built_Year<=1980,"Between_1971_1980",
                                                                                           ifelse(House_df$Garage_Built_Year>=1981 & House_df$Garage_Built_Year<=1990,"Between_1981_1990",
                                                                                                  ifelse(House_df$Garage_Built_Year>=1991 & House_df$Garage_Built_Year<=2000,"Between_1991_2000","Between_2000_2010")))))))))))
House_df$Sale_Type=as.factor(House_df$Sale_Type)

House_df$Garage_Built_Year=as.factor(House_df$Garage_Built_Year)
table(House_df$Garage_Built_Year)

library(plyr)
House_df <- rename(House_df,c('Garage_Finish_Year'='Garage_Interior_Finish'))

unique(House_df$Garage_Size)
House_df$Garage_Size=as.factor(House_df$Garage_Size)
table(House_df$Garage_Size)

range(House_df$Garage_Area)
House_df$Garage_Area[House_df$Garage_Area<0]=NA #Since Area cannot be a negative value
sum(is.na(House_df$Garage_Area))

range(House_df$W_Deck_Area)
House_df$W_Deck_Area[House_df$W_Deck_Area<0]=NA #Since Area cannot be a negative value
sum(is.na(House_df$W_Deck_Area))

range(House_df$Open_Lobby_Area)
House_df$Open_Lobby_Area [House_df$Open_Lobby_Area <0]=NA #Since Area cannot be a negative value
sum(is.na(House_df$Open_Lobby_Area ))

range(House_df$Enclosed_Lobby_Area )
House_df$Enclosed_Lobby_Area  [House_df$Enclosed_Lobby_Area  <0]=NA #Since Area cannot be a negative value
sum(is.na(House_df$Enclosed_Lobby_Area  ))

House_df$Month_Sold
range(House_df$Month_Sold)

House_df$Month_Sold<-ifelse(House_df$Month_Sold==1,"January",
                            ifelse(House_df$Month_Sold==2,"February",
                                   ifelse(House_df$Month_Sold==3,"March",
                                          ifelse(House_df$Month_Sold==4 ,"April",
                                                 ifelse(House_df$Month_Sold==5,"May",
                                                        ifelse(House_df$Month_Sold==6,"June",
                                                               ifelse(House_df$Month_Sold==7,"July",
                                                                      ifelse(House_df$Month_Sold==8,"August",
                                                                             ifelse(House_df$Month_Sold==9,"September",
                                                                                    ifelse(House_df$Month_Sold==10,"October",
                                                                                           ifelse(House_df$Month_Sold==11,"November","December")))))))))))

House_df$Month_Sold=as.factor(House_df$Month_Sold)
table(House_df$Month_Sold)

range(House_df$Year_Sold)
House_df$Year_Sold=as.factor(House_df$Year_Sold)
table(House_df$Year_Sold)

House_df$Brick_Veneer_Type=as.factor(House_df$Brick_Veneer_Type)

House_df$Id=as.factor(House_df$Id)
summary(House_df)
View(House_df)

#Create an user defined function for descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,
             std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

num_var= sapply(House_df,is.numeric)

num_data<-t(data.frame(apply(House_df[num_var], 2, var_Summ)))
View(num_data)

library(summarytools)
Description <- summarytools::descr(House_df)

House_df$Lot_Size[House_df$Lot_Size>150000]=150000

House_df$Lot_Extent[House_df$Lot_Extent>180]=190

House_df$Brick_Veneer_Area[House_df$Brick_Veneer_Area>1250]=1300

House_df$BsmtFinSF1[House_df$BsmtFinSF1>1500]=1500

House_df$BsmtFinSF2[House_df$BsmtFinSF2>1200]=1200
length(which(House_df$BsmtFinSF2 == 0))

House_df$Total_Basement_Area[House_df$Total_Basement_Area>1900]=1900

House_df$First_Floor_Area[House_df$First_Floor_Area>2300]=2300

House_df$Second_Floor_Area[House_df$Second_Floor_Area>1500]=1500

length(which(House_df$Second_Floor_Area == 0))

House_df$Grade_Living_Area[House_df$Grade_Living_Area>3200]=3300

House_df$W_Deck_Area[House_df$W_Deck_Area>410]=410

House_df$Open_Lobby_Area[House_df$Open_Lobby_Area>220]=220

House_df$Enclosed_Lobby_Area[House_df$Enclosed_Lobby_Area>200]=200

House_df$Three_Season_Lobby_Area[House_df$Three_Season_Lobby_Area>300]=300

House_df$Three_Season_Lobby_Area[House_df$Screen_Lobby_Area>300]=310


House_df$Pool_Area

House_df$Sale_Price[House_df$Sale_Price>600000]=600000

barplot(House_df$Sale_Price)
plot(House_df$Sale_Price)

View(Description)

Other_var= !sapply(House_df,is.numeric)

Hmisc::describe(House_df[Other_var])

# Missing Value Treatment in continuous variable with the mean value
House_df[,num_var] <- apply(data.frame(House_df[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})

num_data<-t(data.frame(apply(House_df[num_var], 2, var_Summ)))
View(num_data)

# Missing Value Treatment in continuous variable with the mode value

# Create the function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

House_df[,Other_var] <- apply(data.frame(House_df[,Other_var]), 2, function(x){x <- replace(x, is.na(x), getmode(x))})

apply(is.na(House_df[,]),2,sum)

dfSummary(House_df)

corr=cor(House_df[num_var])
write.csv(corr, "C:/Users/Puneet Kishore/OneDrive/Desktop/Imarticus/R/Project 1 - Property Price Prediction/corr_house.csv")

library(ggcorrplot)

# Correlation Matrix
corr <- round(cor(House_df), 1)

summary(House_df$BsmtUnfSF)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of House_df", 
           ggtheme=theme_bw)

library(ggpubr)
levels(House_df$Sale_Type)
ggboxplot(House_df, x = "Sale_Type", y = "Sale_Price",                       #For plotting Weight Values before and After
          color = "Sale_Type", palette = c("#00AFBB", "#E7B800","#00AFBB","#00AFBB", "#E7B800","#00AFBB","#00AFBB", "#E7B800","#00AFBB"),
          order = c("COD", "Con","ConLD","ConLI","ConLw","CWD","New","Oth","WD"),
          ylab = "Sales_price", xlab = "Sale_Type")
range(House_df$Lot_Extent)
summary(House_df$Lot_Size)


# Scatterplot
gg1 <- ggplot(House_df, aes(x=Lot_Extent, y=Sale_Price)) + 
  geom_point(aes(col=Sale_Price, size=Lot_Extent)) + 
  geom_smooth(method="loess", se=F) + xlim(c(20, 190)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Lot Extent Vs Sale_Price", 
       y="Sale_Price", 
       x="Lot_Extent", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg1)

gg2 <- ggplot(House_df, aes(x=Lot_Size, y=Sale_Price)) + 
  geom_point(aes(col=Sale_Price, size=Lot_Size)) + 
  geom_smooth(method="loess", se=F) + xlim(c(15000,50000)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Lot size Vs Sale_Price", 
       y="Sale_Price", 
       x="Lot_size", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg2)

# Scatterplot
gg3 <- ggplot(House_df, aes(x=Grade_Living_Area, y=Sale_Price))+ geom_point(aes(col=Sale_Price, size=Grade_Living_Area))+
  geom_smooth(method="loess", se=F) + xlim(c(334,4000)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Grade_Living_Area Vs Sale_Price", 
       y="Sale_Price", 
       x="Grade_Living_Area", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg3)


# Scatterplot
gg4 <- ggplot(House_df, aes(x=First_Floor_Area, y=Sale_Price))+ geom_point(aes(col=Sale_Price, size=First_Floor_Area))+
  geom_smooth(method="loess", se=F) + xlim(c(500,2500)) + ylim(c(35000, 500000)) + 
  labs(subtitle="First_Floor_Area Vs Sale_Price", 
       y="Sale_Price", 
       x="First_Floor_Area", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg4)

gg5 <- ggplot(House_df, aes(x=Total_Basement_Area, y=Sale_Price))+ geom_point(aes(col=Sale_Price, size=Total_Basement_Area))+
  geom_smooth(method="loess", se=F) + xlim(c(500,2500)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Total_Basement_Area Vs Sale_Price", 
       y="Sale_Price", 
       x="Total_Basement_Area", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg5)

gg6 <- ggplot(House_df, aes(x=Brick_Veneer_Area, y=Sale_Price))+ geom_point(aes(col=Sale_Price, size=Brick_Veneer_Area))+
  geom_smooth(method="loess", se=F) + xlim(c(0,1300)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Black_Vineer_Area Vs Sale_Price", 
       y="Sale_Price", 
       x="Black_Vineer_Area", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg6)

range(House_df$Second_Floor_Area)
gg7 <- ggplot(House_df, aes(x=Second_Floor_Area, y=Sale_Price))+ geom_point(aes(col=Sale_Price, size=Brick_Veneer_Area))+
  geom_smooth(method="loess", se=F) + xlim(c(0,1600)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Second_Floor_Area Vs Sale_Price", 
       y="Sale_Price", 
       x="Second_Floor_Area", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg7)

range(House_df$Second_Floor_Area)
gg8 <- ggplot(House_df, aes(x=Second_Floor_Area, y=Sale_Price))+ geom_point(aes(col=Sale_Price, size=Brick_Veneer_Area))+
  geom_smooth(method="loess", se=F) + xlim(c(0,1600)) + ylim(c(35000, 500000)) + 
  labs(subtitle="Second_Floor_Area Vs Sale_Price", 
       y="Sale_Price", 
       x="Second_Floor_Area", 
       title="Scatterplot", 
       caption = "Source: House_df")

plot(gg8)

House_df$Sale_Condition=as.factor(House_df$Sale_Condition)
levels(House_df$Sale_Condition)
gg9=ggboxplot(House_df, x = "Sale_Condition", y = "Sale_Price",                       #For plotting Weight Values before and After
              color = "Sale_Condition", palette = c("#00AFBB", "#E7B800","#00AFBB","#00AFBB", "#E7B800","#00AFBB"),
              order = c("Abnorml","AdjLand","Alloca","Family","Normal","Partial"),
              ylab = "Sales_price", xlab = "Sale_condition")
plot(gg9)

House_df$House_Condition=as.factor(House_df$House_Condition)
levels(House_df$House_Condition)
gg10=ggboxplot(House_df, x = "House_Condition", y = "Sale_Price",                       #For plotting Weight Values before and After
               color = "House_Condition", palette = c("#00AFBB", "#E7B800","#00AFBB","#00AFBB", "#E7B800","#00AFBB","#00AFBB", "#E7B800","#00AFBB"),
               order = c("1","2","3","4","5","6","7","8","9"),
               ylab = "Sales_price", xlab = "House_Condition")
plot(gg10)

House_df$Sale_Price=log(House_df$Sale_Price)
House_df$Id=NULL
str(House_df)
names(House_df)


#Splitting data into Training and Testing Dataset
train_ind <- sample(1:nrow(House_df), size = floor(0.70 * nrow(House_df)))
House_training<-House_df[train_ind,]
House_testing<-House_df[-train_ind,]
nrow(House_training)
nrow(House_testing)

describe(House_training)


fit <- lm(Sale_Price ~  + Lot_Extent  + W_Deck_Area + Open_Lobby_Area+First_Floor_Area
          +Second_Floor_Area+Enclosed_Lobby_Area+Three_Season_Lobby_Area+
            Neighborhood+Brick_Veneer_Area+Total_Basement_Area+Garage_Area+BsmtFinSF1+House_Condition
          +Kitchen_Quality+Overall_Material+Roof_Design,House_training)

summary(fit) # show results
House_training$Sale_Price
df=House_training

# Scoring Using Predict Function
House_df_train_pred_sales<-cbind(df, pred_sales = predict(fit))
write.csv(House_df_train_pred_sales, "House_df_train_pred_sales.csv")
House_df_train_pred_sales$pred_sales=exp(House_df_train_pred_sales$pred_sales)
House_df_train_pred_sales$Sale_Price=exp(House_df_train_pred_sales$Sale_Price)


names(House_df_train_pred_sales)
House_df_train_pred_sales<- transform(House_df_train_pred_sales, APE = abs(House_df_train_pred_sales$pred_sales - House_df_train_pred_sales$Sale_Price)/House_df_train_pred_sales$Sale_Price)
mean(House_df_train_pred_sales$APE)
str(House_df_train_pred_sales)
write.csv(House_df_train_pred_sales, "House_df_train_pred_sales_APE.csv")

library(MASS)
library(ISLR)

plot(fit)
abline(fit,col="red")

require(MASS)
step<- stepAIC(fit,direction="both")
?stepAIC()
ls(step3)
step3$anova
step3$coefficients
step3$model

fit_test<-lm(Sale_Price ~ Lot_Extent+Second_Floor_Area+Overall_Material+Kitchen_Quality+Three_Season_Lobby_Area+Total_Basement_Area+BsmtFinSF1+First_Floor_Area+Neighborhood,House_testing)
summary(fit_test) # show results


# Multicollinierity Check using VIF
library(car)

vif(fit_test)

tf=House_testing
# Scoring Using Predict Function
House_df_test_pred_sales<-cbind(tf, pred_sales = predict(fit_test))
write.csv(House_df_test_pred_sales, "House_df_test_pred_sales.csv")
names(House_df_test_pred_sales)

House_df_test_pred_sales<- transform(House_df_test_pred_sales, APE = abs(pred_sales - Sale_Price)/Sale_Price)
mean(House_df_test_pred_sales$APE)
View(House_df_test_pred_sales)
write.csv(House_df_test_pred_sales, "House_df_test_pred_sales_APE.csv")
library(broom)

glance(fit_test) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

# Decile Analysis Reports - House_df_test_pred_sales(testing)

sigma(fit_test)/mean(House_df_test_pred_sales$pred_sales)



# Find the decile locations 
decLocations <- quantile(House_df_test_pred_sales$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use find Interval with -Inf and Inf as upper and lower bounds
House_df_test_pred_sales$decile <- findInterval(House_df_test_pred_sales$pred_sales,c(-Inf,decLocations, Inf))

names(House_df_test_pred_sales)
House_df_test_pred_sales$decile

write.csv(House_df_test_pred_sales, "House_df_test_pred_sales_Decile.csv")
House_df_test_pred_sales$Sale_Price
require(sqldf)
House_df_test_pred_sales_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
                    avg(Sale_Price) as avg_Actual_sales
                    from House_df_test_pred_sales
                    group by decile
                    order by decile desc")

View(House_df_test_pred_sales_DA)
write.csv(House_df_test_pred_sales_DA,"House_df_test_pred_sales_DA.csv")


# Decile Analysis Reports - House_df_train_pred_sales(training)

# find the decile locations 
eecLocations <- quantile(House_df_train_pred_sales$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
House_df_train_pred_sales$decile <- findInterval(House_df_train_pred_sales$pred_sales,c(-Inf,eecLocations, Inf))

names(House_df_train_pred_sales)
House_df_train_pred_sales$decile

write.csv(House_df_train_pred_sales, "House_df_train_pred_sales_Decile.csv")
House_df_train_pred_sales$Sale_Price
require(sqldf)
House_df_train_pred_sales_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
                    avg(Sale_Price) as avg_Actual_sales
                    from House_df_train_pred_sales
                    group by decile
                    order by decile desc")

View(House_df_train_pred_sales_DA)

write.csv(House_df_train_pred_sales,"House_df_train_pred_sales.csv")

#---------------------------------------------------------------------------------------------------------------------------#

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 

fitted(fit) # predicted values

residuals(fit) # residuals

anova(fit) # anova table 

influence(fit) # regression diagnostics


layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
windows()
plot(fit)

#---------------------------------------------------------------------------------------------------------------------------#
coefficients(fit_test)

confint(fit_test, level=0.95) # CIs for model parameters 

fitted(fit_test) # predicted values

residuals(fit_test) # residuals

anova(fit_test) # anova table 

influence(fit_test) # regression diagnostics

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
windows()
plot(fit_test)

library(ggplot2)

House_df_test_pred_sales$Sale_Price
ggplot(House_df_test_pred_sales,aes(x = House_df_test_pred_sales$pred_sales, y = House_df_test_pred_sales$Sale_Price)) +
  geom_point() +
  stat_smooth() +
  labs(x = "Predicted Values", y = "Original Values", title = "Predicted vs. Original Values") +
  theme_minimal()

ggplot(House_df_train_pred_sales,aes(x = House_df_train_pred_sales$pred_sales, y = House_df_train_pred_sales$Sale_Price)) +
  geom_point() +
  stat_smooth() +
  labs(x = "Predicted Values", y = "Original Values", title = "Predicted vs. Original Values") +
  theme_minimal()

