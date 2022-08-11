---
  title: "vehicle_data_analysis"
author: "Yash Kharade"
date: "`r Sys.Date()`"
output:
  pdf_document: default
html_document: default
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width=12, fig.height=8,fig.align="center",
                      echo = TRUE,message = FALSE,
                      warning = FALSE)
```


```{r Loading the required packages,include=FALSE}
packages <-
  c(
    "tidyverse",
    "data.table",
    "tidytext",
    "usmap",
    "bit64",
    "grid",
    "gridExtra",
    "wordcloud",
    "gridGraphics",
    "kableExtra",
    "ggplot2",
    "data.table",
    "plotscale",
    "hrbrthemes",
    "viridis"
  )
install.packages(setdiff(packages, rownames(installed.packages())))
for (package in packages) {
  library(package, character.only = TRUE)
}
```



```{r Reading data, include=FALSE}
df <- fread("/Users/yashkharade/Desktop/vehicles.csv")
#df <- na.omit(df)
```



```{r duplicating a column, include=FALSE}
df <- select(df,-c(county))
df$sfname <- df$state

```


```{r Adding state full name column, include=FALSE}
df$sfname[df$state == 'al'] <- 'Alabama'
df$sfname[df$state == 'ak'] <- 'Alaska'
df$sfname[df$state == 'az'] <- 'Arizona'
df$sfname[df$state == 'ar'] <- 'Arkansas'
df$sfname[df$state == 'ca'] <- 'California'
df$sfname[df$state == 'co'] <- 'Colorado'
df$sfname[df$state == 'ct'] <- 'Connecticut'
df$sfname[df$state == 'de'] <- 'Delaware'
df$sfname[df$state == 'dc'] <- 'Columbia'
df$sfname[df$state == 'fl'] <- 'Florida'
df$sfname[df$state == 'ga'] <- 'Georgia'
df$sfname[df$state == 'hi'] <- 'Hawaii'
df$sfname[df$state == 'id'] <- 'Idaho'
df$sfname[df$state == 'il'] <- 'Illinois'
df$sfname[df$state == 'in'] <- 'Indiana'
df$sfname[df$state == 'ia'] <- 'Iowa'
df$sfname[df$state == 'ks'] <- 'Kansas'
df$sfname[df$state == 'ky'] <- 'Kentucky'
df$sfname[df$state == 'la'] <- 'Louisiana'
df$sfname[df$state == 'me'] <- 'Maine'
df$sfname[df$state == 'md'] <- 'Maryland'
df$sfname[df$state == 'ma'] <- 'Massachusetts'
df$sfname[df$state == 'mi'] <- 'Michigan'
df$sfname[df$state == 'mn'] <- 'Minnesota'
df$sfname[df$state == 'ms'] <- 'Mississippi'
df$sfname[df$state == 'mo'] <- 'Missouri'
df$sfname[df$state == 'mt'] <- 'Montana'
df$sfname[df$state == 'ne'] <- 'Nebraska'
df$sfname[df$state == 'nv'] <- 'Nevada'
df$sfname[df$state == 'nh'] <- 'New Hampshire'
df$sfname[df$state == 'nj'] <- 'New Jersey'
df$sfname[df$state == 'nm'] <- 'New Mexico'
df$sfname[df$state == 'ny'] <- 'New York'
df$sfname[df$state == 'nc'] <- 'North Carolina'
df$sfname[df$state == 'nd'] <- 'North Dakota'
df$sfname[df$state == 'oh'] <- 'Ohio'
df$sfname[df$state == 'ok'] <- 'Oklahama'
df$sfname[df$state == 'or'] <- 'Oregon'
df$sfname[df$state == 'pa'] <- 'Pennsylvania'
df$sfname[df$state == 'ri'] <- 'Rhode Island'
df$sfname[df$state == 'sc'] <- 'South Carolina'
df$sfname[df$state == 'sd'] <- 'South Dakota'
df$sfname[df$state == 'tn'] <- 'Tennessee'
df$sfname[df$state == 'tx'] <- 'Texas'
df$sfname[df$state == 'ut'] <- 'Utah'
df$sfname[df$state == 'vt'] <- 'Vermont'
df$sfname[df$state == 'va'] <- 'Virginia'
df$sfname[df$state == 'wa'] <- 'Washington'
df$sfname[df$state == 'wv'] <- 'West Virginia'
df$sfname[df$state == 'wi'] <- 'Wiskonsin'
df$sfname[df$state == 'wy'] <- 'Wyoming'
```

# **Executive Summary**
* The data was collected from craigslist from 4th April 2021 to 5th May 2021.
* Overall, there are a total of 426880 observations and 26 variables in the data.
* Another table that is used is statepop which comes with the 'usmap' package. This table shows the population of all 50 states in the USA.
* Every column before using was checked for duplicate values. There were around 92858 blanks in most of the columns.
* The large percentage of listed cars use gas as their fuel.
* California, Florida, Texas, and New York have the highest number of listings.
* Montana, Idaho, Delaware, and Oregon have the highest per capita listings.
* "Credit," "vehicle," "car," and " financing" are a few of the most common words used in the description column in the top 4 states.
* Average prices of vehicles have increased from 10000 in the year 2000 to 30000 in recent times.
* "Ford" followed by "Chevrolet" have the highest listings in the top 4 states.
* Sedan, SUV, pickup, and truck are the most listed type of vehicles across all the states.
* Listed cars which are manufactured between 1975 to 2008 are listed at a fair price. 

# **Introduction**

Data allows data analysts to visualize relationships between various variables present in the data. It helps data analysts to back their suggestions on a certain topic by data.   
This report is an attempt to understand and visualize the vehicles dataset. Having a vehicle gives anyone the freedom to commute anywhere one needs to. Recently due to the chip shortage car companies are unable to roll out new vehicles as they used to before. Some reports say that the shortage couls stretch til 2024. Due to the large demand and low supply the prices of the cars have increased on an alarming rate.  
Consumers in urgent need are moving towards the used car market. In an article named "Why are used Cars so expensive right now" published on the Honda website [mshonda.com](https://www.mshonda.com/2021-used-cars-market/) states that even the prices of the used cars have gone up. People are having a hard time deciding which car to buy.  
This report aims to find the best value used cars from all the listed cars on carigslist from 4th April 2021 to 5th May 2021. The dataset has listings of cars manufactured in the early 1900's to recent times.

  






\newpage   


# **Findings:**


This graph shows state-wise listings of all 50 states. Each bar is divided into the type of fuel the listed vehicles use. A duplicate state column was created and named 'sfname' and replaced the abbreviations with full state names.



```{r State wise listing, echo=FALSE,message=FALSE}

fuel <- df %>% filter(fuel %in% c("diesel","electric","gas","hybrid","other"))

ggplot(data = fuel)+geom_bar(mapping = aes(x=sfname,fill=fuel))+
  scale_fill_manual(values = c('#00429d', '#4771b2', '#73a2c6', '#a5d5d8', '#B0B4EE'))+
ggtitle("State wise listing: ")+xlab("State")+ylab("Listings")+
  theme(axis.text.x = element_text(angle = 90),
        plot.title = element_text(color="black", size=25, face="bold"),
        axis.title.x = element_text(color="black", size=15),
        axis.title.y = element_text(color="black", size=15))
```

**********
  
  It is not surprising that most of the listed vehicles use gas as fuel. We can also see that California has a much higher number of listings than the rest of the states. California, Florida, Texas, and New York are the states with the highest number of listings. California has a little over 50000 total listings; the second highest is Florida, with a little under 30000 listings. We can see that California has a huge used car market.



\newpage

This graph shows us the state-wise per capita listing. We have used the 'population' column from the 'statepop' table to calculate the per capita(Count/Population). 

```{r Map, echo=FALSE,message=FALSE}
df_map <- df %>% select(state,sfname)
df_map <- df_map %>% group_by(sfname) %>% tally()

df_map$abbr <- statepop$abbr
df_map$fips <- statepop$fips
df_map$Population <- statepop$pop_2015

colnames(df_map) <-c("State","Count","abbr","fips","Population")
df_map$Density <- df_map$Count/df_map$Population

plot_usmap(data=df_map,values = "Density",labels = TRUE)+
  scale_fill_gradient(low = "#deebf7",high = "#3182bd")+
  ggtitle("Per capita listings: ")+
  theme(plot.title = element_text(color="black", size=25, face="bold"))


```

************
  
  California may have the largest market but does not have a high per capita listing. Even Texas, Florida, and New York have low density. Montana, Idaho, Delaware, and Oregon have the highest per capita listings.





\newpage


```{r prep for text analysis, include=FALSE,message=FALSE}
data(stop_words)

token <- filter(df,sfname %in%c("California","Florida","Texas","New York"))

#Only taking the top 4 states


tokenized_description <- token%>%
  select(description,sfname) %>%
  unnest_tokens(word,description) %>%
  anti_join(stop_words) %>%
  group_by(sfname,word) %>%
  tally()

colnames(tokenized_description) <-c("State","Word","Count")

tokenized_description <- unique(tokenized_description)

```

This plot has used the 'description' column to find the most common words used by the sellers in their descriptions. Each description was first tokenized and then counted. This plot only uses the top 4 states(California, Florida, New York, and Texas) for this plot.

```{r description bar graph, echo=FALSE,message=FALSE}
abc <- tokenized_description %>% group_by(State) %>% top_n(25)%>%arrange(desc(Count))

ggplot(data = abc, mapping = aes(x=reorder(Word,Count),y=Count,fill=factor(State)))+
  geom_bar(stat="identity")+theme(legend.position = "none")+
  scale_fill_manual(values = c("#307197","#307197","#307197","#307197"))+
  facet_wrap(~State,scales = "free")+
  coord_flip()+
  labs(x="Words",y="Frequency",title = "Top words used in description by State")+
  theme(plot.title = element_text(color="black", size=25, face="bold"))
```
*******
  
  Words such as: 'vehicle', 'credit', 'financing', 'car', 'color', ford etc are mentioned in all the 4 states. California has some unique frequent words such as: 'Mercedes,' 'class, and 'BMW,' suggesting that the percentage of 'Mercedes' and 'BMW' cars should be higher in California than in the rest of the states.






\newpage

These plots calculate the average prices of vehicles from early 1900 to 2021 by different manufacturers(top 5) and state (top 5).


```{r Average price, echo=FALSE,message=FALSE}
df_price <- df %>% filter(price>100)
df_price <- df_price %>% filter(price<100000)

abc <- select(df_price,manufacturer)
abc <- as.data.frame(table(abc$manufacturer))
abc <- abc %>% filter(Freq>10000)
abc <- abc[-c(1),]



df_price <- na.omit(df_price)
df_price <- select(df_price,-c(id,url,region,region_url,model,condition,
                               cylinders,fuel,odometer,title_status,VIN,drive,
                               size,type,paint_color,image_url,description,
                               lat,long,transmission,state,sfname,posting_date))

df_price <- filter(df_price , manufacturer %in%c('chevrolet','ford','honda','jeep','toyota'))

df_price <- df_price %>% group_by(manufacturer,year) %>% summarise(Average_price=mean(price))

df_price$Average_price <- as.integer(df_price$Average_price)

line_plot_1 <- ggplot(df_price)+geom_smooth(aes(x=year,y=Average_price,color=manufacturer),se=FALSE)+
  scale_color_manual(values = c('#ca0020', '#f4a582', '#67001f', '#92c5de', '#0571b0'))+
scale_color_discrete("Manufacturer")+
  labs(x="Year",y="Average price",title = "Average prices by manufacturer:")+
  theme(plot.title = element_text(color="black", size=25, face="bold"))

df_price <- df %>% filter(price>100)
df_price <- df_price %>% filter(price<100000)

abc <- select(df_price,sfname)
abc <- as.data.frame(table(abc$sfname))
abc <- abc %>% filter(Freq>10000)

df_price <- na.omit(df_price)
df_price <- select(df_price,-c(id,url,region,region_url,model,condition,
                               cylinders,fuel,odometer,title_status,VIN,drive,
                               size,type,paint_color,image_url,description,
                               lat,long,transmission,state,manufacturer,posting_date))

df_price <- filter(df_price , sfname %in%c('California','Florida','New York','Ohio','Texas'))

df_price <- df_price %>% group_by(sfname,year) %>% summarise(Average_price=mean(price))
df_price$Average_price <- as.integer(df_price$Average_price)

line_plot_2 <- ggplot(df_price)+geom_smooth(aes(x=year,y=Average_price,color=sfname),se=FALSE)+
  scale_color_manual(values = c('#ca0020', '#f4a582', '#67001f', '#92c5de', '#0571b0'))+
  scale_color_discrete("State")+
  labs(x="Year",y="Average price",title = "Average prices by states:")+
  theme(plot.title = element_text(color="black", size=25, face="bold"))


grid.arrange(line_plot_1,line_plot_2,nrow=2)
```

********
  
  Here we can see that in the top graph, the lines are scattered as compared to the bottom graph. It is because every brand has a specific price range for its vehicles. Honda seems to be much cheaper than the rest of the brands. For Honda, the average price in the year 2000 is around 2500\$, whereas the 2nd lowest(Toyota) is about 7500\$ which is three times higher. 
In the state-wise plot, all the lines are close to each other, indicating that the prices do not change drastically in different states. It is no surprise that the cars manufactured after the year 2000 have a high price. The cars manufactured from 1925 to 1975 seem to have higher prices.




\newpage

This is a circular bar plot showing the top 15 manufacturers of the top 4 states. 



```{r circular prep, include=FALSE,warning=FALSE}
circle <- df %>% select(manufacturer,state) %>% filter(!manufacturer=="")
circle<-circle%>%filter(state%in%c("ca","fl","ny","tx"))
circle<-filter(circle,manufacturer %in%c("ford","chevrolet","toyota","ram","nissan","jeep","gmc",
                                         "honda","dodge","bmw","mercedes-benz","hyundai","volkswagen",
                                         "kia","lexus"))
circle<-circle%>%select(manufacturer,state)
circle<-circle%>%group_by(state,manufacturer)%>%tally()
circle$group <- factor(c(rep('California', 15),rep('Florida',15),rep('New York',15),rep('Texas',15)))

circle$id <- 1:60

circle<-circle%>%arrange(group)

empty_bar <- 2

to_add <- data.frame(matrix(NA, empty_bar*nlevels(circle$group), ncol(circle)))
colnames(to_add) <- colnames(circle)
to_add$group <- rep(levels(circle$group),each=empty_bar)
circle <- rbind(circle, to_add)
circle <- circle%>%arrange(group)
circle$id <- seq(1, nrow(circle))

label_data <- circle
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5)/number_of_bar
label_data$hjust <- ifelse(angle< -90,1,0)
label_data$angle <- ifelse(angle< -90,angle+180,angle)

base_data <- circle %>%
  group_by(group)%>%
  summarize(start=min(id),end=max(id)-empty_bar)%>%
  rowwise()%>%
  mutate(title=mean(c(start,end)))

grid_data <- base_data
grid_data$end <- grid_data$end[ c(nrow(grid_data),1:nrow
                                  (grid_data)-1)]+1
grid_data$start <- grid_data$start -1
grid_data <- grid_data[-1,]

circular <- ggplot(circle,aes(x=as.factor(id),y=n,fill=group)) + 
  geom_bar(stat = "identity",alpha=0.5) +
  geom_segment(data=grid_data, 
               aes(x = end, y = 500, 
                   xend = start, yend = 500),
               colour = "grey", alpha=1, 
               size=0.3 , inherit.aes = FALSE ) +
  geom_bar(stat = "identity") +
  geom_segment(data=grid_data, 
               aes(x = end, y = 2000, 
                   xend = start, yend = 2000),
               colour = "grey", alpha=1, 
               size=0.3 , inherit.aes = FALSE ) +
  geom_bar(stat = "identity") +
  geom_segment(data=grid_data, 
               aes(x = end, y = 4000, 
                   xend = start, yend = 4000),
               colour = "grey", alpha=1, 
               size=0.3 , inherit.aes = FALSE ) +
  geom_bar(stat = "identity") +
  geom_segment(data=grid_data, 
               aes(x = end, y = 6000, 
                   xend = start, yend = 6000),
               colour = "grey", alpha=1, 
               size=0.3 , inherit.aes = FALSE ) +
  annotate("text",x=rep(max(circle$id),4),y=c(500,2000,4000 , 6000),
           label=c("500", "2000", "4000", "6000"),color="black",size=2,
           angle=0,fontface="bold",hjust=1) +
  ylim(-7500,7500)+coord_polar(start = 0) +
  theme_minimal() + 
  theme(legend.position="None",axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4),"cm")) +
  geom_text(data=label_data,aes(x=id,y= n + 500,
                                label=manufacturer,
                                hjust=hjust),
            color="black",
            fontface="bold",
            alpha=0.6,
            size=2.5,
            angle=label_data$angle,
            inherit.aes = FALSE)

#circular
```




# Statewise top 15 manufacturers.
```{r circular, fig.=2,echo=FALSE,warning=FALSE}

circular<- circular + geom_segment(data=base_data,
                                   aes(x = start, y = -200, xend = end, yend = -200), 
                                   colour = "black", alpha=0.8, size=0.6 ,
                                   inherit.aes = FALSE ) +
  geom_text(data=base_data, 
            aes(x = title, y = -1500, label=group), 
            hjust=c(0.65,0.65,0.25,0.25), colour = "black", 
            alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

circular+ggtitle("blah")

```

*********
  
  Ford and Chevrolet are dominant in California, Florida, and Texas. New York seems to have an even distribution amongst their top 15 manufacturers. As found in the description column analysis, 'Mercedes' and 'BMW' have a much higher presence in California than in other states.


\newpage



This graph shows us the actual value of the vehicle. The price per mile (ppm) is calculated by dividing the price by the odometer(number of miles). The lower the ppm, the better it is for the buyer.


```{r ppm, echo=FALSE,warning=FALSE}


odom <- df%>%select(price,year,odometer,manufacturer)
odom <- odom%>%filter(odometer>100)%>%filter(odometer<100000)
odom <- odom%>%filter(price>1000)%>%filter(price<100000)
odom <- filter(odom,manufacturer%in%c('chevrolet','ford','honda','jeep','toyota'))


odom<-odom%>%group_by(manufacturer,year)%>%
  summarise(avg_price=mean(price),avg_miles=mean(odometer))

odom <- odom%>%filter(!is.na(year))
odom <- odom%>%arrange(year)
odom$ppm <- odom$avg_price/odom$avg_miles
odom <- odom%>%filter(!ppm>3)
ggplot(data = odom)+geom_area(mapping=aes(x=year,y=ppm,fill=manufacturer),
                              color=1,lwd=0.1,linetype=1)+
  labs(title = "Year wise price per miles",
       subtitle = "",
       caption = "Lower the ppm the better") +
  scale_fill_manual(values = c('#053061','#2166ac','#4393c3','#92c5de','#d1e5f0'  )) +
  theme(plot.title = element_text(color="black", size=25, face="bold"))+xlab("Year") +
  ylab("Price per miles")



```


********
  
  It can be seen that cars manufactured between 1975 till around 2009 are listed at a fair price. The cars manufactured after 2010 seem to be costlier. Cars from 1925 to 1975 are cheaper than the 2010 to 2021 range but costlier than the 1975 to 2009 range.



\newpage  

This plot shows price of a vehicle by its vehicle type.


```{r violin_prep, echo=FALSE,warning=FALSE}
tab <- df%>%select(type)%>%group_by(type)%>%tally()%>%arrange(desc(n))
colnames(tab)<-c("Type","Count")
tab <- tab%>%kbl(caption = "Number of listings type wise") %>%
  kable_classic(full_width = F, html_font = "Cambria") 
#tab

box<-df%>%filter(type%in%c("sedan","SUV","pickup","coupe"))%>%
  select(type,price)%>%group_by(type,price)%>%
  filter(!price>100000)%>%
  filter(!price<1000)

sizesample<-box%>%group_by(type)%>%summarise(num=n())

box$price<- as.integer(box$price)
```


```{r violin, echo=FALSE,warning=FALSE}
box<-box %>% left_join(sizesample) %>%
  mutate(myaxis=paste0(type,"\n","n=",num)) %>%
  ggplot(aes(x=type,y=price,fill=type)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.1,color="grey",alpha=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  theme(legend.position = "none",
        plot.title = element_text(color="black", size=25, face="bold")) +
  ggtitle("Price of vehicle by type:")+xlab("Type")+ylab("Prices")

box
```


********
  
  Sedan is the most economical type among the top 4 listed vehicles. Unsurprisingly pickup trucks are the costliest among all.

\newpage

# Conclusion and findings

It has been found that Montana has the highest per capita listings, although California has the largest market with above 50000 listings. Most of the listings are for gas-fueled cars. For a consumer deciding to buy a vehicle, a few things must be considered.
If one wants an affordable car, they can opt for a Honda vehicle as the average price is way lower than the rest. It went from 2500\$ in 2000 to around 23000\$ in 2021 while the second lowest(Toyota) went from 7500\$ in 2000 to 33000\$ in 2021. Even though Honda vehicles seem cheaper than Toyota ones, Toyota has the best value for money than the rest of the manufacturers. Among the different types of vehicles, many sedan vehicles are around the 10000\$ mark. The average of the rest of the vehicle types is above 15000\$.    
**Suggestion:** The consumer has two options if they want to buy a used car in the current market conditions. They can either buy a Toyota sedan or a Honda sedan (based on what they want, an affordable car vs. a quality car). They can also buy a Honda for now if it is an emergency and change it to a Toyota when the chip shortage problem is resolved, and the prices return to normal.





# Appendix

## **Data dictionary(Vehicles dataset)**

Type          Field Name          Considered using?
  --------      --------------      ----------------
  Integer       ID                          No
Integer       ID                          No
String        Url                         No
String        Region                      No
String        Region Url                  No
Integer       Price                       Yes
String        Manufacturer                Yes
String        Model                       No
String        Condition                   No
String        Cylinders                   No
String        Fuel                        Yes
Integer       Odometer                    Yes
String        Title Status                No
String        Transmission                No
String        VIN                         No
String        Drive                       No
String        Size                        No
String        Type                        Yes
String        Paint                       No
String        Description                 Yes
String        State                       Yes
String        Lat                         No
String        Long                        No
String        Posting Date                Yes
---------     --------------   ----------------
  
  \newpage

## **Data dictionary(statepop table)**

Type          Field Name          Considered using?
  --------      --------------      ----------------
Integer       fips                        Yes
String        abbr                        Yes
String        full                        Yes
Integer       statepop_2015               Yes
---------     --------------   ----------------
  
  
  
  
  