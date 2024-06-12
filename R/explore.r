
##################################################################################################################################
# Import Libraries & Set Up

if(TRUE == FALSE){
    library(matlib)
    library(kableExtra)
    library(xtable)
    library(matlib)
    library(mvtnorm)
    library(ellipse)
    # library(MASS)
    library(reshape2)
    library(lattice)
    library(Rprofet) #Brandenburger Code
    library(ISLR) #Credit Data Set
    library(glmnet)
    library(sqldf)
    library(gains) #For gains chart
    library(RColorBrewer) #For colors
    library(rgl) #For 3D charts
    # library(pivottabler) #For Pivot Table
    # library(rpivotTable) #for Pivot table
    library(tidyr)
    library(fastDummies) #For easily making dummy variables from bins
    library(glmtoolbox) #For hosmer Lemshow Goodness of Fit Test
    library(spaMM) #For hosmer Lemshow Goodness of Fit Test
    library(ResourceSelection) #For Hosmer Lemshow Goodness of Fit Test
    library(gridExtra)
    library(janitor)
    library(lubridate)
    library(car)
    library(fst)
    library(ggpubr)
}


library(dplyr)
library(ggplot2)
library(plotly)
library(fst)
library(lubridate)
library(tidyverse)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999930")

##################################################################################################################################



##################################################################################################################################
# Import Data

dispatch_masked_rtutor_pre <- read.csv("G:/My Drive/RTutor/HMCL/Dispatch_Masked_RTutor.csv")
sales_masked_rtutor_pre <- read.csv("G:/My Drive/RTutor/HMCL/Sales_Masked_RTutor.csv")
vahan_masked_rtutor_pre <- read.csv("G:/My Drive/RTutor/HMCL/Vahan_Share_Masked_RTutor.csv")

##################################################################################################################################
#### Analyse Dispatch
##################################################################################################################################

#Assment of Data as a whole
dispatch_masked_rtutor_pre %>% dim()
dispatch_masked_rtutor_pre %>% head() #7 Columns
dispatch_masked_rtutor <- dispatch_masked_rtutor_pre %>%
    mutate(
    DEALER_NAME = as.factor(DEALER_NAME),
    ZONE = as.factor(ZONE),
    AREA_OFFICE = as.factor(AREA_OFFICE),
    DISPATCH_DATE = as.Date(DISPATCH_DATE), 
    MODEL = as.factor(MODEL), 
    SEGMENT = as.factor(SEGMENT))
# write_fst(dispatch_masked_rtutor, "G:/My Drive/RTutor/HMCL/Dispatch_Masked_RTutor.fst")
dispatch_masked_rtutor %>% summary()
colSums(is.na(dispatch_masked_rtutor)) #Dealer Name has NA's

#DEALER_NAME
dispatch_masked_rtutor %>% 
   group_by(DEALER_NAME) %>% 
   summarise(n=n()) %>% View()

#ZONE
dispatch_masked_rtutor %>% 
   group_by(ZONE) %>% 
   summarise(n=n())

#Confirming that NA DEALER_NAME is a one-to-one mapping to blank ZONE.
dispatch_masked_rtutor %>% 
   filter(is.na(DEALER_NAME) & ZONE !="") %>%
   head()
dispatch_masked_rtutor %>% 
   filter(!is.na(DEALER_NAME) & ZONE =="") %>%
   head()

#AREA_OFFICE
dispatch_masked_rtutor %>% 
   group_by(AREA_OFFICE) %>% 
   summarise(n=n())

#DISPATCH_DATE
dispatch_masked_rtutor %>% 
   group_by(DISPATCH_DATE) %>% 
   summarise(n=n()) %>% 
   arrange(DISPATCH_DATE) %>% 
   View()

#MODEL
dispatch_masked_rtutor %>% 
   group_by(MODEL) %>% 
   summarise(n=n()) %>% View()

#Segment
dispatch_masked_rtutor %>% 
   group_by(SEGMENT) %>% 
   summarise(n=n()) %>% View()










#Does HMCL send multiple dispatches in a single day to a specific dealer?
dispatch_masked_rtutor %>%
  group_by(ZONE, AREA_OFFICE, DEALER_NAME, DISPATCH_DATE) %>%
  summarise(n=n()) %>% 
  filter(n>1) %>% View()
#Answer: Yes. HMCL sends multiple dispatches in a single day to a specific dealer:
#               NA & 1034


#Are these dispatches the same or different models?
dispatch_masked_rtutor %>%
  group_by(ZONE, AREA_OFFICE, DEALER_NAME, DISPATCH_DATE, MODEL) %>%
  summarise(n=n()) %>% 
  filter(n>1) %>% View()
#Answer: For 1034, the multiple dispatches in a single day are different models.
#           For NA, there a multiple dispatches of "unknown_model"


#Are these multiple dispatches in a single day for the "unknown_model" consist
# of the same segment?
dispatch_masked_rtutor %>%
  group_by(ZONE, AREA_OFFICE, DEALER_NAME, DISPATCH_DATE, MODEL, SEGMENT) %>%
  summarise(n=n()) %>% 
  filter(n>1) %>% View()
#Answer: No. For each multiple dispatches in a single day, there is a unique
#           segment for each dispatch

#Selecting a specific example
dispatch_masked_rtutor %>%
   filter(DISPATCH_DATE == "2022-06-29" & ZONE == "" & MODEL=="unknown_model")






##################################################################################################################################
#### Analyse Sales
##################################################################################################################################


sales_masked_rtutor_pre %>% dim()
sales_masked_rtutor_pre %>% head()
sales_masked_rtutor <- sales_masked_rtutor_pre %>% #Takes a while to run. Maybe use Base R?
    mutate(
    DEALER_NAME = as.factor(DEALER_NAME),
    ZONE = as.factor(ZONE),
    AREA_OFFICE = as.factor(AREA_OFFICE),
    SALES_DATE = as.Date(SALES_DATE), 
    TWOWHEELERTYPE = as.factor(TWOWHEELERTYPE),
    MODEL = as.factor(MODEL),
    SEGMENT = as.factor(SEGMENT),
    CC_CATEGORY = as.factor(CC_CATEGORY))
# write_fst(sales_masked_rtutor, "G:/My Drive/RTutor/HMCL/Sales_Masked_RTutor.fst")
sales_masked_rtutor %>% summary()
colSums(is.na(sales_masked_rtutor)) #No NA's!



#DEALER_NAME
sales_masked_rtutor %>% 
   group_by(DEALER_NAME) %>% 
   summarise(n=n()) %>% View()
sales_masked_rtutor %>% 
    anti_join(dispatch_masked_rtutor, by="DEALER_NAME") %>%
    group_by(DEALER_NAME) %>% 
    summarise(n=n())
dispatch_masked_rtutor %>% 
    anti_join(sales_masked_rtutor, by="DEALER_NAME") %>%
    group_by(DEALER_NAME) %>% 
    summarise(n=n())

#ZONE
sales_masked_rtutor %>% 
   group_by(ZONE) %>% 
   summarise(n=n())

#AREA_OFFICE
sales_masked_rtutor %>% 
   group_by(AREA_OFFICE) %>% 
   summarise(n=n()) %>% View()

#SALES_DATE
sales_masked_rtutor %>% 
   group_by(SALES_DATE) %>% 
   summarise(n=n()) %>% 
   arrange(SALES_DATE) %>% 
   View()

#TWO WHEELER TYPE
sales_masked_rtutor %>% 
   group_by(TWOWHEELERTYPE) %>% 
   summarise(n=n()) %>% View()

#MODEL
sales_masked_rtutor %>% 
   group_by(MODEL) %>% 
   summarise(n=n()) %>% View()

#Segment
sales_masked_rtutor %>% 
   group_by(SEGMENT) %>% 
   summarise(n=n()) %>% View()

#Segment
sales_masked_rtutor %>% 
   group_by(CC_CATEGORY) %>% 
   summarise(n=n()) %>% View()







##################################################################################################################################
#### Analyse Vahan Share
##################################################################################################################################


vahan_masked_rtutor_pre %>% dim()
vahan_masked_rtutor_pre %>% head()
vahan_masked_rtutor <- vahan_masked_rtutor_pre %>% 
    mutate(DATE = as.Date(DATE), 
    ZO = as.factor(ZO), 
    AO = as.factor(AO), 
    STATE = as.factor(STATE), 
    MAKER = as.factor(MAKER))
# write_fst(vahan_masked_rtutor, "G:/My Drive/RTutor/HMCL/Vahan_Masked_RTutor.fst")
vahan_masked_rtutor %>% summary()
colSums(is.na(vahan_masked_rtutor)) #No NA's!

#DATE
vahan_masked_rtutor %>% 
   group_by(DATE) %>% 
   summarise(n=n()) %>% 
   arrange(DATE) %>% View()

#ZONE
vahan_masked_rtutor %>% 
   group_by(ZO) %>% 
   summarise(n=n())

#ZONE
vahan_masked_rtutor %>% 
   group_by(AO) %>% 
   summarise(n=n())

#STATE
vahan_masked_rtutor %>% 
   group_by(STATE) %>% 
   summarise(n=n()) %>% View()

#MAKER
vahan_masked_rtutor %>% 
   group_by(MAKER) %>% 
   summarise(n=n()) %>% View()

vahan_masked_rtutor %>% View()




##################################################################################################################################
#### Questions
##################################################################################################################################

# 1) Which dealers have shown the maximum growth for model x nationally in the last 3 months?

#Define growth: growth in Sales.
#Define measurement of growth: 
today = as.Date("2024-03-26")
sales_masked_rtutor %>% 
    filter(MODEL == "model_1" & SALES_DATE >= today %m-% months(6)) %>% #Analysis on model_1
    mutate(Date_Class = case_when(
        SALES_DATE >= today %m-% months(3) ~ "Last 3",
        SALES_DATE >= today %m-% months(6) & SALES_DATE < today %m-% months(3) ~ "Last 6",
        TRUE ~ "Last 6+"
    )) %>% 
    group_by(DEALER_NAME, Date_Class) %>%
    summarise(sum_sales = sum(TOTAL_SALES)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Date_Class, values_from = sum_sales) %>% 
    mutate(rate_change = (`Last 3` - `Last 6`)/`Last 6`) %>% 
    arrange(desc(rate_change)) %>% View()


#Chat-GPT
library(dplyr)
df <- sales_masked_rtutor 
df_last_3_months <- df %>%
  filter(DEALER_NAME=="1000")
  filter(SALES_DATE >= Sys.Date() - months(6), MODEL == "model_1") #ERROR From GPT

monthly_sales <- df_last_3_months %>%
      mutate(SALES_MONTH = format(SALES_DATE, "%Y-%m")) %>%
      group_by(DEALER_NAME, SALES_MONTH) %>%
      summarize(MONTHLY_TOTAL_SALES = sum(TOTAL_SALES), .groups = 'drop')
monthly_sales
dealer_growth <- monthly_sales %>%
      group_by(DEALER_NAME) %>%
      arrange(SALES_MONTH) %>%
      mutate(GROWTH = MONTHLY_TOTAL_SALES - lag(MONTHLY_TOTAL_SALES)) %>%
      filter(!is.na(GROWTH))
dealer_growth   
total_growth <- dealer_growth %>%
      group_by(DEALER_NAME) %>%
      summarize(TOTAL_GROWTH = sum(GROWTH), .groups = 'drop')
total_growth
max_growth_dealers <- total_growth %>%
      filter(TOTAL_GROWTH == max(TOTAL_GROWTH)) %>%
      pull(DEALER_NAME), max_growth_dealers)



# 2) Which are my poor performing offices?

library(dplyr)

poor_performing_offices <- df %>%
  group_by(AREA_OFFICE) %>%
  summarise(total_sales = sum(TOTAL_SALES, na.rm = TRUE),
            avg_sales_per_day = mean(TOTAL_SALES, na.rm = TRUE) / n_distinct(SALES_DATE)) %>%
  arrange(total_sales) %>%
  filter(total_sales <= quantile(total_sales, 0.25))
  poor_performing_offices
