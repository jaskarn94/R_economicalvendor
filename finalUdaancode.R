# i have manually changed the lower cased values of zone, cod in df2. 
#When I was using a function of tolower, the class was getting converted from factor 
# to character, and I couldnt find a  way to reverse that.
library(dplyr)
df1 <- read.csv('file_01_current_suppy_network.csv')
df2 <- read.csv('file_02_cost_datacomma.csv')
df3 <- read.csv('file_03_logistics_vendor_availabilitycomma.csv')
df4 <- read.csv('file_04_zone_infocomm.csv')
tolower(df2$zone)
tolower(df2$mode)
tolower(df2$size)


df1 <- df1[!duplicated(df1[c(1:7)]),]
df2 <- df2[!duplicated(df2[c(1:6)]),]
# Removing duplicates from df3 table as it contains 28 duplicate values.
# check duplicates for only that columns which are kinda like primary key for that table.
df3 <- df3[!duplicated(df3[c(1:6)]),]
df4 <- df4[!duplicated(df4[c(1:3)]),]

#Merging of df1 and df2 on three conditions
j1 <- merge(x=df1,y=df2,by.x=c('shpment_size','payment_option','shipment_zone'),
    by.y=c('size','mode','zone'))

jv1 <- j1 %>%
  filter(current_logistics_vendor == 'v1') %>%
  mutate(total = product_quantity*cost_with_vendor_v1)

sum(jv1$total)
# 370038040

jv2 <- j1 %>%
  filter(current_logistics_vendor == 'v2') %>%
  mutate(total = product_quantity*cost_with_vendor_v2)
sum(jv2$total)
# 12276540

jv3 <- j1 %>%
     filter(current_logistics_vendor == 'v3') %>%
     mutate(total = product_quantity*cost_with_vendor_v3)
sum(jv3$total)
#12715930

jv4 <- j1 %>%
    filter(current_logistics_vendor == 'v4') %>%
   mutate(total = product_quantity*cost_with_vendor_v4)

sum(jv4$total)
# 19999380

present <- function(x){
  if(x['current_logistics_vendor'] == 'v1'){
    y <- (as.numeric(x['product_quantity']) * as.numeric(x['cost_with_vendor_v1']) )
  }else if(x['current_logistics_vendor'] == 'v2'){
    y <- (as.numeric(x['product_quantity']) * as.numeric(x['cost_with_vendor_v2']) )
  }else if(x['current_logistics_vendor'] == 'v3'){
    y <- (as.numeric(x['product_quantity']) * as.numeric(x['cost_with_vendor_v3']) )
  }else {
    y <- (as.numeric(x['product_quantity']) * as.numeric(x['cost_with_vendor_v4']) )
  }
}

j1$present_cost <- apply(j1,1,present)
# changing the value of yes and no to 1 and 0 so that we can perform multiplaces.

df3$v1_is_present <- if_else(df3$v1_is_present=='yes',1,0)

df3$v2_is_present <- if_else(df3$v2_is_present=='yes',1,0)

df3$v3_is_present <- if_else(df3$v3_is_present=='yes',1,0)

df3$v4_is_present <- if_else(df3$v4_is_present=='yes',1,0)

m1 <- merge(x=j1,y=df3,by.x=c('supplier_location','customer_location'),
            by.y = c('supplier_location','customer_location'))

m1 <- m1[!duplicated(m1[c(1:16)]),]
# these are the values where there is no vendor available.
#filter(m1,v1_is_present==0,v2_is_present==0,v3_is_present==0,v4_is_present==0)

m1 <- m1 %>% mutate(costf1 = cost_with_vendor_v1*v1_is_present)

m1<- m1 %>% mutate(costf2 = cost_with_vendor_v2*v2_is_present)

m1<- m1 %>% mutate(costf3 = cost_with_vendor_v3*v3_is_present)

m1<- m1 %>% mutate(costf4 = cost_with_vendor_v4*v4_is_present)

View(m1)
# to get index as another column
m1 <- tibble::rownames_to_column(m1,'ind')
View(filter(m1,costf1 ==0,costf2== 0,costf3 == 0,costf4== 0))


# dropped rows 35171,89139,89131 as these rows have no vendor available. Assigning 
# these values to a new table 
m1resd <- m1[c(35171,89130,89131),]
m1 <- m1[-c(35171,89130,89131),]

# This is to find the minimumcost among vendors excluding zero for every observation 
minimum <- function(x) {
  y <- c(x['costf1'],x['costf2'],x['costf3'],x['costf4'])
  y <- as.numeric(y)
  y<- y[y>0]
  min(y)
}

m1$code <- apply(m1,1,minimum)
View(m1)

m1 <- m1 %>%
  mutate(ecocost = product_quantity*code)

View(m1)
sum(m1$ecocost)
#358678150

#This is to find the vendor which will be economical for every row.
india <- function(x) {
  y <- c(x['costf1'],x['costf2'],x['costf3'],x['costf4'])
  y <- as.numeric(y)
  z <- as.numeric(x['code'])
  which(y==z, arr.ind = TRUE)
}

m1$eco_vendor <- apply(m1,1,india)
View(m1)

# this is used to export data from this table.
library(data.table)
fwrite(z1, file ="myDT.csv")
