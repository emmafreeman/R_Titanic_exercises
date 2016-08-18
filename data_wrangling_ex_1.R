#Springboard Foundations
#Data wrangling exercise #1

rm(list=ls())
getwd()
setwd("~/Desktop/R stuff/Springboard")

df <- read.csv("refine_original.csv")
head(df)
str(df)

library(tidyr)

#standardize company names (lowercase, correct spelling)
unique(df$company)
df$company <- tolower(df$company)
df$company <- gsub(".*ps$", "phillips", df$company)
df$company <- gsub("^ak.*", "akzo", df$company)
df$company <- gsub("^uni.*$", "unilever", df$company)

#Separate the product code and product number into separate columns
df <- separate(df, Product.code...number, c("product_code", "product_number"), sep = "-")

#add a column for product categories
df$product_categories <- c("")
# rearrange columns in correct order
df <- df[c("company", "product_code", "product_number", "product_categories", "address", "city", "country", "name")]
#add values for new column: p = Smartphone, v = TV, x = Laptop, q = Tablet
df$product_categories <- ifelse(df$product_code == "p", "Smartphone", 
                                ifelse(df$product_code == "v", "TV", 
                                       ifelse(df$product_code == "x", "Laptop", 
                                              ifelse(df$product_code == "q", "Tablet", 
                                                     NA))))
  
#Create a new column 'full_address' that unites the three 
#address fields, separated by commas
df <- unite(df, full_address, address, city, country, sep=", ")

#Add four binary (1 or 0) columns for company: 
#company_philips, company_akzo, company_van_houten and company_unilever
#Add four binary (1 or 0) columns for product category: 
#product_smartphone, product_tv, product_laptop and product_tablet
library(dplyr)
df <- mutate(df, company_phillips = ifelse(df$company == "phillips", 1, 0))
df <- mutate(df, company_akzo = ifelse(df$company == "akzo", 1, 0))
df <- mutate(df, company_van_houten = ifelse(df$company == "van houten", 1, 0))
df <- mutate(df, company_unilever = ifelse(df$company == "unilever", 1, 0))
df <- mutate(df, product_smartphone = ifelse(df$product_code == "p", 1, 0))
df <- mutate(df, product_tv = ifelse(df$product_code == "v", 1, 0))
df <- mutate(df, product_laptop = ifelse(df$product_code == "x", 1, 0))
df <- mutate(df, product_tablet = ifelse(df$product_code == "q", 1, 0))

#save the clean file as 'refine_clean.csv'
write.csv(df, 'refine_clean.csv')

