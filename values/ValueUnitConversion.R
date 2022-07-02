library(readxl)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(tidytext)
library(rvest)
library(fuzzyjoin)
library(debkeepr)
library(writexl)

#This R file fixes the value units, such that
#the final data set has a column that lists each textile shipment's
#total value as a decimal in the unit of Dutch Gulders (total_value_dutch_gulders).

#First, I import my data, in the process changing blank cells to be NA
textile.data.proto <- read.csv("final_data.csv", header = TRUE, na.strings = c(""))

#I change NAs to be 0 for use in debkeepr
textile.data.proto$total_value_penningen <- as.numeric(str_replace_na(textile.data.proto$total_value_penningen, 0))
textile.data.proto$total_value_stuivers <- as.numeric(str_replace_na(textile.data.proto$total_value_stuivers, 0))
textile.data.proto$total_value_guldens <- as.numeric(str_replace_na(textile.data.proto$total_value_guldens, 0))



#I then add a column of debkeepr lsd values, representing the total value in lsd
textile.data.proto <- mutate(textile.data.proto, lsd = deb_lsd(l = as.numeric(total_value_guldens),
                                                               s = as.numeric(total_value_stuivers),
                                                               d = as.numeric(total_value_penningen)))

#I now split my data into two sets, the first already in Dutch Gulders. I then
#convert the lsd values into a decimal values
textile.data.Dutch <- textile.data.proto %>%
  filter(tolower(total_value_currency) %in% c("dutch guilders", "guilders")) %>%
  mutate(total_value_dutch_gulders = as.numeric(deb_as_decimal(lsd)))

#The second data set is in Indian Gulders. Here, I convert the lsd value into
#a decimal, but then I multiply that by (10.5/15), which is 0.7, to convert
#the unit from Indian Gulders to Dutch Gulders.
textile.data.Indian <- textile.data.proto %>%
  filter(tolower(total_value_currency) %in% c("indian guilders")) %>%
  mutate(total_value_dutch_gulders = as.numeric(deb_as_decimal(lsd) * 0.7))

#Here, I concatenate the two data sets. Note that I have to drop off the 
#column that includes the lsd values, since rbind cannot handle that data
#type. This is fine, as I've already converted that column into 
#the total_value_dutch_gulders column as a numeric decimal
textile.data.cleaned <- rbind(textile.data.Dutch[ , -60], textile.data.Indian[ , -60])


#Finally, I save the cleaned data set
write_csv(textile.data.cleaned, "C:/Users/dpoul/OneDrive/Documents/Junior Year/Winter/Data Science/Data/final_data.csv")



