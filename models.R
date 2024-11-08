library(patchwork)
library(tidyverse)
library(dplyr)

dataset <- read.csv('laptop_data_cleaned.csv')
col_names <- names(dataset)
dim_data <- dim(dataset)
col_names
dim_data

head(dataset,2)

min_price <- min(dataset$Price)
max_price <- max(dataset$Price)
mean_price <- mean(dataset$Price)
quants_price <- quantile(dataset$Price,probs=c(0.25,0.5,0.75))
var_price <- var(dataset$Price)
std_price <- var_price**0.5

min_price
max_price
mean_price
quants_price
var_price
std_price


min_Ppi <- min(dataset$Ppi)
max_Ppi <- max(dataset$Ppi)
mean_PPi <- mean(dataset$Ppi)
quants_ppi <- quantile(dataset$Ppi,probs=c(0.25,0.5,0.75))
min_Ppi
max_Ppi
mean_PPi
quants_ppi

min_ips <- min(dataset$Ips)
max_ips <- max(dataset$Ips)
mean_ips <- mean(dataset$Ips)
quants_ips <- quantile(dataset$Ips,probs=c(0.25,0.5,0.75))
var_ips <- var(dataset$Ips)
std_ips <- var_ips**0.5
min_ips
max_ips
quants_ips
var_ips
std_ips

min_weight <- min(dataset$Weight)
max_weight <- max(dataset$Weight)
mean_weight <- mean(dataset$Weight)
quants_weight <- quantile(dataset$Weight,probs=c(0.25,0.5,0.75))
var_weight<- var(dataset$Weight)
std_weight <- var_ips**0.5

min_weight
max_weight
mean_weight
quants_weight
var_weight
std_weight


head(dataset,2)

corr_price_ppi <- cor(dataset$Price,dataset$Ppi)
corr_price_ppi

corr_price_weight <- cor(dataset$Price,dataset$Weight)
corr_price_weight

comp_table <- table(dataset$Company)
comp_table

comp_props <- prop.table(comp_table)
comp_props

mode_company <- names(which.max(comp_props))
mode_company

typename_table <- table(dataset$TypeName)
typename_table

typename_props <- prop.table(typename_table)
typename_props

mode_type <- names(which.max(typename_props))
mode_type

touch_table <- table(dataset$TouchScreen)
touch_table

touch_props <- prop.table(touch_table)
touch_props

mode_touch <- names(which.max(touch_props))
mode_touch

cpu_table <- table(dataset$Cpu_brand)
cpu_props <- prop.table(cpu_table)
cpu_mode <- names(which.max(cpu_props))

cpu_table
cpu_props
cpu_mode

gpu_table <- table(dataset$Gpu_brand)
gpu_props <- prop.table(gpu_table)
gpu_mode <- names(which.max(gpu_props))
gpu_table
gpu_props
gpu_mode

hdd_table <- table(dataset$HDD)
hdd_props <- prop.table(hdd_table)
hdd_mode <- names(which.max(hdd_props))
hdd_table
hdd_props
hdd_mode

ssd_table <- table(dataset$SSD)
ssd_props <- prop.table(ssd_table)
ssd_mode <- names(which.max(ssd_props))
ssd_table
ssd_props
ssd_mode



os_table <- table(dataset$Os)
os_props <- prop.table(os_table)
os_mode  <- names(which.max(os_props))
os_table
os_props
os_mode


head(dataset,2)



comp_price <- aov(Price~Company,data=dataset)
attributes(comp_price)
comp_price

tukey_summary <- TukeyHSD(comp_price)
tukey_summary


type_price <- aov(Price~TypeName,data=dataset)
type_price

tukey_type <- TukeyHSD(type_price)
tukey_type
dataset$Ram <- as.factor(dataset$Ram)
ram_price <- aov(Price~Ram,data=dataset)
ram_price

tukey_ram <-TukeyHSD(ram_price)
tukey_ram

cpu_price <- aov(Price~Cpu_brand,data=dataset)
cpu_price

tukey_cpu <- TukeyHSD(cpu_price)
tukey_cpu

dataset$HDD <- as.factor(dataset$HDD)
hdd_price <-aov(Price~HDD,data=dataset)
tukey_hdd <- TukeyHSD(hdd_price)
hdd_price
tukey_hdd

dataset$SSD <- as.factor(dataset$SSD)
sdd_price <- aov(Price~SSD,data=dataset)
sdd_price

tukey_ssd <- TukeyHSD(sdd_price)
tukey_ssd

gpu_price <- aov(Price~Gpu_brand,data=dataset)
tukey_gpu <- TukeyHSD(gpu_price)
gpu_price
tukey_gpu

os_price<- aov(Price~Os,data=dataset)
os_price
tukey_os <- TukeyHSD(os_price)
tukey_os


boxplot(dataset$Price,main='Boxplot For Price',
        ylab='Prices',col='red')
boxplot(dataset$Weight ,main='Boxplot For Weight',
        ylab='Weight',col='red')


q1_price <- quantile(dataset$Price,0.25)
q3_price <- quantile(dataset$Price,0.75)
IQR_price <- q3_price-q1_price

dataset$Price[dataset$Price>q3_price+1.5*IQR_price] <- mean_price

q1_weight <- quantile(dataset$Weight,0.25)
q3_weight <- quantile(dataset$Weight,0.75)
IQR_weight <- q3_weight -q1_weight
dataset$Weight[dataset$Price>q3_weight+1.5*IQR_weight] <- mean_weight
        
boxplot(dataset$Price,main='Boxplot For Price',
        ylab='Prices',col='red')
boxplot(dataset$Weight ,main='Boxplot For Weight',
        ylab='Weight',col='red')

model <- aov(Price~Weight*Os,data=dataset)
model
tukey_weight_os <- TukeyHSD(model)
tukey_weight_os

plot(tukey_weight_os)
head(dataset,2)
model <- aov(Price~Weight*Gpu_brand,data=dataset)
model

tukey_weight_gpu <- TukeyHSD(model)
tukey_weight_gpu
plot(tukey_weight_gpu)


model  <- aov(Price~Weight*Cpu_brand,data=dataset)
model

tukey_weight_cpu <- TukeyHSD(model)
tukey_weight_cpu

