########################################################################
########################################################################
########################################################################
###########   ---  Abundance Anomalies    ---   ########################
########################################################################
########################################################################
# Yi = log10(ind/10m2 + 1)
# Yi = (Yi - mean(Yc))/sd(Yc)
# Yc,reg = cruise mean Yi,region
# Yc cruise mean of Yi within region 
# Y anom = (Yc - mean(Yc,yr)) / (SD)Yc

# euph_10m2 is the sum of taxa codes 2000-2099 and euph1_10m2 is just 
# unidentified euphausicea (just taxa code 2000) -Harvey 

## Packages
library(tidyverse)

## Data 
zp <- read.csv("output/EcoMon_SubEUPHAU_AllRegions_m2_long.csv")
# this csv was created in m2_euphausiid_subset_csv_06JUL script
#############################################################
zp$Date <- as.Date(zp$date, format = "%d-%b-%y")

zp <- zp %>%
  mutate(PA = case_when(ind_10m2 > 0 ~ 1,
                        ind_10m2 <= 0 ~ 0))
#############################################################
#############################################################
##################            SNE             ###############
#############################################################
# Select Southern New England Region 
SNE <- zp %>% filter(Region == "SNE")

# check
SNE %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = ind_10m2)) +
  geom_point()
#############################################################

# 1) add 1 and take log
SNE$Yi <- log10(SNE$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1

# check
SNE %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yi)) +
  geom_point()
#############################################################

# Yc cruise mean of Yi within region 
# 2) take mean by cruise for each taxa within SNE
SNE1 <- SNE %>%
  group_by(cruise_name, year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(cruise_name, year, taxa, .keep_all = T)

# check
SNE1 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()
#############################################################

# 3) calculate mean and SD of Yc by year
SNE2 <- SNE1 %>%
  group_by(year, taxa) %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) 

# check
SNE2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()
SNE2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
SNE2 <- SNE2 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

# check
SNE2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point()

#SNE2aa$Anomaly_yr[is.na(SNE2aa$Anomaly_yr)] <- 0 #probably dont do this
########################################################################
# getting NA values probably because diving by very small SD or 0 
##### OPTION 1 : add a small constant
SNE2b <- SNE2
SNE2b <- SNE2 %>%
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr + 1e-6)) 

SNE2b %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point()
#############################################################
##### OPTION 2 : interpolation
library("imputeTS")
SNE2a <- SNE2
SNE2a$Anomaly_yr <- na_interpolation(SNE2a$Anomaly_yr)

SNE2a %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point()
#############################################################
##### OPTION 3 : build linear model using data and then use that to replace NAs
# build linear model based on existing data (model ignores rows with NAs)
m = lm(Anomaly_yr ~ Date, data = SNE2)

# add predictions as a column
SNE2$pred_value = predict(m, newdata = SNE2)

# replace (only) NAs with predictions
SNE2$interp_value = ifelse(is.na(SNE2$Anomaly_yr), SNE2$pred_value, SNE2$Anomaly_yr)

# plot existing and interpolated data
ggplot()+
  geom_point(data=SNE2, aes(Date, Anomaly_yr), size=5)+
  geom_point(data=SNE2, aes(Date, interp_value), col="red")

SNE2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = interp_value)) +
  geom_point()
########################################################################
########################################################################
#############################################################
######     ---   Meganyctiphanes norvegica  ---    ##########
#############################################################
#############################################################
megan <- SNE2a %>% filter(taxa == "megan")

hist(megan$ind_10m2)
hist(megan$Yi)
hist(log10(megan$ind_10m2+1))
hist(megan$Yc)

ggplot(megan, aes(x = Date, y = ind_10m2)) +
  geom_point()
ggplot(megan, aes(x = Date, y = Yi)) +
  geom_point()
ggplot(megan, aes(x = Date, y = Yc)) +
  geom_point()
ggplot(megan, aes(x = Date, y = YcAV_yr)) +
  geom_point()
ggplot(megan, aes(x = Date, y = YcSD_yr)) +
  geom_point()
ggplot(megan, aes(x = Date, y = Anomaly_yr)) +
  geom_point()
ggplot(megan, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point()

################################################################
################################################################
################################################################
#####      -----  OVERLAY WITH CLIMATIC INDEXES   ------   #####
################################################################
#####      -----           AMO            --------        ######
## Data
amo <- read.csv("raw/amo_month_NCAR_filtered.csv") #from amo script
amo <- amo %>% filter(yr >= 1977 & yr <= 2020)

amo$Date <- zoo::as.yearmon(paste(amo$yr, amo$month), "%Y %b")
amo$Date <- format(amo$Date, '%Y-%m-%01')
amo$Date <- as.Date(amo$Date, '%Y-%m-%d')

# amo by yr
amo_yr <- amo %>%
  group_by(yr) %>%
  mutate(meanAmo = mean(amo)) %>%
  distinct(yr, .keep_all = TRUE)

colnames(amo_yr)[1] <- "year"

amo <- amo %>%
  mutate(col = factor(ifelse(amo > 0, 2, 1)))

ggplot(data = amo, aes(x = Date, y = amo, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)

meganAnom <- ggplot(data = megan, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

amoAnom <- ggplot(data = amo, aes(x = Date, y = amo)) +
  geom_line() +
  geom_point() +
  theme_classic() 

amoMeg <- ggplot() +
  geom_line(data = amo, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megan, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amo, aes(x = Date, y = amo), color = "red") +
  geom_point(data = megan, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

ggplot() +
  geom_line(data = amo_yr, aes(x = Date, y = meanAmo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megan, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amo_yr, aes(x = Date, y = meanAmo), color = "red") +
  geom_point(data = megan, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

library(cowplot)

plot1 <- plot_grid(meganAnom, amoAnom,amoMeg, nrow=3)
#ggsave("MeganSNEamo1.png", plot1, width = 10, height = 10, dpi = 300)

#amofil <- amo %>% filter(yr > 2015)
#meganfil <- megan %>% filter(year>2015)
#ggplot() +
#  geom_line(data = amofil, aes(x = Date, y = amo), color = "red",
#            linewidth = 1.2) +
#  geom_line(data = meganfil, aes(x = Date, y = Anomaly_yr),
#            linewidth = 0.8) +
#  geom_point(data = amofil, aes(x = Date, y = amo), color = "red") +
#  geom_point(data = meganfil, aes(x = Date, y = Anomaly_yr)) +
#  theme_classic() + 
#  ggtitle("SNE - Meganyctiphanes norvegica")


##########################################################
##########################################################
#####      -----           NAO            --------        ######
## Data
nao <- read.csv("raw/nao_month_l.csv") #from nao script
nao <- nao %>% filter(yr >= 1977 & yr <= 2020)

nao$Date <- zoo::as.yearmon(paste(nao$yr, nao$month), "%Y %b")
nao$Date <- format(nao$Date, '%Y-%m-%01')
nao$Date <- as.Date(nao$Date, '%Y-%m-%d')

# nao by yr
nao_yr <- nao %>%
  group_by(yr) %>%
  mutate(meanNao = mean(nao)) %>%
  distinct(yr, .keep_all = TRUE)

colnames(nao_yr)[1] <- "year"

nao <- nao %>%
  mutate(col = factor(ifelse(nao > 0, 2, 1)))

ggplot(data = nao, aes(x = Date, y = nao, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)

naoAnom <- ggplot(data = nao, aes(x = Date, y = nao)) +
  geom_line() +
  geom_point() +
  theme_classic() 

naoMeg <- ggplot() +
  geom_line(data = nao, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = megan, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = nao, aes(x = Date, y = nao), color = "red") +
  geom_point(data = megan, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

ggplot() +
  geom_line(data = nao_yr, aes(x = Date, y = meanNao), color = "red",
            linewidth = 1.2) +
  geom_line(data = megan, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = nao_yr, aes(x = Date, y = meanNao), color = "red") +
  geom_point(data = megan, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

plot2 <- plot_grid(meganAnom, naoAnom, naoMeg, nrow=3)
#ggsave("MeganSNEnao.png", plot2, width = 10, height = 10, dpi = 300)


##########################################################
##########################################################
#####      -----           GSI            --------        ######
#remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
library(ecodata)

## Data
data(package="ecodata")
gsi <- ecodata::gsi

# Fix date column 
gsi$Date <- as.Date(sprintf("%.2f.01", gsi$Time), format = "%Y.%m.%d")

# Create a new column for the year
gsi$yr <- year(gsi$Date)

gsi <- gsi %>% filter(yr >= 1977 & yr <= 2020)

gsiAnom <- ggplot(gsi, aes(x = Date, y = Value)) +
  geom_line() + 
  geom_point() +
  theme_classic() +
  labs(y = "GSI") 
  #geom_hline(yintercept = 0)

gsiMeg <- ggplot() +
  geom_line(data = gsi, aes(x = Date, y = Value), color = "red",
            linewidth = 1.2) +
  geom_line(data = megan, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = gsi, aes(x = Date, y = Value), color = "red") +
  geom_point(data = megan, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")


plot3 <- plot_grid(meganAnom, gsiAnom, gsiMeg, nrow=3)
#ggsave("MeganSNEgsi.png", plot3, width = 10, height = 10, dpi = 300)






##########################################################
##########################################################
#####      -----      SLOPEWATER            --------        ######
# WSW = warm slope water
# LSLW = Labrador slope water 
#remotes::install_github("noaa-edab/ecodata",build_vignettes=TRUE)
library(ecodata)
slopewater <- ecodata::slopewater

ggplot(slopewater, aes(x = Time, y = Value, color = Var)) +
  geom_line() + 
  geom_point() +
  theme_classic() +
  labs(y = "GSI") 


############ chl a 
############ SSTs - surface and bottom temps - netCDF file
############ Heatwaves
############ Mixed layer depth 
############ AO 

#############################################################
#######      -----      Euphausiacea      -----    ##########
#############################################################

##########################################################
##########################################################
euph <- SNE2a %>% filter(taxa == "euph")

ggplot(euph, aes(x = Date, y = ind_10m2)) +
  geom_point()
ggplot(euph, aes(x = Date, y = Yi)) +
  geom_point()
ggplot(euph, aes(x = Date, y = Yc)) +
  geom_point()
ggplot(euph, aes(x = Date, y = YcAV_yr)) +
  geom_point()
ggplot(euph, aes(x = Date, y = YcSD_yr)) +
  geom_point()
ggplot(euph, aes(x = Date, y = Anomaly_yr)) +
  geom_point()
ggplot(euph, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point()


#####      -----           AMO            --------        ######
euphAnom <- ggplot(data = euph, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - EUPH")

amoEuph <- ggplot() +
  geom_line(data = amo, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = euph, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amo, aes(x = Date, y = amo), color = "red") +
  geom_point(data = euph, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - EUPH")

ggplot() +
  geom_line(data = amo_yr, aes(x = Date, y = meanAmo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megan, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amo_yr, aes(x = Date, y = meanAmo), color = "red") +
  geom_point(data = megan, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

plot4 <- plot_grid(euphAnom, amoAnom,amoEuph, nrow=3)
#ggsave("EuphSNEamo.png", plot4, width = 10, height = 10, dpi = 300)


#####      -----           NAO            --------        ######
naoEuph <- ggplot() +
  geom_line(data = nao, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = euph, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = nao, aes(x = Date, y = nao), color = "red") +
  geom_point(data = euph, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - EUPH")

ggplot() +
  geom_line(data = nao_yr, aes(x = Date, y = meanNao), color = "red",
            linewidth = 1.2) +
  geom_line(data = euph, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = nao_yr, aes(x = Date, y = meanNao), color = "red") +
  geom_point(data = euph, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() + 
  ggtitle("SNE - EUPH")

plot5 <- plot_grid(euphAnom, naoAnom, naoEuph, nrow=3)
#ggsave("EuphSNEnao.png", plot5, width = 10, height = 10, dpi = 300)



#############################################################
######     ----    	Euphausia  krohnii   ----      ##########
#############################################################

#############################################################
######     -----   Thysanoessa gregaria   -----    ##########
#############################################################

#############################################################
######    ----   Thysanoessa longicaudata   ----    #########
#############################################################

#############################################################
######    ----    Nematoscelis megalops    ----     #########
#############################################################

#############################################################
######    -----    Thysanoessa raschii    -----     #########
#############################################################

#############################################################
######     ----     Thysanoessa inermis    ----    ##########
#############################################################

#############################################################
#######     -----     Thysanoessa spp.    -----    ##########
#############################################################

#############################################################
#######      -----     Euphausia spp.    -----     ##########
#############################################################

#############################################################
#######     -----     Stylocheiron spp.    -----   ##########
#############################################################


#############################################################
#######      -----     Euphausiacea1      -----    ##########
#############################################################


##########################################################
##########################################################
##########################################################
# trying bar plot anomaly 

megan <- megan %>%
  mutate(col = factor(ifelse(Anomaly_yr > 0, 2, 1)))

ggplot(data = megan, aes(x = Date, y = Anomaly_yr)) +
  geom_bar(stat="identity", color = "red")

ggplot(megan, aes(x= Date, y= Anomaly_yr, color = col)) + 
  geom_bar(stat = "identity", 
           #position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept=0, size=0.8)

amo <- amo %>%
  mutate(col = factor(ifelse(amo > 0, 2, 1)))

ggplot(data = amo, aes(x = Date, y = amo, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)

ggplot(data = amo, aes(x = Date, y = amo, color = col)) +
  geom_segment(aes(xend = Date, yend = 0), size = 0.6)

ggplot(data = amo, aes(x = Date, y = amo, color = col)) + #same thing
  geom_segment(aes(xend = Date, y = 0, yend = amo), size = 0.6)
##########################################################



### need to remove seasonality somehow 
## seasonal decomposition 
## decompose or st1 functions 

## differencing 
# diff function 


# Perform seasonal decomposition using stl
decomposed <- stl(megan$Anomaly_yr, s.window="periodic")

# Remove the seasonal component to get seasonally adjusted data
seasonally_adjusted_data <- seasadj(decomposed)

# Convert your data into a time series object (assuming it's not already)
zooplankton_ts <- ts(megan$Anomaly_yr, frequency = 4)  # Assuming quarterly data

# Perform seasonal decomposition using decompose
decomposed <- decompose(zooplankton_ts)

# Remove the seasonal component to get seasonally adjusted data
seasonally_adjusted_data <- zooplankton_ts - decomposed$seasonal


########################################################################
######getting 1 value per year for zp anomalies 
megana <- megan %>% 
  group_by(year) %>%
  mutate(MedAnom = median(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

megana <- megana %>%
  mutate(col = factor(ifelse(MedAnom > 0, 2, 1)))

ggplot(megana, mapping = aes(x= Date, y= MedAnom, 
                             color = col,
                             fill = col)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           show.legend=FALSE) +
  geom_hline(yintercept=0, size=1)
########################################################################
########################################################################



########################################################################

################################################################################
################################################################################
megancrui <- megan %>%
  group_by(cruise_name, year, month) %>%
  mutate(PA = max(PA)) %>%
  distinct(cruise_name, year, month, .keep_all = TRUE) 

megancrui$month <- factor(megancrui$month, levels = month.abb)

ggplot(meganSNE, aes(x = year, y= cruise_name)) +
  geom_tile(aes(fill = as.factor(PA)))

ggplot(megancrui, aes(x = year, y= month, color = "black")) +
  geom_tile(aes(fill = as.factor(PA))) +
  scale_fill_manual(values = c("black","darkgreen")) +
  theme_classic() +
  theme(axis.title = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)))

ggplot(megancrui, aes(x = year, y= month)) +
  geom_tile(aes(fill = ind_10m2)) +
  #scale_fill_manual(values = c("black","darkgreen")) +
  theme_classic() +
  theme(axis.title = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)))

ggplot(megancrui, aes(x = year, y= month)) +
  geom_tile(aes(fill = Yi)) +
  #scale_fill_manual(values = c("black","darkgreen")) +
  theme_classic() +
  theme(axis.title = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)))



################################################################################
#################    BAD     IGNORE    ###############################
################################################################################
################################################################################
################################################################################
######getting 1 value per year for zp anomalies 
# tried getting 1 value per yr by calculating the 
# 95th percentile but doesnt work bc values end up being almost always positive
# too much variation in data 
megana <- megan %>% 
  group_by(year) %>%
  mutate(Anomaly_95th = quantile(Anomaly_yr, 0.95, na.rm = T)) %>%
  distinct(year, .keep_all = T)

megana <- megana %>%
  mutate(col = factor(ifelse(Anomaly_95th > 0, 2, 1)))

ggplot(megana, mapping = aes(x= Date, y= Anomaly_95th, 
                             color = col,
                             fill = col)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           show.legend=FALSE) +
  geom_hline(yintercept=0, size=1)



################################################################################
################################################################################
## AVERAGE AND SD OF WHOLE TIME SERIES OR PER YEAR???????
# trying it by mean abundance over whole time series instead of by year..
# average for January since begin of ecomon
################################################################
#SNE1a <- SNE %>%
#  group_by(cruise_name, year, taxa) %>%
#  mutate(Yc = mean(Yi)) 

#SNE2ab <- SNE1a %>%
#  group_by(taxa) %>%
#  mutate(YcAV_yr = mean(Yc), 
#         YcSD_yr = sd(Yc)) 

#SNE2ab <- SNE2ab %>%
#  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

#SNE2ab %>% filter(taxa == "megan") %>%
#  ggplot(aes(x = Date, y = Anomaly_yr)) +
#  geom_point()





##########################################################
SNE3a <- SNE1 %>%
  group_by(taxa) %>%
  mutate(YcAV_timeSer = mean(Yi), 
         YcSD_timeSer = sd(Yi)) 

SNE3a <- SNE3a %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_timeSer = (Yc - YcAV_timeSer)/(YcSD_timeSer)) 

meganSNE3a <- SNE3a %>% filter(taxa == "megan") #do i need to distinct? 

ggplot() +
  geom_line(data = amo, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = meganSNE3a, aes(x = Date, y = Anomaly_timeSer),
            linewidth = 1) +
  geom_point(data = amo, aes(x = Date, y = amo), color = "red") +
  geom_point(data = meganSNE3a, aes(x = Date, y = Anomaly_timeSer)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

ggplot(meganSNE3a, aes(x = Date, y = ind_10m2)) +
  geom_point()
ggplot(meganSNE3a, aes(x = Date, y = Yi)) +
  geom_point()
ggplot(meganSNE3a, aes(x = Date, y = Anomaly_timeSer)) +
  geom_point()

SNE2aa <- SNE2a %>%
  group_by(cruise_name, year, taxa) %>%
  mutate(YcAV_yr_cr = mean(Yi), 
         YcSD_yr_cr = sd(Yi)) 

SNE2aa <- SNE2aa %>%
  group_by(cruise_name, year, taxa) %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) 

SNE2a <- SNE2a %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

#SNE2a$Anomaly_yr_test <- (SNE2a$Yc - SNE2a$YcAV_yr)/(SNE2a$YcSD_yr)
#I get the same values doing it this way, 
# just checking that my dplyr approach was right 

SNE2a <- SNE2a %>%
  #group_by(year, taxa) %>%
  mutate(Anomaly_timeSer = (Yc - YcAV_timeSer)/(YcSD_timeSer)) 
