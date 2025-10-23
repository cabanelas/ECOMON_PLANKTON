#############################################################
#############################################################
#############################################################
###########   ---  Abundance Anomalies    ---   #############
#############################################################
#############################################################

## Packages
library(tidyverse)

## Data 
zp <- read.csv("output/EcoMon_SubEUPHAU_AllRegions_m2_long.csv")
# this csv was created in m2_euphausiid_subset_csv_06JUL script

zp$Date <- as.Date(zp$date, format = "%d-%b-%y")
#############################################################
# 1) add 1 and take log
zp$abuLog <- log10(zp$ind_10m2+1)



zp %>% 
  group_by(Region, TAXA_NAME) %>%
  summarise(meantax = mean(abuLog)) %>%
  distinct(Region, TAXA_NAME, .keep_all = T) %>%
  arrange(desc(meantax))
#############################################################
###############    first attempt - wrong? ###################
#############################################################
# 2) anomalies
#need mean log abun of whole time series by taxa by region
zp1 <- zp %>%
  group_by(cruise_name, Region, TAXA_NAME) %>%



zp1 <- zp %>%
  group_by(Region, TAXA_NAME) %>%
  mutate(meanLog = mean(abuLog),
         sdLog = sd(abuLog))

zp1 <- zp1 %>%
  group_by(Region, TAXA_NAME) %>%
  mutate(anomaly = (abuLog-meanLog)/sdLog)

#zp1$anomaly[is.na(zp1$anomaly)] <- 0

zp1 <- zp1 %>%
  mutate(anomBoun = case_when(anomaly >= 1 ~ 1,
                              anomaly <= -1 ~ -1,
                              TRUE ~ anomaly))

###############################################################################
## to check - look at one taxa
megancheck <- zp1 %>% 
  filter(taxa == "megan" & Region == "SNE")

ggplot(megancheck, aes(x = Date, y = anomaly)) +
  geom_point()

ggplot(megancheck, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic()  ## looks weirddddddddd high anomalies 

zp1a <- zp1 %>%
  distinct(Region, year, TAXA_NAME, .keep_all = TRUE)

megancheck <- zp1a %>% 
  filter(taxa == "megan" & Region == "SNE")

ggplot(megancheck, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica") ## looks weirddddddddd high anomalies 
###############################################################################
megancheckm <- megancheck %>%
  group_by(cruise_name, Region, taxa) %>%
  mutate(meanano = mean(anomaly)) %>%
  distinct(cruise_name, Region, taxa, .keep_all = TRUE)

ggplot(megancheckm, aes(x = Date, y = meanano)) +
  geom_point()

ggplot(megancheckm, aes(x = Date, y = meanano)) +
  geom_line()




zp7 <- zp %>%
  group_by(cruise_name, Region, TAXA_NAME) %>%
  mutate(meanLog = mean(abuLog),
         sdLog = sd(abuLog))

zp7 <- zp7 %>%
  group_by(cruise_name, Region, TAXA_NAME) %>%
  mutate(anomaly = (abuLog-meanLog)/sdLog)


zp7meg <- zp7 %>% filter(taxa == "megan" & Region == "SNE")
ggplot(zp7meg, aes(x = Date, y = anomaly)) +
  geom_point()


##############################
# by taxa by region by cruise, is it diff? 
zp2 <- zp %>%
  group_by(cruise_name, Region, TAXA_NAME) %>%
  mutate(meanLog = mean(abuLog),
         sdLog = sd(abuLog))

zp2 <- zp2 %>%
  #group_by(cruise_name, Region, TAXA_NAME) %>%
  mutate(anomaly = (abuLog-meanLog)/sdLog)

zp2$anomaly[is.na(zp2$anomaly)] <- 0

zp2a <- zp2 %>%
  distinct(cruise_name, Region, TAXA_NAME, .keep_all = TRUE)
###############################################################################
## to check - look at one taxa
megancheck2 <- zp2 %>% # no distinct in data - LOTS OF VALS
  filter(taxa == "megan" & Region == "SNE")

ggplot(megancheck2, aes(x = Date, y = anomaly)) +
  geom_point()

ggplot(megancheck2, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic()  ## looks weirddddddddd high anomalies 
##
megancheck2 <- zp2a %>%  #after removing repeated values/grouping
  filter(taxa == "megan" & Region == "SNE")

ggplot(megancheck2, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica") ## looks weirddddddddd high anomalies 
###############################################################################

###############################################################################
###############################################################################
###############################################################################

# 3) take means - to plot just log abundance 

## first take mean by cruise + region 
zp_av_cru <- zp %>%
  group_by(cruise_name, Region, year, TAXA_NAME) %>%
  mutate(mean_log = mean(abuLog)) %>%
  distinct(cruise_name, Region, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup()

### average region + yr
zp_av_reg <- zp_av_cru %>%
  group_by(Region, year, TAXA_NAME) %>%
  mutate(mean_log = mean(mean_log)) %>%
  distinct(Region, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup()


# but I should probably calculate this same way as anomalies (below) by month
zp_av_month <- zp %>%
  group_by(month, Region, year, TAXA_NAME) %>%
  mutate(momean_log = mean(abuLog)) %>%
  distinct(month, Region, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup()

# grouping some more (like done before) - just getting yearly
zp_av_month_yr <- zp_av_month %>%
  group_by(Region, year, TAXA_NAME) %>%
  mutate(mean_logov = mean(momean_log)) %>%
  distinct(Region, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup()

zp_alast <- zp %>%
  group_by(Region, year, TAXA_NAME) %>%
  mutate(mean_logov = mean(abuLog)) %>%
  distinct(Region, year, TAXA_NAME, .keep_all = TRUE) %>%
  ungroup()

#############################################################
###############    monthly anomalies      ###################
#############################################################
# take mean log by cruise+region+species
#zp1 <- zp %>% 
#  group_by(cruise_name, Region, TAXA_NAME) %>%
#  mutate(meanabuLog = mean(abuLog)) %>%
#  distinct(cruise_name, Region, TAXA_NAME, .keep_all = TRUE)

# calculate mean and sd for anomalies - (monthly) time series
zp_anom <- zp %>% 
  group_by(month, Region, TAXA_NAME) %>%
  mutate(meanLOGa = mean(abuLog),
         sdLog = sd(abuLog))

# calculate mean log of abundance by month + yr + region + taxa
zp_anoma <- zp_anom %>% 
  group_by(month, year, Region, TAXA_NAME) %>%
  mutate(meanabuLog = mean(abuLog)) %>%
  distinct(month, year, Region, TAXA_NAME, .keep_all = TRUE)

# calculate monthly anomaly
zp_anoma1 <- zp_anoma %>% 
  #group_by(Region, TAXA_NAME) %>% does this make a difference??
  mutate(anomaly = (meanabuLog-meanLOGa)/sdLog) 

# calculate yearly anomaly (one value per yr) -- NEED TO CHECK ABOUT NAs
yrly <- zp_anoma1 %>%
  group_by(Region, year, TAXA_NAME) %>%
  mutate(anomalyyr = mean(anomaly, na.rm = TRUE)) %>% ##### MAYBE DONT IGNORE NAS!!!
  distinct(Region, year, TAXA_NAME, .keep_all = TRUE)
###############################################################################
## to check - look at one taxa
megancheck3 <- zp_anoma1 %>% # monthly anom
  filter(taxa == "megan" & Region == "SNE")

ggplot(megancheck3, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica") 
##
megancheck3 <- yrly %>%  # yrly anom
  filter(taxa == "megan" & Region == "SNE")

ggplot(megancheck3, aes(x = Date, y = anomalyyr)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica") 
###############################################################################






#############################################################
#############################################################
##################            SNE             ###############
#############################################################
#############################################################
#############################################################


#############################################################
######     ---   Meganyctiphanes norvegica  ---    ##########
#############################################################
meg_SNE_cru <- zp_av_cru %>%
  filter(taxa == "megan" & Region == "SNE")

a <- ggplot(meg_SNE_cru, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

meg_SNE_reg <- zp_av_reg %>%
  filter(taxa == "megan" & Region == "SNE")

b <- ggplot(meg_SNE_reg, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

meg_SNE_month <- zp_av_month %>%
  filter(taxa == "megan" & Region == "SNE")

c <- ggplot(meg_SNE_month, aes(x = Date, y = momean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

meg_SNE_month_yr <- zp_av_month_yr %>%
  filter(taxa == "megan" & Region == "SNE")

d <- ggplot(meg_SNE_month_yr, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

library(cowplot)
plot_grid(a,b,c,d)

meg_SNE_alast <- zp_alast %>%
  filter(taxa == "megan" & Region == "SNE")
ggplot(meg_SNE_alast, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")

#
#### plot anomalies
meg_anomon <- zp_anoma1 %>%
  filter(taxa == "megan" & Region == "SNE")

aa <- ggplot(meg_anomon, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica \nMonthly Anomalies")

meg_anoyr <- yrly %>%
  filter(taxa == "megan" & Region == "SNE")

aa1 <- ggplot(meg_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica \nYearly Anomalies")


plot_grid(aa, aa1, nrow=2)

ggplot(meg_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Meganyctiphanes norvegica \nYearly Anomalies")

#############################################################
######     ----    	Euphausia  krohnii   ----      ##########
#############################################################
kro_SNE <- zp_av_cru %>%
  filter(taxa == "euphkr" & Region == "SNE")

a <- ggplot(kro_SNE, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii")

kro_SNE_reg <- zp_av_reg %>%
  filter(taxa == "euphkr" & Region == "SNE")

b <- ggplot(kro_SNE_reg, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii")

kro_SNE_month <- zp_av_month %>%
  filter(taxa == "euphkr" & Region == "SNE")

c <- ggplot(kro_SNE_month, aes(x = Date, y = momean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii")

kro_SNE_month_yr <- zp_av_month_yr %>%
  filter(taxa == "euphkr" & Region == "SNE")

d <- ggplot(kro_SNE_month_yr, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii")

plot_grid(a,b,c,d)

kro_SNE_alast <- zp_alast %>%
  filter(taxa == "euphkr" & Region == "SNE")

ggplot(kro_SNE_alast, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii")

#
#### plot anomalies
kro_anomon <- zp_anoma1 %>%
  filter(taxa == "euphkr" & Region == "SNE")

ba <- ggplot(kro_anomon, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii \nMonthly Anomalies")

kro_anoyr <- yrly %>%
  filter(taxa == "euphkr" & Region == "SNE")

ba1 <- ggplot(kro_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii \nYearly Anomalies")


plot_grid(ba, ba1, nrow=2)

ggplot(kro_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Euphausia krohnii \nYearly Anomalies")



#############################################################
######     -----   Thysanoessa gregaria   -----    ##########
#############################################################
greg_SNE <- zp_av_cru %>%
  filter(taxa == "thysgr" & Region == "SNE")


a <- ggplot(greg_SNE, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria")

greg_SNE_reg <- zp_av_reg %>%
  filter(taxa == "thysgr" & Region == "SNE")


b <- ggplot(greg_SNE_reg, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria")

greg_SNE_month <- zp_av_month %>%
  filter(taxa == "thysgr" & Region == "SNE")

c <- ggplot(greg_SNE_month, aes(x = Date, y = momean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria")

greg_SNE_month_yr <- zp_av_month_yr %>%
  filter(taxa == "thysgr" & Region == "SNE")

d <- ggplot(greg_SNE_month_yr, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria")

plot_grid(a,b,c,d)


greg_SNE_alast <- zp_alast %>%
  filter(taxa == "thysgr" & Region == "SNE")
ggplot(greg_SNE_alast, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria")


#
#### plot anomalies
greg_anomon <- zp_anoma1 %>%
  filter(taxa == "thysgr" & Region == "SNE")

ca <- ggplot(greg_anomon, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria \nMonthly Anomalies")

greg_anoyr <- yrly %>%
  filter(taxa == "thysgr" & Region == "SNE")

ca1 <- ggplot(greg_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria \nYearly Anomalies")

plot_grid(ca, ca1, nrow=2)

ggplot(greg_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Thysanoessa gregaria \nYearly Anomalies")


#############################################################
######    ----   Thysanoessa longicaudata   ----    #########
#############################################################
thyslo_SNE <- zp_av_cru %>%
  filter(taxa == "thyslo" & Region == "SNE")

a <- ggplot(thyslo_SNE, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata")

thyslo_SNE_reg <- zp_av_reg %>%
  filter(taxa == "thyslo" & Region == "SNE")

b <- ggplot(thyslo_SNE_reg, aes(x = Date, y = mean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata")

thyslo_SNE_month <- zp_av_month %>%
  filter(taxa == "thyslo" & Region == "SNE")

c <- ggplot(thyslo_SNE_month, aes(x = Date, y = momean_log)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata")

thyslo_SNE_month_yr <- zp_av_month_yr %>%
  filter(taxa == "thyslo" & Region == "SNE")

d <- ggplot(thyslo_SNE_month_yr, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata")

plot_grid(a,b,c,d)

thyslo_SNE_alast <- zp_alast %>%
  filter(taxa == "thyslo" & Region == "SNE")
ggplot(thyslo_SNE_alast, aes(x = Date, y = mean_logov)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata")

#
#### plot anomalies
thyslo_anomon <- zp_anoma1 %>%
  filter(taxa == "thyslo" & Region == "SNE")

da <- ggplot(thyslo_anomon, aes(x = Date, y = anomaly)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata \nMonthly Anomalies")


thyslo_anoyr <- yrly %>%
  filter(taxa == "thyslo" & Region == "SNE")

da1 <- ggplot(thyslo_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa longicaudata \nYearly Anomalies")

plot_grid(da, da1, nrow=2)

ggplot(thyslo_anoyr, aes(x = Date, y = anomalyyr)) +
  geom_bar(stat = "identity") +
  theme_classic() + 
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Thysanoessa longicaudata \nYearly Anomalies")

################################################


################################################
################################################

#############################################################
######    ----    Nematoscelis megalops    ----     #########
#############################################################
nemame_SNE <- zp_av_cru %>%
  filter(taxa == "nemame" & Region == "SNE")

ggplot(nemame_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Nematoscelis megalops")

nemame_SNE_reg <- zp_av_reg %>%
  filter(taxa == "nemame" & Region == "SNE")

ggplot(nemame_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Nematoscelis megalops")



#############################################################
######    -----    Thysanoessa raschii    -----     #########
#############################################################
thysra_SNE <- zp_av_cru %>%
  filter(taxa == "thysra" & Region == "SNE")

ggplot(thysra_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa raschii")

thysra_SNE_reg <- zp_av_reg %>%
  filter(taxa == "thysra" & Region == "SNE")

ggplot(thysra_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa raschii")


#############################################################
######     ----     Thysanoessa inermis    ----    ##########
#############################################################
thysin_SNE <- zp_av_cru %>%
  filter(taxa == "thysin" & Region == "SNE")

ggplot(thysin_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa inermis")

thysin_SNE_reg <- zp_av_reg %>%
  filter(taxa == "thysin" & Region == "SNE")

ggplot(thysin_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa inermis")


#############################################################
#######     -----     Thysanoessa spp.    -----    ##########
#############################################################
thysspp_SNE <- zp_av_cru %>%
  filter(taxa == "thysspp" & Region == "SNE")

ggplot(thysspp_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa spp.")

thysspp_SNE_reg <- zp_av_reg %>%
  filter(taxa == "thysspp" & Region == "SNE")

ggplot(thysspp_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa spp.")


#############################################################
#######      -----     Euphausia spp.    -----     ##########
#############################################################
euphspp_SNE <- zp_av_cru %>%
  filter(taxa == "euphspp" & Region == "SNE")

ggplot(euphspp_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia spp.")

euphspp_SNE_reg <- zp_av_reg %>%
  filter(taxa == "euphspp" & Region == "SNE")

ggplot(euphspp_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausia spp.")


#############################################################
#######     -----     Stylocheiron spp.    -----   ##########
#############################################################
stylspp_SNE <- zp_av_cru %>%
  filter(taxa == "stylspp" & Region == "SNE")

ggplot(stylspp_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Stylocheiron spp.")

stylspp_SNE_reg <- zp_av_reg %>%
  filter(taxa == "stylspp" & Region == "SNE")

ggplot(stylspp_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Stylocheiron spp.")



#############################################################
#######      -----      Euphausiacea      -----    ##########
#############################################################
euph_SNE <- zp_av_cru %>%
  filter(taxa == "euph" & Region == "SNE")

ggplot(euph_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausiacea")

euph_SNE_reg <- zp_av_reg %>%
  filter(taxa == "euph" & Region == "SNE")

ggplot(euph_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausiacea")


#############################################################
#######      -----     Euphausiacea1      -----    ##########
#############################################################
euph1_SNE <- zp_av_cru %>%
  filter(taxa == "euph1" & Region == "SNE")

ggplot(euph1_SNE, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausiacea1")

euph1_SNE_reg <- zp_av_reg %>%
  filter(taxa == "euph1" & Region == "SNE")

ggplot(euph1_SNE_reg, aes(x = Date, y = mean_log10m2)) +
  geom_line(linewidth = 0.6) + 
  geom_point(size = 1.2) +
  theme_classic() + 
  ggtitle("SNE - Euphausiacea1")





##### OVERLAY WITH CLIMATIC INDEXES
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

################################################################
#############     MEGAN 
meg_anomon1 <- meg_anomon %>% filter(year <= 2020)
meg_anoyr1 <- meg_anoyr %>% filter(year <= 2020)


ggplot() +
  geom_line(data = amo, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = meg_anomon1, aes(x = Date, y = anomaly),
            linewidth = 1) +
  geom_point(data = amo, aes(x = Date, y = amo), color = "red") +
  geom_point(data = meg_anomon1, aes(x = Date, y = anomaly)) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica")


### by yr 
meganYRamo <- left_join(meg_anoyr1, amo_yr, by = "year")

meganYRamo1 <- meganYRamo[, c(3, 19, 36, 41)]

meganYRamo1L <- meganYRamo1 %>%
  pivot_longer(cols = anomalyyr:meanAmo, 
               names_to = "group",
               values_to = "vals")

ggplot(meganYRamo1L, aes(x = year, y = vals, color = group)) +
  geom_line(linewidth = 1.2) + 
  geom_point() + 
  scale_color_manual(values = c("black","red"),
                     labels = c("Abundance \n Anomaly", "AMO")) +
  theme_classic() +
  theme(legend.title = element_blank()) + 
  ggtitle("SNE - Meganyctiphanes norvegica")

######

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

amonao <- left_join(meganYRamo1, nao_yr, by = "year")

anomaoL <- amonao %>%
  pivot_longer(cols = c(anomalyyr:meanAmo, meanNao),  
               names_to = "group",
               values_to = "vals")

ggplot(anomaoL, aes(x = year, y = vals, color = group)) +
  geom_line(linewidth = 1.2) + 
  geom_point() + 
  scale_color_manual(values = c("black","red", "blue"),
                     labels = c("Abundance \n Anomaly", "AMO", "NAO")) +
  theme_classic() +
  theme(legend.title = element_blank()) + 
  ggtitle("SNE - Meganyctiphanes norvegica")
#######


###### KROHNII

kro_anomon1 <- kro_anomon %>% filter(year <= 2020)
kro_anoyr1 <- kro_anoyr %>% filter(year <= 2020)

ggplot() +
  geom_line(data = amo, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = kro_anomon1, aes(x = Date, y = anomaly),
            linewidth = 1) +
  geom_point(data = amo, aes(x = Date, y = amo), color = "red") +
  geom_point(data = kro_anomon1, aes(x = Date, y = anomaly)) +
  theme_classic() + 
  ggtitle("SNE - E. krohnii")


kroYRamo <- left_join(kro_anoyr1, amo_yr, by = "year")
kroYRamo <- kroYRamo[, c(3, 19, 36, 41)]

kroYRamo1L <- kroYRamo %>%
  pivot_longer(cols = anomalyyr:meanAmo, 
               names_to = "group",
               values_to = "vals")


ggplot(kroYRamo1L, aes(x = year, y = vals, color = group)) +
  geom_line(linewidth = 1.2) + 
  geom_point() + 
  scale_color_manual(values = c("black","red"),
                     labels = c("Abundance \n Anomaly", "AMO")) +
  theme_classic() +
  theme(legend.title = element_blank()) + 
  ggtitle("SNE - E. krohnii")


ggplot(kroYRamo1L, aes(x = year, y = vals, color = group, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") 



meganAL <- zp %>% filter(Region == "SNE" & taxa =="megan")

ggplot(meganAL, aes(x=Date, y=ind_10m2)) +
  geom_line()

meganALD <- meganAL %>%
  group_by(Date) %>%
  mutate(meanA = mean(ind_10m2)) %>%
  distinct(Date, .keep_all = TRUE)

ggplot(meganALD, aes(x=Date, y=meanA)) +
  geom_line()

ggplot(meganAL, aes(x=Date, y=ind_10m2)) +
  geom_point()

