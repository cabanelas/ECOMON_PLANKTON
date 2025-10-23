###############################################################################
###############################################################################
###############################################################################
##################   ---  Abundance Anomalies    ---   ########################
###############################################################################
###############################################################################
##################   ---   SEASONAL ANOMALIES    ---   ########################
###############################################################################
# Yi = log10(ind/10m2 + 1)
# Yi = (Yi - mean(Yc))/sd(Yc)
# Yc,reg = cruise mean Yi,region
# Yc cruise mean of Yi within region 
# Y anom = (Yc - mean(Yc,yr)) / (SD)Yc

#x <- c(1, 2 , 3, 4, 5, 6)
#a <- mean(x)
#b <- sd(x)
#c <- (x-a)/b

# euph_10m2 is the sum of taxa codes 2000-2099 and euph1_10m2 is just 
# unidentified euphausicea (just taxa code 2000) -Harvey 

## Packages
library(tidyverse)
library(cowplot)

## Data 
zp <- read.csv("output/EcoMon_SubEUPHAU_AllRegions_m2_long.csv")
# this csv was created in m2_euphausiid_subset_csv_06JUL script

# format date
zp$Date <- as.Date(zp$date, format = "%d-%b-%y")

# add season - following season grouping that State of Ecosys/NOAA uses
# different from my original df by a month or so
# (i think i orgininally had winter start in Dec)
zp <- zp %>%
  mutate(season = case_when(between(monthNum, 1, 3) ~ "winter",
                            between(monthNum, 4, 6) ~ "spring",
                            between(monthNum, 7, 9) ~ "summer",
                            between(monthNum, 10,12) ~ "fall"))

############################################################################
############################################################################
##################               SNE               #########################
############################################################################
# Select Southern New England Region 
SNE <- zp %>% filter(Region == "SNE")

# check
SNE %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = ind_10m2)) +
  geom_point()

#############################################################
###########             Select Season             ###########
#############################################################

########     ------      WINTER      --------        ########

SNE_Winter <- SNE %>% filter(season == "winter") #& taxa == "megan")

# 1) add 1 to abundances and take log
SNE_Winter$Yi <- log10(SNE_Winter$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1

# check
SNE_Winter %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yi)) +
  geom_point()
#############################################################

# Yc year mean of Yi within region by season
# 2) take mean by cruise for each taxa within SNE
SNE_Winter1 <- SNE_Winter %>%
  group_by(year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(year, taxa, .keep_all = T) %>%
  ungroup()

# check
SNE_Winter1 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()
#############################################################

# 3) calculate mean and SD of Yc whole time series
SNE_Winter2 <- SNE_Winter1 %>%
  group_by(taxa) %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) %>%
  ungroup()

# check
SNE_Winter2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()

SNE_Winter2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
SNE_Winter2 <- SNE_Winter2 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

SNE_Winter2 %>%
  group_by(taxa) %>%
  summarise(mean(Anomaly_yr, na.rm = TRUE),
            sd(Anomaly_yr, na.rm = TRUE))

# check
SNE_Winter2 %>% filter(taxa == "euphkr") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point()

SNE_Winter2 <- SNE_Winter2 %>%
  mutate(col = factor(ifelse(Anomaly_yr > 0, 2, 1)))

########     ------      SUMMER      --------        ########

SNE_summer <- SNE %>% filter(season == "summer") #& taxa == "megan"

# 1) add 1 and take log
SNE_summer$Yi <- log10(SNE_summer$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1

# check
SNE_summer %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yi)) +
  geom_point()
#############################################################

# Yc year mean of Yi within region by season
# 2) take mean by year for each taxa within SNE
SNE_summer1 <- SNE_summer %>%
  group_by(year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(year, taxa, .keep_all = T) %>%
  ungroup()

# check
SNE_summer1 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()
#############################################################

# 3) calculate mean and SD of Yc by whole time series
SNE_summer2 <- SNE_summer1 %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) %>%
  ungroup()

# check
SNE_summer2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()

SNE_summer2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
SNE_summer2 <- SNE_summer2 %>%
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

mean(SNE_summer2$Anomaly_yr, na.rm = TRUE)
sd(SNE_summer2$Anomaly_yr, na.rm = TRUE)

SNE_summer2 <- SNE_summer2 %>%
  mutate(col = factor(ifelse(Anomaly_yr > 0, 2, 1)))


########     ------      SPRING      --------        ########

SNE_spring <- SNE %>% filter(season == "spring") #& taxa == "megan"

# 1) add 1 and take log
SNE_spring$Yi <- log10(SNE_spring$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1

# check
SNE_spring %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yi)) +
  geom_point()
#############################################################

# Yc year mean of Yi within region by season
# 2) take mean by year for each taxa within SNE
SNE_spring1 <- SNE_spring %>%
  group_by(year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(year, taxa, .keep_all = T) %>%
  ungroup()

# check
SNE_spring1 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()
#############################################################

# 3) calculate mean and SD of Yc by whole time series
SNE_spring2 <- SNE_spring1 %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) %>%
  ungroup()

# check
SNE_spring2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()

SNE_spring2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
SNE_spring2 <- SNE_spring2 %>%
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

mean(SNE_spring2$Anomaly_yr, na.rm = TRUE)
sd(SNE_spring2$Anomaly_yr, na.rm = TRUE)

SNE_spring2 <- SNE_spring2 %>%
  mutate(col = factor(ifelse(Anomaly_yr > 0, 2, 1)))


########     ------      FALL      --------        ########

SNE_fall <- SNE %>% filter(season == "fall") #& taxa == "megan"

# 1) add 1 and take log
SNE_fall$Yi <- log10(SNE_fall$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1

# check
SNE_fall %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yi)) +
  geom_point()
#############################################################

# Yc year mean of Yi within region by season
# 2) take mean by year for each taxa within SNE
SNE_fall1 <- SNE_fall %>%
  group_by(year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(year, taxa, .keep_all = T) %>%
  ungroup()

# check
SNE_fall1 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()
#############################################################

# 3) calculate mean and SD of Yc by whole time series
SNE_fall2 <- SNE_fall1 %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) %>%
  ungroup()

# check
SNE_fall2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()

SNE_fall2 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
SNE_fall2 <- SNE_fall2 %>%
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

mean(SNE_fall2$Anomaly_yr, na.rm = TRUE)
sd(SNE_fall2$Anomaly_yr, na.rm = TRUE)

SNE_fall2 <- SNE_fall2 %>%
  mutate(col = factor(ifelse(Anomaly_yr > 0, 2, 1)))


###############################################################################
###############################################################################
###############################################################################
#####      -----         CLIMATIC INDEXES         ------   ####################
###############################################################################
#####      -----                AMO               ------   ####################
###############################################################################
## Data
amo <- read.csv("raw/amo_month_NCAR_filtered.csv") #from amo script
amo <- amo %>% filter(yr >= 1977 & yr <= 2020)

amo$Date <- zoo::as.yearmon(paste(amo$yr, amo$month), "%Y %b")
amo$Date <- format(amo$Date, '%Y-%m-%01')
amo$Date <- as.Date(amo$Date, '%Y-%m-%d')
#############################################################
hist(amo$amo) #looks good; also checked for heteroscedasticity 
qqnorm(amo$amo)
qqline(amo$amo)

amo$monthNum <- match(amo$month, month.abb)

# add seasons
amo <- amo %>%
  mutate(season = case_when(between(monthNum, 1, 3) ~ "winter",
                            between(monthNum, 4, 6) ~ "spring",
                            between(monthNum, 7, 9) ~ "summer",
                            between(monthNum, 10,12) ~ "fall"))

# add column for colors above and below 0 
amo <- amo %>%
  mutate(col = factor(ifelse(amo > 0, 2, 1)))

###############################################################################
##################     --        WINTER     --          #######################

amow <- amo %>% 
  filter(season == "winter")

amoAnomw <- ggplot(data = amow, aes(x = Date, y = amo)) +
  geom_line() +
  geom_point() +
  labs(y = "AMO - Winter") +
  theme_classic() +
  geom_hline(yintercept = 0)

amow %>% group_by(yr) %>%
  mutate(sumA = sum(amo)) %>%
  ggplot(aes(x = Date, y = sumA)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0)

amogroupw <- amow %>%
  group_by(yr) %>%
  mutate(sumA = sum(amo)) %>%
  distinct(yr, .keep_all = T)

amogroupw <- amogroupw %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

amogroupwP <- ggplot(data = amogroupw, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("AMO - winter") +
  labs(y = "AMO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 

###############################################################################
##################        --     SUMMER     --        #########################

amos <- amo %>% 
  filter(season == "summer")

amoAnoms <- ggplot(data = amos, aes(x = Date, y = amo)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(y = "AMO - Summer") +
  geom_hline(yintercept = 0)

amogroups <- amos %>%
  group_by(yr) %>%
  mutate(sumA = sum(amo)) %>%
  distinct(yr, .keep_all = T)

amogroups <- amogroups %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

amogroupsP <- ggplot(data = amogroups, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("AMO - summer") +
  labs(y = "AMO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 

###############################################################################
##################      ----        FALL      ----        #####################
amof <- amo %>% 
  filter(season == "fall")

amoAnomf <- ggplot(data = amof, aes(x = Date, y = amo)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(y = "AMO - Fall") +
  geom_hline(yintercept = 0)

amogroupF <- amof %>%
  group_by(yr) %>%
  mutate(sumA = sum(amo)) %>%
  distinct(yr, .keep_all = T)

amogroupF <- amogroupF %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

amogroupFP <- ggplot(data = amogroupF, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("AMO - fall") +
  labs(y = "AMO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 


###############################################################################
##################       ----      SPRING     ----      #######################
amosp <- amo %>% 
  filter(season == "spring")

amoAnomsp <- ggplot(data = amosp, aes(x = Date, y = amo)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(y = "AMO - Spring") +
  geom_hline(yintercept = 0)

amogroupSPR <- amosp %>%
  group_by(yr) %>%
  mutate(sumA = sum(amo)) %>%
  distinct(yr, .keep_all = T)

amogroupSPR <- amogroupSPR %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

amogroupsprP <- ggplot(data = amogroupSPR, aes(x = Date, y = sumA, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("AMO - spring") +
  labs(y = "AMO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 

##################################################################
##################################################################


###############################################################################
###############################################################################
######     ---   Meganyctiphanes norvegica  ---    ############################
###############################################################################
###############################################################################

###############################################################################
######     ---          Winter              ---    ############################
###############################################################################

meg_w <- SNE_Winter2 %>% 
  filter(taxa == "megan" & year < 2021)

meganAnomw1 <- ggplot(data = meg_w, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - winter") +
  geom_hline(yintercept = 0)

amoMegw1 <- ggplot() +
  geom_line(data = amow, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = meg_w, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amow, aes(x = Date, y = amo), color = "red") +
  geom_point(data = meg_w, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnomw1, amoAnomw, amoMegw1, nrow=3) #NCAR amo

#plot_grid(meganAnomw1, amowinter, amoMegw1, nrow=3) #NOAA amo unsmooth?
###############################################################################
#########                       BAR PLOT                       ################

#meggroupw <- meg_w %>%
#  group_by(year) %>%
#  mutate(sumA = sum(Anomaly_yr)) %>%
#  distinct(year, .keep_all = T)

#meggroupw <- meggroupw %>%
#  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

#meggroupw <- meggroupw %>% filter(year < 2021)

meggroupwP <- ggplot(data = meg_w, aes(x = Date, y = Anomaly_yr, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Meganyctiphanes norvegica - winter") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("red", "blue"))  +
  scale_color_manual(values=c("red", "blue")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(meggroupwP, amogroupwP, nrow = 2, align = "v")

## or replacing NAs with 0 
#megaNA <- SNE2 %>% 
#  filter(taxa == "megan" & season == "winter")
#megaNA$Anomaly_yr[is.na(megaNA$Anomaly_yr)] <- 0

#meganAnomw2 <- ggplot(data = megaNA, aes(x = Date, y = Anomaly_yr)) +
#  geom_line() +
#  geom_point() +
#  theme_classic() + 
#  ggtitle("SNE - Meganyctiphanes norvegica - winter") +
#  geom_hline(yintercept = 0)

amoMegw2 <- ggplot() +
  geom_line(data = amow, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megaNA, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amow, aes(x = Date, y = amo), color = "red") +
  geom_point(data = megaNA, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnomw2, amoAnomw, amoMegw2, nrow=3)


###############################################################################
######     ---          summer              ---    ############################
###############################################################################
meg_summer <- SNE_summer2 %>% 
  filter(taxa == "megan" & season == "summer")

meganAnomsum1 <- ggplot(data = meg_summer, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  geom_hline(yintercept = 0)

amoMegsu1 <- ggplot() +
  geom_line(data = amos, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = meg_summer, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amos, aes(x = Date, y = amo), color = "red") +
  geom_point(data = meg_summer, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnomsum1, amoAnoms, amoMegsu1, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################

#meggroups <- megsum1 %>%
#  group_by(year) %>%
#  mutate(sumA = sum(Anomaly_yr)) %>%
#  distinct(year, .keep_all = T)

#meggroups <- meggroups %>%
#  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

#meggroups <- meggroups %>% filter(year < 2021)

meggroupsuP <- ggplot(data = meg_summer, aes(x = Date, y = Anomaly_yr, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(meggroupsuP, amogroupsP, nrow = 2, align = "v")


## or replacing NAs with 0 
#megaNA <- SNE2 %>% 
#  filter(taxa == "megan" & season == "summer")
#megaNA$Anomaly_yr[is.na(megaNA$Anomaly_yr)] <- 0

meganAnom2 <- ggplot(data = megaNA, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  geom_hline(yintercept = 0)

amoMegNAS2 <- ggplot() +
  geom_line(data = amos, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megaNA, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amos, aes(x = Date, y = amo), color = "red") +
  geom_point(data = megaNA, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnom2, amoAnoms, amoMegNAS2, nrow=3)



###############################################################################
######     ---          spring              ---    ############################
###############################################################################
##without interpolating abundance data
megsp1 <- SNE2 %>% 
  filter(taxa == "megan" & season == "spring") %>%
  ungroup()

meganAnomsp1 <- ggplot(data = megsp1, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - spring") +
  geom_hline(yintercept = 0)


amoMegsp1 <- ggplot() +
  geom_line(data = amosp, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megsp1, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amosp, aes(x = Date, y = amo), color = "red") +
  geom_point(data = megsp1, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnomsp1, amoAnomsp, amoMegsp1, nrow=3, align = "v")


###############################################################################
#########                       BAR PLOT                       ################
meggroupspr <- megsp1 %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

meggroupspr <- meggroupspr %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

#meggroupspr <- meggroupspr %>% filter(year < 2021)

meggroupsprinP <- ggplot(data = meggroupspr, aes(x = Date, y = sumA, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Meganyctiphanes norvegica - spring") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(meggroupsprinP, amogroupsprP, nrow = 2, align = "v")


###############################################################################
######     ---           fall               ---    ############################
###############################################################################

## without interpolating abundance data
megF1 <- SNE2 %>% 
  filter(taxa == "megan" & season == "fall")

meganAnomfal <- ggplot(data = megF1, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - fall") +
  geom_hline(yintercept = 0)

amoMegF <- ggplot() +
  geom_line(data = amof, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = megF1, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amof, aes(x = Date, y = amo), color = "red") +
  geom_point(data = megF1, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnomfal, amoAnomf, amoMegF, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################

meggroupF <- megF1 %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

meggroupF <- meggroupF %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

meggroupF <- meggroupF %>% filter(year < 2021)

meggroupfallP <- ggplot(data = meggroupF, aes(x = Date, y = sumA, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Meganyctiphanes norvegica - fall") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("red", "blue"))  +
  scale_color_manual(values=c("red", "blue")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(meggroupfallP, amogroupFP, nrow = 2, align = "v")


###############################################################################
###############################################################################
###############################################################################
###############################################################################
######              ----    	Euphausia  krohnii   ----         ###############
###############################################################################

###############################################################################
###############################################################################
######     ---          Winter              ---    ############################
###############################################################################
with(SNE2, TAXA_NAME[taxa == "euphkr"][1])
     
kroWin <- SNE2 %>% 
  filter(taxa == "euphkr" & season == "winter") %>%
  ungroup()

kroAnomW <- ggplot(data = kroWin, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii - winter") +
  geom_hline(yintercept = 0)

amoKroW <- ggplot() +
  geom_line(data = amow, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = kroWin, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amow, aes(x = Date, y = amo), color = "red") +
  geom_point(data = kroWin, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(kroAnomW, amoAnomw, amoKroW, nrow=3, align = "v") #NCAR amo

###############################################################################
#########                       BAR PLOT                       ################

krogroupW <- kroWin %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

krogroupW <- krogroupW %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

krogroupWP <- ggplot(data = krogroupW, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Euphausia krohnii - winter") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(krogroupWP, amogroupwP, nrow = 2, align = "v")


###############################################################################
######     ---          summer              ---    ############################
###############################################################################

##without interpolating abundance data
krosum <- SNE2 %>% 
  filter(taxa == "euphkr" & season == "summer")

kroAnomsum <- ggplot(data = krosum, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii - summer") +
  geom_hline(yintercept = 0)

amoKrosu <- ggplot() +
  geom_line(data = amos, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = krosum, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amos, aes(x = Date, y = amo), color = "red") +
  geom_point(data = krosum, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(kroAnomsum, amoAnoms, amoKrosu, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################

krogroups <- krosum %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

krogroups <- krogroups %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

krogroupsP <- ggplot(data = krogroups, aes(x = Date, y = sumA, color = col, 
                                            fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Euphausia krohnii - summer") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(krogroupsP, amogroupsP, nrow = 2, align = "v")


###############################################################################
######     ---          spring              ---    ############################
###############################################################################

##without interpolating abundance data
krospr <- SNE2 %>% 
  filter(taxa == "euphkr" & season == "spring")

kroAnomSpri <- ggplot(data = krospr, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii - spring") +
  geom_hline(yintercept = 0)

amoKrospr <- ggplot() +
  geom_line(data = amos, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = krospr, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amos, aes(x = Date, y = amo), color = "red") +
  geom_point(data = krospr, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(kroAnomSpri, amoAnomsp, amoKrospr, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################
krosprgroupspr <- krospr %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

krosprgroupspr <- krosprgroupspr %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

krogroupsprinP <- ggplot(data = krosprgroupspr, aes(x = Date, y = sumA, 
                                                    color = col, 
                                                    fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Euphausia krohnii - spring") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(krogroupsprinP, amogroupsprP, nrow = 2, align = "v")


###############################################################################
######     ---           fall               ---    ############################
###############################################################################

## without interpolating abundance data
kroF <- SNE2 %>% 
  filter(taxa == "euphkr" & season == "fall")

kroAnomfal <- ggplot(data = kroF, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Euphausia krohnii - fall") +
  geom_hline(yintercept = 0)

amoKroF <- ggplot() +
  geom_line(data = amof, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = kroF, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amof, aes(x = Date, y = amo), color = "red") +
  geom_point(data = kroF, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(kroAnomfal, amoAnomf, amoKroF, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################

krogroupF <- kroF %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

krogroupF <- krogroupF %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

krogroupfallP <- ggplot(data = krogroupF, aes(x = Date, y = sumA, 
                                              color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Euphausia krohnii - fall") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("red", "blue"))  +
  scale_color_manual(values=c("red", "blue")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(krogroupfallP, amogroupFP, nrow = 2, align = "v")



###############################################################################
###############################################################################
###############################################################################
###############################################################################
######            ----    	Thysanoessa gregaria   ----         ###############
###############################################################################

###############################################################################
###############################################################################
######     ---          Winter              ---    ############################
###############################################################################
with(SNE2, TAXA_NAME[taxa == "thysgr"][1])

gregWin <- SNE2 %>% 
  filter(taxa == "thysgr" & season == "winter") %>%
  ungroup()

gregAnomW <- ggplot(data = gregWin, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria - winter") +
  geom_hline(yintercept = 0)

amoGregW <- ggplot() +
  geom_line(data = amow, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = gregWin, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amow, aes(x = Date, y = amo), color = "red") +
  geom_point(data = gregWin, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(gregAnomW, amoAnomw, amoGregW, nrow=3, align = "v") #NCAR amo

###############################################################################
#########                       BAR PLOT                       ################

gregroupW <- gregWin %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

gregroupW <- gregroupW %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

gregroupWP <- ggplot(data = gregroupW, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Thysanoessa gregaria - winter") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("red", "blue"))  +
  scale_color_manual(values=c("red", "blue")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(gregroupWP, amogroupwP, nrow = 2, align = "v")



###############################################################################
######     ---          summer              ---    ############################
###############################################################################

##without interpolating abundance data
gresum <- SNE2 %>% 
  filter(taxa == "thysgr" & season == "summer")

greAnomsum <- ggplot(data = gresum, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria - summer") +
  geom_hline(yintercept = 0)

amoGresu <- ggplot() +
  geom_line(data = amos, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = gresum, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amos, aes(x = Date, y = amo), color = "red") +
  geom_point(data = gresum, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(greAnomsum, amoAnoms, amoGresu, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################

gregroups <- gresum %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

gregroups <- gregroups %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

gregroupsP <- ggplot(data = gregroups, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Thysanoessa gregaria - summer") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(gregroupsP, amogroupsP, nrow = 2, align = "v")


###############################################################################
######     ---          spring              ---    ############################
###############################################################################

##without interpolating abundance data
grespr <- SNE2 %>% 
  filter(taxa == "thysgr" & season == "spring")

greAnomSpri <- ggplot(data = grespr, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria - spring") +
  geom_hline(yintercept = 0)

amoGrespr <- ggplot() +
  geom_line(data = amos, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = grespr, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amos, aes(x = Date, y = amo), color = "red") +
  geom_point(data = grespr, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(greAnomSpri, amoAnomsp, amoGrespr, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################
gresprgroupspr <- grespr %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

gresprgroupspr <- gresprgroupspr %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

gregroupsprinP <- ggplot(data = gresprgroupspr, aes(x = Date, y = sumA, 
                                                    color = col, 
                                                    fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Thysanoessa gregaria - spring") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(gregroupsprinP, amogroupsprP, nrow = 2, align = "v")



###############################################################################
######     ---           fall               ---    ############################
###############################################################################

## without interpolating abundance data
greF <- SNE2 %>% 
  filter(taxa == "thysgr" & season == "fall")

greAnomfal <- ggplot(data = greF, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Thysanoessa gregaria - fall") +
  geom_hline(yintercept = 0)

amoGreF <- ggplot() +
  geom_line(data = amof, aes(x = Date, y = amo), color = "red",
            linewidth = 1.2) +
  geom_line(data = greF, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = amof, aes(x = Date, y = amo), color = "red") +
  geom_point(data = greF, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(greAnomfal, amoAnomf, amoGreF, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################

gregroupF <- greF %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

gregroupF <- gregroupF %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

gregroupfallP <- ggplot(data = gregroupF, aes(x = Date, y = sumA, 
                                              color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Thysanoessa gregaria - fall") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(gregroupfallP, amogroupFP, nrow = 2, align = "v")


###############################################################################
###############################################################################
###############################################################################
###############################################################################
######            ----    	NAO   ----         ###############
###############################################################################
## Data
nao <- read.csv("raw/nao_month_l.csv") #from nao script
nao <- nao %>% filter(yr >= 1977 & yr <= 2020)

nao$Date <- zoo::as.yearmon(paste(nao$yr, nao$month), "%Y %b")
nao$Date <- format(nao$Date, '%Y-%m-%01')
nao$Date <- as.Date(nao$Date, '%Y-%m-%d')

nao <- nao %>%
  mutate(col = factor(ifelse(nao > 0, 2, 1)))

ggplot(data = nao, aes(x = Date, y = nao, color = col, fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)

nao$monthNum <- match(nao$month, month.abb)

# add seasons
nao <- nao %>%
  mutate(season = case_when(between(monthNum, 1, 3) ~ "winter",
                            between(monthNum, 4, 6) ~ "spring",
                            between(monthNum, 7, 9) ~ "summer",
                            between(monthNum, 10,12) ~ "fall"))
###########
naoAnom <- ggplot(data = nao, aes(x = Date, y = nao)) +
  geom_line() +
  geom_point() +
  theme_classic() 

# nao by yr
nao_yr_sum <- nao %>%
  group_by(yr) %>%
  mutate(sNao = sum(nao)) %>%
  distinct(yr, .keep_all = TRUE)

#colnames(nao_yr_sum)[1] <- "year"

ggplot(data = nao_yr_sum, aes(x = Date, y = sNao)) +
  geom_line() +
  geom_point() +
  theme_classic() 

###############################################################################
##################     --        WINTER     --          #######################

naow <- nao %>% 
  filter(season == "winter")

naoAnomw <- ggplot(data = naow, aes(x = Date, y = nao)) +
  geom_line() +
  geom_point() +
  labs(y = "NAO - Winter") +
  theme_classic() +
  geom_hline(yintercept = 0)

naow %>% group_by(yr) %>%
  mutate(meanA = mean(nao)) %>%
  ggplot(aes(x = Date, y = meanA)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0)

naogroupw <- naow %>%
  group_by(yr) %>%
  mutate(meanA = mean(nao)) %>%
  distinct(yr, .keep_all = T)

naogroupw <- naogroupw %>%
  mutate(col = factor(ifelse(meanA > 0, 2, 1)))

naogroupwP <- ggplot(data = naogroupw, aes(x = Date, y = meanA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("NAO - winter") +
  labs(y = "NAO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 

###############################################################################
##################       ----      SPRING     ----      #######################

naosp <- nao %>% 
  filter(season == "spring")

naoAnomsp <- ggplot(data = naosp, aes(x = Date, y = nao)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(y = "NAO - Spring") +
  geom_hline(yintercept = 0)

naogroupSPR <- naosp %>%
  group_by(yr) %>%
  mutate(meanA = mean(nao)) %>%
  distinct(yr, .keep_all = T)

naogroupSPR <- naogroupSPR %>%
  mutate(col = factor(ifelse(meanA > 0, 2, 1)))

naogroupspP <- ggplot(data = naogroupSPR, aes(x = Date, y = meanA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("NAO - spring") +
  labs(y = "NAO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 


###############################################################################
##################       ----      SUMMER     ----      #######################

naosumm <- nao %>% 
  filter(season == "summer")

naoAnomsumm <- ggplot(data = naosumm, aes(x = Date, y = nao)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(y = "NAO - summer") +
  geom_hline(yintercept = 0)

naogroupsumm <- naosumm %>%
  group_by(yr) %>%
  mutate(meanA = mean(nao)) %>%
  distinct(yr, .keep_all = T)

naogroupsumm <- naogroupsumm %>%
  mutate(col = factor(ifelse(meanA > 0, 2, 1)))

naogroupSUP <- ggplot(data = naogroupsumm, aes(x = Date, y = meanA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("NAO - summer") +
  labs(y = "NAO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 

###############################################################################
##################       ----      FALL       ----      #######################

naoF <- nao %>% 
  filter(season == "fall")

naoAnomF <- ggplot(data = naoF, aes(x = Date, y = nao)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  labs(y = "NAO - fall") +
  geom_hline(yintercept = 0)

naogroupF <- naoF %>%
  group_by(yr) %>%
  mutate(meanA = mean(nao)) %>%
  distinct(yr, .keep_all = T)

naogroupF <- naogroupF %>%
  mutate(col = factor(ifelse(meanA > 0, 2, 1)))

naogroupF <- ggplot(data = naogroupF, aes(x = Date, y = meanA, color = col, 
                                               fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE) +
  geom_hline(yintercept = 0) +
  ggtitle("NAO - fall") +
  labs(y = "NAO Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) +
  theme_classic() 

###############################################################################
###############################################################################
####### COMPARE NAO WITH ZP 
###############################################################################
######     ---   Meganyctiphanes norvegica  ---    ############################
###############################################################################
###############################################################################
###############################################################################
##################     --        WINTER     --          #######################

naoMegW <- ggplot() +
  geom_line(data = naow, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = megw1, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = naow, aes(x = Date, y = nao), color = "red") +
  geom_point(data = megw1, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(meganAnomw1, naoAnomw, naoMegW, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################
plot_grid(meggroupwP, naogroupwP, nrow = 2, align = "v")

###############################################################################
##################       ----      SPRING     ----      #######################
naoMegSP <- ggplot() +
  geom_line(data = naosp, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = megw1, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = naosp, aes(x = Date, y = nao), color = "red") +
  geom_point(data = megw1, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)
### FIX
plot_grid(meganAnomsp1, naoAnomsp, naoMegSP, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################
plot_grid(meggroupsprinP, naogroupspP, nrow = 2, align = "v")


###############################################################################
##################       ----      SUMMER     ----      #######################
naoMegSumm <- ggplot() +
  geom_line(data = naosumm, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = megw1, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = naosumm, aes(x = Date, y = nao), color = "red") +
  geom_point(data = megw1, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)
### FIX
plot_grid(meganAnomsum1, naoAnomsumm, naoMegSumm, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################
plot_grid(meggroupsuP, naogroupSUP, nrow = 2, align = "v")

###############################################################################
##################       ----      FALL     ----      #######################
naoMegF <- ggplot() +
  geom_line(data = naoF, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = megw1, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = naoF, aes(x = Date, y = nao), color = "red") +
  geom_point(data = megw1, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)
### FIX
plot_grid(meganAnomfal, naoAnomF, naoMegF, nrow=3)

###############################################################################
#########                       BAR PLOT                       ################
plot_grid(meggroupfallP, naogroupF, nrow = 2, align = "v")



###############################################################################
###############################################################################
######              ----    	Euphausia  krohnii   ----         ###############
###############################################################################

###############################################################################
###############################################################################
######     ---          Winter              ---    ############################
###############################################################################

naoKroW <- ggplot() +
  geom_line(data = naow, aes(x = Date, y = nao), color = "red",
            linewidth = 1.2) +
  geom_line(data = kroWin, aes(x = Date, y = Anomaly_yr),
            linewidth = 0.8) +
  geom_point(data = naow, aes(x = Date, y = nao), color = "red") +
  geom_point(data = kroWin, aes(x = Date, y = Anomaly_yr)) +
  theme_classic() +
  geom_hline(yintercept = 0)

plot_grid(kroAnomW, naoAnomw, naoKroW, nrow=3, align = "v") #NCAR amo

###############################################################################
#########                       BAR PLOT                       ################

krogroupW <- kroWin %>%
  group_by(year) %>%
  mutate(sumA = sum(Anomaly_yr)) %>%
  distinct(year, .keep_all = T)

krogroupW <- krogroupW %>%
  mutate(col = factor(ifelse(sumA > 0, 2, 1)))

krogroupWP <- ggplot(data = krogroupW, aes(x = Date, y = sumA, color = col, 
                                           fill = col)) + #need to add fill argument due to pixel issue 
  geom_bar(stat = "identity", 
           position = "dodge",
           show.legend=FALSE)  +
  geom_hline(yintercept = 0) +
  ggtitle("SNE - Euphausia krohnii - winter") +
  labs(y = "Anomaly") +
  scale_fill_manual(values=c("blue", "red"))  +
  scale_color_manual(values=c("blue", "red")) + 
  theme_classic() +
  theme(axis.title.x = element_blank())

plot_grid(krogroupWP, amogroupwP, nrow = 2, align = "v")










###############################################################################
###############################################################################
###############################################################################
###############################################################################
######            ----    	Correlation   ----         ###############
###############################################################################
# should try doing this by season with all climate and most abu species 
#### SPRING MEGAN
megan_spring <- meggroupspr %>% 
  select(year, sumA)

amo_spring_cut <- amogroupSPR %>% 
  select(yr, sumA)

amo_spring_cut <- amo_spring_cut %>%
  rename(year = yr)

years_df1 <- unique(megan_spring$year)
years_df2 <- unique(amo_spring_cut$year)

# Find the years that are in df1 but not in df2
setdiff(years_df1, years_df2)
# Find the years that are in df2 but not in df1
setdiff(years_df2, years_df1)

megan_spring <- megan_spring %>%
  filter(year != 2021)

amo_spring_cut <- amo_spring_cut %>%
  filter(year != 1989 & year != 1990 & year != 1991 & year != 1997 & year != 2020)

megan_amo_spring_bind <- cbind(megan_spring, amo_spring_cut, by = "year")


library(corrplot)
# Calculate the correlation matrix
cor_matrix <- cor(megan_amo_spring_bind[, c("sumA...2", "sumA...4")], use = "complete.obs")

# Create the correlogram
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", tl.cex = 0.8, tl.col = "black")


###############################################################################
###############################################################################
###############################################################################
###############################################################################












##########################################################
##########################################################
#######################################################################
#######      -----      Euphausiacea      -----    ####################
#######################################################################

##########################################################
##########################################################
euph <- SNE2 %>% 
  filter(taxa == "euph")

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


euph <- euph %>% 
  filter(season == "winter")
    
ggplot(data = euph, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  geom_hline(yintercept = 0)




SNE211111 <- SNE1 %>%
  group_by(season, taxa) %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) 


SNE211111 <- SNE211111 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

euphSNE211111 <- SNE211111 %>% 
  filter(taxa == "euph" & season == "spring")


ggplot(data = euphSNE211111, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  geom_hline(yintercept = 0)




SNE212 <- SNE1 %>%
  group_by(season, year, taxa) %>%
  mutate(YcAV_yr = mean(Yc)) 

SNE212 <- SNE212 %>%
  group_by(season, taxa) %>%
  mutate(YcSD_yr = sd(Yc))

SNE212 <- SNE212 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 



euphSNE212 <- SNE212 %>% 
  filter(taxa == "euph" & season == "spring")


ggplot(data = euphSNE212, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  geom_hline(yintercept = 0)


#############################################################
#############################################################
#############################################################
#############################################################

zp1<-zp
# 1) add 1 and take log
zp1$Yi <- log10(zp$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1


# Yc cruise mean of Yi within region by season
# 2) take mean by cruise for each taxa within SNE
zp11 <- zp1 %>%
  group_by(cruise_name, season, year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(cruise_name, season, year, taxa, .keep_all = T) %>%
  ungroup()

# check
zp11 %>% filter(taxa == "megan" & season == "summer") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()
#############################################################

# 3) calculate mean and SD of Yc by year
zp111 <- zp11 %>%
  group_by(season, year, taxa) %>%
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) %>%
  ungroup()

# check
zp111 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()
zp111 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
zp14 <- zp111 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 



# Select Southern New England Region 
gom <- zp14 %>% filter(Region == "GOM")


megsumgom <- gom %>% 
  filter(taxa == "megan" & season == "spring") %>%
  ungroup()

ggplot(data = megsumgom, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - summer") +
  geom_hline(yintercept = 0)











#################

zpGB1 <- zpGB
zpGB1$Date <- as.Date(zpGB1$date, format = "%d-%b-%y")

# add season - following season grouping that State of Ecosys/NOAA uses
# different from my original df by a month or so
# (i think i orgininally had winter start in Dec)
zpGB1 <- zpGB1 %>%
  mutate(season = case_when(between(monthNum, 1, 3) ~ "winter",
                            between(monthNum, 4, 6) ~ "spring",
                            between(monthNum, 7, 9) ~ "summer",
                            between(monthNum, 10,12) ~ "fall"))

zpGB1$Yi <- log10(zpGB1$ind_10m2+1)

zpGB11 <- zpGB1 %>%
  group_by(cruise_name, season, year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(cruise_name, season, year, taxa, .keep_all = T) %>%
  ungroup()

zpGB12 <- zpGB11 %>%
  group_by(season, year, taxa) %>%
  mutate(YcAV_yr = mean(Yc, na.rm= TRUE), 
         YcSD_yr = sd(Yc, na.rm= TRUE)) %>%
  ungroup()

# check
zpGB12 %>% filter(taxa == "chaeto") %>%
  ggplot(aes(x = Date, y = YcAV_yr)) +
  geom_point()

zpGB12 %>% filter(taxa == "chaeto") %>%
  ggplot(aes(x = Date, y = YcSD_yr)) +
  geom_point()
#############################################################

# 4) calculate anomalies
zpGB12 <- zpGB12 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

# check
zpGB12 %>% filter(taxa == "chaeto") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point()

# any NAs?
zpGB12 %>% filter(taxa == "chaeto") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point() +
  geom_line()
#yes
zpGB121 <- zpGB12
zpGB121 <- zpGB121 %>% filter(season == "winter")
ggplot(data = zpGB121, aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - winter") +
  geom_hline(yintercept = 0)



####
SNESNE <- SNE
SNESNE$Yi <- log10(SNESNE$ind_10m2+1)
#SNE$ind_plus1 <- SNE$ind_10m2+1

# check
SNESNE %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Yi)) +
  geom_point()
#############################################################

# Yc cruise mean of Yi within region by season
# 2) take mean by cruise for each taxa within SNE
SNESNESNE1 <- SNESNE %>%
  group_by(taxa, year) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(taxa, year, .keep_all = T) %>%
  ungroup()

#SNESNESNE1 <- SNESNESNE1 %>%
  group_by(cruise_name, Name, season, year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(cruise_name, Name, season, year, taxa, .keep_all = T) %>%
  ungroup()


SNESNESNE12 <- SNESNESNE1 %>%
  group_by(taxa, year) %>%
  mutate(YcAV_yr = mean(Yc, na.rm= TRUE), 
         YcSD_yr = sd(Yc, na.rm= TRUE)) %>%
  ungroup()

dfsd <- SNESNESNE12 %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

dfsd %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_point()


















SNEq<- SNE
SNEq$Yi <- log10(SNEq$ind_10m2+1)

SNEq1 <- SNEq %>%
  group_by(taxa) %>%
  mutate(log_mean_abundance = mean(Yi, na.rm = TRUE))

# Step 2: Calculate the long-term log10 mean abundance for each species 
#within each year
zooplankton_data <- SNEq1 %>%
  group_by(year, taxa) %>%
  mutate(year_log_mean_abundance = mean(Yi, na.rm = TRUE)) %>%
  distinct(year, taxa, .keep_all = T)


# Step 3: Calculate the standard deviation of long-term log10 abundances for each species within each year
zooplankton_data1 <- zooplankton_data %>%
  group_by(year, taxa) %>%
  mutate(year_log_sd_abundance = sd(Yi, na.rm = TRUE))

zooplankton_data1 <- zooplankton_data %>%
  group_by(year, taxa) %>%
  mutate(year_log_sd_abundance = ifelse(n() > 1, sd(log10(abundance), na.rm = TRUE), 0))

# Step 4: Calculate the abundance anomalies from the long-term log10 mean and standardize by the SD for each year
zooplankton_data1$anomaly <- (log10(zooplankton_data1$Yi) - zooplankton_data1$year_log_mean_abundance) / zooplankton_data1$year_log_sd_abundance


zooplankton_data1 %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = anomaly)) +
  geom_point()









bnvbn <- SNE %>%
  group_by(season, type, year, taxa) %>%
  mutate(Yc = mean(Yi)) %>% 
  distinct(season, type, year, taxa, .keep_all = T) %>%
  ungroup()

bnvbnee <- bnvbn %>%
  group_by(season, type, year, taxa) %>%  
  mutate(YcAV_yr = mean(Yc), 
         YcSD_yr = sd(Yc)) %>%
  ungroup()

bnvbnee <- bnvbnee %>%
  #group_by(year, taxa) %>% doesnt make a difference
  mutate(Anomaly_yr = (Yc - YcAV_yr)/(YcSD_yr)) 

SNE1 %>% filter(taxa == "megan" & season == "summer") %>%
  ggplot(aes(x = Date, y = Yc)) +
  geom_point()

& season == "summer"

bnvbnee %>% filter(taxa == "megan") %>%
  ggplot(aes(x = Date, y = Anomaly_yr)) +
  geom_line() +
  geom_point() +
  #facet_wrap(~type) +
  facet_grid(type~season) +
  theme_classic() + 
  ggtitle("SNE - Meganyctiphanes norvegica - winter") +
  geom_hline(yintercept = 0)
