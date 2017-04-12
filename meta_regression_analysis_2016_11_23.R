# Knee biomechanics meta-regression analysis
# Author: Scott Telfer
# Email: scott.telfer@gmail.com
# Last updated: 2017/03/08

# =============================================================================

## Import libraries
source("C:/Users/telfers/Dropbox/R Code/pkgTest.R")
packages <- c("metafor", "readr", "ggplot2", "dplyr")
lapply(packages, pkgTest)


# =============================================================================

## Import data sheet
meta_data <- read_csv(paste0("C:/Users/telfers/Dropbox/My_Projects/", 
                             "KneeOA GA review/Knee_Add_Moment_Review/", 
                             "KAM_meta_data.csv"))

# Convert relevant columns to factors
cols <- c("Cohort_type", "Footwear", "Units", "Model")
meta_data[cols] <- lapply(meta_data[cols], factor)


# =============================================================================

## Visualise raw data to check for issues, continuous variables
# Number of participants
boxplot(meta_data$Number, main = "Number of Participants")
summary(meta_data$Number)

# Percentage of participants who are female
boxplot(meta_data$Sex_Perc_F, main = "Percentage Female")
summary(meta_data$Sex_Perc_F)

# Age
boxplot(meta_data$Age, main = "Age")
summary(meta_data$Age)

# Height
boxplot(meta_data$Height, main = "Height")
summary(meta_data$Height)

# Mass
boxplot(meta_data$Mass, main = "Weight")
summary(meta_data$Mass)

# BMI
boxplot(meta_data$BMI, main = "BMI")
summary(meta_data$BMI)

# Speed
boxplot(meta_data$Speed, main = "Speed")
summary(meta_data$Speed)

## Visualize data to check for issues with disecrete variables
# Cohort type
barplot(table(meta_data$Cohort_type))
levels(meta_data$Cohort_type)

# Repeated
barplot(table(meta_data$Repeated))
levels(meta_data$Repeated)

# Footwear
barplot(table(meta_data$Footwear))
levels(meta_data$Footwear)

# Units
barplot(table(meta_data$Units))
levels(meta_data$Units)

# Model
barplot(table(meta_data$Model))
levels(meta_data$Model)


# =============================================================================

## Convert all KAM units to to %BW*H
# Select data by reported units 
md_BWxH <- filter(meta_data, Units == "%BW*H")
md_Nmkg <- filter(meta_data, Units == "Nm/kg", !is.na(Height))
md_Nm <- filter(meta_data, Units == "Nm", !is.na(Height), !is.na(Mass))
md_Nmkgm <- filter(meta_data, Units == "Nm/kg*m")

# To convert Nm/kg to %BW*H, divide by height multiplied by 9.81 (Note that 
# this does not give the exact answer but is very close)
md_Nmkg <- mutate(md_Nmkg, KAM_Peak = (KAM_Peak / (Height * 9.81)) * 100, 
                  KAM_Peak_SD = (KAM_Peak_SD / (Height * 9.81)) * 100,
                  KAM_1 = (KAM_1 / (Height * 9.81)) * 100, 
                  KAM_1_SD = (KAM_1_SD / (Height * 9.81)) * 100, 
                  KAM_2 = (KAM_2 / (Height * 9.81)) * 100, 
                  KAM_2_SD = (KAM_2_SD / (Height * 9.81)) * 100, 
                  Units = "%BW*H")
#boxplot(md_Nmkg$KAM_Peak, md_Nmkg$KAM_1, md_Nmkg$KAM_2)

# To convert Nm to %BW*H, divide by mass * height * 9.81 
md_Nm <- mutate(md_Nm, KAM_Peak = (KAM_Peak / (Mass * Height * 9.81)) * 100, 
                KAM_Peak_SD = (KAM_Peak_SD / (Mass * Height * 9.81)) * 100,
                KAM_1 = (KAM_1 / (Mass * Height * 9.81)) * 100, 
                KAM_1_SD = (KAM_1_SD / (Mass * Height * 9.81)) * 100, 
                KAM_2 = (KAM_2 / (Mass * Height * 9.81)) * 100, 
                KAM_2_SD = (KAM_2_SD / (Mass * Height * 9.81)) * 100,
                Units = "%BW*H")
#boxplot(md_Nm$KAM_Peak, md_Nm$KAM_1, md_Nm$KAM_2)

# To convert Nm/kg*m to %BW*H, multiply by 9.81
md_Nmkgm <- mutate(md_Nmkgm, KAM_Peak = KAM_Peak * 9.81,
                   KAM_Peak_SD = KAM_Peak_SD * 9.81, KAM_1 = KAM_1 * 9.81,
                   KAM_1_SD = KAM_1_SD * 9.81, KAM_2 = KAM_2 * 9.81,
                   KAM_2_SD = KAM_2_SD * 9.81, Units = "%BW*H")
#boxplot(md_Nmkgm$KAM_Peak, md_Nmkgm$KAM_1, md_Nmkgm$KAM_2)

# Recombine into single data frame
meta_data_units <- bind_rows(md_BWxH, md_Nmkg, md_Nm, md_Nmkgm)
meta_data_units <- arrange(meta_data_units, Paper_no)


# =============================================================================

## Generate data frames for analysis (inclusing effect sizes)
# Select only studies using PiG
PiG_meta_data <- filter(meta_data_units, Model %in% "PiG")

# Use only rows with KAM1
PiG_meta_data <- PiG_meta_data %>% filter(!is.na(KAM_1))

# Remove cohorts with < 2 
PiG_meta_data <- PiG_meta_data %>% group_by(Cohort_type) %>% filter(n() > 2)

# Remove footwear with < 2
PiG_meta_data <- PiG_meta_data %>% group_by(Footwear) %>% filter(n() > 2)

# Remove cohorts with no walking speed
PiG_meta_data <- PiG_meta_data %>% filter(!is.na(Speed))

# Reorder by cohort type
PiG_meta_data <- PiG_meta_data %>% arrange(Cohort_type)

# Combine standardized footwear into one variable
PiG_meta_data$Footwear <- factor(PiG_meta_data$Footwear)
levels(PiG_meta_data$Footwear) <- c("STANDARD", "BF", "STANDARD", "OWN")
PiG_meta_data$Footwear <- factor(PiG_meta_data$Footwear, 
                                 levels = c("BF",  "OWN", "STANDARD"))

# Convert back to factors to remove non-present levels
PiG_meta_data$Cohort_type <- factor(PiG_meta_data$Cohort_type)
PiG_meta_data$Footwear <- factor(PiG_meta_data$Footwear)

# Determine effect sizes
PiG_meta_data_es <- escalc(measure = "MN", mi = KAM_1, sdi = KAM_1_SD, 
                           ni = Number, data = PiG_meta_data, 
                           slab = paste(First_author, Year))

# Remove non_standardized repeats
PiG_meta_data_STD <- PiG_meta_data_es[c(1, 2, 5:13, 15:21, 23:29), ]


# =============================================================================

## Test for effect of Speed, age, cohort type, and footwear
# Run mixed effects model for cohort type, footwear, walking speed and age
res_ME <- rma(yi, vi, mods = ~ Cohort_type + Speed + Footwear, 
              data = PiG_meta_data_es, method = "REML")
res_ME
anova(res_ME, btt = 1:3) # Test effect of cohort type as a whole
anova(res_ME, btt = 5:7) # Test effect of footwear as a whole

# Re-run mixed effect model using STD dataframe
res_ME_STD <- rma(yi, vi, mods = ~ Cohort_type + Speed + Footwear, 
                  data = PiG_meta_data_STD, method = "REML")
res_ME_STD
anova(res_ME_STD, btt = 1:3) # Test effect of cohort type as a whole
anova(res_ME_STD, btt = 5:7) # Test effect of footwear as a whole


# =============================================================================

## Make cohort forest plot
# Set up image to save
png(filename = "Forest_Cohorts.png", units = "in", width = 3.5, height = 4.5, 
    pointsize = 12, res = 300)

# decrease margins so the full space is used
par(mar = c(4,4,1,2))

# set up forest plot (with 2x2 table counts added; rows argument is used
# to specify exactly in which rows the outcomes will be plotted)
forest(res_ME, xlab = "KAM (%BW*H)", xlim = c(-6, 10), 
       rows = c(2:14, 20:22, 28:40), mlab = "KAM (%BW*H)", ylim = c(-1,45),
       alim = c(0, 6), at = c(0:6),psize = 1, cex = 0.35, addfit = FALSE, 
       width = 2.0, refline = NA)

# set font expansion factor (as in forest() above) and use bold italic
# font and save original settings in object 'op'
op <- par(cex = 0.4, font = 4)

# add text for the subgroups
text(-6, c(15.5, 23.5, 41.5), pos = 4, c("Healthy", "KOA", "Medial KOA"))

# switch to bold font
par(font = 2)

# add column headings to the plot
text(-6, 46, "Author(s) and Year", pos = 4)
text(10, 46, "KAM (%BW*H) [95% CI]", pos = 2)

# set par back to the original settings
par(op)

# fit random-effects model in the three subgroups
res_HEALTHY <- rma(yi, vi, data = PiG_meta_data, subset = (Cohort_type == "HEALTHY"), 
                   method = "REML")
res_KOA <- rma(yi, vi, data = PiG_meta_data, subset = (Cohort_type == "KOA"), 
               method = "REML")
res_MED_KOA <- rma(yi, vi, data = PiG_meta_data, subset = (Cohort_type == "MED_KOA"), 
                   method = "REML")

# add summary polygons for the three subgroups
addpoly(res_HEALTHY, row =  0.5, cex = 0.4, mlab = "RE Model for Subgroup")
addpoly(res_KOA, row = 18.5, cex = 0.4, mlab = "RE Model for Subgroup")
addpoly(res_MED_KOA, row = 26.5, cex = 0.4, mlab = "RE Model for Subgroup")

dev.off()


# =============================================================================

## Make footwear forest plots 
# Dataframe including only studies with footwear reported
PiG_meta_data_Footwear <- PiG_meta_data %>% filter(!is.na(Footwear))
PiG_meta_data_Footwear <- PiG_meta_data_Footwear %>% arrange(Footwear) %>% 
  filter(n() > 1)
PiG_meta_data_Footwear <- escalc(measure = "MN", mi = KAM_1, sdi = KAM_1_SD, 
                                 ni = Number, data = PiG_meta_data_Footwear, 
                                 slab = paste(First_author, Year))
res_ME_fw <- rma(yi, vi, mods = ~ Footwear + Cohort_type + Speed, 
                 data = PiG_meta_data_Footwear, method = "REML")

# Set up image to save
png(filename = "Forest_footwear.png", units = "in", width = 3.5, height = 4.5, 
    pointsize = 12, res = 300)

# decrease margins so the full space is used
par(mar = c(4,4,1,2))

# set up forest plot (with 2x2 table counts added; rows argument is used
# to specify exactly in which rows the outcomes will be plotted)
forest(res_ME_fw, xlab = "KAM (%BW*H)", xlim = c(-6, 10), 
       rows = c(2:15, 21:29, 35:40), mlab = "KAM (%BW*H)", 
       ylim = c(-1, 45), alim = c(0, 6), at = c(0:6), psize = 1, cex = 0.35,
       addfit = FALSE, width = 2.0, refline = NA)

# set font expansion factor (as in forest() above) and use bold italic
# font and save original settings in object 'op'
op <- par(cex = 0.4, font = 4)

### add text for the subgroups
text(-6, c(41.5, 16.5, 30.5), pos = 4, c("Standard", "Barefoot", "Own"))

# switch to bold font
par(font = 2)

# add column headings to the plot
text(-6, 45, "Author(s) and Year", pos = 4)
text(10, 45, "KAM (%BW*H) [95% CI]", pos = 2)

# set par back to the original settings
par(op)

# fit random-effects model in the three subgroups
res_STANDARD <- rma(yi, vi, data = PiG_meta_data_Footwear, 
                    subset = (Footwear == "STANDARD"), method = "REML")
res_BF <- rma(yi, vi, data = PiG_meta_data_Footwear, 
              subset = (Footwear == "BF"), method = "REML")
res_OWN <- rma(yi, vi, data = PiG_meta_data_Footwear, 
               subset = (Footwear == "OWN"), method = "REML")

# add summary polygons for the three subgroups
addpoly(res_STANDARD, row = 33.5, cex = 0.4, mlab = "RE Model for Subgroup")
addpoly(res_BF, row = 0.5, cex = 0.4, mlab = "RE Model for Subgroup")
addpoly(res_OWN, row = 19.5, cex = 0.4, mlab = "RE Model for Subgroup")
        
dev.off()


# =============================================================================

## Combined figure 
g <- ggplot(PiG_meta_data, aes(x = Speed, y = KAM_1), group = Cohort_type)
g <- g + geom_point(na.rm = TRUE, aes(colour = Cohort_type, shape = Footwear),
                    size = 2)
g <- g + geom_smooth(method = "lm", se = FALSE, na.rm = TRUE, colour = "Grey50")
g <- g + theme_bw()
g <- g + theme(legend.background = element_rect(colour = "black"),
               legend.title = element_text(face = "bold"))
g <- g + xlab("Speed (m/s)") + ylab("KAM 1st Peak (%BW*H)")
g <- g + labs(colour = "Cohort", shape = "Footwear")
g

ggsave("Figure_2.png", g, dpi = 300)


# =============================================================================

## Summary data
# No. of participants (no repeats)
sum(PiG_meta_data$Number[-c(4, 6, 15, 23)])

# Mean walking speeds by cohort type
PiG_meta_data %>%
  group_by(Cohort_type) %>%
  summarize(mean(Speed, na.rm = TRUE))

# Mean walking speed by footwear
PiG_meta_data %>%
  group_by(Footwear) %>%
  summarize(mean(Speed, na.rm = TRUE))


###############################################################################
# =============================================================================
# END OF CODE 
# =============================================================================
###############################################################################