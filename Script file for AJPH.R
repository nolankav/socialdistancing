# County-level determinants of engagement in social distancing for COVID-19
# N.M. Kavanagh, R.R. Goel, A.S. Venkataramani
# September 22, 2020

# Please direct questions about this script file to nolan.kavanagh@pennmedicine.upenn.edu.

library(foreign)     # Import dataset
library(psych)       # Analysis tools
library(car)         # Analysis tools
library(dplyr)       # Analysis tools
library(tidyr)       # Analysis tools
library(Hmisc)       # Analysis tools
library(forcats)     # Reverse factors
library(stringr)     # Data editing
library(gtools)      # Analysis tools
library(ggplot2)     # Graphing tools
library(scales)      # Graphing tools
library(corrplot)    # Graphing tools
library(cowplot)     # Graphing tools
library(viridis)     # Graphing tools
library(zoo)         # Analysis tools

##############################################################################
# Data preparation
##############################################################################

# Set working directory (for exporting figures)
setwd("/Users/nolankavanagh/Dropbox/COVID projects/")

# Set maximum date
MAX_DATE <- as.Date("2020-09-13")

##############################################################################
# Social distancing dataset
##############################################################################

# Read dataset into R
social <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/US SDS-2020-09-21-sds-v3-full-county.csv")

##############################################################################
# Presidential voting dataset
##############################################################################

# Read dataset into R
politics <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/countypres_2000-2016.csv")
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

# Calculate vote percentages
politics$percent <- politics$candidatevotes/politics$totalvotes

# Select necessary columns
politics <- politics %>% select(year, state, county, FIPS, party, percent)

# Restructure dataset to wide
politics <- politics %>%
  pivot_wider(names_from = c(party, year), values_from = c(percent))

##############################################################################
# COVID-19 dataset
##############################################################################

# Read dataset into R
covid <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/us-counties.csv")

##############################################################################
# Census/ACS datasets
##############################################################################

# Source: American Community Survey, 5-year averages 2014-2018

# County size, percent male, age distribution, percent Black, percent Hispanic
census_demogr <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/ACSDP5Y2018.DP05_data_with_overlays_2020-03-27T211621.csv")
census_demogr <- census_demogr %>% select(GEO_ID, NAME, DP05_0001E, DP05_0002PE, DP05_0065PE, DP05_0071PE, DP05_0005PE, DP05_0006PE, DP05_0007PE, DP05_0008PE, DP05_0009PE, DP05_0010PE, DP05_0011PE, DP05_0012PE, DP05_0013PE, DP05_0014PE, DP05_0015PE, DP05_0016PE, DP05_0017PE)

# Percent with college degree (age 25+), percent foreign-born
census_demog2 <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/ACSDP5Y2018.DP02_data_with_overlays_2020-04-08T113651.csv")
census_demog2 <- census_demog2 %>% select(GEO_ID, NAME, DP02_0067PE, DP02_0092PE)

# Income per capita
census_income <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/ACSST5Y2018.S1902_data_with_overlays_2020-03-27T201025.csv")
census_income <- census_income %>% select(GEO_ID, NAME, S1902_C03_019E)

# Percent in specific industries
census_sector <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/ACSDP5Y2018.DP03_data_with_overlays_2020-03-29T185057.csv")
census_sector <- census_sector %>% select(GEO_ID, NAME, DP03_0037PE, DP03_0038PE, DP03_0042PE)

# Percent of population in rural area
census_rural <- read.csv("/Users/nolankavanagh/Dropbox/COVID projects/Datasets/DECENNIALSF12010.H2_data_with_overlays_2020-03-28T144434.csv")
census_rural <- census_rural %>% select(GEO_ID, NAME, H002001, H002005)

# Re-specify FIPS for renamed counties
census_rural <- census_rural %>%
  mutate(
    GEO_ID = case_when(
      GEO_ID == "0500000US46113" ~ "0500000US46102", # Ogala Lakota, South Dakota
      GEO_ID == "0500000US02270" ~ "0500000US02158", # Kusilvak Census Area, Alaska
      TRUE ~ as.character(GEO_ID),
    )
  )

# Merge Census datasets
census <- census_demogr %>% full_join(census_demog2)
census <- census        %>% full_join(census_income)
census <- census        %>% full_join(census_sector)
census <- census        %>% full_join(census_rural, by = c("GEO_ID"))

# Rename column names
census <- census %>%
  rename(population = DP05_0001E,
         perc_male  = DP05_0002PE,
         perc_65_74 = DP05_0015PE,
         perc_75_84 = DP05_0016PE,
         perc_85_up = DP05_0017PE,
         perc_black = DP05_0065PE,
         perc_Hisp  = DP05_0071PE,
         income     = S1902_C03_019E,
         perc_rtail = DP03_0037PE,
         perc_trspt = DP03_0038PE,
         perc_hlthc = DP03_0042PE,
         perc_collg = DP02_0067PE,
         num_rural  = H002005,
         num_2010   = H002001,
         perc_forgn = DP02_0092PE,
         )

# Remove extraneous row
census <- census[-grep("id", census$GEO_ID),]

# Reformat FIPS codes
census$GEO_ID <- as.numeric(str_remove(census$GEO_ID, "0500000US"))

# Treat variables as numeric
convert <- c("population", "perc_male", "perc_65_74", "perc_75_84", "perc_85_up", "perc_black", "perc_Hisp", "income", "perc_rtail", "perc_trspt", "perc_hlthc", "perc_collg", "num_rural", "num_2010", "perc_forgn")
census <- census %>% mutate_at(convert, ~as.numeric(as.character(.x)))

# Calculate elderly age bracket
census$elder = census$perc_65_74 + census$perc_75_84 + census$perc_85_up

# Calculate percentage rural population in 2020
census$perc_rural = census$num_rural/census$num_2010*100

##############################################################################
# Merge datasets
##############################################################################

# Merge datasets
merged <- census %>% full_join(social,   by = c("GEO_ID" = "county_fips"))
merged <- merged %>% full_join(covid,    by = c("GEO_ID" = "fips", "date"))
merged <- merged %>% full_join(politics, by = c("GEO_ID" = "FIPS"))

# Treat date variable as date
merged$date <- as.Date(merged$date)

# Order by county and date
merged <- merged %>% arrange(GEO_ID, date)

# Eliminate dates after max. date
merged <- subset(merged, date <= MAX_DATE)

# Eliminate duplicate dates
merged <- merged %>% distinct()

# Fill in no reported cases/deaths as "0"
merged <- merged %>% mutate(
  cases_full = case_when(
    !is.na(cases) ~ as.numeric(cases),
    TRUE ~ 0)
)
merged <- merged %>% mutate(
  deaths_full = case_when(
    !is.na(deaths) ~ as.numeric(deaths),
    TRUE ~ 0)
)

# Calculate rolling averages
merged <- merged %>%
  group_by(GEO_ID) %>%
  mutate(dist_roll_7 = rollmean(daily_distance_diff, k = 7, align = "right", fill = NA))

# Assign days of week
merged$day_of_week <- weekdays(as.Date(as.character(merged$date),'%Y-%m-%d'))
merged$day_of_week <- factor(merged$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Complete cases for regression variables
merged <- merged %>% 
  filter_at(vars(c("dist_roll_7", "income", "perc_rural", "republican_2016")), all_vars(!is.na(.)))

##############################################################################
# Regressions
##############################################################################

# Regression dataset
merged_reg <- merged

# Properly scale variables
merged_reg$dist_roll_7           <- merged_reg$dist_roll_7*100
merged_reg$daily_distance_diff   <- merged_reg$daily_distance_diff*100

# Scale predictors by IQR
scale <- c("perc_male", "elder", "perc_black", "perc_Hisp", "perc_collg", "income", "perc_rtail", "perc_trspt", "perc_hlthc", "perc_forgn", "perc_rural", "republican_2016")
merged_reg <- as.data.frame(merged_reg) %>% mutate_at(scale, ~((.x-median(.x))/IQR(.x)))

# Dataframe for values
reg_df <- NULL

for(i in 1:200) {
  # Regressions
  reg_dist <- glm(dist_roll_7 ~ state_name + perc_male + elder + perc_black + perc_Hisp + income + perc_rtail + perc_trspt + perc_hlthc + perc_forgn + perc_rural + republican_2016,
                  subset(merged_reg, date==(as.Date("2020-03-08")+i)), family="gaussian")
  
  # Capture coefficients and errors
  temp_df1 <- as.data.frame(cbind(coef(summary(reg_dist))[,1], coef(summary(reg_dist))[,2], "Distance"))
  colnames(temp_df1) <- c("beta","error","outcome")

  # Add variable names
  temp_df1 <- tibble::rownames_to_column(temp_df1, var="var")
  
  # Assign proper date
  temp_df1$date <- (as.Date("2020-03-08")+i)
  
  # Add to dataframe
  reg_df <- rbind(reg_df, temp_df1)
}

# Evaluate assumptions
# plot(density(merged$daily_distance_diff)); plot(reg)
# plot(density(reg$residuals))

# Clean up model for export
reg_df <- reg_df[-grep("Intercept",  reg_df$var),]
reg_df <- reg_df[-grep("state_name", reg_df$var),]

# Treat values as numeric
reg_df$beta  <- as.numeric(as.character(reg_df$beta))
reg_df$error <- as.numeric(as.character(reg_df$error))

# Calculate 95% confidence intervals
reg_df$lowerOR <- reg_df$beta - 1.96*reg_df$error
reg_df$upperOR <- reg_df$beta + 1.96*reg_df$error

# Rename variables
reg_df$var <- sub("perc_male",  "Pct. Male", reg_df$var)
reg_df$var <- sub("perc_black", "Pct. Black", reg_df$var)
reg_df$var <- sub("perc_Hisp",  "Pct. Hispanic", reg_df$var)
reg_df$var <- sub("elder",      "Pct. 65 Years or Older", reg_df$var)
reg_df$var <- sub("perc_forgn", "Pct. Foreign-Born", reg_df$var)
reg_df$var <- sub("income",     "Per Capita Income", reg_df$var)
reg_df$var <- sub("perc_rural", "Pct. Rural", reg_df$var)
reg_df$var <- sub("republican_2016", "Pct. Trump Support", reg_df$var)
reg_df$var <- sub("perc_rtail", "Pct. Employment in\nRetail", reg_df$var)
reg_df$var <- sub("perc_trspt", "Pct. Employment in\nTransport.", reg_df$var)
reg_df$var <- sub("perc_hlthc", "Pct. Employment in\nHealth, Education, Social", reg_df$var)
reg_df$var <- factor(reg_df$var, levels = c("Pct. Male", "Pct. Black", "Pct. Hispanic", "Pct. 65 Years or Older", "Pct. Foreign-Born", "Pct. with College Degree", "Per Capita Income", "Pct. Rural", "Pct. Trump Support", "Pct. Employment in\nRetail", "Pct. Employment in\nTransport.", "Pct. Employment in\nHealth, Education, Social"))

# Graph rolling coefficients
coeff_plot <- ggplot(data=subset(reg_df, outcome=="Distance"),
                     aes(x=as.Date(date), y=beta, ymin=lowerOR, ymax=upperOR, group=var)) +
  facet_wrap(~var, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  geom_ribbon(alpha=0.2, color=NA) +
  geom_line(alpha=1) +
  theme_test() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_text(angle = 90),
        strip.background = element_blank(),
        strip.text = element_text(face="bold", color="black"),
        panel.grid.major.x = element_line(color="light gray", size=0.25),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  xlab("Date (End of 7-Day Rolling Average)") +
  ylab("Percentage-Point Change in Average Momement,\nGiven Interquartile Increase in Characteristic") +
  scale_x_date(labels = date_format("%m/%d"),
               limits = c(as.Date("2020-03-16"), MAX_DATE),
               breaks = seq(as.Date("2020-03-16"), MAX_DATE, by="month")) +
  scale_y_continuous(limits = c(-5.5,8), breaks = c(-5,-2.5,0,2.5,5,7.5))

# Print figure
ggsave(plot=coeff_plot, file="Coefficients over time, roll.pdf", width=7, height=6, units='in', dpi=600)

##############################################################################
# Graph: Distancing by income/politics
##############################################################################

# Define quintiles for income and politics
merged$income_cat <- cut_number(merged$income, n = 5)
merged$trump_cat  <- cut_number(merged$republican_2016, n = 5)

# Get mean values for each quintile
merged %>% 
  group_by(income_cat) %>%  
  summarize(income_min = min(income),
            income_max = max(income),
            .groups = "keep")
merged %>% 
  group_by(trump_cat) %>%  
  summarize(trump_min = min(republican_2016),
            trump_max = max(republican_2016),
            .groups = "keep")

# Rename quintiles
merged <- merged %>% mutate(
  income_cat = dplyr::recode(income_cat,
    "[1.09e+04,2.2e+04]"  = "Q1 ($10,931 - $21,963)",
    "(2.2e+04,2.47e+04]"  = "Q2 ($21,965 - $24,732)",
    "(2.47e+04,2.75e+04]" = "Q3 ($24,739 - $27,522)",
    "(2.75e+04,3.11e+04]" = "Q4 ($27,523 - $31,091)",
    "(3.11e+04,7.28e+04]" = "Q5 ($31,096 - $72,832)"
    )
  )
merged <- merged %>% mutate(
  trump_cat = dplyr::recode(trump_cat,
    "[0.0409,0.502]" = "Q1 (4.1% - 50.2%)",
    "(0.502,0.618]"  = "Q2 (50.2% - 61.8%)",
    "(0.618,0.697]"  = "Q3 (61.9% - 69.7%)",
    "(0.697,0.762]"  = "Q4 (69.8% - 76.2%)",
    "(0.762,0.916]"  = "Q5 (76.3% - 91.6%)"
    )
  )

# Define week variable
merged <- merged %>%
  mutate(week = cut.Date(as.Date(date), breaks = "1 week", labels = F, start.on.monday = T)) %>% arrange(date)

# Rename week variable
merged <- merged %>% mutate(
  week = dplyr::recode(week,
    "1"  = NA_character_,
    "2"  = "03/02-\n03/08",
    "3"  = "03/09-\n03/15",
    "4"  = "03/16-\n03/22",
    "5"  = "03/23-\n03/29",
    "6"  = "03/30-\n04/05",
    "7"  = "04/06-\n04/12",
    "8"  = "04/13-\n04/19",
    "9"  = "04/20-\n04/26",
    "10" = "04/27-\n05/03",
    "11" = "05/04-\n05/10",
    "12" = "05/11-\n05/17",
    "13" = "05/18-\n05/24",
    "14" = "05/25-\n05/31",
    "15" = "06/01-\n06/07",
    "16" = "06/08-\n06/14",
    "17" = "06/15-\n06/21",
    "18" = "06/22-\n06/28",
    "19" = "06/29-\n07/05",
    "20" = "07/06-\n07/12",
    "21" = "07/13-\n07/19",
    "22" = "07/20-\n07/26",
    "23" = "07/27-\n08/02",
    "24" = "08/03-\n08/09",
    "25" = "08/10-\n08/16",
    "26" = "08/17-\n08/23",
    "27" = "08/24-\n08/30",
    "28" = "08/31-\n09/13"
  )
)

# Summarize county movement by week
distance_df <- merged %>% 
  group_by(week, GEO_ID, income_cat, trump_cat) %>%
  summarise(daily_distance_diff = mean(daily_distance_diff, na.rm = TRUE))

# Plot movement by income quintiles
income_plot <- ggplot(subset(distance_df, !is.na(week)),
                      aes(y=daily_distance_diff, fill=income_cat)) +
  geom_boxplot(outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  facet_wrap(week ~ ., ncol = 27, strip.position = "bottom") +
  scale_fill_viridis_d() +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  theme_test() +
  theme(legend.position = "top",
        legend.title = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  # xlab("Week") +
  ylab("Change in Average Movement") +
  labs(fill = "Income Quintiles") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-1, 0.5))

# Plot movement by politics quintiles
trump_plot <- ggplot(subset(distance_df, !is.na(week)),
                     aes(y=daily_distance_diff, fill=trump_cat)) +
  geom_boxplot(outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  facet_wrap(week ~ ., ncol = 27, strip.position = "bottom") +
  scale_fill_viridis_d() +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  theme_test() +
  theme(legend.position = "top",
        legend.title = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  xlab("Week") +
  ylab("Change in Average Movement") +
  labs(fill = "Trump Support Quintiles") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-1, 0.5))

# Compile figures into object
quintile_plot <- plot_grid(income_plot, trump_plot, labels = c("A.", "B."), nrow = 2)

# Print figure
ggsave(plot=quintile_plot, file="Quintile plot.pdf", width=13, height=7, units='in', dpi=600)

##############################################################################
# Graph: Distancing by day with correlations
##############################################################################

# Plot movement by day
distance_plot <- ggplot(merged, aes(x=date, y=daily_distance_diff, group=date)) +
  geom_rect(aes(xmin=as.Date("2020-02-10"), xmax=as.Date("2020-03-08"),
                ymin=-Inf, ymax=Inf), fill='gray90') +
  geom_boxplot(fill="grey", outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  theme_test() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_text(angle=0, hjust=0),
        panel.grid.major.x = element_line(color="light gray", size=0.25),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  # xlab("Date") +
  ylab("Change in Average Movement") +
  scale_x_date(position = "top",
               labels = date_format("%m/%d"),
               breaks = seq(as.Date("2020-03-01"), MAX_DATE+1, by="month")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(-1, 0.5),
                  xlim = as.Date(c("2020-03-06", "2020-09-08"))) +
  geom_vline(xintercept=as.Date('2020-03-13'), linetype="solid", color="black", size=0.5) +
  annotate("text", x=as.Date('2020-03-14'), y=0.4, label="National emergency declared", fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=as.Date('2020-02-28'), y=-0.75, label="Reference\nperiod", fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=as.Date('2020-08-18'), y=0.4, label="Less social distancing", fontface=4, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=as.Date('2020-08-18'), y=-0.6, label="More social distancing", fontface=4, size=3, hjust=0, vjust=0.5)

# Print figure
ggsave(plot=distance_plot, file="Distance plot.pdf", width=11, height=3.5, units='in', dpi=600)

# Create dataframe of county-level characteristics
merged_corr <- merged %>% 
  group_by(GEO_ID) %>%
  summarise("Pct. Male"                 = mean(perc_male),
            "Pct. Black"                = mean(perc_black),
            "Pct. Hispanic"             = mean(perc_Hisp),
            "Pct. 65 Years or Older"    = mean(elder),
            "Pct. Foreign"              = mean(perc_forgn),
            "Per Capita Income"         = mean(income),
            "Pct. Rural"                = mean(perc_rural),
            "Pct. Trump Support"        = mean(republican_2016),
            "Pct. Emp. in Retail"       = mean(perc_rtail),
            "Pct. Emp. in Transport."   = mean(perc_trspt),
            "Pct. Emp. in Health, etc." = mean(perc_hlthc))

# Reformat date
merged$date_short <- format(merged$date, "%m-%d")

# Spread movement by date
merged_spread <- merged %>%
  select(GEO_ID, date_short, daily_distance_diff) %>%
  spread(key = date_short, value = daily_distance_diff)

# Merge with county-level characteristics
merged_redux <- merge(merged_spread, merged_corr, by="GEO_ID")

# Remove county ID
merged_redux <- merged_redux %>% select(-GEO_ID)

# Run correlation matrix
corr_matrix <- rcorr(as.matrix(merged_redux), type="pearson")

# Create matrices for coefficients, p-values
corr_coef <- corr_matrix$r
corr_pval <- corr_matrix$P

# Remove date-date correlations
corr_coef <- corr_coef[(ncol(corr_coef)-10):ncol(corr_coef), 1:(ncol(corr_coef)-11)]
corr_pval <- corr_pval[(ncol(corr_coef)-10):ncol(corr_coef), 1:(ncol(corr_coef)-11)]

# Define colors for correlation plot
col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# Prepare graphic for export
pdf(file = "Correlation matrix by date.pdf")

# Plot correlation matrix
corr_plot <- corrplot(corr_coef, method = "color", col = col(200), cl.pos = "n",
                      
                      # Not a correlation matrix
                      is.corr = FALSE,
                      
                      # Reorder variables
                      order = "original",
                      
                      # Labels color, size, and rotation
                      tl.col = "black", tl.cex = 0.25, number.cex = 0.25, tl.srt = 90,
                      
                      # Use significance levels
                      p.mat = corr_pval, sig.level = 0.00003,
                      insig = "label_sig", pch.cex = 0.25)

# Print figure
dev.off()

##############################################################################
# Graph: Correlations of county-level characteristics
##############################################################################

# Create dataframe of county-level characteristics
merged_corr <- merged %>% 
  group_by(GEO_ID) %>%
  summarise("Pct. Male"                 = mean(perc_male),
            "Pct. Black"                = mean(perc_black),
            "Pct. Hispanic"             = mean(perc_Hisp),
            "Pct. 65 Years or Older"    = mean(elder),
            "Pct. Foreign"              = mean(perc_forgn),
            "Per Capita Income"         = mean(income),
            "Pct. Rural"                = mean(perc_rural),
            "Pct. Trump Support"        = mean(republican_2016),
            "Pct. Emp. in Retail"       = mean(perc_rtail),
            "Pct. Emp. in Transport."   = mean(perc_trspt),
            "Pct. Emp. in Health, etc." = mean(perc_hlthc))

# Remove county ID
merged_corr <- merged_corr %>% select(-GEO_ID)

# Run correlation matrix
corr_matrix <- rcorr(as.matrix(merged_corr), type="pearson")

# Create matrices for coefficients, p-values
corr_coef <- corr_matrix$r
corr_pval <- corr_matrix$P

# Define colors for correlation plot
col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# Prepare graphic for export
pdf(file = "Correlation matrix.pdf")

# Plot correlation matrix
corr_plot <- corrplot(corr_coef, method = "color", col = col(200),
                      cl.pos = "n", type = "upper", 
         
                      # Reorder variables
                      order = "original",
                      
                      # Add coefficient of correlation
                      addCoef.col = "black", number.digits = 2,
                      
                      # Labels color, size, and rotation
                      tl.col = "black", tl.cex = 0.8, number.cex = 0.8, tl.srt = 45,
                      
                      # Hide correlation coefficient on principal diagonal
                      diag = FALSE, insig = "blank")

# Print figure
dev.off()