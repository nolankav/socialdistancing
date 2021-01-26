# County-level socioeconomic and political predictors of distancing for COVID-19
# N.M. Kavanagh, R.R. Goel, A.S. Venkataramani
# January 24, 2021

# Please direct questions about this script file to nolan.kavanagh@pennmedicine.upenn.edu.

library(foreign)     # Import dataset
library(psych)       # Analysis tools
library(car)         # Analysis tools
library(dplyr)       # Analysis tools
library(tidyr)       # Analysis tools
library(Hmisc)       # Analysis tools
library(lmtest)      # Analysis tools
library(miceadds)    # Cluster-robust models
library(forcats)     # Reverse factors
library(stringr)     # Data editing
library(gtools)      # Analysis tools
library(ggplot2)     # Graphing tools
library(scales)      # Graphing tools
library(corrplot)    # Graphing tools
library(cowplot)     # Graphing tools
library(zoo)         # Analysis tools

##############################################################################
# Data preparation
##############################################################################

# Set working directory
setwd("/Users/nolankavanagh/Dropbox/COVID projects/")

# Set maximum date
MAX_DATE <- as.Date("2021-01-17")

##############################################################################
# Social distancing dataset
##############################################################################

# Read dataset into R
social <- read.csv("Datasets/US SDS-2021-01-24-sds-v3-full-county.csv")

##############################################################################
# Presidential voting dataset
##############################################################################

# Read dataset into R
politics <- read.csv("Datasets/countypres_2000-2016.csv")
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
covid <- read.csv("Datasets/us-counties.csv")

##############################################################################
# Census/ACS datasets
##############################################################################

# Source: American Community Survey, 5-year averages 2014-2018

# County population, percent male, Black, Hispanic, 65+
census_demogr <- read.csv("Datasets/ACSDP5Y2018.DP05_data_with_overlays_2020-03-27T211621.csv")
census_demogr <- census_demogr %>% select(GEO_ID, NAME, DP05_0001E, DP05_0002PE, DP05_0065PE, DP05_0071PE, DP05_0015PE, DP05_0016PE, DP05_0017PE)

# Percent foreign-born
census_demog2 <- read.csv("Datasets/ACSDP5Y2018.DP02_data_with_overlays_2020-04-08T113651.csv")
census_demog2 <- census_demog2 %>% select(GEO_ID, NAME, DP02_0092PE)

# Income per capita
census_income <- read.csv("Datasets/ACSST5Y2018.S1902_data_with_overlays_2020-03-27T201025.csv")
census_income <- census_income %>% select(GEO_ID, NAME, S1902_C03_019E)

# Percent in specific industries
census_sector <- read.csv("Datasets/ACSDP5Y2018.DP03_data_with_overlays_2020-03-29T185057.csv")
census_sector <- census_sector %>% select(GEO_ID, NAME, DP03_0037PE, DP03_0038PE, DP03_0042PE)

# Percent of population in rural area
census_rural <- read.csv("Datasets/DECENNIALSF12010.H2_data_with_overlays_2020-03-28T144434.csv")
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
         num_rural  = H002005,
         num_2010   = H002001,
         perc_forgn = DP02_0092PE,
  )

# Remove extraneous row
census <- census[-grep("id", census$GEO_ID),]

# Reformat FIPS codes
census$GEO_ID <- as.numeric(str_remove(census$GEO_ID, "0500000US"))

# Treat variables as numeric
convert <- c("population", "perc_male", "perc_65_74", "perc_75_84", "perc_85_up", "perc_black", "perc_Hisp", "income", "perc_rtail", "perc_trspt", "perc_hlthc", "num_rural", "num_2010", "perc_forgn")
census <- census %>% mutate_at(convert, ~as.numeric(as.character(.x)))

# Calculate elderly age bracket
census$elder = census$perc_65_74 + census$perc_75_84 + census$perc_85_up

# Calculate percentage rural population in 2010
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

# Eliminate duplicate rows
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

# Calculate cases in past week
merged <- merged %>% group_by(GEO_ID) %>%
  mutate(case_lag_7 = dplyr::lag(cases_full, n = 7, default = NA))
merged$weekly_cases <- merged$cases_full - merged$case_lag_7

# Replace missing or negative cases with "0"
merged <- merged %>% mutate(
  weekly_cases = case_when(
    date == "2020-03-01" ~ 0, # No values to calculate
    weekly_cases < 0     ~ 0,
    TRUE ~ weekly_cases)
)

# Calculate cases per million
merged$weekly_cases_per_mil <- merged$weekly_cases/merged$population*1000000

# Define month variable
merged <- merged %>% mutate(
  month = case_when(
    date < as.Date("2020-04-01") ~ "March",
    date < as.Date("2020-05-01") ~ "April",
    date < as.Date("2020-06-01") ~ "May",
    date < as.Date("2020-07-01") ~ "June",
    date < as.Date("2020-08-01") ~ "July",
    date < as.Date("2020-09-01") ~ "August",
    date < as.Date("2020-10-01") ~ "September",
    date < as.Date("2020-11-01") ~ "October",
    date < as.Date("2020-12-01") ~ "November",
    date < as.Date("2021-01-01") ~ "December",
    date < as.Date("2021-02-01") ~ "January",
  )
)
merged$month <- factor(merged$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January"))

# Complete cases for regression variables
merged <- merged %>% 
  filter_at(vars(c("daily_distance_diff", "income", "perc_rural", "republican_2016")), all_vars(!is.na(.)))

##############################################################################
# Prepare pooled dataframe
##############################################################################

# Regression dataset
merged_pool <- subset(merged, date >= "2020-03-09" & date <= as.Date("2021-01-17"))

# Properly scale distancing
merged_pool$daily_distance_diff <- merged_pool$daily_distance_diff*100

# Scale predictors by IQR
scale <- c("perc_male", "elder", "perc_black", "perc_Hisp", "income", "perc_rtail", "perc_trspt", "perc_hlthc", "perc_forgn", "perc_rural", "republican_2016")
merged_pool <- as.data.frame(merged_pool) %>% mutate_at(scale, ~((.x-median(.x))/IQR(.x)))

# Define week variable
merged_pool <- merged_pool %>%
  mutate(week = cut.Date(date, breaks = "1 week", start.on.monday = T))

# Pool distancing by week
merged_pool <- merged_pool %>% 
  group_by(week, GEO_ID, state_name, perc_male, elder, perc_black, perc_Hisp, income, perc_rtail, perc_trspt, perc_hlthc, perc_forgn, perc_rural, republican_2016) %>%  
  summarise(distance = mean(daily_distance_diff),
            cases    = mean(weekly_cases_per_mil))

# Scale cases by IQR by week
merged_pool <- as.data.frame(merged_pool) %>% group_by(week) %>%
  mutate_at("cases", ~((.x-median(.x))/IQR(.x)))

##############################################################################
# Pooled main regression
##############################################################################

# Run pooled regression
reg_pool <- lm.cluster(formula=distance ~ state_name:week + perc_male:week + elder:week + perc_black:week + perc_Hisp:week + income:week + perc_rtail:week + perc_trspt:week + perc_hlthc:week + perc_forgn:week + perc_rural:week + republican_2016:week, data=merged_pool, cluster=merged_pool$GEO_ID)
summary(reg_pool)

# Capture coefficients and errors
pool_df <- as.data.frame(base::cbind(coeftest(reg_pool)[,1], coeftest(reg_pool)[,2]))
colnames(pool_df) <- c("beta","error")

# Add variable names
pool_df <- tibble::rownames_to_column(pool_df, var="var")
pool_df$date <- pool_df$var

# Remove states and intercept
pool_df <- pool_df[-grep("Intercept",  pool_df$var),]
pool_df <- pool_df[-grep("state_name", pool_df$var),]

# Clean up date column
pool_df$date <- str_remove(pool_df$date, "\\:[^.]*$")
pool_df$date <- str_remove(pool_df$date, "week")

# Clean up variable column
pool_df$var <- str_remove(pool_df$var, "[^.]*\\:")

# Treat values as numeric
pool_df$beta  <- as.numeric(as.character(pool_df$beta))
pool_df$error <- as.numeric(as.character(pool_df$error))

# Calculate 95% confidence intervals
pool_df$lowerOR <- pool_df$beta - 1.96*pool_df$error
pool_df$upperOR <- pool_df$beta + 1.96*pool_df$error

# Rename variables
pool_df$var <- sub("perc_male",  "Pct. Male", pool_df$var)
pool_df$var <- sub("perc_black", "Pct. Black", pool_df$var)
pool_df$var <- sub("perc_Hisp",  "Pct. Hispanic", pool_df$var)
pool_df$var <- sub("elder",      "Pct. 65 Years or Older", pool_df$var)
pool_df$var <- sub("perc_forgn", "Pct. Foreign-Born", pool_df$var)
pool_df$var <- sub("income",     "Per Capita Income", pool_df$var)
pool_df$var <- sub("perc_rural", "Pct. Rural", pool_df$var)
pool_df$var <- sub("republican_2016", "Pct. Trump Support", pool_df$var)
pool_df$var <- sub("perc_rtail", "Pct. Employment in\nRetail", pool_df$var)
pool_df$var <- sub("perc_trspt", "Pct. Employment in\nTransport.", pool_df$var)
pool_df$var <- sub("perc_hlthc", "Pct. Employment in\nHealth, Education, Social", pool_df$var)
pool_df$var <- sub("weekly_cases_per_mil", "Cases per Million\nin Past Week", pool_df$var)
pool_df$var <- factor(pool_df$var, levels = c("Pct. Male", "Pct. Black", "Pct. Hispanic", "Pct. 65 Years or Older", "Pct. Foreign-Born", "Pct. with College Degree", "Per Capita Income", "Pct. Rural", "Pct. Trump Support", "Pct. Employment in\nRetail", "Pct. Employment in\nTransport.", "Pct. Employment in\nHealth, Education, Social", "Cases per Million\nin Past Week"))

# Graph rolling coefficients
pool_plot <- ggplot(data=pool_df,
                    aes(x=as.Date(date), y=beta, ymin=lowerOR, ymax=upperOR, group=var)) +
  facet_wrap(~var, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  geom_errorbar(alpha=0.2) +
  # geom_ribbon(alpha=0.2, color=NA) +
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
  xlab("Date (Start of Pooled Week)") +
  ylab("Percentage-Point Change in Average Momement,\nGiven Interquartile Increase in Characteristic") +
  scale_x_date(labels = date_format("%b"),
               limits = c(as.Date("2020-03-01"), as.Date("2021-01-17")),
               breaks = seq(as.Date("2020-03-01"), as.Date("2021-01-17"), by="2 months")) +
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5,7.5)) +
  coord_cartesian(ylim = c(-5.5,8))

# Print figure
ggsave(plot=pool_plot, file="Pooled coefficients plot.pdf", width=7, height=6, units='in', dpi=600)

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

# Summarize county movement by month
distance_df <- subset(merged, date >= as.Date("2020-03-09")) %>% 
  group_by(month, GEO_ID, income_cat, trump_cat) %>%
  summarise(daily_distance_diff = mean(daily_distance_diff, na.rm = TRUE))

# Plot movement by income quintiles
income_plot <- ggplot(subset(distance_df, !is.na(month)),
                      aes(y=daily_distance_diff, fill=income_cat)) +
  geom_boxplot(outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  facet_wrap(month ~ ., ncol=11, strip.position = "bottom") +
  scale_fill_grey() +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  theme_test() +
  theme(legend.position = "none",
        legend.title = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  ylab("Change in Average Movement") +
  labs(fill = "Income Quintiles") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-1, 0.5))

# Legend for income quintiles
income_legend <- ggplot(subset(distance_df, !is.na(month)),
                        aes(y=daily_distance_diff, fill=income_cat)) +
  geom_boxplot(outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  scale_fill_grey() +
  theme_test() +
  theme(legend.position = "right",
        legend.title = element_text(face="bold")) +
  labs(fill = "Income Quintiles")

# Plot movement by politics quintiles
trump_plot <- ggplot(subset(distance_df, !is.na(month)),
                     aes(y=daily_distance_diff, fill=trump_cat)) +
  geom_boxplot(outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  facet_wrap(month ~ ., ncol=11, strip.position = "bottom") +
  scale_fill_grey() +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  theme_test() +
  theme(legend.position = "none",
        legend.title = element_text(face="bold"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  xlab("Month") +
  ylab("Change in Average Movement") +
  labs(fill = "Trump Support Quintiles") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(-1, 0.5))

# Legend for politics quintiles
trump_legend <- ggplot(subset(distance_df, !is.na(month)),
                       aes(y=daily_distance_diff, fill=trump_cat)) +
  geom_boxplot(outlier.size=0, outlier.shape=NA, notch=T, size=0.5, outlier.alpha=0.5) +
  scale_fill_grey() +
  theme_test() +
  theme(legend.position = "right",
        legend.title = element_text(face="bold")) +
  labs(fill = "Trump Support Quintiles")

# Compile figures into object
quintile_plot <- plot_grid(income_plot, get_legend(income_legend), trump_plot, get_legend(trump_legend), labels = c("A.", "", "B.", ""), rel_widths = c(1,0.2), nrow = 2)

# Print figure
ggsave(plot=quintile_plot, file="Quintile plot.pdf", width=11, height=6, units='in', dpi=600)

##############################################################################
# Graph: Distancing by day
##############################################################################

# Plot movement by day
distance_plot <- ggplot(merged, aes(x=date, y=daily_distance_diff, group=date)) +
  geom_rect(aes(xmin=as.Date("2020-02-10"), xmax=as.Date("2020-03-08"),
                ymin=-Inf, ymax=Inf), fill='gray90') +
  geom_boxplot(fill="grey", outlier.size=0, outlier.shape=NA, notch=T, size=0.25, outlier.alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  theme_test() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold"),
        axis.text.x  = element_text(angle=0, hjust=0),
        panel.grid.major.x = element_line(color="light gray", size=0.25),
        panel.grid.major.y = element_line(color="light gray", size=0.25)) +
  # xlab("Date") +
  ylab("Change in Average Movement") +
  scale_x_date(labels = date_format("%b"),
               breaks = seq(as.Date("2020-03-01"), as.Date("2021-01-10"), by="month")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(-1, 0.5),
                  xlim = as.Date(c("2020-03-06", "2021-01-10"))) +
  geom_vline(xintercept=as.Date('2020-03-13'), linetype="solid", color="black", size=0.5) +
  annotate("text", x=as.Date('2020-03-16'), y=0.4, label="National emergency declared", fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=as.Date('2020-02-23'), y=-0.75, label="Reference\nperiod", fontface=2, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=as.Date('2020-11-20'), y=0.4, label="Less physical distancing", fontface=4, size=3, hjust=0, vjust=0.5) +
  annotate("text", x=as.Date('2020-11-20'), y=-0.75, label="More physical distancing", fontface=4, size=3, hjust=0, vjust=0.5)

# Print figure
ggsave(plot=distance_plot, file="Distance plot.pdf", width=11, height=3.5, units='in', dpi=600)

##############################################################################
# Graph: Daily correlations between characteristics and distancing
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
corr_pval <- corr_pval[(ncol(corr_pval)-10):ncol(corr_pval), 1:(ncol(corr_pval)-11)]

# Define colors for correlation plot
col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# Prepare graphic for export
pdf(file = "Correlation matrix by date.pdf")

# Plot correlation matrix
corr_plot <- corrplot(corr_coef, method = "color", col = col(200), cl.pos = "r", cl.cex = 0.14,
                      
                      # Not a correlation matrix
                      is.corr = FALSE,
                      
                      # Reorder variables
                      order = "original",
                      
                      # Labels color, size, and rotation
                      tl.col = "black", tl.cex = 0.14, number.cex = 0.14, tl.srt = 90,
                      
                      # Use significance levels
                      p.mat = corr_pval, sig.level = 0.000015,
                      insig = "label_sig", pch.cex = 0.14)

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

##############################################################################
# Sensitivity analysis: Pooled regression with state cluster
##############################################################################

# Run pooled regression
reg_pool <- lm.cluster(formula=distance ~ state_name:week + perc_male:week + elder:week + perc_black:week + perc_Hisp:week + income:week + perc_rtail:week + perc_trspt:week + perc_hlthc:week + perc_forgn:week + perc_rural:week + republican_2016:week, data=merged_pool, cluster=merged_pool$state_name)
summary(reg_pool)

# Capture coefficients and errors
pool_df <- as.data.frame(base::cbind(coeftest(reg_pool)[,1], coeftest(reg_pool)[,2]))
colnames(pool_df) <- c("beta","error")

# Add variable names
pool_df <- tibble::rownames_to_column(pool_df, var="var")
pool_df$date <- pool_df$var

# Remove states and intercept
pool_df <- pool_df[-grep("Intercept",  pool_df$var),]
pool_df <- pool_df[-grep("state_name", pool_df$var),]

# Clean up date column
pool_df$date <- str_remove(pool_df$date, "\\:[^.]*$")
pool_df$date <- str_remove(pool_df$date, "week")

# Clean up variable column
pool_df$var <- str_remove(pool_df$var, "[^.]*\\:")

# Treat values as numeric
pool_df$beta  <- as.numeric(as.character(pool_df$beta))
pool_df$error <- as.numeric(as.character(pool_df$error))

# Calculate 95% confidence intervals
pool_df$lowerOR <- pool_df$beta - 1.96*pool_df$error
pool_df$upperOR <- pool_df$beta + 1.96*pool_df$error

# Rename variables
pool_df$var <- sub("perc_male",  "Pct. Male", pool_df$var)
pool_df$var <- sub("perc_black", "Pct. Black", pool_df$var)
pool_df$var <- sub("perc_Hisp",  "Pct. Hispanic", pool_df$var)
pool_df$var <- sub("elder",      "Pct. 65 Years or Older", pool_df$var)
pool_df$var <- sub("perc_forgn", "Pct. Foreign-Born", pool_df$var)
pool_df$var <- sub("income",     "Per Capita Income", pool_df$var)
pool_df$var <- sub("perc_rural", "Pct. Rural", pool_df$var)
pool_df$var <- sub("republican_2016", "Pct. Trump Support", pool_df$var)
pool_df$var <- sub("perc_rtail", "Pct. Employment in\nRetail", pool_df$var)
pool_df$var <- sub("perc_trspt", "Pct. Employment in\nTransport.", pool_df$var)
pool_df$var <- sub("perc_hlthc", "Pct. Employment in\nHealth, Education, Social", pool_df$var)
pool_df$var <- sub("weekly_cases_per_mil", "Cases per Million\nin Past Week", pool_df$var)
pool_df$var <- factor(pool_df$var, levels = c("Pct. Male", "Pct. Black", "Pct. Hispanic", "Pct. 65 Years or Older", "Pct. Foreign-Born", "Pct. with College Degree", "Per Capita Income", "Pct. Rural", "Pct. Trump Support", "Pct. Employment in\nRetail", "Pct. Employment in\nTransport.", "Pct. Employment in\nHealth, Education, Social", "Cases per Million\nin Past Week"))

# Graph rolling coefficients
pool_plot <- ggplot(data=pool_df,
                    aes(x=as.Date(date), y=beta, ymin=lowerOR, ymax=upperOR, group=var)) +
  facet_wrap(~var, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  geom_errorbar(alpha=0.2) +
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
  xlab("Date (Start of Pooled Week)") +
  ylab("Percentage-Point Change in Average Momement,\nGiven Interquartile Increase in Characteristic") +
  scale_x_date(labels = date_format("%b"),
               limits = c(as.Date("2020-03-01"), as.Date("2021-01-17")),
               breaks = seq(as.Date("2020-03-01"), as.Date("2021-01-17"), by="2 months")) +
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5,7.5)) +
  coord_cartesian(ylim = c(-5.5,8))

# Print figure
ggsave(plot=pool_plot, file="Pooled coefficients plot, state.pdf", width=7, height=6, units='in', dpi=600)

##############################################################################
# Sensitivity analysis: Pooled regression with COVID-19 cases
##############################################################################

# Regression dataset
merged_pool_2 <- subset(merged_pool, !(week %in% c("2020-03-09", "2020-03-16")))

# Run pooled regression
reg_pool <- lm.cluster(formula=distance ~ state_name:week + perc_male:week + elder:week + perc_black:week + perc_Hisp:week + income:week + perc_rtail:week + perc_trspt:week + perc_hlthc:week + perc_forgn:week + perc_rural:week + republican_2016:week + cases:week, data=merged_pool_2, cluster=merged_pool_2$GEO_ID)
summary(reg_pool)

# Capture coefficients and errors
pool_df <- as.data.frame(base::cbind(coeftest(reg_pool)[,1], coeftest(reg_pool)[,2]))
colnames(pool_df) <- c("beta","error")

# Add variable names
pool_df <- tibble::rownames_to_column(pool_df, var="var")
pool_df$date <- pool_df$var

# Remove states and intercept
pool_df <- pool_df[-grep("Intercept",  pool_df$var),]
pool_df <- pool_df[-grep("state_name", pool_df$var),]

# Clean up date column
pool_df$date <- str_remove(pool_df$date, "\\:[^.]*$")
pool_df$date <- str_remove(pool_df$date, "week")

# Clean up variable column
pool_df$var <- str_remove(pool_df$var, "[^.]*\\:")

# Treat values as numeric
pool_df$beta  <- as.numeric(as.character(pool_df$beta))
pool_df$error <- as.numeric(as.character(pool_df$error))

# Calculate 95% confidence intervals
pool_df$lowerOR <- pool_df$beta - 1.96*pool_df$error
pool_df$upperOR <- pool_df$beta + 1.96*pool_df$error

# Rename variables
pool_df$var <- sub("perc_male",  "Pct. Male", pool_df$var)
pool_df$var <- sub("perc_black", "Pct. Black", pool_df$var)
pool_df$var <- sub("perc_Hisp",  "Pct. Hispanic", pool_df$var)
pool_df$var <- sub("elder",      "Pct. 65 Years or Older", pool_df$var)
pool_df$var <- sub("perc_forgn", "Pct. Foreign-Born", pool_df$var)
pool_df$var <- sub("income",     "Per Capita Income", pool_df$var)
pool_df$var <- sub("perc_rural", "Pct. Rural", pool_df$var)
pool_df$var <- sub("republican_2016", "Pct. Trump Support", pool_df$var)
pool_df$var <- sub("perc_rtail", "Pct. Employment in\nRetail", pool_df$var)
pool_df$var <- sub("perc_trspt", "Pct. Employment in\nTransport.", pool_df$var)
pool_df$var <- sub("perc_hlthc", "Pct. Employment in\nHealth, Education, Social", pool_df$var)
pool_df$var <- sub("cases",      "Cases per Million\nin Past Week", pool_df$var)
pool_df$var <- factor(pool_df$var, levels = c("Pct. Male", "Pct. Black", "Pct. Hispanic", "Pct. 65 Years or Older", "Pct. Foreign-Born", "Pct. with College Degree", "Per Capita Income", "Pct. Rural", "Pct. Trump Support", "Pct. Employment in\nRetail", "Pct. Employment in\nTransport.", "Pct. Employment in\nHealth, Education, Social", "Cases per Million\nin Past Week"))

# Graph rolling coefficients
pool_plot <- ggplot(data=pool_df,
                    aes(x=as.Date(date), y=beta, ymin=lowerOR, ymax=upperOR, group=var)) +
  facet_wrap(~var, nrow=3) +
  geom_hline(yintercept=0, linetype="dashed", color="red", size=0.5) +
  geom_errorbar(alpha=0.2) +
  # geom_ribbon(alpha=0.2, color=NA) +
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
  xlab("Date (Start of Pooled Week)") +
  ylab("Percentage-Point Change in Average Momement,\nGiven Interquartile Increase in Characteristic") +
  scale_x_date(labels = date_format("%b"),
               limits = c(as.Date("2020-03-01"), as.Date("2021-01-17")),
               breaks = seq(as.Date("2020-03-01"), as.Date("2021-01-17"), by="2 months")) +
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5,7.5)) +
  coord_cartesian(ylim = c(-5.5,8))

# Print figure
ggsave(plot=pool_plot, file="Pooled coefficients plot, cases.pdf", width=7, height=6, units='in', dpi=600)
