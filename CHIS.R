# CHIS - Descriptive Statistics Project
# Produce a mosaic plot (aka Marimekkp plot)

# GENERAL DATA

# California Health Information Survey Descriptive Statistics

# Largest state health survey in the US
# Wide variety of variables
# Personal health and economic measurements

# dim(adult) = 47614 observations with 536 variables

# Reduced the original adult data sets to only 10 managable variables.
# RBMI - BMI Category Description
# BMI_P - BMI value
# RACEHPR2 - Race
# SRSEX - Sex
# SRAGE_P - Age
# MARIT2 - Martial Status
# AB1 - General Health Condition
# ASTCUR - Current Asthma Status
# AB51 - Type 1 or Type 2 Diabetes
# POVLL - Poverty level

# DATA DOWNLOAD

# Can download data from this site: http://healthpolicy.ucla.edu/
# http://healthpolicy.ucla.edu/chis/data/public-use-data-file/Pages/public-use-data-files.aspx
# 2009 CHIS Data
# 2009 CHIS adult, adolescent, child data file downloads (data files)
# CHIS 2009 Adult - SAS version

# R Project Steps

# Step 1: Import the SAS data file.
library(haven)
adult <- read_sas("R Files/Examples/Data Visualization/CHIS 2009 PUF- Adult SAS.zip", 
                  NULL)

# Step 2: Limit the data structure to the variables that we are interested in.
library(dplyr)
adult <- adult %>%
  select(RBMI, BMI_P, RACEHPR2, SRSEX, SRAGE_P, MARIT2, AB1, ASTCUR, AB51, POVLL)

# Step 3: Review RBMI compared to SRAGE_P.
library("ggplot2")

# Explore the dataset with summary and str
summary(adult)
str(adult)

# Age histogram
ggplot(adult, aes (x = SRAGE_P)) +
  geom_histogram()

# BMI histogram
ggplot(adult, aes (x = BMI_P)) +
  geom_histogram()

# Age colored by BMI, binwidth = 1
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1)

# There is an unexpectdly large number of older Califorians; there is a spike at age 85.
# The bidwidth of the age dataset is 2.23.

# Step 4: Clean up data.
#You should have noticed in the age distribution that there is an unusual spike of 
#individuals at 85, which seems like an artifact of data collection and storage. 
# Solve this by only keeping observations for which adult$SRAGE_P is smaller than or 
# equal to 84.

# Keep adults younger than or equal to 84
adult <- adult[adult$SRAGE_P <= 84, ]

# There is a long positive tail on the BMIs that we'd like to remove. Only keep 
# ...observations for which adult$BMI_P is larger than or equal to 16 and adult$BMI_P 
# ...is strictly smaller than 52.

# Keep adults with BMI at least 16 and less than 52
adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

# We'll focus on the relationship between the BMI score (& category), age and race. 
# To make plotting easier later on, we'll change the labels in the dataset. 
# Define adult$RACEHPR2 as a factor with labels c("Latino", "Asian", "African American", 
# "White"). Do the same for adult$RBMI, using the labels c("Under-weight", "Normal-weight", 
# "Over-weight", "Obese").

# Can view all of the race combination and view the online defintion file to assign the correct race.
adult %>%
  select(RACEHPR2) %>% distinct() %>%
  arrange(RACEHPR2, desc(RACEHPR2))

# Relabel the race variable; identify the six races
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino",
                                                    "Pacific Islander",
                                                    "Native American",
                                                    "Asian",
                                                    "African American",
                                                    "White",
                                                    "Others"))


# Relabel the BMI categories variable
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight",
                                            "Normal-weight",
                                            "Over-weight",
                                            "Obese"))

# Step 5: Fine tune the visual for better insights.
# The dataset adult is available

# The color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

# Histogram, add BMI_fill and customizations
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  fix_strips +
  BMI_fill +
  facet_grid(RBMI ~ .) +
  theme_classic()

# Step 6: Review alternative plots to view different insights.
# Plot 1 - Count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill

# Plot 2 - Density histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill

# Plot 3 - Faceted count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Plot 4 - Faceted density histogram
ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Plot 5 - Density histogram with position = "fill"
ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..density..), binwidth = 1, position = "fill") +
  BMI_fill

# Plot 6 - The accurate histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill

# Step 7: How to facet the accurate frequency histogram from before (failed).
# An attempt to facet the accurate frequency histogram from before (failed)
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Create DF with table()
DF <- table(adult$RBMI, adult$SRAGE_P)

# Use apply on DF to get frequency of each group: DF_freq
DF_freq <- apply(DF, 2, function(x) x/sum(x))

# Load reshape2 and use melt() on DF_freq to create DF_melted
library(reshape2)
DF_melted <- melt(DF_freq)

# Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

# Add code to make this a faceted plot
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_col(position = "stack") +
  BMI_fill + 
  facet_grid(FILL ~ .)

# Step 8: Construct a Mosaic Plot to simplify the amount of bars or data points and address skewing by younger
# ...healthy-weight persons and older overweight persons.

# Giant cOntegiency table from two categorical values with white borders to better outline the data bars.
# Uses the Chi-Squared Test - creates residuals; observations, expected, residuals - the amount over the expected.
# The higher residuals, the more overrespresented the plot is and vice versa
# Reporting on the underlying statistics
# Can color the low and high residuals a different color.
# Going to add labels to the group
# Generalize to a function to use with any two categorial variables.

# The initial contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

# Create groupSum, xmax and xmin columns
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum
# The groupSum column needs to be removed; don't remove this line
DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)

# Melt the dataset
library(reshape2)
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# dplyr call to calculate ymin and ymax - don't change
library(dplyr)
DF_melted <- DF_melted %>%
  group_by(X) %>%
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

# Plot rectangles - don't change
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin,
                      ymax = ymax,
                      xmin = xmin,
                      xmax = xmax,
                      fill = FILL)) +
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte()

# Reporting on the underlying statistics

# Perform chi.sq test (RBMI and SRAGE_P)
results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))

# Melt results$residuals and store as resid
resid <- melt(results$residuals)

# Change names of resid
names(resid) <- c("FILL", "X", "residual")

# merge the two datasets:
DF_all <- merge(DF_melted, resid)

# Update plot command
library(ggthemes)
ggplot(DF_all, aes(ymin = ymin,
                   ymax = ymax,
                   xmin = xmin,
                   xmax = xmax,
                   fill = residual)) +
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()

# Add labels to the group
# Plot so far
p <- ggplot(DF_all, aes(ymin = ymin,
                       ymax = ymax,
                       xmin = xmin,
                       xmax = xmax,
                       fill = residual)) +
  geom_rect() +
  scale_fill_gradient2() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte()

p

# Position for labels on y axis (don't change)
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

# Plot 1: geom_text for BMI (i.e. the fill axis)
p1 <- p %+% DF_all + 
  geom_text(aes(x = max(xmax), 
                y = yposn,
                label = FILL),
            size = 3, hjust = 1,
            show.legend  = FALSE)
p1

# Plot 2: Position for labels on x axis
DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

# geom_text for ages (i.e. the x axis)
p1 %+% DF_all + 
  geom_text(aes(x = xposn, label = X),
            y = 1, angle = 90,
            size = 3, hjust = 1,
            show.legend = FALSE)

# Generalize to a function to use with any two categorial variables.

# Load all packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)

# Script generalized into a function
mosaicGG <- function(data, X, FILL) {
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xposn <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$yposn <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # Plot
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xposn, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = yposn, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

# BMI described by age
mosaicGG(adult, X = "SRAGE_P", FILL = "RBMI")

# Poverty described by age
mosaicGG(adult, X = "SRAGE_P", FILL = "POVLL")

# mtcars: am described by cyl
mosaicGG(mtcars, X = "cyl", FILL = "am")

# Vocab: vocabulary described by education
library(carData)
mosaicGG(Vocab, X = "education", FILL = "vocabulary")




