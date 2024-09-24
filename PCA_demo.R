#PiA PCA

rm(list = ls())

library(readxl)
library(openxlsx)
library(stargazer)
library(ggplot2)
#library(FactoMineR) # for when there is NAs

#W2
excel_file <- "/PASTE PATH"
data = read.xlsx(excel_file)

#All waves 
excel_file <- "/PASTE PATH"
data = read.xlsx(excel_file)

str(data)
#In this demo script, we assumed that mental health outcome was assessed by a battery of self-reported items, including the BDI, PCL-5, and three visual analogue scales that asked for individual's level of stress, anxiety, and sleep. We use PCA to extract the common variance (or latent mental health factor) for the calculation of the SR score


data$Wave <- factor(data$Wave, ordered = TRUE)
data$Wave <- as.numeric(data$Wave)
data$ID <- as.factor(data$ID)
data$BDI_Total <- as.numeric(data$BDI_Total)
data$PCL_Total <- as.numeric(data$PSS_Total)
data$VAS_1 <- as.numeric(data$VAS_1)
data$VAS_3 <- as.numeric(data$VAS_3)
data$VAS_4 <- as.numeric(data$VAS_4)

#Exclude W1 data
data <- data[data$Wave != 1, ]

#Check how many NAs
na_count <- colSums(is.na(data))
print(na_count)

#Filter NAs for variables to be included in PCA
data <- data %>%
  filter(!is.na(`VAS_4`))

#There are two different functions for running PCA: prcomp (base package R) and princomp
#Will try both and compare 
#Note: princomp does not allow for scaling variables so one should scale the variables before running princomp

#prcomp -----

#For all waves analyses, remove all missings from the dataframe 
data <- na.omit(data)

# Assuming data is your data frame
PCA <- prcomp(data[, c("PCL_Total", "BDI_Total", "VAS_1", "VAS_3", "VAS_4")], scale. = TRUE)

# Summary of PCA results
summary(PCA)

# Get principal component scores
scores <- PCA$x

# Get loadings (eigenvectors)
loadings <- PCA$rotation

# Plot the results
plot(scores[, 1], scores[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Plot")

# Calculate the proportion of variance explained by each principal component
prop_var <- (PCA$sdev^2) / sum(PCA$sdev^2)

# Print proportion of variance explained by each principal component
print(prop_var)

# Accessing loadings of original variables on each component
PCA$rotation

#Create new P variable Based on PC1 
data$PC1 <- PCA$x[, 1]

#princomp (one needs to scale the variables before using princomp) -----
# Assuming data is your data frame
# Removing rows with NA values in specific columns
data_clean <- na.omit(data[, c("PCL_Total", "BDI_Total", "VAS_1", "VAS_3", "VAS_4")])
PCA <- princomp(scale(data[, c("PCL_Total", "BDI_Total", "VAS_1", "VAS_3", "VAS_4")]))

# Summary of PCA results
summary(PCA)

# Accessing proportion of variance explained by each component
PCA$sdev^2 / sum(PCA$sdev^2)

# Accessing loadings of original variables on each component
PCA$loadings

# Create a biplot
biplot(PCA)

#Create new P variable Based on PC1 
data$PC1 <- PCA$scores[,1]

#compute E-P regression NORMAL ---- Here we have a number of stressor variables (details not important). The general idea is to regress the latent mental health factor extracted by PCA with stressor load and use the residual for the calculation of SR scores.

m1 <- lm(scale(PC1)~scale(Es.SCM.1), data=data) #PLES_
m2 <- lm(scale(PC1)~scale(Es.SCM.2), data=data) #PLESFR_ frequency 
m3 <- lm(scale(PC1)~scale(Es.SSM), data=data) #PLESSV_ severity 
m4 <- lm(scale(PC1)~scale(Eg.SCM), data=data) #LTE_
m5 <- lm(scale(PC1)~scale(Ec.SCM.1), data=data) #Combine scaled PLES_ + LTE_
m6 <- lm(scale(PC1)~scale(Ec.SCM.2), data=data) #Combine scaled PLESFR_ + LTE_
m7 <- lm(scale(PC1)~scale(Es.SSM) + scale(Eg.SCM), data=data) #PLESSV_ severity + LTE occurence
m8 <- lm(scale(PC1)~scale(Es.SCM.1) + scale(Eg.SCM), data=data) #Like Ec.SCM.1 (m5) but with separate predictors
m9 <- lm(scale(PC1)~scale(Es.SCM.2) + scale(Eg.SCM), data=data) #Like Ec.SCM.2 (m6) but with separate predictors

summary (m1)
summary (m2)
summary (m3)
summary (m4)
summary (m5)
summary (m6)
summary (m7)
summary (m8)
summary (m9)

anova(m1, m2, m3, m4, m5, m6, m7, m8, m9)

# Compute E-P regression (normal) by individual wave when using Allwaves data ----
# Subset W2 ----
data_W2 <- subset(data, Wave == 2)

m1 <- lm(scale(PC1)~scale(Es.SCM.1), data=data_W2) #PLES_
m2 <- lm(scale(PC1)~scale(Es.SCM.2), data=data_W2) #PLESFR_ frequency 
m3 <- lm(scale(PC1)~scale(Es.SSM), data=data_W2) #PLESSV_ severity 
m4 <- lm(scale(PC1)~scale(Eg.SCM), data=data_W2) #LTE_
m5 <- lm(scale(PC1)~scale(Ec.SCM.1), data=data_W2) #Combine scaled PLES_ + LTE_
m6 <- lm(scale(PC1)~scale(Ec.SCM.2), data=data_W2) #Combine scaled PLESFR_ + LTE_
m7 <- lm(scale(PC1)~scale(Es.SSM) + scale(Eg.SCM), data=data_W2) #PLESSV_ severity + LTE occurence
m8 <- lm(scale(PC1)~scale(Es.SCM.1) + scale(Eg.SCM), data=data_W2) #Like Ec.SCM.1 (m5) but with separate predictors
m9 <- lm(scale(PC1)~scale(Es.SCM.2) + scale(Eg.SCM), data=data_W2) #Like Ec.SCM.2 (m6) but with separate predictors

summary (m1)
summary (m2)
summary (m3)
summary (m4)
summary (m5)
summary (m6)
summary (m7)
summary (m8)
summary (m9)

# Subset W3 -----
data_W3 <- subset(data, Wave == 3)

m1 <- lm(scale(PC1)~scale(Es.SCM.1), data=data_W3) #PLES_
m2 <- lm(scale(PC1)~scale(Es.SCM.2), data=data_W3) #PLESFR_ frequency 
m3 <- lm(scale(PC1)~scale(Es.SSM), data=data_W3) #PLESSV_ severity 
m4 <- lm(scale(PC1)~scale(Eg.SCM), data=data_W3) #LTE_
m5 <- lm(scale(PC1)~scale(Ec.SCM.1), data=data_W3) #Combine scaled PLES_ + LTE_
m6 <- lm(scale(PC1)~scale(Ec.SCM.2), data=data_W3) #Combine scaled PLESFR_ + LTE_
m7 <- lm(scale(PC1)~scale(Es.SSM) + scale(Eg.SCM), data=data_W3) #PLESSV_ severity + LTE occurence
m8 <- lm(scale(PC1)~scale(Es.SCM.1) + scale(Eg.SCM), data=data_W3) #Like Ec.SCM.1 (m5) but with separate predictors
m9 <- lm(scale(PC1)~scale(Es.SCM.2) + scale(Eg.SCM), data=data_W3) #Like Ec.SCM.2 (m6) but with separate predictors

summary (m1)
summary (m2)
summary (m3)
summary (m4)
summary (m5)
summary (m6)
summary (m7)
summary (m8)
summary (m9)




# Check for collinearity-----
# Calculate the correlation matrix for the predictors
correlation_matrix <- cor(data[, c("PC1", "Es.SCM.1", "Es.SCM.2", "Es.SSM", "Eg.SCM", "Ec.SCM.1", "Ec.SCM.2")])

# Display the correlation matrix
print(correlation_matrix)

# If you want to check for multicollinearity through VIF
library(car)
vif_results <- vif(lm(PC1 ~ Es.SCM.1 + Es.SCM.2 + Es.SSM + Eg.SCM + Ec.SCM.1 + Ec.SCM.2, data = data))
print(vif_results)

# examine explained variance for each model ----
modelfits <- matrix(c(
  summary(m1)$r.squared, 
  summary(m2)$r.squared, 
  summary(m3)$r.squared, 
  summary(m4)$r.squared, 
  summary(m5)$r.squared,
  summary(m6)$r.squared,
  summary(m7)$r.squared,
  summary(m8)$r.squared, 
  summary(m9)$r.squared
), ncol = 1)
rownames(modelfits)  <-  c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9")
colnames(modelfits) <- c("Rsq")
stargazer(modelfits, title="R squared for 'P'", type = "text", out="correlation.P.txt")

modelfits.df <- data.frame(
  category = c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"),
  Rsq.adj = c(
    summary(m1)$adj.r.squared, 
    summary(m2)$adj.r.squared, 
    summary(m3)$adj.r.squared, 
    summary(m4)$adj.r.squared, 
    summary(m5)$adj.r.squared,
    summary(m6)$adj.r.squared,
    summary(m7)$adj.r.squared,
    summary(m8)$adj.r.squared,
    summary(m9)$adj.r.squared
  ),
  model = c(
    as.character(summary(m1)$call)[2], 
    as.character(summary(m2)$call)[2], 
    as.character(summary(m3)$call)[2], 
    as.character(summary(m4)$call)[2], 
    as.character(summary(m5)$call)[2],
    as.character(summary(m6)$call)[2], 
    as.character(summary(m7)$call)[2],
    as.character(summary(m8)$call)[2],
    as.character(summary(m9)$call)[2]
  )
)
# Rename the second column
names(modelfits.df)[2] <- "adj. R-sq."
# Generate a Pandoc table and display it
library(pander)
pander(modelfits.df, digits = 4, round = 4, keep.trailing.zeros = TRUE)

#Poly ---- 
summary(m1_p <- lm(scale(PC1) ~ poly(scale(Es.SCM.1,2)), data= data))
summary(m2_p <- lm(scale(PC1) ~ poly(scale(Es.SCM.2,2)), data= data))
summary(m3_p <- lm(scale(PC1) ~ poly(scale(Es.SSM,2)), data= data))
summary(m4_p <- lm(scale(PC1) ~ poly(scale(Eg.SCM,2)), data= data))
summary(m5_p <- lm(scale(PC1) ~ poly(scale(Ec.SCM.1,2)), data= data))
summary(m6_p <- lm(scale(PC1) ~ poly(scale(Ec.SCM.2,2)), data= data))
summary(m7_p <- lm(scale(PC1) ~ poly(scale(Es.SSM,2))  + poly(Eg.SCM,2), data= data))
summary(m8_p <- lm(scale(PC1) ~ poly(scale(Es.SCM.1,2))  + poly(Eg.SCM,2), data= data)) 
summary(m9_p <- lm(scale(PC1) ~ poly(scale(Es.SCM.2,2))  + poly(Eg.SCM,2), data= data)) 

# overall anova poly 
anova(m1_p, m2_p, m3_p, m4_p, m5_p, m6_p, m7_p, m8_p, m9_p)

anova(m9, m9_p)

# Generate the HTML table ---- 
dir.create("tables", showWarnings = FALSE)
reg_title <- "Linear regression models of stressor exposure analysis"
model.labels <- NULL
labels.IV <- NULL
notes.regression <- "Precise p-values available upon request<br>99% CI in brakets<br>"

html_table <- stargazer(
  m1_p, m2_p, m3_p, m4_p, m5_p, m6_p, m7_p, m8_p, m9_p,
  type = "html",
  digits = 2, # digits.extra = 1,
  report = "vcs*",
  star.cutoffs = c(0.01, 0.001, 0.0001),
  ci = TRUE, ci.level = 0.99, ci.separator = ";",
  single.row = TRUE,
  model.numbers = FALSE,
  initial.zero = F,
  title = reg_title,
  covariate.labels = labels.IV,
  dep.var.caption  = "DV: <i>P</i>",
  dep.var.labels.include = FALSE,
  omit.stat = c("ll",  "aic", "bic"),
  notes = notes.regression
)

# Save the HTML table to a file
writeLines(html_table, "tables/stressor_exposure_analysis_W2_PCL.html")

# examine explained variance for each model
modelfits <- matrix(c(
  summary(m1_p)$r.squared, 
  summary(m2_p)$r.squared, 
  summary(m3_p)$r.squared, 
  summary(m4_p)$r.squared, 
  summary(m5_p)$r.squared,
  summary(m6_p)$r.squared,
  summary(m7_p)$r.squared,
  summary(m8_p)$r.squared,
  summary(m9_p)$r.squared
), ncol = 1)
rownames(modelfits)  <-  c("Es.SCM.1", "Es.SCM.2", "Es.SSM", "Eg.SCM", "Ec.SCM.1", "Ec.SCM.2", "Es.SSM + Eg.SCM", "Es.SCM.1 + Eg.SCM", "Es.SCM.2 + Eg.SCM")
colnames(modelfits) <- c("Rsq")
stargazer(modelfits, title="R squared for 'P'", type = "text", out="correlation.P.txt")

modelfits.df <- data.frame(
  category = c("Es.SCM.1", "Es.SCM.2", "Es.SSM", "Eg.SCM", "Ec.SCM.1","Ec.SCM.2", "Es.SSM + Eg.SCM", "Es.SCM.1 + Eg.SCM", "Es.SCM.2 + Eg.SCM"),
  Rsq.adj = c(
    summary(m1_p)$adj.r.squared, 
    summary(m2_p)$adj.r.squared, 
    summary(m3_p)$adj.r.squared, 
    summary(m4_p)$adj.r.squared, 
    summary(m5_p)$adj.r.squared,
    summary(m6_p)$adj.r.squared,
    summary(m7_p)$adj.r.squared,
    summary(m8_p)$adj.r.squared,
    summary(m9_p)$adj.r.squared
  ),
  model = c(
    as.character(summary(m1_p)$call)[2], 
    as.character(summary(m2_p)$call)[2], 
    as.character(summary(m3_p)$call)[2], 
    as.character(summary(m4_p)$call)[2], 
    as.character(summary(m5_p)$call)[2],
    as.character(summary(m6_p)$call)[2],
    as.character(summary(m7_p)$call)[2],
    as.character(summary(m8_p)$call)[2],
    as.character(summary(m9_p)$call)[2]
  )
)

# Rename the second column
names(modelfits.df)[2] <- "adj. R-sq."
# Generate a Pandoc table and display it
library(pander)
pander(modelfits.df, digits = 4, round = 4, keep.trailing.zeros = TRUE)

# SR score calc ---- 
#Linear (adjust dataset as required)
data_W3$SR_m1 <-as.numeric(scale(resid(m1)))
data_W3$SR_m2 <-as.numeric(scale(resid(m2)))
data_W3$SR_m3 <-as.numeric(scale(resid(m3)))
data_W3$SR_m4 <-as.numeric(scale(resid(m4)))
data_W3$SR_m5 <-as.numeric(scale(resid(m5)))
data_W3$SR_m6 <-as.numeric(scale(resid(m6)))
data_W3$SR_m7 <-as.numeric(scale(resid(m7)))
data_W3$SR_m8 <-as.numeric(scale(resid(m8)))
data_W3$SR_m9 <-as.numeric(scale(resid(m9)))

#Polynomial 
data$SR_m7_p <-as.numeric(scale(resid(m7_p)))
data$SR_m8_p <-as.numeric(scale(resid(m8_p)))
data$SR_m9_p <-as.numeric(scale(resid(m9_p)))

write.xlsx(data_W3, file = "/Users/gretamikneviciute/Mirror/Documents/Emploi/Postdoc/Mainz/Research/Projects/PiA/Data/W3/W3_SR_PCA.xlsx")

# Item-level PLES plot ----
# Assuming your data is in a data frame called 'data' and columns are named PLES_1, PLES_2, ..., PLES_41
library(ggplot2)
data[is.na(data)] <- 0 #only for the sake of the plot replace NA values with 0

# Calculate the sum for each PLES_ item
event_occurrences <- colSums(data[, grep("PLES_", names(data))])

# Create a data frame for plotting
event_data <- data.frame(Event = names(event_occurrences), Occurrence = event_occurrences)

# Order the data by occurrence for a more informative plot
event_data <- event_data[order(-event_data$Occurrence), ]

# Plot using ggplot2
ggplot(event_data, aes(x = reorder(Event, -Occurrence), y = Occurrence)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Event", y = "Number of Participants", title = "Most common Police stressors W2")

----- 
# Calculate the sum of frequencies for each PLESFR_ item
event_frequencies <- colSums(data[, grep("PLESFR_", names(data))])

# Create a data frame for plotting
event_frequency_data <- data.frame(Event = names(event_frequencies), TotalFrequency = event_frequencies)

# Order the data by total frequency for a more informative plot
event_frequency_data <- event_frequency_data[order(-event_frequency_data$TotalFrequency), ]

# Plot using ggplot2
ggplot(event_frequency_data, aes(x = reorder(Event, -TotalFrequency), y = TotalFrequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Event", y = "Total count", title = "Most frequently recurring police stressors W2")

--- 

# Calculate the mean severity rating for each PLESSV_ item
event_severity <- apply(data[, grep("PLESSV_", names(data))], 2, mean, na.rm = TRUE)

# Create a data frame for plotting
event_severity_data <- data.frame(Event = names(event_severity), MeanSeverity = event_severity)

# Order the data by mean severity for a more informative plot
event_severity_data <- event_severity_data[order(-event_severity_data$MeanSeverity), ]

# Plot using ggplot2
ggplot(event_severity_data, aes(x = reorder(Event, -MeanSeverity), y = MeanSeverity)) +
  geom_bar(stat = "identity", fill = "tomato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Event", y = "Mean Severity Rating", title = "Most severe Police stressors W2")

# Item-level LTE plot --
# Assuming your data is in a data frame called 'data' and columns are named PLES_1, PLES_2, ..., PLES_41
library(ggplot2)
data[is.na(data)] <- 0 #only for the sake of the plot replace NA values with 0

# Select columns that match "LTE_" but not "LTE_Tot"
selected_columns <- grep("LTE_", names(data), value = TRUE)
selected_columns <- selected_columns[!selected_columns %in% "LTE_Tot"]

# Calculate the sum of occurrences for each LTE_ item, excluding LTE_Tot
event_occurrences <- colSums(data[, selected_columns])

# Create a data frame for plotting
event_data <- data.frame(Event = names(event_occurrences), Occurrence = event_occurrences)

# Order the data by occurrence for a more informative plot
event_data <- event_data[order(-event_data$Occurrence), ]

# Plot using ggplot2
ggplot(event_data, aes(x = reorder(Event, -Occurrence), y = Occurrence)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Event", y = "Number of Participants", title = "Most common Life stressors W2")
  



