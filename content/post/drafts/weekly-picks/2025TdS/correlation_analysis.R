
# Load required libraries
library(Hmisc)
library(corrplot)
library(reshape2)
library(ggplot2)
library(logger)

# Identify potential predictor columns
pelo_cols <- names(men_df)[grep("Pelo_Pct$", names(men_df))]
last_5_cols <- names(men_df)[grep("Last_5", names(men_df))]

# Create predictor dataframe
predictors_df <- men_df[c(pelo_cols, last_5_cols)]

# Compute correlation matrix
cor_matrix <- cor(predictors_df, use = "pairwise.complete.obs")

# Perform variable clustering
var_clusters <- varclus(as.matrix(predictors_df))

# Create correlation plot
# Save cluster information and analysis

