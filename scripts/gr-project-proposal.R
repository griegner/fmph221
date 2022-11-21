# functions
scale_zero_to_one <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

plot_hist_by_col <- function(df) {
  for (i in seq_len(ncol(df))) {
    hist(df[, i], main = colnames(df[i]), xlab = colnames(df[i]))
  }
}


# column 1 is the response (y), and the rest are predictors (Xs)
df <- read.csv("data/neuromaps-mni152_y-cpdm_res-basc444.csv")
df <- apply(df, 2, scale_zero_to_one) |> as.data.frame()

# log transformation of skewed variables
logcols <- c( # choosen by visual inspection of histograms
  "aghourian2017",
  "bedard2019",
  "gallezot2017",
  "jaworska2020",
  "sandiego2015",
  "sasaki2012",
  "tuominen"
)
df[logcols] <- log(df[logcols] + 1)

# select predictors that have some marginal relationship with the response
cor_w_y <- cor(df[, -1], df$cpdm)
subset <- colnames(df[, -1])[abs(cor_w_y) > 0.1]
df_subset <- df[, c("cpdm", subset)]
print(paste0("dropped ", ncol(df) - ncol(df_subset), " columns"))

# histogram for each column of subset
plot_hist_by_col(df_subset)

### model 1 ###
m1 <- lm(cpdm ~ ., data = df_subset)
summary(m1)$coefficients |> round(3)
summary(m1)$r.squared

# pairwise plots for predictors with largest t-statistics
tvals <- summary(m1)$coefficients[, 3]
largest_tvals <- tvals[order(abs(tvals), decreasing = TRUE)][2:5] |> names()
pairs(df_subset[c("cpdm", largest_tvals)], pch = 20)

# residual plot: shows data not iid
plot(m1)

### model 2: PCA to reduce collinearity btw predictors ###
df_subset_pc <- prcomp(df_subset[, -1])
summary(df_subset_pc)

# select PCs with proportion of variance >= 0.05
xs <- as.matrix(df_subset[, -1]) %*% df_subset_pc$rotation[, 1:4]

pairs(data.frame(cpdm = df_subset$cpdm, xs), pch = 20)

m2 <- lm(df$cpdm ~ xs)
summary(m2)$coefficients |> round(3)
summary(m2)$r.squared

plot(m2)
