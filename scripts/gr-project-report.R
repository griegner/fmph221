# imports
library(alr4)
library(leaps)
library(GGally)

# functions
scale_zero_to_one <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

# column 1 is the response (y), and the rest are predictors (Xs)
df <- read.csv("data/neuromaps-mni152_y-siips_res-basc444.csv")
df <- apply(df, 2, scale_zero_to_one) |> as.data.frame()
df_key <- read.csv("data/neuromaps-key.csv")

# log transformation of skewed variables
logcols <- c( # choosen by visual inspection of histograms
    "aghourian2017.feobv",
    "bedard2019.feobv",
    "gallezot2010.p943",
    "jaworska2020.fallypride",
    "sandiego2015.flb457",
    "sasaki2012.fepe2i",
    "tuominen.feobv"
)
df[logcols] <- log(df[logcols] + 1)

# first PC of each group to reduce dimensionality and collinearity
df_pc <- data.frame(siips = df$siips)

for (group in unique(df_key$description)) {
    colnames <- df_key[df_key$description == group, "colname"]
    if (length(colnames) > 1) { # first PC
        pcs <- prcomp(df[, colnames])
        df_pc[, paste0(group, ".pc1")] <- pcs$x[, 1]
    } else { # leave as is
        df_pc[group] <- df[, colnames]
    }
}

### variable selection
# all subsets regression (leaps) or backwards elimination
# using in-sample criteria (maximize Adj R^2)
xs <- as.matrix(df_pc[, -1])
all_subsets <- leaps(xs, df_pc$siips, method = "adjr2")
col_idx <- all_subsets$which[which.max(all_subsets$adjr2), ]
df_subset <- data.frame(siips = df_pc$siips, xs[, col_idx])

# pairs plot
ggpairs(df_subset)

# initial model
ols1 <- lm(siips ~ ., data = df_subset)
residualPlots(ols1) # nonlinearity
vif(ols1) # collinearity
ncvTest(ols1) # heteroscedasticity

# fix nonlinearity by adding quadratic terms
# fix collinearity by removing serotonin
ols2 <- update(
    ols1,
    ~ . - serotonin.pc1 + I(cognitive.activation^2) + I(mu.opioid.pc1^2)
)
anova(ols1, ols2) # confirm ols2 is a better model
residualPlots(ols2) # fixes nonlinearities
ncvTest(ols2) # heteroscedasticity still an issue

# check if errors are autocorrelated
resid <- residuals(ols2)
cor(resid[-1], resid[-length(resid)])
durbinWatsonTest(ols2) # GLS model doesn't seem nessessary

# bootstrap to re-estimate std errors and t-values, under heteroscedasticity
n_bootstraps <- 1000
n_samples <- nrow(df_subset)
ols_coefs <- ols2$coefficients
boot_coefs <- matrix(0, n_bootstraps, length(ols_coefs))

for (i in 1:n_bootstraps) {
    idx <- sample(1:n_samples, replace = TRUE)
    m <- update(ols2, ~., data = df_subset[idx, ])
    boot_coefs[i, ] <- m$coefficients
}
stderr <- apply(boot_coefs, 2, sd)
ci <- apply(boot_coefs, 2, quantile, probs = c(0.025, 0.975))
tvals <- ols_coefs / stderr
pvals <- 2 * pt(-abs(tvals), df = nrow(df_subset) - length(ols_coefs))

# make into a table
boot_summary <- data.frame(
    "Estimate" = ols_coefs,
    "Std Error" = stderr,
    "t value" = tvals,
    "p value" = pvals
)

# compare summary tables
summary(ols2)$coefficients |> round(3)
boot_summary |> round(3)

# compare confidence intervals
confint(ols2) |>
    cbind(t(ci)) |>
    round(3)

# outliers
plot(ols2, 4)
outlierTest(ols2)

# normality of errors
qqPlot(ols2)
