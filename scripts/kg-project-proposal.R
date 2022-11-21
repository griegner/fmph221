# function to create histograms for each column in a dataframe
plot_histogram = function(dataframe){
  for (i in 1:length(dataframe)){
    hist(dataframe[[i]],
         main = paste0("Histogram of ", colnames(dataframe[i])),
         xlab = paste0(colnames(dataframe[i])))
  }
}

# normalize from 0 to 1 because log transformation of negative number or 0 is complex
normalize = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

## column 1 is the response, and the rest are predictors
neuromaps_siips = read.csv("/Users/kevinnguyen/Desktop/fmph221/data/neuromaps-mni152_y-siips_res-basc444.csv")
neuromaps_siips = apply(neuromaps_siips, 2, normalize) |> as.data.frame()

# creating histograms for each variable (response and predictor)
plot_histogram(neuromaps_siips)

# log transforming variables who have visibly skewed data (+1 because log(0) is infinity)
cols_to_transform = c(
  "aghourian2017", 
  "bedard2019", 
  "gallezot2017", 
  "jaworska2020",  
  "sandiego2015",
  "sasaki2012", 
  "tuominen")
for (i in cols_to_transform){
  neuromaps_siips[i] = log(neuromaps_siips[i] + 1)
  plot_histogram(neuromaps_siips[i])
}

# rule out variables who have little to no relationships with the response variable
corr_with_resp = cor(neuromaps_siips[, -1], neuromaps_siips$siips)
rel_w_resp = colnames(neuromaps_siips[, -1])[abs(corr_with_resp) > 0.1]
have_rel = neuromaps_siips[, c("siips", rel_w_resp)]
paste0("dropped ", ncol(neuromaps_siips) - ncol(have_rel), " columns")

plot_histogram(have_rel)

# initial model
m1 = lm(siips ~ ., data = have_rel)
round(summary(m1)$coefficients, digits = 3)
summary(m1)$r.squared

# pairwise with largest coefficients (absolute value)
largest_values = summary(m1)$coefficients[-1, 1]
largest_coef_abs = largest_values[order(abs(largest_values), decreasing = TRUE)][1:5] |> names()
pairs(have_rel[c("siips", largest_coef_abs)], pch = 20)

# residual plot: shows data not iid
plot(m1)

# since there are many variables, we will be using principal components
siips_pc = prcomp(have_rel)
siips_pc
summary(siips_pc)

### we pick all of the principal components with proportion of variance >= 0.05
## we identify PC4 as the cutoff point
zed = as.matrix(neuromaps_siips) %*% siips_pc$rotation[,1:4]

# pairs plots of principal components
pairs(zed)

m2 = lm(neuromaps_siips$siips ~ zed)
summary(m2)
plot(m2)
