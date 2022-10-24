# function to create histograms for each column in a dataframe
plot_histogram = function(dataframe){
  for (i in 1:length(dataframe)){
    hist(dataframe[[i]],
         main = paste0("Histogram of ", colnames(dataframe[i])),
         xlab = paste0(colnames(dataframe[i])))
  }
}

normalize = function(x) {
  (x - min(x)) / (max(x) - min(x))
}

## column 1 is the response, and the rest are predictors
neuromaps_siips = read.csv("/Users/kevinnguyen/Desktop/fmph221/data/neuromaps-mni152_y-siips_res-basc444.csv")
neuromaps_siips = apply(neuromaps_siips, 2, normalize) |> as.data.frame()

# creating histograms for each variable (response and predictor)
par(mfrow=c(3,3))
plot_histogram(neuromaps_siips)

# log transforming variables who have visibly skewed data
cols_to_transform = c(
  "aghourian2017", "alarkurtti2015",
  "bedard2019", "dukart2018", "fazio2016",
  "gallezot2017", "hesse2017", "hillmer2016",
  "jaworska2020", "kaller2017", 
  "sandiego2015",
  "sasaki2012", "smith2017",
  "tuominen")
for (i in cols_to_transform){
  neuromaps_siips[i] = log(neuromaps_siips[i] + 1)
}

# rule out variables who have little to no relationships with the response variable
corr_with_resp = cor(neuromaps_siips[, -1], neuromaps_siips$siips)
rel_w_resp = colnames(neuromaps_siips[, -1])[abs(corr_with_resp) > 0.1]
have_rel = neuromaps_siips[, c("siips", no_rel_w_resp)]
paste0("dropped ", ncol(neuromaps_siips) - ncol(have_rel), " columns")

plot_histogram(have_rel)

# initial model
m1 = lm(siips ~ ., data = have_rel)
round(summary(m1)$coefficients, digits = 3)
summary(m1)$r.squared

# pairwise with smallest p-values
p_values = summary(m1)$coefficients[, 4]
largest_p_values = p_values[order(abs(p_values), decreasing = TRUE)][2:5] |> names()
pairs(have_rel[c("siips", largest_p_values)], pch = 20)

# residual plot: shows data not iid
plot(m1)

# since there are many variables, we will be using principal components
siips_pc = prcomp(scaled_siips)
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
