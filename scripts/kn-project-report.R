library(ggplot2)
library(GGally)
library(dplyr)

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

# key
neuromaps_key = read.csv("/Users/kevinnguyen/Desktop/fmph221/data/neuromaps-key.csv")
unique_groups = unique(neuromaps_key$description)

# creating histograms for each variable (response and predictor)
plot_histogram(neuromaps_siips)

# log transforming variables who have visibly skewed data (+1 because log(0) is infinity)
cols_to_transform = c(
  "aghourian2017.feobv", 
  "bedard2019.feobv", 
  "gallezot2017.gsk189254", 
  "jaworska2020.fallypride",  
  "sandiego2015.flb457",
  "sasaki2012.fepe2i", 
  "tuominen.feobv")
for (i in cols_to_transform){
  neuromaps_siips[i] = log(neuromaps_siips[i] + 1)
  plot_histogram(neuromaps_siips[i])
}

# initial model
m1 = lm(siips ~ ., data = neuromaps_siips)
round(summary(m1)$coefficients, digits = 3)
summary(m1)$r.squared

# too many variables so we do principal component to cut it down
neuromaps_df = neuromaps_siips[1]

for (i in unique_groups){
  selected_variables = neuromaps_key[neuromaps_key$description == i,][[1]]
  if (length(selected_variables) > 1) {
    df_for_pca = neuromaps_siips[selected_variables]
    siips_pca = prcomp(df_for_pca)
    new_col = data.frame(siips_pca$x[,1])
  } else {
    new_col = data.frame(neuromaps_siips[selected_variables])
  }
  colnames(new_col) = i
  neuromaps_df = cbind(neuromaps_df, new_col)
}

# AIC
# Backward elimination
neuromaps_df_full <- lm(siips ~ ., data=neuromaps_df)
summary(neuromaps_df_full)
# Drop one variable at a time
drop1(neuromaps_df_full, test="F")
# Repeated elimination:
step(neuromaps_df_full, direction="backward")

# Forward selection
# Add one variable at a time:
add1(lm(siips ~ 1, data = neuromaps_df), ~ `cognitive.activation` + 
       `cerebral.blood.flow` + `synaptic.density` + 
       `mu.opioid` + dopamine + serotonin + acetylcholine +
       glutamate + gaba + cannabinoid + norepinephrine + histamine, 
     test="F")
add1(lm(siips ~ `mu.opioid`, data = neuromaps_df), ~ `cognitive.activation` + 
       `cerebral.blood.flow` + `synaptic.density` + 
       `mu.opioid` + dopamine + serotonin + acetylcholine +
       glutamate + gaba + cannabinoid + norepinephrine + histamine, 
     test="F")
add1(lm(siips ~ `mu.opioid` + `cerebral.blood.flow`, data = neuromaps_df), ~ `cognitive.activation` + 
       `cerebral.blood.flow` + `synaptic.density` + 
       `mu.opioid` + dopamine + serotonin + acetylcholine +
       glutamate + gaba + cannabinoid + norepinephrine + histamine, 
     test="F")
# Repeated addition:
step(lm(siips ~ 1, data = neuromaps_df), ~ `cognitive.activation` + 
       `cerebral.blood.flow` + `synaptic.density` + 
       `mu.opioid` + dopamine + serotonin + acetylcholine +
       glutamate + gaba + cannabinoid + norepinephrine + histamine, 
     direction="forward")
# Repeated addition / elimination:
step(lm(siips ~ 1, data = neuromaps_df), ~ `cognitive.activation` + 
       `cerebral.blood.flow` + `synaptic.density` + 
       `mu.opioid` + dopamine + serotonin + acetylcholine +
       glutamate + gaba + cannabinoid + norepinephrine + histamine, 
     direction="both")

# All subsets regression by adjusted R squared
library(leaps)
best.adjr2 <- leaps(as.matrix(neuromaps_df[,2:13]), neuromaps_df[,1], method="adjr2")
best.adjr2$which
best.adjr2$size
(best.adjr2.ind = which.max(best.adjr2$adjr2))
best.adjr2$which[best.adjr2.ind,]
plot(best.adjr2$size, best.adjr2$adjr2)

# All subsets regression by Cp
best.Cp <- leaps(as.matrix(neuromaps_df[,2:13]), neuromaps_df[,1], method="Cp")
(best.Cp.ind = which.min(best.Cp$Cp))
best.Cp$which[best.Cp.ind,]
plot(best.Cp$size, best.Cp$Cp)

