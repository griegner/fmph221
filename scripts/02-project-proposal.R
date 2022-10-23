# load ../fmph221/data/neuromaps-mni152_y-siips_res-basc444.csv into data frame
## column 1 is the response, and the rest are predictors
neuromaps_siips = read.csv("/Users/kevinnguyen/Desktop/fmph221/data/neuromaps-mni152_y-siips_res-basc444.csv")

# creating histograms for each variable (response and predictor)
par(mfrow=c(3,3))
for (i in 1:length(neuromaps_siips)){
  hist(neuromaps_siips[[i]],
       main = paste0("Histogram of ", colnames(neuromaps_siips[i])),
       xlab = paste0(colnames(neuromaps_siips[i])))
}

# since there are so many variables, we will be using principal components
## transforming the data to set all of the variables to the same units
scaled_siips = scale(neuromaps_siips)
siips_pc = prcomp(scaled_siips)
siips_pc
summary(siips_pc)

### we pick all of the principal components with proportion of variance >= 0.05
## we identify PC4 as the cutoff point
sZ = as.matrix(neuromaps_siips) %*% siips_pc$rotation[,1:4]

# pairs plots of principal components
pairs(sZ)

# data frame description: sampling size, summary statistics, apparent trends etc
sample_size = nrow(neuromaps_siips)
summary_statistics = summary(neuromaps_siips)

# initial analysis: multiple regression, interpretation, basic diagnostics
model = lm(neuromaps_siips$siips ~ sZ)
summary(model)

model_unrefined = lm(siips ~ ., data = neuromaps_siips)
summary(model_unrefined)
## we notice that all of the predictors have very small coefficients in terms of a linear model 
## towards the siips column. Also, only a few of them have statistically significant p values 
## indicating that many of the predictors do not contribute much to the response variable.

### let me know if I put that p-value interpretation correctly.^

