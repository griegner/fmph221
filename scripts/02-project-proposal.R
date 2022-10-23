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

# transforming the data


# pairs plots
pairs()

# data frame description: sampling size, summary statistics, apparent trends etc


# initial analysis: multiple regression, interpretation, basic diagnostics
