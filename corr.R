## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the 
## number of completely observed observations (on all
## variables) requi?red to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
## NOTE: Do not round the result!


source("complete.R")

corr <- function(directory, threshold = 0) {
        files <- list.files(directory, pattern = ".csv", full.names = TRUE)
        dat <- vector(mode = "numeric", length = 0)
        
        for (i in 1:length(files)) {
                data <- read.csv(files[i])
                csum <- sum((complete.cases(data$sulfate)) & (complete.cases(data$nitrate)))
                if (csum > threshold) {
                        tmpdata <- data[complete.cases(data$sulfate),]
                        nitsul <- tmpdata[complete.cases(tmpdata$nitrate),]
                        dat <- c(dat, cor(nitsul$sulfate, nitsul$nitrate))
                }
        }
        dat
}