library(forecast)
library(foreach)
library(doMC)

argv <- commandArgs(TRUE)

if(length(argv) < 4){
  q(status = -1)
}
input_file <- argv[1]
output_file <- argv[2]
predict_range <- as.integer(argv[3])
core_number <- as.integer(argv[4])

all_train_inventory <- read.table(input_file, header = TRUE)
predict_result <- matrix(0, dim(all_train_inventory)[1], predict_range)
train_number <- dim(all_train_inventory)[1]

registerDoMC(core_number)

result_matrix <- foreach(i = 1:train_number, .combine = rbind) %dopar% {
    key = row.names(all_train_inventory[i,])
    inventory <- t(all_train_inventory[key,])
    inventory_len <- length(inventory)
    xname = deparse(substitute(inventory))
    xdata = ts(xdata, frequency = 7)
    fit <- auto.arima(xdata, trace = T)
    fore = forecast(fit, h = predict_range, robust = TRUE)
    predict_result <-exp(fore$mean)
}

print(result_matrix)
predict_frame <- data.frame(result_matrix, row.names = (row.names(all_train_inventory)))
write.table(predict_frame, output_file, quote = FALSE, sep = "\t", row.names = TRUE, col.names = FALSE)