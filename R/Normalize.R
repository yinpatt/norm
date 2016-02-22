#' Normalize Function
#'
#' This function allows you to normalize a dataframe.
#' @param Data = Dataframe you need to normalize.
#' @keywords Normalize
#' @export
#' @examples
#' normalize()
normalize = function(data){
  data = na.omit(data)
  name = names(data)
  data = data.matrix(data)
data_mean = sapply(1:ncol(data), function(p) mean(data[,p]))
data_sd = sapply(1:ncol(data), function(p) sd(data[,p]))
temp2 = rep(1, nrow(data))
for (c in 1:ncol(data)){
temp = sapply(1:nrow(data), function(r) pnorm(data[,c][[r]],data_mean[c],data_sd[c]))
temp2 = data.frame(temp2,temp)  
}
temp2 = as.data.frame(temp2)
temp2$temp2 = NULL
names(temp2) = name
temp2
}
