get_id<-function (data){
  df<-Reduce(function(df1, df2)  merge(df1, df2, by = "id"), data)
  df$Avg_score = rowMeans(df[, -1])
  df<-df[c(1,ncol(df))]
}

setwd(paste0(getwd(), "/data"))
data <- get(load("lab1_e2.Rdata"))
print(get_id(data))

