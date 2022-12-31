get_id<-function (data){
  df<-Reduce(function(df1, df2)  merge(df1, df2, by = "id"), data)
  df$Avg_temp = rowMeans(df[, -1])
  df<-df[c(1,ncol(df))]
}

data <- get(load("D:/studyr/data/lab1_e2.Rdata"))
print(get_id(data))

