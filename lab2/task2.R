get_named_dataframe <- function(df, city, measure_name,name) {
    county<-subset(df, df$City == city & df$Payment.Measure.Name == paste("Payment for", measure_name, "patients"))[name][[1]][1]    
    tmp_df <- subset(df, df[[name]] == county & df$Payment.Measure.Name == paste("Payment for", measure_name, "patients"))
    tmp_df$Payment <- as.numeric(gsub(",", "", substr(tmp_df$Payment, start = 2, stop = nchar(tmp_df$Payment))))
    tmp_df <-tmp_df[order(tmp_df$Payment),]
    return(tmp_df[c(2,14,15)])
}

get_named_list<-function(df, city, measure_name){
  return(list(head(get_named_dataframe(df, city, measure_name,"City"),1),head(get_named_dataframe(df, city, measure_name,"County.Name"),1),head(get_named_dataframe(df, city, measure_name,"State"),1)))
}
df <- read.csv("D:/studyr/data/Payment_and_Value_of_Care-Hospital.csv",sep = ";")
print(get_named_list(df, "LOS ANGELES", "pneumonia"))


