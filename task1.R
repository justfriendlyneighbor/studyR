fix_number <-function (x){
  loop_index_1 <- 0
  for (y in x){
    loop_index_1 <- loop_index_1+1
    if (!is.na(as.numeric(sub(" ", "", y)))){
      x[loop_index_1]<-as.numeric(as.character(sub(" ","",y)))
      x<-as.numeric(x)
    }
  }
  x
}

fix_data <-function (data){
  data.frame(lapply(data,function(x) fix_number(x)))
}

data <- read.csv("D:/studyr/data/lab1_e1.csv")
data<-fix_data(data)
str(data)
