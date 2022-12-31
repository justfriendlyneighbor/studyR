library(gridExtra)
data_1 <- read.table("D:/studyr/data/data1.txt", header = TRUE, sep = " ")
data_1 <- data.frame(t(data_1))
colnames(data_1) <- data_1[1, ]
data_1 <- data_1[- 1, ]
rownames(data_1) <- data_1[,1]
data_1 <- data_1[,-1]

data_2 <- read.csv("D:/studyr/data/data2.csv")
tmp_names <- data_2$X
data_2[[1]] <- NULL
rownames(data_2) <- tmp_names

total_data <- t(rbind(data_1, data_2))
total_data <- data.frame(total_data[complete.cases(total_data), ])
total_data <- mutate_all(total_data, function(x) as.numeric(as.character(x)))
str(total_data)

ggplot(total_data, aes(x=total_data$Height)) +
  geom_histogram(aes(y=..density..),fill = "yellowgreen",color = "black")+
  geom_density(alpha=0.5,fill="blue")+
  xlab("Value") +
  ylab("Density")+
  ggtitle("Height")

ggplot(total_data, aes(x=total_data$Protein)) +
  geom_histogram(aes(y=..density..),fill = "yellowgreen",color = "black")+
  geom_density(alpha=0.5,fill="blue")+
  xlab("Value") +
  ylab("Density")+
  ggtitle("Protein")

ggplot(total_data, aes(x=total_data$Oil)) +
  geom_histogram(aes(y=..density..),fill = "yellowgreen",color = "black")+
  geom_density(alpha=0.5,fill="blue")+
  xlab("Value") +
  ylab("Density")+
  ggtitle("Oil")

total_data$GrowthType<-as.factor(total_data$GrowthType)
ggplot(total_data, aes(x=Polegaemost,fill=GrowthType)) +
  geom_bar()


gd1 <-  ggplot(total_data, aes(x=total_data$GenmBMatur)) +
  geom_histogram(aes(y=..density..),fill = "yellowgreen",color = "black")+
  geom_density(alpha=0.5,fill="blue")+
  xlab("Value") +
  ylab("Density")+
  ggtitle("GenmBMatur")+
  labs(caption = "Hist+density")
gd2 <- ggplot(total_data, aes(x=total_data$GenmBMatur))+
    geom_boxplot(fill="#f68060", alpha=.6)+
    labs(caption = "Boxplot")

gridExtra::grid.arrange(gd1, gd2, nrow = 1)
