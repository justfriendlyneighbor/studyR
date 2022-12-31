library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

cond <- 'Compare'
REGION <- 'Центральный федеральный округ'

plot_graphics <- function(data, str){
  data <- data[complete.cases(data),]
  for (i in 2:length(names(data))) {
    data[[i]] <- gsub("-", 0, data[[i]])
    data[[i]] <- as.numeric(data[[i]])
  }

  flt <- str_detect(data$Регион, 'федеральный округ')
  rdf <- mutate(data, Округ = if_else(flt, Регион, NULL))
  rdf <- fill(rdf, Округ)
  flt2 <- !str_detect(rdf$Регион, 'Федерация|федеральный округ|г.')
  rdf <- filter(rdf, flt2)

  match_exp <- select_at(rdf, vars(matches("Экспорт")))
  match_imp <- select_at(rdf, vars(matches("Импорт")))

  match_dif <- rowSums(match_imp, na.rm = TRUE)-rowSums(match_exp, na.rm = TRUE)
  match_exp$Сумма <- rowSums(match_exp, na.rm = TRUE)
  match_imp$Сумма <- rowSums(match_imp, na.rm = TRUE)

  if (str == cond){
    rdf$SumExport <- match_exp$Сумма
    rdf$SumImport <- match_imp$Сумма
    rdf$Diff <- match_dif
    rdf <- rdf[,c("Регион", "SumExport", "SumImport","Diff","Округ" )]
  }else{
    rdf$Sum <- match_imp$Сумма+match_exp$Сумма
    rdf <- rdf[,c("Регион", "Sum","Округ" )]
  }
  
  rdf <- filter(rdf, Округ == REGION)%>%mutate(Округ=NULL)
  rdf <- rdf%>%pivot_longer( starts_with("Sum"), names_to = "Экспорт/Импорт", values_to = "млн долларов США")
  sum_reg <- rdf %>% group_by(Регион, `Экспорт/Импорт`)
  sum_reg <- sum_reg %>% summarise(sum = sum(`млн долларов США`))
  sum_reg |>
    ggplot(mapping = aes(x = Регион, y = sum, fill = `Экспорт/Импорт`)) +
    geom_col(color = 'black', size = 0.2, position = 'identity',alpha=0.7) +
    {if (str == cond) ggtitle(paste(REGION," (разница между импортом и экспортом)")) else ggtitle(REGION)}+ 
    ylab('млн долларов США') + coord_flip()+
    {if (str == cond) geom_text(aes(y=20000, label = rdf$Diff))}
}

load('D:/studyr/data/ExpImp.RData')
plot_graphics(ExpImp, paste('Non',cond))
plot_graphics(ExpImp, cond)


