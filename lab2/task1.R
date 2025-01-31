load("D:/studyr/data/ExpImp.RData")
# first task

REGION_DELIMITER <- "�����������"  # ���������, ������������ ������ � ��������� ������ ��� ����������� ���������
MISC <- c("���������� ���������", "� ��� �����:")  # ���������, ������������ "������������" � ������ ������ ������
EMPTY_CELL_MARKER <- "-"
EMPTY_CELL_PLACEHOLDER <- 0

fix_data <- function(df) {
  change <- function(x) {
    vec <- as.numeric(gsub("-", "0", x))
    ifelse(is.na(vec), x, vec)
  }
  data.frame(lapply(df, change))
}

get_regions_df <- function(data) {
  for (i in 2:length(names(data))) {
    data[[i]] <- gsub(EMPTY_CELL_MARKER, EMPTY_CELL_PLACEHOLDER, data[[i]])
    data[[i]] <- as.numeric(data[[i]])
  }
  regions <- data.frame('', '')
  tmp_index <- 0
  tmp_region <- ""
  for (subject in data$'������') {
    tmp_index <- tmp_index + 1
    if (subject %in% MISC)
      next
    if (grepl(REGION_DELIMITER, subject)) {
      tmp_region <- subject
      next
    }
    tmp_df <- data.frame()
    tmp_df <- rbind(tmp_df, tmp_region)
    tmp_df <- rbind(tmp_df, sum(data[tmp_index, 2:length(names(data))]))
    names(tmp_df) <- subject
    regions <- cbind(regions, tmp_df)
  }
  return(regions)
}


get_region_info <- function(subject, regions_df) {
  column <- regions_df[[subject]]
  if (length(column) == 0) {
    print("������� �������� �������� ��������")
  }
  else {
    print(paste("��������� ������: ", subject))
    print(paste("����������� ������: ", column[[1]]))
    print(paste("��������� ������ � ������: ", column[[2]]))
  }
}

get_region_info('��������� ����', get_regions_df(ExpImp))

