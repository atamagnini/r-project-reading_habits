library(tidyverse)
library(hrbrthemes)
library(highcharter)
library(htmlwidgets)

ds <- read.csv("D:/R scripts/script_290920_Reading_habits/Reading habit Dataset/dataset.csv",
               stringsAsFactors = FALSE)
df <- as.data.frame(ds)
#Remove duplicated data from all columns
df2 <- df %>% distinct()
#Remove and rename columns
df3 <- df2[, -c(3:7, 9:14)]
colnames(df3)[3] <- 'Books'
#Calculate means
table(df3$Age, df3$Sex)
df4 <- df3 %>% group_by(Age, Sex) %>% summarise(Books = round(mean(Books),0))
#Formating columns
glimpse(df4)
df4$Sex <- as.factor(as.character(df4$Sex))
df4$Age <- as.factor(as.integer(df4$Age))
#df5 <- spread(df4, Sex, Books)
#df6 <- df6 %>% arrange(Female)
#df6$id <- seq.int(nrow(df6)) #new column with id (number of rows)
#df6 <- mutate(df6, Male = -Male)

#Plot
df5 <- df4[-c(141:152),]
colnames(df5)[3] <- 'Books_'
df5$Books <- with(df5, ifelse(Sex == 'Male', -Books_, Books_))
df5 <- df5[c(1,2,4,3)]
x2 <- c('Age:', 'Books (average):')
y2 <- sprintf('{point.%s}', c('Age', 'Books_'))
tltip <- tooltip_table(x2,y2)

p <- hchart(df5, type = "column",
       hcaes(x = 'Age', y = 'Books', group = 'Sex'), stacking = 'normal') %>%
  hc_chart(polar = TRUE) %>%
  hc_yAxis(max= 30, min = -50, title = list(text = NULL), labels = FALSE) %>%
  hc_xAxis(lineWidth = 0, labels = list(format = "Age {value}"), tickInterval = 2) %>%
  hc_xAxis(title = list(text = NULL)) %>%
  hc_title(text = 'Number of books people read per year',
           style = list(fontWeight = 'bold', fontSize = '20px'),
           align = 'left') %>%
  hc_legend(align = 'left', verticalAlign = 'top') %>%
  hc_tooltip(useHTML = TRUE, 
             pointFormat = tltip,
             headerFormat = '',
             crosshairs = TRUE) %>%
  hc_add_theme(hc_theme_bloom()) %>%
  hc_credits(enabled = TRUE, text = 'By Antonela Tamagnini
             <br> Source: pewresearch.org')
p
saveWidget(p, file = 'Number_of_books_people_read_per_year.html')