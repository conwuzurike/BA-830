num_1 <- 1
num_2 <- 5
num_1 + num_2

list1 <- c(1, 3, 4)
mean(list1)

library(data.table)
my_data <- fread("covid_nc_college.csv")
head(my_data)
names(my_data)

library(ggplot2)
this_plot <- ggplot(my_data, aes(x = Campus, y = `Cumulative positive cases`)) +
  geom_point() + theme_bw() 
this_plot <- this_plot + xlab("") + ylab("Cumulative Cases") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
this_plot + coord_flip()

# This is a comment. When you run it, nothing happens. It is meant to make code easier to read.