

info201finaldata <- read.csv("~/Desktop/info201finaldata.csv")
View(info201finaldata)




#view <- ggplot(info201finaldata) + geom_point(aes(x = food.waste, y = PPP.capita)) + geom_smooth(method = lm)
view <- ggplot(info201finaldata,aes(food.waste, PPP.capita)) + geom_point() + scale_y_log10() +geom_smooth(method = lm, se = FALSE) + labs(title = "Food Waste VS PPP Capita", x = "Food Waste", y = "PPP capita") + theme(plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5))

ggsave(view, file = "testing.jpg")

plot(view)
              