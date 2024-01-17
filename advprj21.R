hanes<-read.csv('han.csv')
colors <- ifelse(hanes$gender == 'M', 'purple', 'green')
shapes <- ifelse(hanes$gender == 'M', 3, 4) 
plot(hanes$height, hanes$weight, col = colors, pch = shapes, 
     xlab = "Height", ylab = "Weight", 
     main = "Weight VS Height",
)
legend("topright", legend = c("Male", "Female"), pch = c(3, 4), col = c("purple", "green"))

hangind <- ifelse(hanes$gender == 'M', 1, 0)
hangmdl1 <- lm(weight ~ height + hangind, data = hanes)
summary(hangmdl1) 

coeffmdl1 <- coef(hangmdl1)
intcptmdl1 <- coeffmdl1[1]
slpmdl1 <- coeffmdl1[2]
geff <- coeffmdl1[3]
intcptmdl1
slpmdl1
geff
hran <- range(hanes$height)
weight_male <- intcptmdl1 + slpmdl1 * hran + geff
weight_female <- intcptmdl1 + slpmdl1 * hran
lines(hran, weight_male, col = "purple", lwd = 3)
lines(hran, weight_female, col = "green", lwd = 3)
legend("topleft",legend = c("Weight_Males", "Weight_Females"),lwd=c(3,3),col = c("purple", "green"))



