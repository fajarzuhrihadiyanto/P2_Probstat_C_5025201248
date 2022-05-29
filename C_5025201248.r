# SOAL 1
data <- data.frame(
  Responden=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  X=c(78, 75, 67, 77, 70, 72, 78, 74, 77),
  Y=c(100, 95, 70, 90, 90, 90, 89, 90, 100)
)
significance <- 0.05
dataDf <- 8

# 1.a Standard Deviation
dataDiff <- data$Y - data$X
dataSd <- sd(dataDiff)

# 1.b p-value from t-score
dataTScore <- qt(p=(significance/2), df=dataDf, lower.tail = FALSE)
dataPValue <- pt(q=dataTScore, df =dataDf, lower.tail=FALSE)

print(dataTScore)
print(dataPValue)

# 1.c
# h0 X = Y
# h1 X != Y

# Test Statistics -> num of sample <30 -> t test
dataX <- data$X
dataY <- data$Y

t.test(dataY, dataX, var.equal = TRUE)

# SOAL 2
sampleMean <- 23500
sampleSd <- 3900
n <- 100
df <- n-1
u <- 20000
alpha <- 0.05
qt(alpha, df)
Z <- (sampleMean - u) / (sampleSd / sqrt(n))
print(Z)

# SOAL 3
n <- 19 + 27 - 2
df <- 2

alpha <- 0.05
alpha_half <- alpha/2

tTable <- qt(alpha_half, df, lower.tail = FALSE)
print(tTable)

data_variance <- ((19-1)*(1.67^2)+(27-1)*(1.32^2))/(19+27-2)
T <- (3.64-2.79)/(sqrt(data_variance)*sqrt((1/19)+(1/27)))
print(T)

# SOAL 4
library(dplyr)
library("ggpubr")

my_data <- read.delim(file.choose(), stringsAsFactors = T)

set.seed(0)
dplyr::sample_n(my_data, 10)

newData <- my_data %>% mutate(Group = as.factor(Group))

levels(newData["Group"])

ggboxplot(newData, x = "Group", y = "Length",
          color = "Group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "2", "3"),
          ylab = "Length", xlab = "Group")

ggline(newData, x = "Group", y = "Length",
      add = c("mean_se", "jitter"),
      order = c(1, 2, 3),
      ylab = "Length", xlab = "Group")

res.aov <- aov(Length ~ Group, data=newData)
summary(res.aov)
TukeyHSD(res.aov)

# SOAL 5
my_data <- read.csv(file.choose())

my_data$Temp <- factor(my_data$Temp, levels = c(100, 125, 150), labels = c("100C", "125C", "150C"))
my_data$Glass <- factor(my_data$Glass)

str(my_data)

table(my_data$Glass, my_data$Temp)

library("ggpubr")
ggboxplot(my_data, x = "Temp", y = "Light", color = "Glass",
          palette = c("#FF0000", "#00FF00", "#0000FF"))

library("ggpubr")
ggline(my_data, x = "Temp", y = "Light", color = "Glass",
      add = c("mean_se", "dotplot"),
      palette = c("#FF0000", "#00FF00", "#0000FF"))

interaction.plot(x.factor = my_data$Temp, trace.factor =
                my_data$Glass,
                response = my_data$Light, fun = mean,
                type = "b", legend = TRUE,
                xlab = "Temp", ylab="Light",
                pch=c(1,19), col = c("#FF0000", "#00FF00", "#0000FF"))

res.aov2 <- aov(Light ~ Glass + Temp, data = my_data)
summary(res.aov2)
TukeyHSD(res.aov2)

res.aov3 <- aov(Light ~ Glass * Temp, data = my_data)
res.aov3 <- aov(Light ~ Glass + Temp + Glass:Temp, data = my_data)
summary(res.aov3)
TukeyHSD(res.aov3)
