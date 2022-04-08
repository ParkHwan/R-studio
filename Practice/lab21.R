library(psych)
library(corrplot)
library(psych)
library(corrgram)

factory <- read.csv("data/factory.csv", encoding = "UTF-8")

factory

#(1)
plot(factory,
     main = "사용연도와 정비비용",
     xlab = "사용연도",
     ylab = "정비비용")

#(2)
cor(factory$age,factory$cost)

#(3)
cor.test(factory$age,factory$cost)

#(4)
factory.lm <- lm(cost~age, data = factory)
factory.lm

#(5)
plot(cost~age, data= factory,
     main = "사용연도와 정비비용",
     xlab = "사용연도",
     ylab = "정비비용",
     col = "blue")
abline(factory.lm, col="red")

#(6)
summary(factory.lm)
coef(factory.lm)
#결정계수 0.6098 p-value 0.0009779 < 0.05이므로 유의미함

#(7)
#회귀 식 =  cost =  29.107 + age * 13.637

df <- 4
29.107 + 13.637 * df

predict.lm(factory.lm, newdata = data.frame(age=4))
