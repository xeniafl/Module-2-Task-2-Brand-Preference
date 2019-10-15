library(readr)
CompleteResponses <- read_csv("C:/Users/user/Downloads/CompleteResponses.csv", col_types = cols(brand = col_character()))
View(CompleteResponses)

which(CompleteResponses$brand == 0)
CompleteResponses[-which(CompleteResponses$brand == 0), "brand"] <- "Sony"
CompleteResponses[which(CompleteResponses$brand == 0), "brand"] <- "Acer"
CompleteResponses$brand <- as.factor(CompleteResponses$brand)

CompleteResponses$brand
table(CompleteResponses$brand)

#Data understanding:

summary(CompleteResponses)

hist(CompleteResponses$salary, breaks = 1000)
table(CompleteResponses$salary)
#we'll need to do somme binning because too many people at = 20.000 & 150.000

which(CompleteResponses$salary == 20000)
salaryok1 <- CompleteResponses[-which(CompleteResponses$salary == 20000), ]
salaryok1

which(salaryok1$salary == 150000)
salaryok <- salaryok1[-which(salaryok1$salary == 150000),]
salaryok


hist(salaryok$salary, breaks = 1000)


hist(CompleteResponses$age, breaks = 120)
table(CompleteResponses$age)
boxplot(CompleteResponses$age)
#very skewed to 20 yo and 80, why?

which(salaryok$age == 20)
ageok1 <- salaryok[-which(salaryok$age == 20),]
ageok1

which(ageok1$age == 80)
ageok <- ageok1[-which(ageok1$age == 80),]
ageok

hist(ageok$age, breaks = 120)
table(ageok$age)
boxplot(ageok$age)



elevelgraph <- table(ageok$elevel)
elevelgraph
hist(elevelgraph, breaks = 8)

cargraph<-table(ageok$car)
hist(cargraph, breaks = 80)

zipcodegraph <- table(ageok$zipcode)
hist(zipcodegraph, breaks = 32)

hist(ageok$credit, breaks = 20)

table(ageok$brand)

###########################

library(ggplot2)

ggplot(ageok, aes(x = ageok$age, y = ageok$salary, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$elevel, y = ageok$salary, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$car, y = ageok$salary, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$zipcode, y = ageok$salary, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$credit, y = ageok$salary, color = brand)) + geom_point()

ggplot(ageok, aes(x = ageok$salary, y = ageok$age, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$elevel, y = ageok$age, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$car, y = ageok$age, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$zipcode, y = ageok$age, color = brand)) + geom_point()
ggplot(ageok, aes(x = ageok$credit, y = ageok$age, color = brand)) + geom_point()

ggplot(ageok, aes(x = ageok$salary, y = ageok$elevel, color = brand)) + geom_point()

library(party)

################################

library(lattice)
library(caret)

set.seed(107)
inTrain <- createDataPartition(y = ageok$brand, p = .75, list = FALSE)
str(inTrain)

training <- ageok[ inTrain,]
testing <- ageok[-inTrain,]
nrow(training)
nrow(testing)

######### NOW TRAINING AND TESTING SETS ARE READY, WE'LL TRY THE DIFFERENT METHODS: ######

## DECISION TREE ##


DecisionTreeFit <- ctree(brand ~ ., data = training, controls = ctree_control(maxdepth = 3))
plot(DecisionTreeFit, type = "simple")


DecisionTreeFitbrand <- predict(DecisionTreeFit, newdata = testing)
str(DecisionTreeFit)
confusionMatrix(data = DecisionTreeFitbrand, testing$brand)

## C5.0 ##

library(C50)

C5_0TreeFit <- C5.0(brand ~ ., data = training, trials = 10)
C5_0TreeFit
summary(C5_0TreeFit)
plot(C5_0TreeFit, type = "simple")

C5_0TreeFitbrand <- predict(C5_0TreeFit, newdata = testing, )
str(C5_0TreeFitbrand)
confusionMatrix(data = C5_0TreeFitbrand, testing$brand)

varImp(C5_0TreeFit)

## RANDOM FOREST ##

library(randomForest)

RandomForestFit <- randomForest(brand ~ ., data = training, trials = 10, mtry = 3)
RandomForestFit
summary(RandomForestFit)
plot(RandomForestFit, type = "simple")

RandomForestFitbrand <- predict(RandomForestFit, newdata = testing, )
str(RandomForestFitbrand)
confusionMatrix(data = RandomForestFitbrand, testing$brand)

varImp(RandomForestFit)



################## GGPLOT LESSON ##############


ggplot(ageok, aes(x = brand, fill = brand)) + geom_bar()
ggplot(ageok, aes(x = salary, fill = brand)) + geom_histogram(colour = "black")
ggplot(ageok, aes(x = salary, fill = brand)) + geom_histogram() + facet_grid(. ~ elevel)
ggplot(ageok, aes(x = salary, fill = brand)) + geom_histogram() + facet_grid(. ~ zipcode)
ggplot(ageok, aes(x = salary, fill = brand)) + geom_histogram() + facet_grid(zipcode ~ .)
ggplot(ageok, aes(x = salary, fill = brand)) + geom_histogram() + facet_grid(elevel ~ zipcode)
ggplot(ageok, aes(x = age, y = salary, color = brand)) + geom_point() + geom_smooth()
ggplot(ageok, aes(x = age, y = salary, color = brand)) + geom_point() + facet_grid(. ~ elevel)
ggplot(ageok, aes(x = brand, y = salary, fill = brand)) + geom_boxplot() + theme_bw()
ggplot(ageok, aes(x = brand, y = salary, fill = brand)) + geom_boxplot() + stat_summary(fun.y = mean, geom = "text", vjust = -1, aes(label = round(..y.., digits =1)))

library("ggthemes")
ggplot(ageok, aes(x = "", fill = brand)) + geom_bar() + coord_polar(theta = "y") + facet_grid(. ~ zipcode) + theme_economist()
ggplot(ageok, aes(x = brand, fill = brand)) + geom_bar() + theme_excel()

library("plotly")
plot_ly(ageok, x = ~age, y = ~salary, color = ~brand)

