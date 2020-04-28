install.packages("readxl")
library("readxl")

### Loading Data
novel <- read_excel("D:/Suman/PGDBA/ISI/SSD/Assignment2/Assignment_2_data.xlsx", sheet = "author")
air <- read_excel("D:/Suman/PGDBA/ISI/SSD/Assignment2/Assignment_2_data.xlsx", sheet = "USairpollution")
pot <- read_excel("D:/Suman/PGDBA/ISI/SSD/Assignment2/Assignment_2_data.xlsx", sheet = "pottery")
tea <- read_excel("D:/Suman/PGDBA/ISI/SSD/Assignment2/Assignment_2_data.xlsx", sheet = "tea")
flowers <- read_excel("D:/Suman/PGDBA/ISI/SSD/Assignment2/Assignment_2_data.xlsx", sheet = "gardenflowers")
concrete <- read_excel("D:/Suman/PGDBA/ISI/SSD/Assignment2/Concrete_Data.xls")

## Q1: Concrete Data
head(concrete)
### Covariance Matrix
S <- cov(concrete)
S
### Correlation Matrix
R <- cor(concrete)
R

install.packages("factoextra")
library("factoextra")
### PCA using Dispersion Matrix
S_pca <- prcomp(concrete, scale = FALSE)
### Q1) a)
S_pca$rotation
### Q1) b)
eig.val <- get_eigenvalue(S_pca)
eig.val
### Q1) c)
fviz_eig(S_pca)
### Q1) d)
summary(S_pca)

### PCA using Correlation Matrix
R_pca <- prcomp(concrete, scale = TRUE)
### Q1) a)
R_pca$rotation
### Q1) b)
eig.val <- get_eigenvalue(R_pca)
eig.val
### Q1) c)
fviz_eig(R_pca)
### Q1) d)
summary(R_pca)

## Q2 Novel- Author
install.packages("FactoMineR")
library("FactoMineR")

head(novel)
### Q2) a)
author <- data.frame(novel, row.names = 1)
author_CA <- CA(author, graph = TRUE)
#### Eigen values of CA 
author_CA$eig
### Q2) b)
fviz_ca_biplot(author_CA, repel = TRUE)

## Q3 Tea data
#### Selecting required features for CA
tea_data <- tea[c("individual","Tea", "How", "sugar", "how", "where", "always")]

head(tea_data)

install.packages("mltools")
library("mltools")
### Q3) a)
row.names(tea_data) <- tea_data$individual
tea_data$individual <- NULL
rownames(tea_data)

tea_MCA <- MCA(tea_data)
#### Eigen values of MCA 
tea_MCA$eig
### Q3) b)
fviz_mca_biplot(tea_MCA, repel = TRUE, ggtheme = theme_minimal())
### Q3) d)
#### Selecting required features for finding Tetrachoric correlations
tea_data2 <- tea[c("individual","sophisticated", "slimming", "exciting", "relaxing", "effect.on.health")]

head(tea_data2)

row.names(tea_data2) <- tea_data2$individual
tea_data2$individual <- NULL

tea_data2$sophisticated <- ifelse(tea_data2$sophisticated== 'sophisticated',1,0)
tea_data2$slimming <- ifelse(tea_data2$slimming== 'slimming',1,0)
tea_data2$exciting <- ifelse(tea_data2$exciting== 'exciting',1,0)
tea_data2$relaxing <- ifelse(tea_data2$relaxing== 'relaxing',1,0)
tea_data2$effect.on.health <- ifelse(tea_data2$effect.on.health== 'effect on health',1,0)

head(tea_data2)
#### Tetrachoric Correlation Matrix
library("psych")

tea_tetracorr= tetrachoric(tea_data2)$rho
tea_tetracorr

tetracorr_pca <- princomp(covmat=tea_tetracorr)
fviz_eig(tetracorr_pca)   
#### Percentage Variation explained by each PC
eig.val <- get_eigenvalue(tetracorr_pca)
eig.val
summary(tetracorr_pca)

## Q4 Pottery data
head(pot)
### Q4) a)
dist <- dist(pot, method = "euclidean", diag = TRUE, upper = TRUE)
dist
### Q4) b)
#### 2D MDS Plot
mds<-cmdscale(dist)
library("ggpubr")
colnames(mds) <- c("Dimension1", "Dimension2")
ggscatter(as.data.frame(mds), x = "Dimension1", y = "Dimension2", 
          label = row.names(pot),
          size = 1,
          repel = TRUE)
### Q4) c)
#### 2D MDS plot with color coding as per kiln 
colour <- c(rep("black", 21), rep("blue", 12), rep("green", 2), rep("red", 5), rep("orange", 5))
ggscatter(as.data.frame(mds), x = "Dimension1", y = "Dimension2", 
          label = row.names(pot),
          color = colour,
          size = 1,
          repel = TRUE)

## Q5 Flowers Data
head(flowers)

library("MASS")
### Q5) a)
#### Converting 'flower' column name to row names
row.names(flowers) <- flowers$flower
flowers$flower <- NULL
rownames(flowers)

non_mds <- isoMDS(dist(flowers))
colnames(non_mds$points) <- c("Dimension1", "Dimension2")
ggscatter(as.data.frame(non_mds$points), x = "Dimension1", y = "Dimension2", 
          label = row.names(flowers),
          size = 1,
          repel = TRUE)
### Q5) b)
stress <- c(1:15)
for (i in stress){
    stress[i] <- isoMDS(dist(flowers), k= i, trace = FALSE)$stress}
round(stress, 2)
obj <- as.data.frame(cbind(c(1:15), stress))

obj

plot <- barplot(obj$stress, xlab = 'Dimensions', ylab = 'Stress', main = 'Scree-Plot', ylim = c(0,35), xlim = c(0,15))
lines(plot,obj$stress,col="blue")

## Q6 Air Pollution Data
head(air)
### Q6) a)
fit<-lm(SO2~temp+manu+popul+wind+precip+predays,data=air)
### Q6) b)
plot(fit, which=1, col=c("blue"))
### Q6) c)
summary(fit)
### Q6) d)
anova(fit)
### Q6) e)
confint(fit)
### Q6) f)
newdata = data.frame(temp=c(55),manu=c(440),popul=c(500),wind=c(10.0),precip=c(11.75),predays=c(80))
newdata
predict(fit, newdata, interval="confidence")
### Q6) g)
predict(fit, newdata, interval="prediction")
### Q6) h)
install.packages("olsrr")
library("olsrr")
#### Studentized Residual Plot
ols_plot_resid_stud(fit)
#### Cook's Distance
ols_plot_cooksd_chart(fit)
### Q6) i)
#### Removing suspected outliers
air_data<-air[-c(5,31,33),]

fit2<-lm(SO2~temp+manu+popul+wind+precip+predays,data=air_data)

plot(fit2, which=1, col=c("blue"))

summary(fit2)
