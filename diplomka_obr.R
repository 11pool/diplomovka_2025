
## most of the pictures were created here

# Load libraries
library(ggplot2)
library(MASS)

#### adverserial examples ####
# internet, easy

#### multi lda, linear descicion boundary, pairwise ####
# R(prob. chat + ggplot) or internet
# NOT WORKING, SEE CHAT

# 1) Simulate data
set.seed(123)
n <- 100
data <- data.frame(
  x1    = c(rnorm(n, mean=0),  rnorm(n, mean=5),  rnorm(n, mean=-2),  rnorm(n, mean=3)),
  x2    = c(rnorm(n, mean=0),  rnorm(n, mean=1),  rnorm(n, mean=3),  rnorm(n, mean=3)),
  class = factor(rep(1:4, each=n))
)

# 2) Fit LDA
lda_fit <- lda(class ~ x1 + x2, data)

# 3) Compute the pooled covariance and its inverse
pooled_cov <- Reduce(`+`,
                     lapply(split(data[,1:2], data$class),
                            function(df) cov(df)*(nrow(df)-1))
) / (nrow(data) - length(levels(data$class)))
inv_cov <- solve(pooled_cov)

# 4) Extract class means
mus <- lda_fit$means

# 5) Build a fine grid and get its Bayesian posterior
xr <- seq(min(data$x1)-1, max(data$x1)+1, length=300)
yr <- seq(min(data$x2)-1, max(data$x2)+1, length=300)
grid <- expand.grid(x1=xr, x2=yr)
post <- predict(lda_fit, grid)$posterior
grid$pred <- factor(max.col(post))  # class with highest δ

# 6) Compute each pairwise boundary analytically
boundary_lines <- do.call(rbind, lapply(combn(1:4,2, simplify=FALSE), function(pair) {
  i <- pair[1]; j <- pair[2]
  μi <- as.numeric(mus[i,]);  μj <- as.numeric(mus[j,])
  # w = Σ^{-1}(μ_i - μ_j)
  w  <- inv_cov %*% (μi - μj)
  # intercept term: w0 = -½(μ_i'Σ^{-1}μ_i - μ_j'Σ^{-1}μ_j)
  w0 <- -0.5*(t(μi) %*% inv_cov %*% μi - t(μj) %*% inv_cov %*% μj)
  data.frame(
    slope     = -w[1]/w[2],
    intercept = -w0 / w[2]
  )
}))


# nastavenie priečinku, do ktorého sa uloží PDF-graf:
cesta <- file.path("C:", "diplo_obr", paste("pair-lda", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");



# 7) Plot
ggplot() +
  # shaded regions (300×300 grid = smooth)
  geom_tile(data=grid, aes(x=x1, y=x2, fill=pred), alpha=0.3) +
  # exact LDA boundaries
  geom_abline(data=boundary_lines,
              aes(slope=slope, intercept=intercept),
              color="black", size=1) +
  # true data points
  geom_point(data=data, aes(x=x1, y=x2, color=class), size=2) +
  # palettes & labels
  scale_fill_manual(values=c("#FFCCCC","#CCFFCC","#CCCCFF","#FFEECC")) +
  scale_color_manual(values=c("#CC0000","#009900","#0000CC","#FF6600")) +
  labs(
    title = "LDA - viac tried, párové hranice",
    x = "Znak 1", y = "Znak 2",
    fill  = "Odhad triedy",
    color = "Skutočná trieda"
  ) +
  theme_minimal(base_size=15) +
  theme(legend.position="right")

# zatvorenie grafického súboru
dev.off()

#### multi LDA, not pairwise ####
# get intersections for line drawing (line 3 has all the points)
for (i in c(1:2, 4:6)){
  print('X:')
  x = (boundary_lines[3,'intercept']-boundary_lines[i,'intercept'])/(boundary_lines[i,'slope']-boundary_lines[3,'slope'])
  print(x)
  print('Y:')
  y = boundary_lines[3,'slope']*x+boundary_lines[3,'intercept']
  print(y)
}
 
segment_data = data.frame(
  x = c(2.47419, 0.5504072, 0.5504072, 2.47419, 0.5504072),
  xend = c(5, -7, 2.587031, 11, 0), 
  y = c(0.6544149, 2.587031, 2.587031, 0.6544149, 2.587031),
  yend = c(-17.31359,-3.207448, 0.5410555, 8.731316, 28.56482)
)

# nastavenie priečinku, do ktorého sa uloží PDF-graf:
cesta <- file.path("C:", "diplo_obr", paste("multi-lda", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");


ggplot() +
  # shaded regions (300×300 grid = smooth)
  geom_tile(data=grid, aes(x=x1, y=x2, fill=pred), alpha=0.3) +
  # exact LDA boundaries
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend)
               , color="black", size=1) +
  # true data points
  geom_point(data=data, aes(x=x1, y=x2, color=class), size=2) +
  # palettes & labels
  scale_fill_manual(values=c("#FFCCCC","#CCFFCC","#CCCCFF","#FFEECC")) +
  scale_color_manual(values=c("#CC0000","#009900","#0000CC","#FF6600")) +
  labs(
    title = "LDA - viac tried",
    x = "Znak 1", y = "Znak 2",
    fill  = "Odhad triedy",
    color = "Skutočná trieda"
  ) +
  theme_minimal(base_size=15) +
  theme(legend.position="right") +
  coord_cartesian(xlim = c(min(data$x1)-1, max(data$x1)+1), ylim = c(min(data$x2)-1, max(data$x2)+1))

dev.off()

#### multi-lda, together ####
cesta <- file.path("C:", "diplo_obr", paste("together-lda", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");

par(mfrow=c(2,1), mar=c(4.5,4.5,3,1)+.1, mgp = c(3.0, 1, 0))
ggplot() +
  # shaded regions (300×300 grid = smooth)
  geom_tile(data=grid, aes(x=x1, y=x2, fill=pred), alpha=0.3) +
  # exact LDA boundaries
  geom_abline(data=boundary_lines,
              aes(slope=slope, intercept=intercept),
              color="black", size=1) +
  # true data points
  geom_point(data=data, aes(x=x1, y=x2, color=class), size=2) +
  # palettes & labels
  scale_fill_manual(values=c("#FFCCCC","#CCFFCC","#CCCCFF","#FFEECC")) +
  scale_color_manual(values=c("#CC0000","#009900","#0000CC","#FF6600")) +
  labs(
    title = "LDA - viac tried, párové hranice",
    x = "Znak 1", y = "Znak 2",
    fill  = "Odhad triedy",
    color = "Skutočná trieda"
  ) +
  theme_minimal(base_size=15) +
  theme(legend.position="right")

ggplot() +
  # shaded regions (300×300 grid = smooth)
  geom_tile(data=grid, aes(x=x1, y=x2, fill=pred), alpha=0.3) +
  # exact LDA boundaries
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend)
               , color="black", size=1) +
  # true data points
  geom_point(data=data, aes(x=x1, y=x2, color=class), size=2) +
  # palettes & labels
  scale_fill_manual(values=c("#FFCCCC","#CCFFCC","#CCCCFF","#FFEECC")) +
  scale_color_manual(values=c("#CC0000","#009900","#0000CC","#FF6600")) +
  labs(
    title = "LDA - viac tried",
    x = "Znak 1", y = "Znak 2",
    fill  = "Odhad triedy",
    color = "Skutočná trieda"
  ) +
  theme_minimal(base_size=15) +
  theme(legend.position="right") +
  coord_cartesian(xlim = c(min(data$x1)-1, max(data$x1)+1), ylim = c(min(data$x2)-1, max(data$x2)+1))

dev.off()



#### single qda ####
set.seed(123)

# Simulate 4 classes of 2D data
n <- 100
class1 <- mvrnorm(n, mu = c(0, 2), Sigma = matrix(c(1,0.9,0.9,1), ncol=2))
class2 <- mvrnorm(n, mu = c(6, 2), Sigma = matrix(c(1,-0.3,-0.3,1), ncol=2))

# Combine into a dataframe
df <- data.frame(
  x = c(class1[,1], class2[,1]),
  y = c(class1[,2], class2[,2]),
  class = factor(rep(1:2, each = n))
)

# Fit QDA model
qda_model <- qda(class ~ x + y, data = df)

# Create a grid over the feature space
x_seq <- seq(min(df$x) - 1, max(df$x) + 1, length.out = 300)
y_seq <- seq(min(df$y) - 1, max(df$y) + 1, length.out = 300)
grid <- expand.grid(x = x_seq, y = y_seq)

# Predict class probabilities on the grid
grid_pred <- predict(qda_model, grid)
grid$class <- as.factor(grid_pred$class)

cesta <- file.path("C:", "diplo_obr", paste("qda", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");
# Plot
ggplot() +
  geom_tile(data = grid, aes(x = x, y = y, fill = class), alpha = 0.4) +
  geom_point(data = df, aes(x = x, y = y, color = class), size = 1.5) +
  stat_contour(data = grid, aes(x = x, y = y, z = as.numeric(class)), 
               bins = 2, color = "black", size = 1) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  labs(title = "Kvadrtická diskriminačná analýza",
       x = "Znak 1", y = "Znak 2",
       fill = "Odhad triedy", color = "Skutočná trieda") +
  theme(legend.position = "right")

dev.off()

#### multi qda ####

set.seed(123)

# Simulate 4 classes of 2D data
n <- 100
class1 <- mvrnorm(n, mu = c(0, 2), Sigma = matrix(c(1,0.5,0.5,1), ncol=2))
class2 <- mvrnorm(n, mu = c(6, 2), Sigma = matrix(c(1,-0.3,-0.3,1), ncol=2))
class3 <- mvrnorm(n, mu = c(2, 6), Sigma = matrix(c(1,0.7,0.7,1), ncol=2))
class4 <- mvrnorm(n, mu = c(6, 6), Sigma = matrix(c(1,0.2,0.2,1), ncol=2))

# Combine into a dataframe
df <- data.frame(
  x = c(class1[,1], class2[,1], class3[,1], class4[,1]),
  y = c(class1[,2], class2[,2], class3[,2], class4[,2]),
  class = factor(rep(1:4, each = n))
)

# Fit QDA model
qda_model <- qda(class ~ x + y, data = df)

# Create a grid over the feature space
x_seq <- seq(min(df$x) - 1, max(df$x) + 1, length.out = 300)
y_seq <- seq(min(df$y) - 1, max(df$y) + 1, length.out = 300)
grid <- expand.grid(x = x_seq, y = y_seq)

# Predict class probabilities on the grid
grid_pred <- predict(qda_model, grid)
grid$class <- as.factor(grid_pred$class)

cesta <- file.path("C:", "diplo_obr", paste("qda-multi", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");

# Plot
ggplot() +
  geom_tile(data = grid, aes(x = x, y = y, fill = class), alpha = 0.4) +
  geom_point(data = df, aes(x = x, y = y, color = class), size = 1.5) +
  stat_contour(data = grid, aes(x = x, y = y, z = as.numeric(class)), 
               bins = 4, color = "black", size = 1) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 14) +
  labs(title = "QDA na štyroch triedach",
       x = "Znak 1", y = "Znak 2",
       fill = "Odhadnutá trieda", color = "Skutočná trieda") +
  theme(legend.position = "right")

dev.off()

#### svm 2class, like wiki but not ####
# internet or R

#### metody zhlukovvania, zhluk vs klasifikator ####
# klasif
set.seed(123)
data <- matrix(c(rnorm(200), rep(NA, 100)), ncol=3, nrow=100)
data[data[,2]>0,3] <- 1
data[data[,2]<=0,3] <- 2
df <- data.frame(x = data[,1], y = data[,2], class = factor(data[,3]))

cesta <- file.path("C:", "diplo_obr", paste("klasif", ".pdf", sep = ""));
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");

par(mfrow=c(1,2), mar=c(4.5,4.5,3,1)+.1, mgp = c(3.0, 1, 0))

ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Dáta bez označenia",
    x = "Znak 1",
    y = "Znak 2",
    color = "Trieda"
  ) +
  theme(legend.position = "right")

ggplot(df, aes(x = x, y = y, color = class)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "red"), labels = c("Class 1", "Class 2")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Dáta s označeniami",
    x = "Znak 1",
    y = "Znak 2",
    color = "Trieda"
  ) +
  theme(legend.position = "right")

dev.off()

# zhluk
set.seed(123)
data <- matrix(c(rnorm(100), rnorm(50, -4), rnorm(50, 4) , rep(NA, 100)), ncol=3, nrow=100)
data[data[,2]>0,3] <- 1
data[data[,2]<=0,3] <- 2
df <- data.frame(x = data[,1], y = data[,2], class = factor(data[,3]))

ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Dáta na zhlukovanie",
    x = "Znak 1",
    y = "Znak 2",
    color = "Trieda"
  ) +
  theme(legend.position = "right")



cesta <- file.path("C:", "diplo_obr", paste("clust", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");

ggplot(df, aes(x = x, y = y)) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Dáta na zhlukovanie",
    x = "Znak 1",
    y = "Znak 2",
    color = "Trieda"
  ) +
  theme(legend.position = "right")

dev.off()

#### k-means ####
# internet, something basic

#### kapitola 3 hl myslienka ako na prezentacii ####
# uz mam lebo som to ukazoval Harmanovi ako sanity check

#### kapitola 3 mala granularita ####

set.seed(123)
data <- matrix(c(rnorm(100, 0.3), rnorm(100, -10), rnorm(50, -1, 0.3), rnorm(50, 1, 0.3) , rnorm(100), rep(NA, 200)), ncol=3, nrow=200)
data[data[,2]>0,3] <- 1
data[data[,2]<=0,3] <- 2
df <- data.frame(x = data[,1], y = data[,2], class = factor(data[,3]))
# Convert to data frame
df <- data.frame(x = data[,1], y = data[,2], true_class = factor(data[,3]))

# Apply k-means clustering with k = 2
kmeans_result <- kmeans(df[, c("x", "y")], centers = 2)

# Add cluster assignments to the dataframe
df$cluster <- factor(kmeans_result$cluster)

# Plot
ggplot(df, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("darkorange", "navy")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "K-means Clustering (k = 2)",
    x = "X-axis",
    y = "Y-axis",
    color = "Cluster"
  ) +
  theme(legend.position = "right")


#### global vs lokal fit ####
# R

#### dendrogram vyska a dendrogram acc ####
# diplo_run

#### iterative cluster repair ####
set.seed(123)
data <- rbind(
  matrix(c(rnorm(40, -4), rnorm(40), rep(1, 40)), ncol=3),
  matrix(c(rnorm(20,2, 0.4), rnorm(20, 1, 0.4), rep(1, 20)), ncol=3), 
  matrix(c(rnorm(20, 2, 0.4), rnorm(20, -0.5, 0.4), rep(2, 20)), ncol=3),
  matrix(c(rnorm(20, 2, 0.4), rnorm(20, -2, 0.4), rep(1, 20)), ncol=3)
)

df <- data.frame(x = data[,1], y = data[,2], class = factor(data[,3]))
# Convert to data frame
# df <- data.frame(x = data[,1], y = data[,2], true_class = factor(data[,3]))

# Apply k-means clustering with k = 2
kmeans_result <- kmeans(df[c(1:60, 80:100), c("x", "y")], centers = 2)

# Add cluster assignments to the dataframe
df$cluster <- factor(c(kmeans_result$cluster[1:60],rep(3, 20), kmeans_result$cluster[61:80]))


cesta <- file.path("C:", "diplo_obr", paste("repair", ".pdf", sep = ""));

# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");

# Plot
ggplot(df, aes(x = x, y = y, color = class, shape = as.factor(cluster))) +
  geom_point(size = 2) +
  scale_color_manual(values = c("darkorange", "navy"),
                     labels = c('l_1', 'l_2')) +
  scale_shape_manual(
    values = c("1" = 16, "2" = 17, "3" = 15),
    labels = c("1" = "l_1_1", "2" = "l_1_2", "3" = 'l_2')  # hides Cluster 3 from legend
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Zhlukovanie pre k=2",
    x = "Znak 1",
    y = "Znak 2",
    color = "Pôvodné triedy",
    shape = "Triedy po zhlukovaní"
  ) +
  theme(legend.position = "right")

dev.off()

#### priklad na dendrogram do vysledkoch na datach ####
priklad_na_dendr_1 <- diplomovka_rewrite(land[,-37], land[,37], 'qda', 'kmedoids', 'pair', 2, 4)
priklad_na_dendr_2 <- diplomovka_rewrite(land[,-37], land[,37], 'svm', 'kmeans', 'pair', 2, 4)
priklad_na_dendr_3 <- diplomovka_rewrite(mnistpca_train$x[,1:484], mnist$train$labels,'qda', 'kmeans', 'single', 2, 4)
# priklad_na_dendr_4 <- diplomovka_rewrite(mnistpca_train$x[,1:484], mnist$train$labels, ML, clust, method, step, max_depth)

cesta <- file.path("C:", "diplo_obr", paste("dendr_mnist_train", ".pdf", sep = ""));
# dendr_land_train
# nastavenie veľkosti plátna a kódovania znakov (slovenských písmen)
pdf(file=cesta, width=(15.5*0.3937), height=(15.5*0.618*0.3937), encoding="CP1250.enc");
# par(mfrow=c(1,2), mar=c(4.5,4.5,3,1)+.1, mgp = c(3.0, 1, 0))

plot(MlClust_dendrogram(priklad_na_dendr_1, 'acc', first_bump = 0.02), center=TRUE)
plot(MlClust_dendrogram(priklad_na_dendr_2, 'acc', first_bump = 0.02), center=TRUE, ylim=c(0.03, 0.1))
plot(MlClust_dendrogram(priklad_na_dendr_3, 'acc', first_bump = 0.03), center=TRUE, ylim=c(0.15, 0.25))

dev.off()

