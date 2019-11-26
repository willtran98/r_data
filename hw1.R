library(readr)
yelp <- read_csv("yelp.csv", quote = "\"", comment.char = "")
View(yelp)

# Part 3
summary(yelp)
names(yelp)

# Part 4a
# a/ Histogram of stars
hist(yelp$stars,
        main = "Histogram of stars feature",
        xlab = "Number of stars",
        col = "darkmagenta",
        freq = FALSE
        )

# b/ Logged stars
yelp$log_stars <- log(yelp$stars)
hist(yelp$log_stars,
     main = "Histogram of Log Values of stars",
     xlab = "Log Values",
     col = "darkmagenta",
     freq = FALSE
)

# c/ Density plot
plot(density(yelp$log_stars), main="Density of Log Values for stars")
polygon(density(yelp$log_stars), col="red", border="blue")

# Part 4b
states <- table(yelp$state)
names(states)
barplot(states, main="State Distribution",
        xlab="State")

# Part 5A
reliableReview <- ifelse(yelp$reviewCount > 10, TRUE, FALSE)
highStar <- ifelse(yelp$reviewCount > 10 & yelp$stars > 4, TRUE, FALSE)
yelp_new <- yelp
yelp_new <- cbind(yelp_new, reliableReview, highStar)
View(yelp_new)

names(yelp)
names(yelp_new)

# Part 5b
# a/ Quantiles
quantile(yelp$checkins)

# b/ Subset
checkins_subset <- subset(yelp, yelp$checkins <= 16)

# c/ Summary
summary(checkins_subset$checkins)
summary(checkins_subset$stars)
summary(checkins_subset$noiseLevel)
summary(checkins_subset$priceRange)
summary(checkins_subset$reviewCount)
summary(checkins_subset$goodForGroups)

summary(yelp$checkins)
summary(yelp$stars)
summary(yelp$noiseLevel)
summary(yelp$priceRange)
summary(yelp$reviewCount)
summary(yelp$goodForGroups)

# Part 6
# A. Scatterplot matrix
pairs(~ yelp$stars + yelp$reviewCount + yelp$checkins,data = yelp,
      main = "Scatterplot Matrix")

# B. Correlation
cor(yelp$stars, yelp$stars)
cor(yelp$stars, yelp$reviewCount)
cor(yelp$stars, yelp$checkins)

cor(yelp$reviewCount, yelp$stars)
cor(yelp$reviewCount, yelp$reviewCount)
cor(yelp$reviewCount, yelp$checkins)

cor(yelp$checkins, yelp$stars)
cor(yelp$checkins, yelp$reviewCount)
cor(yelp$checkins, yelp$checkins)

# C. Boxplots
boxplot(yelp$stars ~ yelp$alcohol,data = yelp, main="stars vs. alcohol",
        xlab="Types of alcohol", ylab="Number of stars")
boxplot(yelp$reviewCount ~ yelp$alcohol,data = yelp, main="reviewCount vs. alcohol",
        xlab="Types of alcohol", ylab="Number of reviewCount")
boxplot(yelp$checkins ~ yelp$alcohol,data = yelp, main="checkins vs. alcohol",
        xlab="Types of alcohol", ylab="Number of checkins")
boxplot(yelp$longitude ~ yelp$alcohol,data = yelp, main="longitude vs. alcohol",
        xlab="Types of alcohol", ylab="longitude")
boxplot(yelp$latitude ~ yelp$alcohol,data = yelp, main="latitude vs. alcohol",
        xlab="Types of alcohol", ylab="latitude")

# C(b). IQRs
none_sub <- subset(yelp, yelp$alcohol == "none")
baw_sub <- subset(yelp, yelp$alcohol == "beer_and_wine")
full_sub <- subset(yelp, yelp$alcohol == "full_bar")
empty_sub <- subset(yelp, is.na(yelp$alcohol))

quantile(none_sub$reviewCount)
quantile(baw_sub$reviewCount)
quantile(full_sub$reviewCount)
quantile(empty_sub$reviewCount)

# Part 7
# A. reviewCount vs. priceRange
plot(yelp$reviewCount, yelp$priceRange, main="reviewCount vs. priceRange",
     xlab="Number of reviewCounts", ylab="Price Range")
boxplot(yelp$reviewCount ~ yelp$priceRange, data = yelp, main="reviewCount vs. priceRange",
        xlab="priceRange", ylab="reviewCount")

# B. 
boxplot(yelp$reviewCount ~ yelp$state, data = yelp, main="reviewCount vs. state",
        xlab="State", ylab="reviewCount")
