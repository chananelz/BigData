## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
# The following packages must be installed
library(stringr)
library(dplyr)
library(ggplot2)
library(mosaic)
library(dplyr)
library(stringr)
library(xtable)
library(gridExtra)
library(stopwords)

# Set rounding to 2 digits
options(digits=2)

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles <- read.csv( file.path( 'dating', 'profiles.csv' ), header=TRUE, stringsAsFactors=FALSE)
n <- nrow(profiles)

str(profiles)

pdf( file.path( 'dating', 'dating.pdf') )
# Heights of all users
#favstats(height, data=profiles)

profiles.subset <- filter(profiles, height>=55 & height <=80)

# Histograms of user heights split by sex
histogram( ~ height | sex, width=1, layout=c(1,2), xlab="Height in inches",
          data=profiles.subset)

# Distributions of gender and diet
par(mfrow=c(1, 2))
barplot(table(profiles$sex)/n, xlab="sex", ylab="proportion")
barplot(table(profiles$diet)/n, xlab="diet", ylab="proportion")

# Joint distribution of sex and sexual diet
addmargins ( xtabs(~ diet + sex, data=profiles) )
addmargins ( tally(diet ~ sex, data=profiles, format='proportion') )

#
# collapse the many variations of diet
#     anything             /
#     mostly anything     | ---      anything
#     strictly anything    \
#     halal                /
#     strictly halal       \ ---     halal
#     kosher                /
#     mostly kosher        | ---      kosher 
#     strictly kosher       \
#     other                /
#     mostly other        | ---      other  
#     strictly other       \
#     vegan                /
#     mostly vegan        | ---      vegan  
#     strictly vegan       \
#     vegetarian             /
#     mostly vegetarian     | ---      vegetarian
#     strictly vegetarian    \
#
profiles$Diet <- sapply( profiles$diet, function(e) last( strsplit( e, "\\s", perl = T)[[1]] ) )

sex.by.diet <- tally(~ sex + Diet, data=profiles)
sex.by.diet
mosaicplot(sex.by.diet, main="Gender vs Diet", las=1)

essays <- select(profiles, starts_with("essay"))
essays <- apply(essays, MARGIN = 1, FUN = paste, collapse=" ")

html <- c( "<a", "class=.ilink.", "\n", "\\n", "<br ?/>", "/>", "/>\n<br" )
stop.words <-  c( "a", "am", "an", "and", "as", "at", "are", "be", "but", "can", "do", "for", "have", "i'm", "if", "in", "is", "it", "like", "love", "my", "of", "on", "or", "so", "that", "the", "to", "with", "you", "i" )

# list languages for a specific source
stopwords::stopwords_getlanguages("snowball")
##  [1] "da" "de" "en" "es" "fi" "fr" "hu" "ir" "it" "nl" "no" "pt" "ro" "ru" "sv"
head(stopwords::stopwords("english"), 20)
##  [1] "i"          "me"         "my"         "myself"     "we"
##  [6] "our"        "ours"       "ourselves"  "you"        "your"
## [11] "yours"      "yourself"   "yourselves" "he"         "him"
## [16] "his"        "himself"    "she"        "her"        "hers"

html.pat <- paste0( "(", paste(html, collapse = "|"), ")" )
html.pat
stop.words.pat <- paste0( "\\b(", paste(stop.words, collapse = "|"), ")\\b" )
stop.words.pat
essays <- str_replace_all(essays, html.pat, " ")
essays <- str_replace_all(essays, stop.words.pat, " ")


## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles$has.book <- str_detect(essays, "book")
tally(has.book ~ sex, profiles, format='proportion')

## ----echo=FALSE, cache=TRUE, warning=FALSE, message=FALSE, results='asis'----
queries <- c("travel", "food", "wine", "beer", "football", "art")
output <- data.frame(word=queries, female=rep(0, length(queries)), male=rep(0, length(queries)))
for(i in 1:length(queries)) {
  query <- queries[i]
  has.query <- str_detect(essays, query)
  results <- table(has.query, profiles$sex)
  output[i, 2:3] <- results[2, ] / colSums(results)
}
print(xtable(output, digits=c(0, 0, 3, 3),
	     caption ="Proportions of each gender using 'word' in essays.",
	     label = "tab:word_use"), include.rownames=FALSE)

# Co-occurrence of `travel' and `wine.'
profiles$has.wine <- str_detect(essays, "wine")
profiles$has.travel <- str_detect(essays, "travel")
travel.vs.wine <- tally(~has.travel + has.wine, data=profiles)
mosaicplot(travel.vs.wine, xlab="travel", ylab="wine", main = "has Travel vs. has Wine")

#
# hack to plot an xtable on a pdf page as part of a pdf sequence:
# qplot of the tableGrob
#
qplot(1:10, 1:10, geom = "blank") + theme_bw() +
  theme(line = element_blank(),
	axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.title = element_text(face = "bold")
      ) +
  annotation_custom(grob = tableGrob(output), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
   labs(title = "", subtitle = "Proportions of each gender using word in essays", caption = "")

profiles$has.football <- str_detect(essays, "football")
results <- tally( ~ has.football + sex, data=profiles)
prop.test(x=results[1, ], n=colSums(results), alternative="two.sided")

male.words <- subset(essays, profiles$sex == "m") %>%
  str_split(" ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  names()
female.words <- subset(essays, profiles$sex == "f") %>%
  str_split(" ") %>%
  unlist() %>%
  table() %>%
  sort(decreasing=TRUE) %>%
  names()

# Top 25 male words:
print( male.words[1:25] )
# Top 25 female words
print( female.words[1:25] )

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
# Words in the males top 500 that weren't in the females' top 500:
setdiff(male.words[1:500], female.words[1:500])
# Words in the male top 500 that weren't in the females' top 500:
setdiff(female.words[1:500], male.words[1:500])

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
set.seed(76)
sample(1:10)
set.seed(76)
sample(1:10)
set.seed(79)
sample(1:10)

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles <- filter(profiles, height>=55 & height <=80)
set.seed(76)
profiles <- sample_n(profiles, 5995)

## ----cache=TRUE, warning=FALSE, message=FALSE----------------------------
profiles <- mutate(profiles, is.female = ifelse(sex=="f", 1, 0))
base.plot <- ggplot(data=profiles, aes(x=height, y=is.female)) +
  scale_y_continuous(breaks=0:1) +
  theme(panel.grid.minor.y = element_blank()) +
  xlab("Height in inches") +
  ylab("Is female?")

# Female indicator vs height
base.plot + geom_point()

# Female indicator vs height (jittered)
base.plot + geom_jitter(position = position_jitter(width = .2, height=.2))

linear.model <- lm(is.female ~ height, data=profiles)
msummary(linear.model)
b1 <- coef(linear.model)
b1

logistic.model <- glm(is.female ~ height, family=binomial, data=profiles)
msummary(logistic.model)
b2 <- coefficients(logistic.model)
b2

# Predicted linear (red) and logistic (blue) regression curves
inverse.logit <- function(x, b){
  linear.equation <- b[1] + b[2]*x
  1/(1+exp(-linear.equation))
}
base.plot + geom_jitter(position = position_jitter(width = .2, height=.2)) +
  geom_abline(intercept=b1[1], slope=b1[2], col="red", size=2) +
  stat_function(fun = inverse.logit, args=list(b=b2), color="blue", size=2)

# Fitted probabilities of being female and decision threshold (in red)
profiles$p.hat <- fitted(logistic.model)
ggplot(data=profiles, aes(x=p.hat)) +
  geom_histogram(binwidth=0.1) +
  xlab(expression(hat(p))) +
  ylab("Frequency") +
  xlim(c(0,1)) +
  geom_vline(xintercept=0.5, col="red", size=1.2)
profiles <- mutate(profiles, predicted.female = p.hat >= 0.5)
tally( ~ is.female + predicted.female, data=profiles)

# Compute misclassification error rate
perf.table <- table(truth=profiles$is.female, prediction=profiles$predicted.female)
misclass.error <- 1 - sum(diag(perf.table))/sum(perf.table)

perf.table

misclass.error

dev.off()
