# parallel computing using parLapply()


# remove na in a vector
x <- c(1:4, NA, 6) %>% print()

is.na(x)
x[!is.na(x)]

na.rm <- F
if(na.rm){x <- x[!is.na(x)]}

# check if iris data set has any NA
any(is.na(iris))
str(iris)


# which()
direction <- "left"
test_type <- which(direction == c("left", "right", "two-sided"))
tails <- c(1, 1, 2)[test_type]


# t.test() 
t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, ...)

qt(p, df, ncp, lower.tail = TRUE, log.p = FALSE)

## t quantile
qt(1-0.05/2, 149)
qt(0.05/2, 149, lower.tail = F)


# example 1
x <- iris$Sepal.Length
mu <- 5.5

t_test(x, mu) %>% ifelse("Reject H0", "Fail to reject H0")

oneSampleTTest(x, mu)
t.test(x, mu = mu)


length(x)
mean(x)
sd(x)

(mean(x) - mu) / ( sd(x)/sqrt(length(x)) )

t_score(x, mu)


# example 2
x <- iris$Sepal.Width
mu <- 3.5
direction = "right"

t_test(x, mu, direction) %>% ifelse("Reject H0", "Fail to reject H0")

oneSampleTTest(x, mu, one.sided = "greater")
t.test(x, mu = mu, alternative = "greater")

# string subset for test name
str <- "left"
str <- "two-sided"

str %>% paste0("-sided") %>% str_sub(end = str_locate(., "-sided")[2])



# gamma dist
replicate(10000, t_test(rgamma(10, shape =2, rate = 1), mu = 2*1, verbose = F)) %>% mean()


# list of parameter
sample_sizes <- c(10, 20, 30)
shape_values <- c(0.5, 1)
rate_values <- c(1)

X <- list(
  list(n = 10, shape = 0.5, rate = 1),
  list(n = 10, shape = 1, rate = 1),
  list(n = 20, shape = 0.5, rate = 1),
  list(n = 20, shape = 1, rate = 1)
)

length(X)
typeof(X)
length(X[[1]])
typeof(X[[1]])
X
X[[1]]$n
X[[1]]$shape


X2 <- expand_grid(sample_sizes, shape_values, rate_values) %>% 
  `names<-`(c("n", "shape", "rate")) %>% 
  split(., 1:nrow(.)) %>% 
  unname() %>% 
  lapply(as.list)

str(X2)
X2[1:4]



getResults <- function(X){
  replicate(10000, t_test(rgamma(X$n, X$shape, rate = 1), mu =  shape * 1, verbose = F)) %>% 
  mean()
}

results <- lapply(X, getResults)
results2 <- lapply(X2, getResults)

results[[2]]



# parallelization
parLapply()
