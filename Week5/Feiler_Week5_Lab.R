library(MASS)

# Data with No Correlation ----------------------------------------------------------

data <- mvrnorm(n = 100,
                c(1, 2),
                matrix(c(3, 0, 0, 3),
                       2,
                       2)
                )

sample.a <- data[,1]
sample.b <- data[,2]

plot(sample.a, sample.b)

t <- t.test(sample.a, sample.b)

# Checkpoint 1 ----------------------------------------------------------------------

# A
a <- list(mean(sample.a), 
          sd(sample.a), 
          length(sample.a),
          var(sample.a)
)
names(a) <- c("mean", "sd", "n", "var")

# B
b <- list(mean(sample.b), 
          sd(sample.b), 
          length(sample.b),
          var(sample.b)
)
names(b) <- c("mean", "sd", "n", "var")

# Create list for hypothesis testing results
HypTest <- vector("list", length = 4)
names(HypTest) <- c("stErr", "t", "dof", "p")

# Calculate standard error of the means
HypTest$stErr <- sqrt(((a$var)/a$n)+((b$var)/b$n))

# Calculate the test statistic
HypTest$t <- (a$mean-b$mean)/HypTest$stErr

# Calculate the degrees of freedom
top <- ((a$var/a$n)+(b$var/b$n))^2
bottom1 <- ((a$var/a$n)^2)/(a$n-1)
bottom2 <- ((b$var/b$n)^2)/(b$n-1)

HypTest$dof <- top/(bottom1 + bottom2)

# Find the p value
HypTest$p <- 2*pt(HypTest$t, 
                  HypTest$dof
                  )

# Calculate the 95% confidence interval 
CI <- vector("list", length = 2)
names(CI) <- c("LL", "UL")

CI$LL <- a$mean-b$mean-qt(0.975, HypTest$dof)*HypTest$stErr
CI$UL <- a$mean-b$mean+qt(0.975, HypTest$dof)*HypTest$stErr

rm(top, bottom1, bottom2)
# -----------------------------------------------------------------------------------

# One-sided test
t.test(sample.a, sample,b, alternative = "greater")
t.test(sample.a, sample.b, alternative = "less")

# Checkpoint 2 ----------------------------------------------------------------------
# Smaller sample means a wider variance, a larger sample means a narrower variance
# -----------------------------------------------------------------------------------


# Data with Correlation -------------------------------------------------------------

data <- mvrnorm(n = 100,
                c(1, 1.2),
                matrix(c(3, 2, 2, 3),
                       2,
                       2)
                )

sample.a <- data[,1]
sample.b <- data[,2]

plot(sample.a, sample.b)

t.test(sample.a, sample.b)
t.test(sample.a, sample.b, paired = FALSE)
# Same values because default is to assume unpaired

# Convince ourselves of the formulas we learned for paired t-tests
z <- sample.a-sample.b

var(z); var(sample.a)+var(sample.b)-2*cov(sample.a,sample.b)
# Same! Exciting

# Checkpoint 3 ----------------------------------------------------------------------
# What happens to the t-test results when you make the correlation weak?

data <- mvrnorm(n = 100,
                c(1, 1.2),
                matrix(c(3, 0.1, 0.1, 3),
                       2,
                       2)
                )

sample.a <- data[,1]
sample.b <- data[,2]

plot(sample.a, sample.b)

t.test(sample.a, sample.b)

# -----------------------------------------------------------------------------------

# F-test

sample.a <- rnorm(100,
                  mean = 5,
                  sd = 4)
sample.b <- rnorm(100,
                  mean = 2,
                  sd = 3)

var.A <- var(sample.a)
var.B <- var(sample.b)

F.ratio <- var.A/var.B

qf(p = 0.95, 
   df1 = 99,
   df2 = 99)


# Checkpoint 4 ----------------------------------------------------------------------
# Do you understand why df1 = 99 and df2 = 99?
# Yes, because the variance was calculated, meaning we owe the Statistics Gods one 
# tribute, so df = n-1.
# -----------------------------------------------------------------------------------
