# NA values ----
set.seed(11991)                              # Set seed
N <- 1000                                    # Sample size

x_num <- round(rnorm(N, 0, 5))               # Random Rounded Numeric (observation, mean, sd)
x_fac <- as.factor(round(runif(N, 0, 3)))    # Random Levels as Factors (observation, min range, max range) 
x_cha <- sample(letters, N, replace = TRUE)  # Random From Data of Characters (data set, observations, should data points be replaced after use?)

# letters is built in data set of 26 alphabets

x_num[rbinom(N, 1, 0.2) == 1] <- NA          # 20% missings
x_fac[rbinom(N, 1, 0.3) == 1] <- NA          # 30% missings
x_cha[rbinom(N, 1, 0.05) == 1] <- NA         # 5% missings

data <- data.frame(x_num, x_fac, x_cha,      # Create data frame
                   stringsAsFactors = FALSE)

head(data, 5)                                   # First 5 rows of data

# Matrix of missing values 
is.na(data)

# Is there any missing values?
any(is.na(data))
