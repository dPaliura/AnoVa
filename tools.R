# Function to get sum of squares of elements
# @values - vector, matrix or single numeric value to be
# sumarized after been squared each value of it
sum_sq <- function(values) sum(values^2)

# Cochran's G-criteria
# @signlev - significance level, e.g. 0.95
# @n - number of data points per data series
# @N - number of data series that remain in the data set
G <- function(signlev, n, N) {
	alph <- 1-signlev
	1/(1 + (N-1)/qf(alph/N, n-1, (N-1)*(n-1), lower.tail=FALSE))
}

# New Line character constant as abbreviation
NL <- "\n"
# New Line twice constant
NL2 <- "\n\n"