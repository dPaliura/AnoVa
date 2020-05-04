DS_file_name <- "kpi15.txt"

# Parameters for matrix creation
ncol <- 7
colnames <- c("time", paste("param", 1:(ncol-1), sep=""))

# Getting and preparing data to analysis
DataSet <- scan(DS_file_name)
mtrx <- matrix(DataSet, ncol=ncol, byrow=TRUE, dimnames=list(NULL, colnames))
time <- mtrx[,1]
mtrx <- mtrx[,-1]
sign_lev <- 0.95


# Parameters for ANalysis OF VAriances with SINGLE FACTOR
# n is volumes of each level samples
n <- nrow(mtrx)
# m is number of single factor levels
m <- ncol-1

# Total Amount of elements
N <- m*n


# Parameters for ANalysis Of VAriances with PAIR OF FACTORS

# m_ is number of first factor levels
m_ <- m
# k_ is number of second factor levels
k_ <- 4
# n_ is amount of elements per factors' levels intersection
n_ <- n/k_

Data_arr <- array(dim=c(k_, m_, n_))
for (i in 1:m_){ for (j in 1:k_) {
	Data_arr[j,i,] <- mtrx[((j-1)*n_+1):(j*n_),i]
	}}