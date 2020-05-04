# Average values of each cell of Data
averages_matrix <- mapply(
					function(i) mapply(
						function(j) mean(Data_arr[j,i,]),
						1:k_),
					1:m_)

# Sum by rows (degrees of levels of first factor)
X_i <- mapply(	function(i) sum(averages_matrix[,i]),
				1:m_)
###
#	TODO - summarize averages x_ij or all elements x_ijv?
#			where j=1:k_, i=1:m_, v=1:n_.
#			At the momemtn realised summation x_ij
###
# Sum by rows (degrees of levels of first factor)
X_j <- mapply(	function(j) sum(averages_matrix[j,]),
				1:k_)

Q1 <- sum_sq(averages_matrix)
Q2 <- sum_sq(X_i)/m_
Q3 <- sum_sq(X_j)/k_
Q4 <- (sum(X_i)^2)/(m_*k_)

# Numbers of freedom degrees for each factor
df1_A 	<- (m_-1)
df1_B 	<- (k_-1)
df2 	<- df1_A*df1_B

# variance estimates
S_sq_0 <- (Q1-Q2-Q3+Q4)/df2
S_sq_A <- (Q3-Q4)/df1_A
S_sq_B <- (Q2-Q4)/df1_B


# Empirical F value for first factor A
F_A_emp <- S_sq_A/S_sq_0
# Theoretical F value (critical point) for this factor
F_A_theor <- qf(sign_lev, df1_A, df2)


# Empirical F value for second factor B
F_B_emp <- S_sq_B/S_sq_0
# Theoretical F value (critical point) for this factor
F_B_theor <- qf(sign_lev, df1_B, df2)

# Make conclusions about relevance of factor A and factor B
conclusion_A <- F_A_emp > F_A_theor
conclusion_B <- F_B_emp > F_B_theor

# Outputing the conclusion about factor A
cat("F-value (empirical) for first factor A is:",
	NL,
	"F_A = ", F_A_emp,
	NL,
	"critical point is:",
	NL,
	"F = ", F_A_theor,
	" (significance level ", sign_lev, ")",
	NL,
	"F_A ",
	ifelse(
		test = conclusion_A,
		yes  = ">",
		no   = "<"),
	" F",
	NL2,
	sep='')

cat("Conclusion:",
	NL,
	"Factor A is ",
	ifelse(
		test = conclusion_A,
		yes  = "RELEVANT",
		no   = "NOT RELEVANT"),
	NL2,
	sep='')

# Outputing the conclusion about factor B
cat("F-value (empirical) for second factor B is:",
	NL,
	"F_B = ", F_B_emp,
	NL,
	"critical point is:",
	NL,
	"F = ", F_B_theor,
	" (significance level ", sign_lev, ")",
	NL,
	"F_B ",
	ifelse(
		test = conclusion_B,
		yes  = ">",
		no   = "<"),
	" F",
	NL2,
	sep='')

cat("Conclusion:",
	NL,
	"Factor B is ",
	ifelse(
		test = conclusion_B,
		yes  = "RELEVANT",
		no   = "NOT RELEVANT"),
	NL2,
	sep='')


# Make allowance about dependency between factor A and factor B
# Let's analyse interaction
Q5 <- sum_sq(Data_arr)

# Numbers of freedom degrees for interaction between factors
df1_AB <- df2
df2_AB <- m_*k_*(n_-1)

# variance estimate for A-B interaction
S_sq_AB <- (Q5-n_*Q1)/df2_AB

# Empirical F value for interaction between factors A and B
F_AB_emp <- n_*S_sq_0/S_sq_AB
# Theoretical F value (critical point) for this option
F_AB_theor <- qf(sign_lev, df1_AB, df2_AB)

# Conclusion about interaction between factors
conclusion_AB <- F_AB_emp > F_AB_theor

# Outputing the conclusion about interaction between factors
cat("F-value (empirical) for interaction of factors:",
	NL,
	"F_AB = ", F_AB_emp,
	NL,
	"critical point is:",
	NL,
	"F = ", F_AB_theor,
	" (significance level ", sign_lev, ")",
	NL,
	"F_AB ",
	ifelse(
		test = conclusion_AB,
		yes  = ">",
		no   = "<"),
	" F",
	NL2,
	sep='')

cat("Conclusion:",
	NL,
	"Interaction between factors A and B is ",
	ifelse(
		test = conclusion_AB,
		yes  = "RELEVANT",
		no   = "NOT RELEVANT"),
	NL,
	sep='')