# Vector of average values of samples
x_i_av <- mapply(
	function(i) mean(mtrx[,i]),
	1:m)

# Variances of samples
S_sq_i <- mapply(
	function(i) sum_sq(mtrx[,i]-x_i_av[i]),
	1:m)/(n-1)

# Cochran's maximum empirical value to be compared with critical
G_emp <- max(S_sq_i)/sum(S_sq_i)

# Cochran's critical point
G_theor <- G(sign_lev, n, m)

# Conclusion about H0 hypothesis
conclusion <- G_emp < G_theor 


cat("G = ", G_emp,
	ifelse(
		test = conclusion,
		yes  = " < ",
		no   = " >= "),
	"G","(",n,",",m,") = ", G_theor,
	" (significance level is ", sign_lev, ")",
	NL,
	"where G is Cochran's criteria",
	NL,
	sep='')

cat("H0 hypothesis about stochastic difference of variances is",
	ifelse(
		test = conclusion,
		yes  = "ACCEPTED",
		no   = "REJECTED"),
	NL2)

cat("Conclusion:",
	NL,
	"Factor A is ",
	ifelse(
		test = conclusion,
		yes  = "NOT RELEVANT",
		no   = "RELEVANT"),
	NL,
	sep='')