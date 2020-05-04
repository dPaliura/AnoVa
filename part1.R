group_av <- mapply(function(j) mean(mtrx[,j]), 1:m)
total_av <- mean(mtrx)
# bg - between groups, var_bg - variance between groups
var_bg <- sum_sq(group_av - total_av)/m

sigma_gr_sq <- mapply(
					function(j) sum_sq(mtrx[,j]-group_av[j]),
					1:m)/N
# wg - within groups, var_wg - variance within all groups
var_wg <- sum(sigma_gr_sq)
# var_total - variance for all elements of all groups
var_total <- var(c(mtrx))


cat("Total variance is ", var_total,
	NL,
	"Sum Within Group and Between Group variances (accordingly):",
	NL,
	var_wg, " + ", var_bg, " = ", 
	var_wg + var_bg,
	NL2,
	"Absolute difference between total variance and said sum: ",
	NL,
	abs(var_total - (var_wg+var_bg)),
	NL,
	sep='')