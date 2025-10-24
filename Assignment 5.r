# 1. Create 8x8 matrix with random integers between 50 and 300
print("K.Nichal haas RegNo:22BCE9651")
set.seed(123)  # for reproducibility
A <- matrix(sample(50:300, 64, replace = TRUE), nrow = 8, ncol = 8)
A

# i. Standard deviation, mean, variance
std_dev <- sd(A)
mean_val <- mean(A)
var_val <- var(as.vector(A))

cat("Standard Deviation:", std_dev, "\n")
cat("Mean:", mean_val, "\n")
cat("Variance:", var_val, "\n\n")

# ii. Column sum, row sum, diagonal sum, anti-diagonal sum
col_sum <- colSums(A)
row_sum <- rowSums(A)
diag_sum <- sum(diag(A))
anti_diag_sum <- sum(diag(apply(A, 2, rev)))  # reverse columns for anti-diagonal

cat("Column Sum:", col_sum, "\n")
cat("Row Sum:", row_sum, "\n")
cat("Diagonal Sum:", diag_sum, "\n")
cat("Anti-Diagonal Sum:", anti_diag_sum, "\n\n")

# iii. Extract 3×3 submatrix from center
sub_matrix <- A[3:5, 3:5]
cat("3x3 Sub-matrix from center:\n")
print(sub_matrix)

# iv. Replace first row with -1
A[1, ] <- -1
cat("\nMatrix after replacing first row with -1:\n")
print(A)

# v. Index of min and max value
min_index <- which(A == min(A), arr.ind = TRUE)
max_index <- which(A == max(A), arr.ind = TRUE)

cat("\nMin Value:", min(A), "at position", min_index, "\n")
cat("Max Value:", max(A), "at position", max_index, "\n")

# Generate even numbers between 20 and 100
print("K.Nichal haas RegNo:22BCE9651")
even_nums <- seq(20, 100, by = 2)

# Create two 4x4 matrices
B <- matrix(sample(even_nums, 16, replace = TRUE), nrow = 4)
C <- matrix(sample(even_nums, 16, replace = TRUE), nrow = 4)

cat("Matrix B:\n"); print(B)
cat("Matrix C:\n"); print(C)

# i. Element-wise multiplication
elem_mult <- B * C
cat("\nElement-wise multiplication:\n"); print(elem_mult)

# ii. Dot product (matrix multiplication)
dot_product <- B %*% C
cat("\nDot product (B %*% C):\n"); print(dot_product)

#iii. Replace all values greater than 50 in the second array with 99
C[C > 50] <- 99
cat("\nMatrix C after replacing values > 50 with 99:\n"); print(C)

# iv. Concatenate vertically and split into equal parts
combined <- rbind(B, C)
cat("\nCombined matrix:\n"); print(combined)

print("K.Nichal haas RegNo:22BCE9651")
#3. Create a structured NumPy array to store employee records with attributes: Employee_ID (integer), Name (string, max length 15), Department (string, max length 10), and Salary (float).
# Create structured employee data frame
employees <- data.frame(
  Employee_ID = c(101, 102, 103, 104),
  Name = c("Alice", "Bob", "Charlie", "David"),
  Department = c("HR", "IT", "Finance", "IT_Manager"),
  Salary = c(70000, 80000, 65000, 90000)
)
cat("Original Employee Data:\n")
print(employees)

# i. Add new employee record
new_emp <- data.frame(Employee_ID = 105, Name = "Eve", Department = "Manager", Salary = 95000)
employees <- rbind(employees, new_emp)
cat("\nAfter adding new employee:\n")
print(employees)

# ii. Extract employees with salary > 75,000
high_salary <- subset(employees, Salary > 75000)
cat("\nEmployees with Salary > 75,000:\n")
print(high_salary)

# iii. Sort by Employee_ID (ascending)
sorted_emp <- employees[order(employees$Employee_ID), ]
cat("\nSorted by Employee_ID:\n")
print(sorted_emp)

# iv. Increase salary of IT department employees by 10%
employees$Salary[employees$Department == "IT"] <- 
  employees$Salary[employees$Department == "IT"] * 1.10
cat("\nAfter 10% raise for IT department:\n")
print(employees)

# v. Retrieve names of employees who have 'Manager' in their department
manager_names <- employees$Name[grepl("Manager", employees$Department)]
cat("\nEmployees with 'Manager' in Department:\n")
print(manager_names)