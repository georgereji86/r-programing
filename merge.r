display_menu <- function() {
  cat("Menu:\n")
  cat("1. Merge\n")
  cat("2. Palindrome\n")
  cat("3. Max and Min\n")
  cat("4. Factorial of Fibonacci\n")
  cat("5. Exit\n")
}

merge1 <- function() {
  
  df1 <- data.frame(ID = c(1, 2, 4, 5), name = c("george", "reji", "gelsa","febin"))
  df2 <- data.frame(ID = c(2, 4, 6, 8), age = c(19, 20, 21, 22))
  
  print("DataFrame 1:")
  print(df1)
  
  print("DataFrame 2:")
  print(df2)
  
  merged <- base::merge(df1, df2, by = "ID", all = TRUE) 
  print("Merged DataFrame:")
  print(merged)
}



max_and_min <- function() {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6),
    C = c(7, 8, 9)
  )
  
  print("Original DataFrame:")
  print(df)
  max_values_rows <- apply(df, 1, max)
  min_values_rows <- apply(df, 1, min)
  max_values_columns <- apply(df, 2, max)
  min_values_columns <- apply(df, 2, min)
  
  print("Maximum values for each row:")
  print(max_values_rows)
  
  print("Minimum values for each row:")
  print(min_values_rows)
  
  print("Maximum values for each column:")
  print(max_values_columns)
  
  print("Minimum values for each column:")
  print(min_values_columns)
}

is_fibonacci <- function(num) {
  a <- 0
  b <- 1
  while (a < num) {
    temp <- a
    a <- b
    b <- temp + b
  }
  return(a == num) 
}

factorial <- function(n) {
  if (n < 0) {
    return(NA)  
  } else if (n == 0 || n == 1) {
    return(1) 
  } else {
    result <- 1
    for (i in 2:n) {
      result <- result * i
    }
    return(result)
  }
}

check_fibonacci_and_factorial <- function(df) {
  for (num in df$numbers) { 
    if (is_fibonacci(num)) {
      cat("Factorial of", num, "is", factorial(num), "\n")
    }
  }
}
df <- data.frame(numbers = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 13, 21, 34, 55, 69, 77, 89))

repeat {
  display_menu()
  choice <- as.integer(readline(prompt = "Choose option from 1 to 5: "))
  
  if (choice == 1) {
    merge1()
  } else if (choice == 2) {
    palindrome()
  } else if (choice == 3) {
    max_and_min()
  } else if (choice == 4) {
    check_fibonacci_and_factorial(df)
  } else if (choice == 5) {
    cat("Exiting program.....\n")
    break  
  } else {
    cat("Invalid choice! Please select a valid option.\n")
  }
}