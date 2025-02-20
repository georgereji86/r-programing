menu_program <- function() {
  repeat {
    cat("\n--- MENU ---\n")
    cat("1. Merge Two Datasets\n")
    cat("2. Row-Wise Palindromes\n")
    cat("3. Max and Min of Rows and Columns\n")
    cat("4. Factorial of Fibonacci \n")
    cat("5. Exit\n")
    choice <- as.integer(readline(prompt = "Enter your choice: "))
    
    if (choice == 1) {
      cat("\n--- Merge Two Datasets ---\n")
      df1 <- data.frame(ID = c(1, 2, 3), Value1 = c(10, 20, 30))
      df2 <- data.frame(Rollno = c(1, 4, 5), Value2 = c(100, 200, 400))
      merged_df <- merge(df1, df2, by.x = "ID", by.y = "Rollno", all.x = TRUE, all.y = TRUE)
      
      print("Merged Dataset:")
      print(merged_df)
      
    } else if (choice == 2) {
      cat("\n--- Find Row-Wise Palindromes ---\n")
      
      find_rowwise_palindromes <- function(data_frame, output_file = "/home/ds-ds-26/george/pallindrome.csv") {
        is_row_palindrome <- function(row) {
          row_string <- paste(row, collapse = "")
          return(row_string == paste(rev(strsplit(row_string, NULL)[[1]]), collapse = ""))
        }
        
        palindromic_rows <- data.frame()
        for (i in 1:nrow(data_frame)) {
          row <- as.character(data_frame[i, ])
          if (is_row_palindrome(row)) {
            palindromic_rows <- rbind(palindromic_rows, data_frame[i, ])
          }
        }
        
        if (nrow(palindromic_rows) > 0) {
          write.csv(palindromic_rows, output_file)
          cat("rows written to:", output_file, "\n")
          print(palindromic_rows)
        } else {
          cat("No palindromic rows found.\n")
        }
      }
      
      data <- data.frame(
        Col1 = c("a", "b", "c", "r"),
        Col2 = c("b", "e", "b", "a"),
        Col3 = c("a", "d", "c", "c"),
        stringsAsFactors = FALSE
      )
      print("Input Data Frame:")
      print(data)
      find_rowwise_palindromes(data)
      
    } else if (choice == 3) {
      cat("\n--- Find Max and Min of Rows and Columns using apply() ---\n")
      df <- data.frame(A = c(5, 8, 3), B = c(2, 4, 7), C = c(6, 9, 1))
      row_max <- apply(df, 1, max)
      row_min <- apply(df, 1, min)
      col_max <- apply(df, 2, max)
      col_min <- apply(df, 2, min)
      print(df)
      cat("Row-wise Maximum:\n")
      print(row_max)
      cat("Row-wise Minimum:\n")
      print(row_min)
      cat("Column-wise Maximum:\n")
      print(col_max)
      cat("Column-wise Minimum:\n")
      print(col_min)
      
    } else if (choice == 4) {
      cat("\n--- Find Factorial of Fibonacci Numbers in a Data Frame ---\n")
      
      find_factorial_of_fibonacci <- function(data_frame) {
        is_fibonacci <- function(n, fib_seq) {
          return(n %in% fib_seq)
        }
        
        
        factorial_calc <- function(n) {
          if (n == 0 || n == 1) return(1)
          return(prod(1:n))
        }
        
        
        generate_fibonacci <- function(limit) {
          fib_seq <- c(0, 1)
          while (tail(fib_seq, 1) < limit) {
            fib_seq <- c(fib_seq, sum(tail(fib_seq, 2)))
          }
          return(fib_seq[fib_seq <= limit])
        }
        
        
        numeric_values <- as.numeric(unlist(data_frame))
        max_value <- max(numeric_values)
        
        fibonacci_numbers <- generate_fibonacci(max_value)
        
        
        fib_numbers_in_df <- numeric_values[numeric_values %in% fibonacci_numbers]
        
        
        if (length(fib_numbers_in_df) > 0) {
          factorials <- sapply(fib_numbers_in_df, factorial_calc)
          result <- data.frame(Fibonacci_Number = fib_numbers_in_df, Factorial = factorials)
          print("Factorials of Fibonacci Numbers in Data Frame:")
          print(result)
        } else {
          print("No Fibonacci numbers found.")
        }
      }
      
      
      df <- data.frame(A = c(2, 11, 6), B = c(1, 4, 5))
      
      print("Input Data Frame:")
      print(df)
      find_factorial_of_fibonacci(df)
      
    } else if (choice == 5) {
      cat("Exiting the program.\n")
      break
      
    } else {
      cat("Invalid choice!\n")
    }
  }
}

menu_program()

