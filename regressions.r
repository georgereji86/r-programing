library(ggplot2)
library(lattice)

simple_linear <- function(){
  data <- read.csv("/home/ds-ds-26/george/height.csv")  # Updated to use height.csv
  print(head(data))
  simple <- lm(formula = Weight ~ Height, data = data)
  
  predictions <- predict(simple, newdata = data)
  mse <- mean((data$Weight - predictions)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(data$Weight - predictions))  # Added Absolute Mean Error
  print(paste("\nMean Squared Error (MSE):", mse))
  print(paste("Root Mean Squared Error (RMSE):", rmse))
  print(paste("Absolute Mean Error (MAE):", mae))
  
  test_X <- as.numeric(readline("Enter the test data Height: "))
  prediction <- predict(simple, newdata = data.frame(Height = test_X))
  print(paste("Predicted Weight for Height", test_X, ":", prediction))
  
  suppressMessages(print(ggplot(data, aes(x = Height, y = Weight)) +
                           geom_point(color = "blue") +
                           geom_smooth(method = "lm", col = "red") +
                           labs(title = "GGPlot - Simple Linear Regression: Weight vs Height", x = "Height", y = "Weight") +
                           theme_minimal()))
  
  print(xyplot(Weight ~ Height, data = data, type = c("p", "r"),
               main = "Lattice Plot - Simple Linear Regression: Weight vs Height",
               xlab = "Height", ylab = "Weight",
               col = "blue", lwd = 2))
}

multiple_linear <- function() {
  data <- read.csv("/home/ds-ds-26/aslam/student.csv")
  
  data$Extracurricular.Activities <- ifelse(data$Extracurricular.Activities == "Yes", 1, 0)
  
  model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = data)
  summary(model)
  
  data$predicted <- predict(model, data)
  
  print(ggplot(data, aes(x = Performance.Index, y = predicted)) +
          geom_point(color = "blue") +
          geom_abline(intercept = 0, slope = 1, color = "red") +
          labs(title = "Actual vs Predicted Values",
               x = "Actual Performance Index",
               y = "Predicted Performance Index") +
          theme_minimal())
  
  print(xyplot(predicted ~ Performance.Index, data = data,
               type = c("p", "r"),
               col.line = "red",
               main = "Actual vs Predicted Values",
               xlab = "Actual Performance Index",
               ylab = "Predicted Performance Index"))
  
  data$residuals <- residuals(model)
  print(ggplot(data, aes(x = predicted, y = residuals)) +
          geom_point(color = "blue") +
          geom_hline(yintercept = 0, color = "red") +
          labs(title = "Residuals vs Predicted Values",
               x = "Predicted Performance Index",
               y = "Residuals") +
          theme_minimal())
  
  mse <- mean(data$residuals^2)
  amse <- mse / var(data$Performance.Index)
  print(paste("Mean Squared Error (MSE):", mse))
  print(paste("Adjusted Mean Squared Error (AMSE):", amse))
  
  # User input for prediction
  hours_studied <- as.numeric(readline("Enter Hours Studied: "))
  previous_scores <- as.numeric(readline("Enter Previous Scores: "))
  extracurricular_activities <- readline("Extracurricular Activities (Yes/No): ")
  extracurricular_activities <- ifelse(tolower(extracurricular_activities) == "yes", 1, 0)
  sleep_hours <- as.numeric(readline("Enter Sleep Hours: "))
  sample_question_papers_practiced <- as.numeric(readline("Enter Sample Question Papers Practiced: "))
  
  new_data <- data.frame(Hours.Studied = hours_studied,
                         Previous.Scores = previous_scores,
                         Extracurricular.Activities = extracurricular_activities,
                         Sleep.Hours = sleep_hours,
                         Sample.Question.Papers.Practiced = sample_question_papers_practiced)
  
  predicted_value <- predict(model, new_data)
  print(paste("Predicted Performance Index for input data:", predicted_value))
}

menu <- function() {
  repeat {
    cat("\n\n1: Simple Linear Regression\n")
    cat("2: Multiple Linear Regression\n")
    cat("3: Exit\n")
    
    ch <- readline("Enter the choice: ")
    
    if (ch == "1") {
      simple_linear()
    } else if (ch == "2") {
      multiple_linear()
    } else if (ch == "3") {
      cat("Exiting the program\n")
      break
    } else {
      cat("Invalid choice, please enter again.\n")
    }
  }
}
menu()
