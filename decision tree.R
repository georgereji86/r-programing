
library(caTools)
library(e1071)
library(ggplot2)
library(C50)


run_svm_program <- function() {

  dataset = read.csv('/home/ds-da-29/parthiv/svm.csv')
  

  dataset = dataset[, 3:5] 

  dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
  
 
  set.seed(123)
  split = sample.split(dataset$Purchased, SplitRatio = 0.75)
  training_set = subset(dataset, split == TRUE)
  test_set = subset(dataset, split == FALSE)
  

  training_set[, 1:2] = scale(training_set[, 1:2])
  test_set[, 1:2] = scale(test_set[, 1:2])
  
 
  classifier = svm(formula = Purchased ~ ., 
                   data = training_set, 
                   type = 'C-classification', 
                   kernel = 'radial')
  
 
  y_pred = predict(classifier, newdata = test_set[, -3])
  

  cm = table(test_set[, 3], y_pred)
  print("Confusion Matrix:")
  print(cm)
  

  plot_decision_boundary <- function(set, title) {
    X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
    X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
    
    grid_set = expand.grid(X1, X2)
    colnames(grid_set) = c('Age', 'EstimatedSalary')
    y_grid = predict(classifier, newdata = grid_set)
    plot(set[, -3], 
         main = title, 
         xlab = 'Age', ylab = 'Estimated Salary', 
         xlim = range(X1), ylim = range(X2))
    
    contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
    
    points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'yellow', 'aquamarine'))
    
    points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
  }
  

  plot_decision_boundary(training_set, "SVM with RBF Kernel (Training set)")
  

  plot_decision_boundary(test_set, "SVM with RBF Kernel (Test set)")
  

  pred1 <- function() {
   
    new_age = as.numeric(readline(prompt = "Enter Age: "))
    new_salary = as.numeric(readline(prompt = "Enter Estimated Salary: "))
    
  
    if (is.na(new_age) | is.na(new_salary)) {
      stop("Invalid input! Please enter numeric values.")
    }
    
   
    new_data = data.frame(Age = (new_age - mean(dataset$Age)) / sd(dataset$Age),
                          EstimatedSalary = (new_salary - mean(dataset$EstimatedSalary)) / sd(dataset$EstimatedSalary))
    
    
    prediction = predict(classifier, newdata = new_data)
    print(prediction)
    
  
    if (prediction == 1) {
      cat("\nPrediction: The person is likely to Purchase.\n")
    } else {
      cat("\nPrediction: The person is NOT likely to Purchase.\n")
    }
  }
  
 
  pred1()
}


run_decision_tree_program <- function() {
  
  play_tennis_data <- read.csv("/home/ds-da-29/parthiv/tennis.csv")
  
  
  play_tennis_data$Outlook <- factor(play_tennis_data$Outlook, levels = c("Sunny", "Overcast", "Rain"))
  play_tennis_data$Temperature <- factor(play_tennis_data$Temperature, levels = c("Hot", "Mild", "Cool"))
  play_tennis_data$Humidity <- factor(play_tennis_data$Humidity, levels = c("High", "Normal"))
  play_tennis_data$Wind <- factor(play_tennis_data$Wind, levels = c("Weak", "Strong"))
  play_tennis_data$PlayTennis <- factor(play_tennis_data$PlayTennis, levels = c("No", "Yes"))
  

  model <- C5.0(PlayTennis ~ Outlook + Temperature + Humidity + Wind, data = play_tennis_data)
  
 
  get_user_input <- function() {
    cat("Enter the following options:\n")
    
    cat("1. Outlook (Sunny, Overcast, Rain): ")
    outlook <- readline()
    
    cat("2. Temperature (Hot, Mild, Cool): ")
    temperature <- readline()
    
    cat("3. Humidity (High, Normal): ")
    humidity <- readline()
    
    cat("4. Wind (Weak, Strong): ")
    wind <- readline()
    
    return(data.frame(
      Outlook = factor(outlook, levels = c("Sunny", "Overcast", "Rain")),
      Temperature = factor(temperature, levels = c("Hot", "Mild", "Cool")),
      Humidity = factor(humidity, levels = c("High", "Normal")),
      Wind = factor(wind, levels = c("Weak", "Strong"))
    ))
  }
  
  
  plot(model, main = "Decision Tree Visualization", type = "simple")
  

  cat("Provide input values for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class:", prediction, "\n"))
}


main_menu <- function() {
  while (TRUE) {
    cat("\n--- Main Menu ---\n")
    cat("1. Run SVM Program\n")
    cat("2. Run Decision Tree Program\n")
    cat("3. Exit\n")
    choice <- as.numeric(readline(prompt = "Enter your choice (1, 2, or 3): "))
    
    if (choice == 1) {
      run_svm_program()
    } else if (choice == 2) {
      run_decision_tree_program()
    } else if (choice == 3) {
      cat("Exiting the program. Goodbye!\n")
      break
    } else {
      cat("Invalid choice! Please try again.\n")
    }
  }
}

main_menu() 