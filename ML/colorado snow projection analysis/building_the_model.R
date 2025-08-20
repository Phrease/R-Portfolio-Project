## ---------------------------------
## Step 0: Load Libraries
## ---------------------------------

library(tidymodels)
library(ranger)
library(glue)

# We'll be using our 'model_ready_data' to train our AI
# Let's read in our 'model_ready_data' data frame
model_ready_data <- readRDS(file = "model_ready_data.rds")


## -------------------------------------------------------------------
## Step 3.1: Split Data into Training and Testing Set
## -------------------------------------------------------------------
# We need to hold back some data to test our model on later.
# Setting a seed ensures that this split is reproducible
set.seed(123)

# We'll put 80% of the data into training and 20% into testing.
# `strata` ensures both sets have a similar distribution of first snow days.
data_split <- initial_split(model_ready_data, prop = 0.80, strata = day_of_first_snow)

# Create the two new data frames
train_data <- training(data_split)
test_data <- testing(data_split)


## -------------------------------------------------------------------
## Step 3.2: Define the Model Specification
## -------------------------------------------------------------------
# We'll use a Random Forest model. It's powerful and great for this problem.
# We need to tell tidymodels we're doing a regression (predicting a number).
# The "engine" is the underlying package that does the work ('ranger' is a fast one)
rd_model <- rand_forest(mode = "regression") %>%
  set_engine("ranger")


## -------------------------------------------------------------------
## Step 3.3: Create the WorkFlow
## -------------------------------------------------------------------
# A workflow bundles our model and our formula together.
# The formula 'day_of_first_snow ~ .' means "predict day_of_first_snow"
# using every other column as a predictor variable."
rf_workflow <- workflow() %>%
  add_model(rd_model) %>%
  add_formula(day_of_first_snow ~ .)


## -------------------------------------------------------------------
## Step 3.4: Train the Model
## -------------------------------------------------------------------
# This is the "magic" step where the model learns from the training data.
# We 'fit' the workflow to our training data.
rf_fit <- fit(rf_workflow, data = train_data)

# We can print the fitted object to see the results
print(rf_fit)

## -------------------------------------------------------------------
## Step 3.5: Evaluate Performance on the Test Set
## -------------------------------------------------------------------
# Now, let's see how well our trained model does on the unseen test data.

# First, get the model's predictions for the test data
predictions <- predict(rf_fit, new_data = test_data)

# Combine the actual values from the test set with our model's predictions
results_df <- bind_cols(test_data, predictions) %>%
  rename(predicted_snow_day = .pred) # Give the prediction column a nice name

# Calculate our performance metrics. How good were the predictions?
metrics <- metrics(results_df, truth = day_of_first_snow, estimate = predicted_snow_day)

# Let's checks the metrics
print(metrics)

## -------------------------------------------------------------------
## Step 4: Summarize our Findings
## -------------------------------------------------------------------
# Isolate the MAE value from our metrics table
mae_value <- metrics %>%
  filter(.metric == "mae") %>%
  pull(.estimate) # pull() extracts the single value

# Isolate the R-squared value
rsq_value <- metrics %>%
  filter(.metric == "rsq") %>%
  pull(.estimate)

# Create the summary text using glue()
# We can format numbers right inside the brackets ;)
summary_text <- glue(
  "--- Model Results Summary ---\n",
  "On average, the model's prediction was off by {round(mae_value, 1)} days (MAE).\n",
  "The model's features explain only {round(rsq_value * 100, 1)}% of the variation in the first snow date (R-squared).\n"
)

# Print the final summary to the console
cat(summary_text)