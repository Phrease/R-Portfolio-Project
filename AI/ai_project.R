# Load the Gemini library
library(gemini.R)
library(httr)
library(base64enc)

## ---------------------------------------------------------------------------
## Step 1: Generate text from a simple prompt
## ---------------------------------------------------------------------------

# Define a text prompt. We can ask a question, request a summary, or ask for code.
text_prompt <- "Write a short, uplifting poem about data science."

# Send the prompt to the Gemini Model.
# The gemini() function hundles the API call.
# It will automatically use the API key from my. Renviron file.
cat("--- Sending Text Promp ---\n")
text_response <- gemini(prompt = text_prompt)

# Print the generated text to the console.
# The `cat()` function provides a clean, formatted output.
cat("Gemini's Response:\n")
cat(text_response)

## ---------------------------------------------------------------------------
## Example 2: Analyze an Image (Multimodal Prompt)
## ---------------------------------------------------------------------------

cat("\n\n--- Sending Image Prompt using httr ---\n")

# Import API Key from .env file
api_key <- Sys.getenv("GEMINI_API_KEY")

# The URL for the Gemini Pro Vision model API endpoint
vision_api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash-latest:generateContent?key=", api_key)

# The url of the image we want to analyze
image_url <- "https://images.pexels.com/photos/45201/kitty-cat-kitten-pet-45201.jpeg"

# The text prompt for the image
image_prompt <- "What is in this image? Describe the main subject and its surroundings."

# Use httr::GET() for a more reliable download, as download.file() can fail
# on servers with certain protections (like Wikimedia).
image_response_download <- httr::GET(
  image_url,
  add_headers('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36')
)
base64_image <- ""

if (status_code(image_response_download) == 200) {
  # Get the raw image data and encode it directly to base64
  image_raw <- content(image_response_download, "raw")
  base64_image <- base64encode(image_raw)
} else {
  cat("Failed to download the image. Status:", status_code(image_response_download), "\n")
}

# Create the request body in the specific JSON format the API needs
request_body <- list(
  contents = list(
    list(
      parts = list(
        list(text = image_prompt),
        list(inline_data = list(
          mime_type = "image/jpeg",
          data = base64_image
        ))
      )
    )
  )
)

# Make the POST request to the API, only if the image was downloaded successfully
if (base64_image != "") {
  response <- POST(
    url = vision_api_url,
    body = request_body,
    encode = "json",
    add_headers(`Content-Type` = "application/json")
  )

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Extract and print the text content from the response
    response_content <- content(response, "parsed")
    generated_text <- response_content$candidates[[1]]$content$parts[[1]]$text
    cat("Gemini's Image Analysis:\n")
    cat(generated_text, "\n")
  } else {
    # Print an error message if something went wrong
    cat("Error:", status_code(response), "\n")
    cat(content(response, "text"), "\n")
  }
}















