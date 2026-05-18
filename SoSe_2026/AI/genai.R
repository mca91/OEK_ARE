# AI chapter companion code for students
# Source slides: SoSe_2026/AI/AI.Rmd
#
# Run this script section by section in an interactive R session.
# Many examples require API keys, internet access, or interactive input.

# Slide: Setup Checklist
#install.packages("ellmer")
library(ellmer)
usethis::edit_r_environ()

key <- Sys.getenv("GROQ_API_KEY")
nzchar(key)
substr(key, 1, 8)




# Slide: The Minimal Pattern
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are a helpful assistant."
)

chat$chat("What is R in one sentence?")




# Exercise: One Prompt, Three Terms
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = paste(
    "You are a concise econometrics tutor.",
    "For every term the user sends, respond with exactly two lines:",
    "Line 1: a one-sentence definition.",
    "Line 2: one concrete empirical example."
  ),
  echo = "none"
)

chat$chat("heteroskedasticity")
chat$chat("endogeneity")
chat$chat("multicollinearity")




# Slide: The Chat Object Remembers
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are a helpful R tutor."
)

chat$chat("What is a data frame?")
print(chat)
token_usage(chat)




# Slide: Streaming vs. Capturing Output
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "Be concise.",
  echo = "none"
)

response <- chat$chat("What is GDP?")
cat(response)
nchar(response)




# Exercise: Capture and Inspect
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are a precise econometrics tutor.",
  echo = "none"
)

answer <- chat$chat("Define heteroskedasticity in one sentence.")
cat(answer)
nchar(answer)




# Slide: System Prompts Shape Behavior
q <- "What is OLS regression?"

chat1 <- chat_groq(
  system_prompt = "Explain like I am 5 years old.",
  echo = "none"
)
chat1$chat(q)

chat2 <- chat_groq(
  system_prompt = "Answer in exactly one sentence.",
  echo = "none"
)
chat2$chat(q)

chat3 <- chat_groq(
  system_prompt = "You are a strict econometrics professor. Be formal and precise.",
  echo = "none"
)
chat3$chat(q)




# Slide: Make Prompt Experiments Reusable
ask <- function(system_message, user_question) {
  chat <- chat_groq(
    model = "llama-3.3-70b-versatile",
    system_prompt = system_message,
    echo = "none"
  )
  chat$chat(user_question)
}

question <- "What is OLS regression?"

ask("Explain like I am 5 years old.", question)
ask("Answer in exactly one sentence.", question)
ask("You are a strict econometrics professor. Be formal and precise.", question)




# Slide: Multi-Turn Conversations
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are a helpful R tutor."
)

chat$chat("What is a factor in R?")
chat$chat("Can you give me an example?")
chat$chat("How is it different from a character vector?")

print(chat)

turns <- chat$get_turns(include_system_prompt = TRUE)
length(turns)
turns[[1]]




# Exercise: Context Matters
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are an econometrics tutor. Be concise."
)

chat$chat("What is endogeneity?")
chat$chat("Give me an example from labor economics.")
chat$chat("How would I test for it?")

print(chat)
turns <- chat$get_turns(include_system_prompt = TRUE)
length(turns)




# Slide: Interactive Chat
# Run interactively only.
chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are a friendly R tutor."
)

live_console(chat)

# Browser version; run interactively only.
install.packages("shinychat")

chat <- chat_groq(
  model = "llama-3.3-70b-versatile",
  system_prompt = "You are a data science assistant."
)

live_browser(chat)




# Slide: Document Scan to Markdown
cwd <- getwd()
document_path <- paste0(cwd, "/SoSe_2026/AI/med_invoice.webp")

chat <- chat_groq(
  model = "meta-llama/llama-4-scout-17b-16e-instruct",
  system_prompt = paste(
    "You are a careful OCR assistant.",
    "Extract the visible document content and return only Markdown.",
    "Preserve headings, paragraphs, lists, tables, and signatures.",
    "Mark unreadable text as [illegible]."
  ),
  echo = "none",
  params = params(temperature = 0)
)

markdown_doc <- chat$chat(
  "Convert this scanned document into clean Markdown and translate to English.",
  content_image_file(document_path, resize = "high")
)

cat(markdown_doc)




# Exercise: Prompt the OCR
ocr_prompt <- paste(
  "You are a careful OCR assistant.",
  "Return only Markdown.",
  "Preserve headings, paragraphs, lists, and tables.",
  "Mark uncertain text as [unclear].",
  "End with a section named ## Extraction Notes that briefly lists any quality issues."
)

chat <- chat_groq(
  model = "meta-llama/llama-4-scout-17b-16e-instruct",
  system_prompt = ocr_prompt,
  echo = "none",
  params = params(temperature = 0)
)

# Slide: Temperature
low <- chat_groq(
  echo = "none",
  params = params(temperature = 0)
)
low$chat("Write a haiku about coding.")
low$chat("Write a haiku about coding.")

high <- chat_groq(
  echo = "none",
  params = params(temperature = 1.2)
)
high$chat("Write a haiku about coding.")
high$chat("Write a haiku about coding.")




# Exercise: Stable Study Output
prompt <- "Create exactly three flashcards about omitted variable bias. Format as Q: ... A: ..."

chat_low <- chat_groq(
  echo = "none",
  params = params(temperature = 0)
)

chat_high <- chat_groq(
  echo = "none",
  params = params(temperature = 1)
)

cat(chat_low$chat(prompt))
cat(chat_low$chat(prompt))

cat(chat_high$chat(prompt))
cat(chat_high$chat(prompt))




# Slide: Switching Providers
chat <- chat_groq(system_prompt = "You are a helpful assistant.")
chat <- chat_openai(system_prompt = "You are a helpful assistant.")
chat <- chat_anthropic(system_prompt = "You are a helpful assistant.")
chat <- chat_google_gemini(system_prompt = "You are a helpful assistant.")

chat <- chat_ollama(
  model = "llama3.2",
  system_prompt = "You are a helpful assistant."
)




# Slide: Summarizer Design
summarize_text <- function(text, detail = "short", audience = "students") {
  system_msg <- paste(
    "You are a study assistant.",
    "Summarize the user's text for", audience, ".",
    "Use a", detail, "summary in bullet points.",
    "Do not add facts that are not in the text."
  )

  chat <- chat_groq(system_prompt = system_msg, echo = "none")
  chat$chat(text)
}

txt <- paste(
  "Heteroskedasticity means the variance of the error term is not constant",
  "across observations. This violates one of the Gauss-Markov assumptions",
  "and makes the usual OLS standard errors unreliable."
)

cat(summarize_text(txt))




# Exercise: Add a Constraint
summarize_text <- function(text,
                           detail = "short",
                           audience = "students",
                           max_bullets = 3) {
  system_msg <- paste(
    "You are a study assistant.",
    "Summarize the user's text for", audience, ".",
    "Use a", detail, "summary.",
    "Use at most", max_bullets, "bullet points.",
    "Do not add facts that are not in the text."
  )

  chat <- chat_groq(system_prompt = system_msg, echo = "none")
  chat$chat(text)
}




# Slide: Flashcards and Writing Feedback
generate_flashcards <- function(topic, count = 5) {
  system_msg <- paste0(
    "You are a study assistant. Generate exactly ", count,
    " flashcards for the given topic. ",
    "Format each as:\nQ: [question]\nA: [concise answer]\n"
  )

  chat <- chat_groq(system_prompt = system_msg, echo = "none")
  chat$chat(paste("Create flashcards about:", topic))
}

cat(generate_flashcards("R data structures: vectors, lists, and data frames"))

focus_instructions <- list(
  clarity  = "Make the text clearer and easier to understand.",
  academic = "Make the text more formal and academic in tone.",
  concise  = "Make the text shorter while keeping the key message.",
  grammar  = "Fix grammar and spelling errors only."
)

improve_writing <- function(text, focus = "clarity") {
  focus <- match.arg(focus, names(focus_instructions))
  instruction <- focus_instructions[[focus]]
  system_msg <- paste("You are a writing tutor.", instruction)

  chat <- chat_groq(system_prompt = system_msg, echo = "none")
  chat$chat(text)
}




# Exercise: Build a Translator
translate_text <- function(text, target_language = "German") {
  system_msg <- paste(
    "You are a precise academic translator.",
    "Translate the user's text to", target_language, ".",
    "Preserve technical terms.",
    "Return only the translation."
  )

  chat <- chat_groq(system_prompt = system_msg, echo = "none")
  chat$chat(text)
}

cat(translate_text("Ordinary least squares estimates a linear conditional mean."))




# Slide: One General Helper
ask_llm <- function(user_message,
                    system_message = "You are a helpful assistant.",
                    temperature = 0.5) {
  chat <- chat_groq(
    system_prompt = system_message,
    echo = "none",
    params = params(temperature = temperature)
  )
  chat$chat(user_message)
}

ask_llm(
  "R loops",
  system_message = "Ask me one quiz question about this topic."
)




# Slide: When Text Is Not Enough
chat <- chat_openai(echo = "none")

result <- chat$chat_structured(
  "My name is Susan and I'm 13 years old",
  type = type_object(
    name = type_string(),
    age  = type_number()
  )
)

str(result)




# Slide: Extract a Data Frame
prompt <- "
  John Smith: Age 30. Income $55,000. Education: B.A.
  Jane Doe: Age 25. Income $42,000. Education: M.Sc.
  Jose Rodriguez: Age 40. Income $78,000. Education: Ph.D.
"

type_person <- type_object(
  name      = type_string("Full name"),
  age       = type_integer("Age in years"),
  income    = type_number("Annual income in USD"),
  education = type_enum(c("B.A.", "M.Sc.", "Ph.D."), "Highest degree")
)

chat <- chat_openai(echo = "none")
people <- chat$chat_structured(prompt, type = type_array(type_person))
people




# Exercise: Sentiment Schema
review <- paste(
  "The app is easy to use and the charts are beautiful,",
  "but it crashes whenever I import a large CSV file."
)

type_review <- type_object(
  sentiment = type_enum(c("positive", "negative", "neutral")),
  confidence = type_number("Confidence between 0 and 1"),
  reason = type_string("One short sentence explaining the label")
)

chat <- chat_openai(echo = "none")
chat$chat_structured(review, type = type_review)




# Exercise: Country Table
country_text <- paste(
  "Germany has a GDP of approximately 4.2 trillion USD and is located in Europe.",
  "Brazil's GDP is about 1.9 trillion USD, in South America."
)

type_country <- type_object(
  country = type_string(),
  gdp_trillion_usd = type_number(),
  continent = type_string()
)

chat <- chat_openai(echo = "none")
countries <- chat$chat_structured(country_text, type = type_array(type_country))
countries




# Slide: Study Assistant: Architecture
show_menu <- function() {
  cat("\n=== Study Assistant ===\n")
  cat("1 - Summarize text\n")
  cat("2 - Generate flashcards\n")
  cat("3 - Improve my writing\n")
  cat("4 - Ask a question\n")
  cat("q - Quit\n")
  readline("Choose an option: ")
}

# Run interactively only.
repeat {
  choice <- show_menu()

  if (choice == "q") {
    cat("Happy studying!\n")
    break
  }

  switch(choice,
    "1" = {
      text <- readline("Enter text to summarize: ")
      cat("\nSummary:\n")
      cat(ask_llm(text, "Summarize in clear bullet points.", temperature = 0.3))
    },
    "2" = {
      topic <- readline("Enter a topic: ")
      count <- readline("How many flashcards? (default: 5) ")
      if (count == "") count <- "5"
      sys_msg <- paste0("Generate ", count, " flashcards. Format: Q: ... / A: ...")
      cat(ask_llm(topic, sys_msg, temperature = 0.5))
    },
    "3" = {
      text <- readline("Enter text to improve: ")
      style <- readline("Focus (clarity/academic/concise/grammar): ")
      if (style == "") style <- "clarity"
      sys_msg <- paste("Improve this text. Focus on", style, ". Show improved version.")
      cat(ask_llm(text, sys_msg, temperature = 0.3))
    },
    "4" = {
      question <- readline("Your question: ")
      cat(ask_llm(question))
    },
    cat("Invalid option. Try again.\n")
  )
}




# Exercise: Add One Menu Item
show_menu <- function() {
  cat("\n=== Study Assistant ===\n")
  cat("1 - Summarize text\n")
  cat("2 - Generate flashcards\n")
  cat("3 - Improve my writing\n")
  cat("4 - Ask a question\n")
  cat("5 - Translate text\n")
  cat("q - Quit\n")
  readline("Choose an option: ")
}




# Exercise: Add One Menu Item
# Updated version with the translation branch included; run interactively only.
repeat {
  choice <- show_menu()

  if (choice == "q") {
    cat("Happy studying!\n")
    break
  }

  switch(choice,
    "1" = {
      text <- readline("Enter text to summarize: ")
      cat("\nSummary:\n")
      cat(ask_llm(text, "Summarize in clear bullet points.", temperature = 0.3))
    },
    "2" = {
      topic <- readline("Enter a topic: ")
      count <- readline("How many flashcards? (default: 5) ")
      if (count == "") count <- "5"
      sys_msg <- paste0("Generate ", count, " flashcards. Format: Q: ... / A: ...")
      cat(ask_llm(topic, sys_msg, temperature = 0.5))
    },
    "3" = {
      text <- readline("Enter text to improve: ")
      style <- readline("Focus (clarity/academic/concise/grammar): ")
      if (style == "") style <- "clarity"
      sys_msg <- paste("Improve this text. Focus on", style, ". Show improved version.")
      cat(ask_llm(text, sys_msg, temperature = 0.3))
    },
    "4" = {
      question <- readline("Your question: ")
      cat(ask_llm(question))
    },
    "5" = {
      text <- readline("Enter text to translate: ")
      lang <- readline("Target language (default: German): ")
      if (lang == "") lang <- "German"
      cat(translate_text(text, target_language = lang))
    },
    cat("Invalid option. Try again.\n")
  )
}




# Slide: Error Handling for API Calls
ask_llm_safe <- function(user_message,
                         system_message = "You are a helpful assistant.") {
  tryCatch(
    {
      chat <- chat_groq(system_prompt = system_message, echo = "none")
      chat$chat(user_message)
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("401|authentication|unauthorized", msg, ignore.case = TRUE)) {
        "Error: Invalid API key. Check your .Renviron file."
      } else if (grepl("429|rate.limit", msg, ignore.case = TRUE)) {
        "Error: Too many requests. Wait a moment and try again."
      } else if (grepl("connection|network|timeout", msg, ignore.case = TRUE)) {
        "Error: Cannot reach the API. Check your internet connection."
      } else {
        paste("Error:", msg)
      }
    }
  )
}
