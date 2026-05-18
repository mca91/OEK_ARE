library(ellmer)

readRenviron("/Users/martin/git_projects/OEK_ARE/.Renviron")

document_path <- paste0("~/git_projects/OEK_ARE/SoSe_2026/AI/med_invoice.webp")  # use your own JPG or PNG scan

chat <- chat_groq(
  model = "meta-llama/llama-4-scout-17b-16e-instruct",
  system_prompt = paste(
    "You are a careful OCR assistant.",
    "Extract the visible document content and return only Markdown.",
    "Preserve headings, paragraphs, lists, tables, and signatures.",
    "Mark unreadable text as [illegible].",
    "Return ONLY the Markdown converted document."
  ),
  echo = "none",
  params = params(temperature = 0)
)
markdown_doc <- chat$chat(
  "Convert this scanned document into clean Markdown and translate to English.",
  content_image_file(document_path, resize = "high")
)
cat(markdown_doc)

library(ellmer)
chat <- chat_google_gemini(
    echo = "none"
)

result <- chat$chat_structured(
  "My name is Susan and I'm 13 years old",
  type = type_object(
    name = type_string(),
    age  = type_number()
  )
)
str(result)  # returns a named list

chat <- chat_google_gemini(
  echo = "none",
  system_prompt = paste(
    "You are a careful OCR assistant.",
    "Extract the visible document content and return only Markdown.",
    "Preserve headings, paragraphs, lists, tables, and signatures.",
    "Mark unreadable text as [illegible].",
    "Translate the results to english."
  ),
  echo = "none",
  params = params(temperature = 0)
)

result <- chat$chat_structured(
  content_image_file(document_path, resize = "high"),
  type = type_array(
    type_object(
      item_quantity = type_number(),
      item_name = type_string(),
      item_price = type_number() 
    )
  )
)
str(result)  # returns a named list
result
