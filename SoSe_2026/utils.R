# ICONS
ICONS <- list()
ICONS$desktop <- fontawesome::fa("desktop", fill = "#004c93")
ICONS$checker_green <- fontawesome::fa("check", fill = "green")
ICONS$incorrect <- fontawesome::fa("times", fill = "red")

# PROGRESSBAR
xaringanExtra::use_progress_bar(color = "#004c93", location = "bottom")

# COPY TO CLIPBOARD
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "<i class=\"fa fa-clipboard\"></i>",
    success_text = "<i class=\"fa fa-check\" style=\"color: #00ff00\"></i>",
    error_text = "<i class=\"fa fa-times-circle\" style=\"color: #F94144\"></i>"
  ),
  rmarkdown::html_dependency_font_awesome()
)