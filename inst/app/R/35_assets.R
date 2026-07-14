# Concept-model assets.

concept_image <- function(filename, alt) {
  if (!file.exists(file.path("www", filename))) {
    return(tags$div(
      style = paste0(
        "height:240px;border:1px dashed #bbb;border-radius:6px;",
        "display:flex;align-items:center;justify-content:center;color:#777;"
      ),
      paste0("Missing concept image: ", filename)
    ))
  }

  tags$img(
    src = filename,
    alt = alt,
    style = paste0(
      "max-width:90%;max-height:500px;height:auto;display:block;",
      "margin:0 auto;border:1px solid #ddd;border-radius:6px;"
    )
  )
}
