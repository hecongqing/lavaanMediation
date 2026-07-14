# Concept-model assets.

# Concept images are preferred because they render consistently in browsers.
# A PDF with the same base name is used as a read-only fallback.
concept_img_or_pdf <- function(base) {
  img_candidates <- c(paste0(base, ".png"), paste0(base, ".jpg"), paste0(base, ".jpeg"), paste0(base, ".svg"))
  for (img in img_candidates) {
    if (file.exists(file.path("www", img))) {
      return(tags$img(src = img, style = "max-width:90%;max-height:500px;height:auto;display:block;margin:0 auto;border:1px solid #ddd;border-radius:6px;"))
    }
  }

  pdf_file <- file.path("www", paste0(base, ".pdf"))
  if (file.exists(pdf_file)) {
    return(tags$embed(
      src = paste0(base, ".pdf"),
      type = "application/pdf",
      style = "width:100%;height:320px;border:1px solid #ddd;border-radius:6px;"
    ))
  }

  tags$div(style="height:240px;border:1px dashed #bbb;border-radius:6px;display:flex;align-items:center;justify-content:center;color:#777;",
           paste0("Please add ", base, ".pdf (or image) to inst/app/www/"))
}
