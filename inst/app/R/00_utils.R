# Shared formatting and HTML helpers.

`%||%` <- function(a,b) if (is.null(a)) b else a

# --------- 安全取值 + 数字格式 ----------
num <- function(x, d=3) {
  if (is.null(x) || length(x)==0 || is.na(x)) return(NA_character_)
  sprintf(paste0("%.", d, "f"), as.numeric(x))
}
get1 <- function(fm_all, name) if (name %in% names(fm_all)) fm_all[[name]] else NA_real_

format_pvalue <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< .001")
  sprintf("%.3f", p)
}

build_html_table <- function(title, data, headers, empty_msg) {
  if (is.null(data) || nrow(data) == 0) {
    return(paste0("<h4>", title, "</h4><p>", empty_msg, "</p>"))
  }
  header_html <- paste0("<thead><tr>", paste0("<th>", headers, "</th>", collapse = ""), "</tr></thead>")
  rows_html <- apply(data, 1, function(row) {
    paste0("<tr>", paste0("<td>", row, "</td>", collapse = ""), "</tr>")
  })
  paste0(
    "<h4>", title, "</h4>",
    "<table class='table table-striped table-bordered'>",
    header_html,
    "<tbody>", paste(rows_html, collapse = ""), "</tbody></table>"
  )
}

format_estimate_cell <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("NA")
  sprintf("%.3f", x)
}
