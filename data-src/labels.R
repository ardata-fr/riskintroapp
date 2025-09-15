
# labels are formatted as text
# help is formatted as html from markdown

labels_list <- yaml::yaml.load_file("data-src/labels.yml")

labels_int <- purrr::map_df(labels_list, function(x) {
  out <- purrr::map_df(x$data, function(x) {
    # Convert only help fields from markdown to HTML, leave labels as plain text
    if (!is.null(x$help)) {
      x$help <- commonmark::markdown_html(x$help, hardbreaks = TRUE, smart = TRUE)
    }
    # Labels remain as plain text (no conversion needed)
    as.data.frame(x, stringsAsFactors = FALSE)
  })
  out$key <- rep(x$key, nrow(out))
  out
})


usethis::use_data(labels_int, internal = TRUE, overwrite = TRUE)
