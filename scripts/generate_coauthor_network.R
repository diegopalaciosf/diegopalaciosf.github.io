args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1) args[[1]] else "_pages/publications.md"
output <- if (length(args) >= 2) args[[2]] else "images/coauthor_network.png"

suppressPackageStartupMessages(library(igraph))

standardize_name <- function(name) {
  clean <- trimws(gsub("\\*", "", name))
  clean <- gsub("^&\\s*", "", clean)
  clean_ascii <- iconv(clean, to = "ASCII//TRANSLIT")

  if (grepl("^Palacios(-Farias)?\\s*,\\s*D\\.?$", clean_ascii, ignore.case = TRUE)) {
    return("Diego Palacios Farias")
  }

  clean_ascii
}

short_label <- function(name) {
  if (identical(name, "Diego Palacios Farias")) {
    return("Diego Palacios Farias")
  }

  trimws(strsplit(name, ",", fixed = TRUE)[[1]][1])
}

extract_authors <- function(line) {
  entry <- sub("^-\\s+", "", line)
  author_text <- sub("\\s*\\((submitted|\\d{4})\\).*", "", entry, perl = TRUE)
  author_text <- gsub("\\s+&\\s+", ", ", author_text)
  parts <- trimws(unlist(strsplit(author_text, ",")))
  parts <- parts[nzchar(parts)]

  if (length(parts) < 2) {
    return(character())
  }

  pair_count <- floor(length(parts) / 2)
  authors <- vapply(seq_len(pair_count), function(i) {
    surname <- parts[(2 * i) - 1]
    initials <- parts[2 * i]
    standardize_name(paste(surname, initials, sep = ", "))
  }, character(1))

  unique(authors)
}

lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
pub_lines <- grep("^- ", lines, value = TRUE)
author_lists <- lapply(pub_lines, extract_authors)
author_lists <- Filter(function(x) length(x) >= 2, author_lists)

edge_names <- unlist(lapply(author_lists, function(authors) {
  pairs <- combn(sort(authors), 2)
  apply(pairs, 2, paste, collapse = "|||")
}), use.names = FALSE)

edge_table <- sort(table(edge_names), decreasing = TRUE)
edges <- do.call(rbind, strsplit(names(edge_table), "\\|\\|\\|"))
graph <- graph_from_data_frame(
  data.frame(
    from = edges[, 1],
    to = edges[, 2],
    weight = as.numeric(edge_table),
    stringsAsFactors = FALSE
  ),
  directed = FALSE
)

diego_name <- "Diego Palacios Farias"
V(graph)$weighted_degree <- strength(graph, weights = E(graph)$weight)
V(graph)$is_diego <- V(graph)$name == diego_name
V(graph)$size <- ifelse(V(graph)$is_diego, 26, 8 + log1p(V(graph)$weighted_degree) * 3.4)
V(graph)$color <- ifelse(V(graph)$is_diego, "#c0392b", "#2c7fb8")
V(graph)$frame.color <- ifelse(V(graph)$is_diego, "#7f1d1d", "#1d4f91")

top_collaborators <- names(sort(V(graph)$weighted_degree, decreasing = TRUE))
top_collaborators <- setdiff(top_collaborators, diego_name)
top_collaborators <- head(top_collaborators, 10)

V(graph)$label <- ifelse(
  V(graph)$is_diego | V(graph)$name %in% top_collaborators,
  vapply(V(graph)$name, short_label, character(1)),
  NA
)
V(graph)$label.cex <- ifelse(V(graph)$is_diego, 1.0, 0.7)
V(graph)$label.color <- "#1f2937"

layout <- layout_with_fr(graph, weights = E(graph)$weight, niter = 3000)
diego_index <- which(V(graph)$name == diego_name)
if (length(diego_index) == 1) {
  layout <- sweep(layout, 2, layout[diego_index, ], "-")
}

label_degree <- atan2(layout[, 2], layout[, 1])
label_dist <- ifelse(is.na(V(graph)$label), 0, ifelse(V(graph)$is_diego, 0, 1.1))

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
png(output, width = 1800, height = 1400, res = 220)
par(mar = c(0, 0, 2, 0), bg = "white")
plot(
  graph,
  layout = layout,
  vertex.label.family = "sans",
  vertex.label.font = 1,
  vertex.label.dist = label_dist,
  vertex.label.degree = label_degree,
  edge.width = 0.8 + E(graph)$weight * 0.8,
  edge.color = rgb(0.4, 0.5, 0.7, alpha = 0.25),
  main = "Co-authorship network"
)
mtext("Node size reflects weighted collaboration frequency", side = 3, line = 0.1, cex = 0.8, col = "#4b5563")
dev.off()
