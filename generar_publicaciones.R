library(glue)

carpeta <- "nbib_publicaciones"
archivos <- list.files(carpeta, pattern = "\\.nbib$", full.names = TRUE)

leer_nbib <- function(path) {
  lineas <- readLines(path, warn = FALSE)
  
  for (i in length(lineas):2) {
    if (grepl("^\\s", lineas[i])) {
      lineas[i - 1] <- paste0(lineas[i - 1], " ", trimws(lineas[i]))
      lineas <- lineas[-i]
    }
  }
  
  campos <- sub("^(\\w{2,4})  -\\s+", "\\1|", lineas)
  claves <- sub("\\|.*", "", campos)
  valores <- sub(".*?\\|", "", campos)
  datos <- tapply(valores, claves, function(x) paste(x, collapse = "|"))
  
  get_single <- function(clave) {
    if (clave %in% names(datos)) {
      gsub("\\|", " ", datos[[clave]])
    } else {
      NA
    }
  }
  
  get_multiple <- function(clave) {
    if (clave %in% names(datos)) {
      unlist(strsplit(datos[[clave]], "\\|"))
    } else {
      character(0)
    }
  }
  
  titulo <- get_single("TI")
  if (is.na(titulo)) return(NULL)
  
  autores_fau <- get_multiple("FAU")
  autores_au <- get_multiple("AU")
  autores_all <- unique(c(autores_fau, autores_au))
  autores <- if (length(autores_all) > 0) paste(autores_all, collapse = ", ") else "No disponible"
  
  resumen <- get_single("AB")
  
  doi_line <- grep("(doi: 10\\.|10\\.\\d+/.*\\[doi\\])", lineas, value = TRUE)
  enlace_doi <- NA
  if (length(doi_line) > 0) {
    if (grepl("doi: 10\\.", doi_line[1])) {
      doi_raw <- sub(".*doi: (10\\.[^ ]+).*", "\\1", doi_line[1])
    } else {
      doi_raw <- sub("^.*?(10\\.\\d+/[^ ]+).*\\[doi\\].*$", "\\1", doi_line[1])
    }
    doi_clean <- sub("\\.(?=\\s|$)", "", doi_raw, perl = TRUE)
    enlace_doi <- paste0("https://doi.org/", doi_clean)
  }
  
  pmid_line <- grep("^PMID- ", lineas, value = TRUE)
  enlace_pmid <- NA
  if (length(pmid_line) > 0) {
    pmid <- sub("^PMID- ", "", pmid_line[1])
    enlace_pmid <- paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
  }
  
  link_doi <- if (!is.na(enlace_doi)) {
    glue('<a href="{enlace_doi}" target="_blank">DOI</a>')
  } else {
    ""
  }
  
  link_pmid <- if (!is.na(enlace_pmid)) {
    glue('<a href="{enlace_pmid}" target="_blank">PubMed</a>')
  } else {
    ""
  }
  
  enlaces_html <- paste(link_doi, link_pmid, sep = " | ")
  enlaces_html <- gsub("^\\s*\\|\\s*|\\s*\\|\\s*$", "", enlaces_html) 
  
  bloque <- glue(
    '<details>
<summary><strong>{titulo}</strong></summary>

<br>

**Autores:**<br> {autores}

<br><br>

**Enlaces:**<br>
{enlaces_html}

<br><br>

**Resumen:**<br> {resumen}

<br><br>

</details>\n'
  )
  
  return(bloque)
}

bloques_md <- lapply(archivos, leer_nbib)
bloques_md <- bloques_md[!sapply(bloques_md, is.null)]
writeLines(unlist(bloques_md), "publicaciones.md")

message("✅ Se generaron ", length(bloques_md), " publicaciones correctamente.")
