# Cargar librería necesaria
library(glue)

# Ruta donde están los archivos .nbib
carpeta <- "nbib_publicaciones"
archivos <- list.files(carpeta, pattern = "\\.nbib$", full.names = TRUE)

# Función para leer y procesar cada archivo .nbib
leer_nbib <- function(path) {
  lineas <- readLines(path, warn = FALSE)
  
  # Unir líneas continuadas (que empiezan con espacios)
  for (i in length(lineas):2) {
    if (grepl("^\\s", lineas[i])) {
      lineas[i - 1] <- paste0(lineas[i - 1], " ", trimws(lineas[i]))
      lineas <- lineas[-i]
    }
  }
  
  # Extraer campos y valores
  campos <- sub("^(\\w{2,4})  -\\s+", "\\1|", lineas)
  claves <- sub("\\|.*", "", campos)
  valores <- sub(".*?\\|", "", campos)
  datos <- tapply(valores, claves, function(x) paste(x, collapse = "|"))
  
  # Funciones auxiliares
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
  
  # Extraer datos relevantes
  titulo <- get_single("TI")
  if (is.na(titulo)) return(NULL)
  
  autores_fau <- get_multiple("FAU")
  autores_au <- get_multiple("AU")
  autores_all <- unique(c(autores_fau, autores_au))
  autores <- if (length(autores_all) > 0) paste(autores_all, collapse = ", ") else "No disponible"
  
  resumen <- get_single("AB")
  anio <- NA
  if ("DP" %in% names(datos)) {
    anio <- sub("^(\\d{4}).*", "\\1", datos[["DP"]])
  }
  
  # Extraer DOI limpio (hasta el primer espacio o fin de línea)
  doi_line <- grep("doi: 10\\.", lineas, value = TRUE)
  enlace_doi <- NA
  if (length(doi_line) > 0) {
    doi_raw <- sub(".*doi: (10\\.[^[:space:];]+).*", "\\1", doi_line[1])
    enlace_doi <- paste0("https://doi.org/", doi_raw)
  }
  
  # Extraer PMID
  pmid_line <- grep("^PMID- ", lineas, value = TRUE)
  enlace_pmid <- NA
  if (length(pmid_line) > 0) {
    pmid <- sub("^PMID- ", "", pmid_line[1])
    enlace_pmid <- paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
  }
  
  # Crear enlaces HTML si están disponibles
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
  enlaces_html <- gsub("^\\s*\\|\\s*|\\s*\\|\\s*$", "", enlaces_html) # Limpiar separadores extra
  
  # Crear bloque en HTML con <details>
  bloque <- glue(
    '<details>
<summary><strong>{titulo}</strong></summary>

<p><strong>Autores:</strong><br>{autores}</p>
<p>{enlaces_html}</p>
<p><strong>Resumen:</strong><br>{resumen}</p>

</details>\n'
  )
  
  return(list(anio = as.numeric(anio), html = bloque))
}

# Procesar todos los archivos
bloques <- lapply(archivos, leer_nbib)
bloques <- bloques[!sapply(bloques, is.null)]

# Ordenar por año descendente
bloques_ordenados <- bloques[order(sapply(bloques, `[[`, "anio"), decreasing = TRUE)]

# Extraer solo el HTML
html_publicaciones <- sapply(bloques_ordenados, `[[`, "html")

# Guardar en Markdown
writeLines(html_publicaciones, "publicaciones.md")

message("✅ Se generaron ", length(html_publicaciones), " publicaciones ordenadas por año correctamente.")
