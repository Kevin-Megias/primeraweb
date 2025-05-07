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
      "No disponible"
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
  if (titulo == "No disponible") return(NULL)
  
  autores_fau <- get_multiple("FAU")
  autores_au <- get_multiple("AU")
  autores_all <- unique(c(autores_fau, autores_au))
  autores <- if (length(autores_all) > 0) paste(autores_all, collapse = ", ") else "No disponible"
  
  resumen <- get_single("AB")
  
  # Extraer DOI desde línea que contiene 'doi:'
  doi_line <- grep("doi: 10\\.", lineas, value = TRUE)
  enlace_doi <- "No disponible"
  if (length(doi_line) > 0) {
    doi_raw <- sub(".*doi: (10\\.[^ ]+?)\\.*\\s*$", "\\1", doi_line[1])
    enlace_doi <- paste0("https://doi.org/", doi_raw)
  }
  
  # Crear bloque en HTML con <details>
  bloque <- glue(
    '<details>
<summary><strong>{titulo}</strong></summary>

<br>

**Autores:**<br> {autores}

<br><br>

**Enlace a la revista:** <a href="{enlace_doi}" target="_blank">{enlace_doi}</a>

<br><br>

**Resumen:**<br> {resumen}

<br><br>

</details>\n'
  )
  
  return(bloque)
}

# Procesar todos los archivos y guardar en Markdown
bloques_md <- lapply(archivos, leer_nbib)
bloques_md <- bloques_md[!sapply(bloques_md, is.null)]
writeLines(unlist(bloques_md), "publicaciones.md")

message("✅ Se generaron ", length(bloques_md), " publicaciones correctamente.")
