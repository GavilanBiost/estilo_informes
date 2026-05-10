# ============================================================
# FUNCIONES DE ESTILO - Plantilla de informes
# ============================================================

library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# ------------------------------------------------------------
# Tema propio para ggplot2
# ------------------------------------------------------------

theme_gav <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = base_size + 3,
        color = "#1F4E5F"
      ),
      plot.subtitle = element_text(
        size = base_size,
        color = "#444444"
      ),
      plot.caption = element_text(
        size = base_size - 2,
        color = "#666666",
        hjust = 0
      ),
      axis.title = element_text(
        face = "bold",
        color = "#333333"
      ),
      axis.text = element_text(
        color = "#333333"
      ),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# ------------------------------------------------------------
# Tabla estándar
# ------------------------------------------------------------

tabla_gav <- function(
    x,
    caption = NULL,
    digits = 2,
    align = NULL,
    full_width = FALSE
) {
  
  formato <- if (knitr::is_latex_output()) {
    "latex"
  } else {
    "html"
  }
  
  tabla <- knitr::kable(
    x,
    format = formato,
    caption = caption,
    digits = digits,
    align = align,
    booktabs = TRUE
  )
  
  if (knitr::is_latex_output()) {
    tabla |>
      kableExtra::kable_styling(
        latex_options = c("hold_position", "striped"),
        full_width = full_width,
        position = "center",
        font_size = 10
      )
  } else {
    tabla |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = full_width,
        position = "center"
      )
  }
}

# ------------------------------------------------------------
# Tabla resumen de variables numéricas
# ------------------------------------------------------------

resumen_numerico <- function(datos) {
  datos |>
    summarise(
      across(
        where(is.numeric),
        list(
          n = ~ sum(!is.na(.x)),
          media = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          mediana = ~ median(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      )
    )
}

# ------------------------------------------------------------
# Porcentaje de valores perdidos
# ------------------------------------------------------------

resumen_na <- function(datos) {
  datos |>
    summarise(
      across(
        everything(),
        ~ mean(is.na(.x)) * 100
      )
    ) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "porcentaje_na"
    ) |>
    arrange(desc(porcentaje_na))
}