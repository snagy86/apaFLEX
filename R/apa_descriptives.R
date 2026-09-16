#' Create APA 7 Style Descriptive Statistics Tables
#'
#' Calculates descriptive statistics (mean, standard deviation, etc.) from data and outputs a ready-for-use customisable APA 7 styled table using flextable.
#'
#' @param data The name of data frame
#' @param vars Vector of variable names in `data` to include. Variables can also be named to be relabeled for output (e.g. `c("mpg" = "Miles per Gallon")`); unnamed entries just use the raw column name.
#' @param title Logical. Optional title list or string.
#' @param range Logical.Whether to include observed min-max range (default FALSE)
#' @param possible_range Optional custom text per variable under column tilted "Possible Range".
#' @param median Logical.Whether to include median column (default FALSE)
#' @param group Optional categorical grouping variable.
#' @param group_labels #' @param group_labels Optional display labels for the levels of `group` (e.g. `c("0" = "Automatic", "1" = "Manual")`).
#' @param font_family Font family (default "Times New Roman")
#' @param font_size Font size in points (default 12)
#' @param line_thickness Border line thickness (default 1)
#' @param spacer Logical. insert blank gap columns (default TRUE)
#' @param padding_v Vertical padding (default 4)
#' @param padding_h Horizontal padding (default 6)
#' @param note Optional APA note text.
#' @param note_font_size Note font size (default 10)
#' @param footnotes Optional named list mapping column names to footnotes.
#' @param save_as_docx Optional path to save Word file.
#' @examples
#' apa_descriptives(
#'   data = mtcars,
#'   vars = c("mpg" = "Miles Per Gallon", "hp" = "Horsepower"),
#'   group = "am",
#'   group_labels = c("0" = "Automatic", "1" = "Manual"),
#'   title = list(number = "1", text = "Vehicle Performance by Transmission Type"),
#'   note = "Note goes here",
#'   footnotes = list("mpg" = "footnote here")
#' )
#'
#' @return A flextable object.
#' @export

apa_descriptives <- function(data, vars,
                             title = NULL,
                             range = FALSE, possible_range = NULL, median = FALSE,
                             group = NULL, group_labels = NULL,
                             font_family = "Times New Roman", font_size = 12,
                             line_thickness = 1, spacer = TRUE,
                             padding_v = 4, padding_h = 6,
                             note = NULL, note_font_size = 10,
                             footnotes = NULL,
                             save_as_docx = NULL) {

  results <- list()

  footnote_row_map <- list()


  if (!is.null(group)) {
    group_col <- as.factor(data[[group]])
    if (!is.null(group_labels)) {
      lvls <- levels(group_col)
      new_lvls <- if (!is.null(names(group_labels))) unname(group_labels[lvls]) else group_labels
      levels(group_col) <- new_lvls
    }
    group_levels <- levels(group_col)
  }

  for (i in seq_along(vars)) {
    col_name <- names(vars)[i]
    if (is.null(col_name) || col_name == "") {
      col_name <- vars[i]
    }
    display_name <- vars[i]
    row <- list(Variable = display_name)

    if (!is.null(footnotes) && col_name %in% names(footnotes)) {
      footnote_row_map[[col_name]] <- i
    }

    if (is.null(group)) {
      x <- data[[col_name]]
      row$M  <- sprintf("%.2f", mean(x, na.rm = TRUE))
      row$SD <- sprintf("%.2f", stats::sd(x, na.rm = TRUE))
      if (median) row$Median <- sprintf("%.2f", stats::median(x, na.rm = TRUE))
      if (range) {
        row$Range <- if (all(is.na(x))) NA_character_ else
          paste0(sprintf("%.2f", min(x, na.rm = TRUE)), "\u2013", sprintf("%.2f", max(x, na.rm = TRUE)))
      }

    } else {
      for (gi in seq_along(group_levels)) {
        g <- group_levels[gi]
        x <- data[[col_name]][group_col == g]
        row[[paste0("M_", g)]]  <- sprintf("%.2f", mean(x, na.rm = TRUE))
        row[[paste0("SD_", g)]] <- sprintf("%.2f", stats::sd(x, na.rm = TRUE))
        if (median) row[[paste0("Median_", g)]] <- sprintf("%.2f", stats::median(x, na.rm = TRUE))
        if (range) {
          row[[paste0("Range_", g)]] <- if (all(is.na(x))) NA_character_ else
            paste0(sprintf("%.2f", min(x, na.rm = TRUE)), "\u2013", sprintf("%.2f", max(x, na.rm = TRUE)))
        }

        if (spacer && gi < length(group_levels)) {
          row[[paste0("spacer_", gi)]] <- ""
        }
      }
    }

    if (!is.null(possible_range)) {
      if (col_name %in% names(possible_range)) {
        row$Possible_Range <- possible_range[[col_name]]
      } else {
        row$Possible_Range <- "\u2013"
      }
    }

    results[[i]] <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  }

  final_table <- do.call(rbind, results)
  rownames(final_table) <- NULL

  col_names <- names(final_table)

  ft <- flextable::flextable(final_table)

  if (!is.null(group)) {
    top <- character(length(col_names))
    bottom <- character(length(col_names))
    for (i in seq_along(col_names)) {
      cn <- col_names[i]
      if (grepl("^spacer_", cn)) {
        top[i] <- ""; bottom[i] <- ""
      } else if (cn == "Variable") {
        top[i] <- "Variable"; bottom[i] <- ""
      } else if (cn == "Possible_Range") {
        top[i] <- "Possible Range"; bottom[i] <- ""
      } else if (grepl("_", cn)) {
        pos <- regexpr("_", cn)
        bottom[i] <- substr(cn, 1, pos - 1)
        top[i] <- substr(cn, pos + 1, nchar(cn))
      } else {
        top[i] <- cn; bottom[i] <- ""
      }
    }
    header_df <- data.frame(col_keys = col_names, top = top, bottom = bottom, stringsAsFactors = FALSE)
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")
    ft <- flextable::merge_v(ft, part = "header")
  } else {
    header_labels <- stats::setNames(col_names, col_names)
    if ("Possible_Range" %in% names(header_labels)) header_labels["Possible_Range"] <- "Possible Range"
    ft <- flextable::set_header_labels(ft, values = header_labels)
  }

  ft <- flextable::border_remove(ft)

  border_std <- officer::fp_border(width = line_thickness)

  align_cols <- col_names[col_names != "Variable" & !grepl("^spacer_", col_names)]
  ft <- flextable::align(ft, j = align_cols, align = "center", part = "all")
  ft <- flextable::align(ft, j = "Variable", align = "left", part = "all")

  italic_stat_keys <- col_names[grepl("^(M|SD|Median)($|_)", col_names)]
  if (length(italic_stat_keys) > 0) {
    ft <- flextable::italic(ft, j = italic_stat_keys, part = "header")
  }

  ft <- flextable::font(ft, fontname = font_family, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")

  ft <- flextable::padding(
    ft,
    padding.top = padding_v, padding.bottom = padding_v,
    padding.left = padding_h, padding.right = padding_h,
    part = "all"
  )

  spacer_cols <- col_names[grepl("^spacer_", col_names)]
  if (length(spacer_cols) > 0) {
    ft <- flextable::width(ft, j = spacer_cols, width = 0.15)
  }

  header_offset <- 0
  if (!is.null(title)) {
    tbl_num <- "Table 1"
    tbl_text <- ""

    if (is.list(title)) {
      if (!is.null(title$number)) tbl_num <- paste0("Table ", title$number)
      if (!is.null(title$text)) tbl_text <- title$text
    } else if (is.character(title)) {
      tbl_text <- title
    }

    ft <- flextable::add_header_lines(ft, values = c(tbl_num, tbl_text), top = TRUE)
    ft <- flextable::font(ft, fontname = font_family, i = 1:2, part = "header")
    ft <- flextable::fontsize(ft, size = font_size, i = 1:2, part = "header")
    ft <- flextable::bold(ft, i = 1, part = "header")
    ft <- flextable::italic(ft, i = 2, part = "header")
    ft <- flextable::align(ft, i = 1:2, align = "left", part = "header")
    header_offset <- 2
  }

  if (header_offset > 0) {
    ft <- flextable::hline(ft, i = header_offset, part = "header", border = border_std)
  } else {
    ft <- flextable::hline_top(ft, part = "header", border = border_std)
  }
  ft <- flextable::hline_bottom(ft, part = "header", border = border_std)
  ft <- flextable::hline_bottom(ft, part = "body", border = border_std)

  if (!is.null(group)) {
    stat_cols <- col_names[col_names != "Variable" & !grepl("^spacer_", col_names)]
    if (length(stat_cols) > 0 && (header_offset + 1) <= flextable::nrow_part(ft, "header")) {
      ft <- flextable::hline(ft, i = header_offset + 1, j = stat_cols, part = "header", border = border_std)
    }
  }

  fn_letters <- NULL
  if (!is.null(footnotes) && length(footnote_row_map) > 0) {
    fn_letters <- stats::setNames(letters[seq_along(footnote_row_map)], names(footnote_row_map))

    for (col_name in names(footnote_row_map)) {
      row_i <- footnote_row_map[[col_name]]
      letter <- fn_letters[[col_name]]
      display_name <- final_table$Variable[row_i]

      ft <- flextable::compose(
        ft, i = row_i, j = "Variable", part = "body",
        value = flextable::as_paragraph(display_name, flextable::as_sup(letter))
      )
    }
  }

  if (!is.null(note)) {
    ft <- flextable::add_footer_lines(ft, values = "")
    ft <- flextable::compose(
      ft, i = flextable::nrow_part(ft, "footer"), j = 1, part = "footer",
      value = flextable::as_paragraph(flextable::as_i("Note. "), note)
    )
  }

  if (!is.null(footnotes) && length(footnote_row_map) > 0) {
    for (col_name in names(footnote_row_map)) {
      letter <- fn_letters[[col_name]]
      def_text <- footnotes[[col_name]]

      ft <- flextable::add_footer_lines(ft, values = "")
      ft <- flextable::compose(
        ft, i = flextable::nrow_part(ft, "footer"), j = 1, part = "footer",
        value = flextable::as_paragraph(flextable::as_sup(letter), " ", def_text)
      )
    }
  }

  if (!is.null(note) || (!is.null(footnotes) && length(footnote_row_map) > 0)) {
    ft <- flextable::fontsize(ft, size = note_font_size, part = "footer")
    ft <- flextable::font(ft, fontname = font_family, part = "footer")
    ft <- flextable::align(ft, align = "left", part = "footer")
  }

  ft <- flextable::autofit(ft)

  if (!is.null(save_as_docx)) {
    tryCatch({
      flextable::save_as_docx(ft, path = save_as_docx)
      message("Table successfully saved to ", save_as_docx)
    }, error = function(e) {
      if (grepl("is open", e$message, ignore.case = TRUE)) {
        warning("\nCould not save: The Word file is currently open in Microsoft Word.\nPlease close '", save_as_docx, "' and re-run your code.", call. = FALSE)
      } else {
        warning("\nFailed to save Word document: ", e$message, call. = FALSE)
      }
    })
  }

  ft
}
