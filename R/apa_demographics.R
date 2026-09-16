#' Create APA 7 Style Demographic Tables
#'
#' Summarises categorical demographic variables into separate N and \% columns and outputs a ready-for-use customisable APA 7 styled flextable.
#'
#' @param data The data frame
#' @param demo_vars Vector of categorical variable names in `data` to summarize.
#' @param group Optional categorical grouping variable.
#' @param group_labels Optional display labels for the levels of `group`. Name each one with the exact value as it appears in the data (e.g. `c("f" = "Female", "m" = "Male")`) — order doesn't matter, only the names.
#' @param total Logical. Whether to include an overall Total column when grouping (default TRUE).
#' @param title Optional title list or string.
#' @param font_family Font family (default "Times New Roman")
#' @param font_size Font size in points (default 12)
#' @param line_thickness Border line thickness (default 1)
#' @param spacer Logical. insert blank gap columns (default TRUE)
#' @param padding_v Vertical padding (default 4)
#' @param padding_h Horizontal padding (default 6)
#' @param note Optional APA note text.
#' @param note_font_size Note font size (default 10)
#' @param footnotes Optional named list mapping variable names to footnotes.
#' @param save_as_docx Optional path to save Word file.
#' @examples
#'
#' test_df <- data.frame(
#'   Gender = c("Female", "Male", "Female", "Male", "Female"),
#'   Ethnicity = c("White", "Black", "Asian", "White", "Hispanic"),dev
#'   Condition = c("A", "A", "B", "B", "A")
#' )
#'
#' apa_demographics(
#'   data = test_df,
#'   demo_vars = c("Gender", "Ethnicity"),
#'   group = "Condition",
#'   group_labels = c("A" = "Control", "B" = "Treatment"),
#'   total = TRUE,
#'   title = list(number = "1", text = "Sample Demographics"),
#'   note = "Note goes here",
#'   footnotes = list(Gender = "footnote here")
#' )
#'
#' @return A flextable object.
#' @export

apa_demographics <- function(data, demo_vars,
                             group = NULL, group_labels = NULL, total = TRUE,
                             title = NULL,
                             font_family = "Times New Roman", font_size = 12,
                             line_thickness = 1,
                             spacer = TRUE,
                             padding_v = 4, padding_h = 6,
                             note = NULL, footnotes = NULL, note_font_size = 10,
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
    if (total) {
      group_levels <- c(group_levels, "Total")
    }
  }

  for (i in seq_along(demo_vars)) {
    col_name <- demo_vars[i]

    if (!is.null(footnotes) && col_name %in% names(footnotes)) {
      footnote_row_map[[col_name]] <- i
    }

    header_row <- list(Category = col_name)
    if (is.null(group)) {
      header_row$n <- ""
      header_row$pct <- ""
    } else {
      for (gi in seq_along(group_levels)) {
        g <- group_levels[gi]
        header_row[[paste0("n_", g)]] <- ""
        header_row[[paste0("pct_", g)]] <- ""
        if (spacer && gi < length(group_levels)) {
          header_row[[paste0("spacer_", gi)]] <- ""
        }
      }
    }

    header_df <- as.data.frame(header_row, stringsAsFactors = FALSE, check.names = FALSE)
    header_df$is_header <- TRUE

    cat_levels <- unique(as.character(data[[col_name]]))
    cat_levels <- cat_levels[!is.na(cat_levels)]

    cat_rows <- list()
    for (cat_idx in seq_along(cat_levels)) {
      cat_level <- cat_levels[cat_idx]
      row <- list(Category = paste0("   ", cat_level))

      if (is.null(group)) {
        x <- data[[col_name]]
        n_valid <- sum(!is.na(x))
        n_matching <- sum(x == cat_level, na.rm = TRUE)
        pct_val <- if (n_valid > 0) (n_matching / n_valid) * 100 else 0

        row$n <- as.character(n_matching)
        row$pct <- sprintf("%.1f", pct_val)
      } else {
        for (gi in seq_along(group_levels)) {
          g <- group_levels[gi]
          x <- if (g == "Total") data[[col_name]] else data[[col_name]][group_col == g]

          n_valid <- sum(!is.na(x))
          n_matching <- sum(x == cat_level, na.rm = TRUE)
          pct_val <- if (n_valid > 0) (n_matching / n_valid) * 100 else 0

          row[[paste0("n_", g)]] <- as.character(n_matching)
          row[[paste0("pct_", g)]] <- sprintf("%.1f", pct_val)

          if (spacer && gi < length(group_levels)) {
            row[[paste0("spacer_", gi)]] <- ""
          }
        }
      }
      cdf <- as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
      cdf$is_header <- FALSE
      cat_rows[[cat_idx]] <- cdf
    }

    results[[i]] <- dplyr::bind_rows(header_df, dplyr::bind_rows(cat_rows))
  }

  final_table <- dplyr::bind_rows(results)
  header_indices <- which(final_table$is_header)
  final_table$is_header <- NULL

  col_names <- names(final_table)

  ft <- flextable::flextable(final_table)

  if (!is.null(group)) {
    top <- character(length(col_names))
    bottom <- character(length(col_names))
    for (i in seq_along(col_names)) {
      cn <- col_names[i]
      if (grepl("^spacer_", cn)) {
        top[i] <- ""; bottom[i] <- ""
      } else if (cn == "Category") {
        top[i] <- "Category"; bottom[i] <- ""
      } else if (grepl("^(n|pct)_", cn)) {
        pos <- regexpr("_", cn)
        stat_type <- substr(cn, 1, pos - 1)
        bottom[i] <- if (stat_type == "n") "n" else "%"
        top[i] <- substr(cn, pos + 1, nchar(cn))
      }
    }
    header_df <- data.frame(col_keys = col_names, top = top, bottom = bottom, stringsAsFactors = FALSE)
    ft <- flextable::set_header_df(ft, mapping = header_df, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")
    ft <- flextable::merge_v(ft, part = "header")
  } else {
    header_labels <- stats::setNames(col_names, col_names)
    if ("n" %in% names(header_labels)) header_labels["n"] <- "n"
    if ("pct" %in% names(header_labels)) header_labels["pct"] <- "%"
    ft <- flextable::set_header_labels(ft, values = header_labels)
  }

  ft <- flextable::border_remove(ft)

  border_std <- officer::fp_border(width = line_thickness)

  align_cols <- col_names[col_names != "Category" & !grepl("^spacer_", col_names)]
  ft <- flextable::align(ft, j = align_cols, align = "center", part = "all")
  ft <- flextable::align(ft, j = "Category", align = "left", part = "all")

  italic_stat_keys <- col_names[grepl("^(n|pct)($|_)", col_names)]
  if (length(italic_stat_keys) > 0) {
    ft <- flextable::italic(ft, j = italic_stat_keys, part = "header")
  }

  ft <- flextable::bold(ft, i = header_indices, j = "Category", part = "body")

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

  #title
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

  #hlines
  if (header_offset > 0) {
    ft <- flextable::hline(ft, i = header_offset, part = "header", border = border_std)
  } else {
    ft <- flextable::hline_top(ft, part = "header", border = border_std)
  }
  ft <- flextable::hline_bottom(ft, part = "header", border = border_std)
  ft <- flextable::hline_bottom(ft, part = "body", border = border_std)

  if (!is.null(group)) {
    stat_cols <- col_names[col_names != "Category" & !grepl("^spacer_", col_names)]
    if (length(stat_cols) > 0 && (header_offset + 1) <= flextable::nrow_part(ft, "header")) {
      ft <- flextable::hline(ft, i = header_offset + 1, j = stat_cols, part = "header", border = border_std)
    }
  }

  #subscripts
  fn_letters <- NULL
  if (!is.null(footnotes) && length(footnote_row_map) > 0) {
    fn_letters <- stats::setNames(letters[seq_along(footnote_row_map)], names(footnote_row_map))

    for (col_name in names(footnote_row_map)) {
      row_i <- header_indices[footnote_row_map[[col_name]]]
      letter <- fn_letters[[col_name]]
      display_name <- final_table$Category[row_i]

      ft <- flextable::compose(
        ft, i = row_i, j = "Category", part = "body",
        value = flextable::as_paragraph(display_name, flextable::as_sup(letter))
      )
    }
  }

  #Note and Footnote formatting
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

  #export to word
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
