core_unloaded <- function() {
  search <- paste0("package:", core_packages())
  core_packages()[!search %in% search()]
}

same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE,
          quietly = TRUE)
}

ggsegverse_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0) return(invisible())

  suppressPackageStartupMessages(
    purrr::walk(to_load, same_library)
  )

  invisible()
}

ggsegverse_attach_message <- function() {
  pkgs <- ggsegverse_packages(include_self = TRUE)
  header <- cli::rule(
    left = cli::style_bold("ggsegverse"),
    right = utils::packageVersion("ggsegverse")
  )

  n <- length(pkgs)
  half <- ceiling(n / 2)
  col1 <- purrr::map_chr(pkgs[seq_len(half)], format_package_line)
  col2_idx <- seq(half + 1, n)
  col2 <- if (length(col2_idx) > 0) {
    purrr::map_chr(pkgs[col2_idx], format_package_line)
  } else {
    character()
  }

  col2 <- c(col2, rep("", half - length(col2)))

  info <- purrr::map2_chr(col1, col2, function(x, y) {
    paste0(x, " ", y)
  })

  paste0(header, "\n", paste(info, collapse = "\n"))
}

package_version_string <- function(pkg) {
  v <- tryCatch(
    as.character(utils::packageVersion(pkg)),
    error = function(e) NA_character_
  )
  if (is.na(v)) "[not installed]" else v
}

format_package_line <- function(pkg) {
  v <- package_version_string(pkg)
  mark <- if (v == "[not installed]") {
    cli::col_red(cli::symbol$cross)
  } else {
    cli::col_green(cli::symbol$tick)
  }
  paste0(mark, " ", cli::col_blue(pkg), " ", v)
}
