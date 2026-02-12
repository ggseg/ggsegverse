#' Update ggsegverse packages
#'
#' Checks for outdated ggsegverse packages and offers to update them.
#'
#' @export
ggsegverse_update <- function() {
  deps <- ggsegverse_deps()
  behind <- deps[deps$behind, ]

  if (nrow(behind) == 0) {
    cli::cli_alert_success("All ggsegverse packages are up to date.")
    return(invisible())
  }

  cli::cli_h2("The following packages are out of date:")
  cli::cli_ul(paste0(behind$package, " (", behind$local, " -> ", behind$available, ")"))
  cli::cli_text("")

  pkgs_fmt <- paste0(
    "'", paste0("ggseg/", behind$package, collapse = "', '"), "'"
  )
  cli::cli_text("Update with:")
  cli::cli_code(paste0("pak::pak(c(", pkgs_fmt, "))"))

  invisible(behind)
}

#' List ggsegverse package dependencies and versions
#'
#' @return A data frame with columns: `package`, `local`, `available`,
#'   `behind`.
#' @export
ggsegverse_deps <- function() {
  pkgs <- core_packages()

  local_version <- purrr::map_chr(pkgs, function(pkg) {
    tryCatch(
      as.character(utils::packageVersion(pkg)),
      error = function(e) NA_character_
    )
  })

  available_version <- purrr::map_chr(pkgs, function(pkg) {
    desc_url <- paste0(
      "https://raw.githubusercontent.com/ggseg/", pkg,
      "/main/DESCRIPTION"
    )
    tryCatch({
      desc <- readLines(desc_url, warn = FALSE)
      ver_line <- grep("^Version:", desc, value = TRUE)
      gsub("^Version:\\s*", "", ver_line)
    }, error = function(e) NA_character_)
  })

  behind <- !is.na(local_version) & !is.na(available_version) &
    local_version != available_version

  data.frame(
    package = pkgs,
    local = local_version,
    available = available_version,
    behind = behind,
    stringsAsFactors = FALSE
  )
}

#' Situation report for ggsegverse
#'
#' Prints diagnostic information about installed ggsegverse packages.
#'
#' @export
ggsegverse_sitrep <- function() {
  cli::cli_h1("ggsegverse situation report")

  deps <- ggsegverse_deps()

  cli::cli_h2("Installed packages")
  purrr::pwalk(deps, function(package, local, available, behind) {
    if (is.na(local)) {
      cli::cli_alert_danger("{package}: not installed")
    } else if (behind) {
      cli::cli_alert_warning("{package}: {local} (update available: {available})")
    } else {
      cli::cli_alert_success("{package}: {local}")
    }
  })

  cli::cli_h2("R version")
  cli::cli_text("{R.version.string}")

  invisible(deps)
}
