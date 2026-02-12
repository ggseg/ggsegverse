#' Conflicts between ggsegverse packages and other loaded packages
#'
#' Lists all function name conflicts between ggsegverse packages and
#' other loaded packages.
#'
#' @return A `ggsegverse_conflicts` object (invisibly).
#' @export
ggsegverse_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(purrr::map(envs, ls_env))

  ggseg_pkgs <- paste0("package:", ggsegverse_packages())
  conflicts <- purrr::keep(objs, ~ length(.x) > 1 && any(.x %in% ggseg_pkgs))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "ggsegverse_conflicts")
}

confirm_conflict <- function(pkgs, name) {
  dominated <- pkgs[pkgs != pkgs[[1]]]
  ggseg_pkgs <- paste0("package:", ggsegverse_packages())

  dominated_ggseg <- dominated[dominated %in% ggseg_pkgs]
  if (length(dominated_ggseg) == 0) return(NULL)

  dominated_ggseg
}

ls_env <- function(env) {
  x <- .getNamespaceInfo(asNamespace(gsub("package:", "", env)), "exports")
  if (inherits(x, "environment")) ls(x) else character()
}

#' @export
print.ggsegverse_conflicts <- function(x, ...) {
  if (length(x) == 0) {
    cli::cli_inform("No conflicts detected.")
    return(invisible(x))
  }

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "ggsegverse_conflicts()"
  )
  cli::cli_inform(header)
  purrr::iwalk(x, function(pkgs, name) {
    winner <- setdiff(
      purrr::keep(search(), ~ name %in% ls_env(.x)),
      pkgs
    )[[1]]
    loser <- pkgs[[1]]
    cli::cli_inform("{cli::col_red(cli::symbol$cross)} {name}: {loser} masks {winner}")
  })

  invisible(x)
}

#' @export
format.ggsegverse_conflicts <- function(x, ...) {
  if (length(x) == 0) return("")

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "ggsegverse_conflicts()"
  )
  bullets <- purrr::imap_chr(x, function(pkgs, name) {
    paste0(cli::col_red(cli::symbol$cross), " ", name, ": ", pkgs[[1]], " masks another package")
  })
  paste(c(header, bullets), collapse = "\n")
}
