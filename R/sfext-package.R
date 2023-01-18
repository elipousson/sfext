#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom rlang .data .env caller_env has_length is_interactive check_installed
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom cliExtras cli_abort_ifnot cli_warn_ifnot cli_yesno
#' @importFrom glue glue glue_collapse
#' @importFrom filenamr make_filename
## usethis namespace: end
rlang::on_load(rlang::local_use_cli())
