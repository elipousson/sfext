#' @import rlang
#' @importFrom rlang .data .env caller_env has_length is_interactive
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom glue glue glue_collapse
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
rlang::on_load(rlang::local_use_cli())
