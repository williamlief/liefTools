

#' Read all sheets in an excel file
#' @description This function pulls in all sheets in an excel document. It
#'   passes the same arguments for every sheet. Thefore it is best used on excel
#'   files where the sheets have identical layouts but have split the data into
#'   separate sheets by some grouping characteristic (e.g. state, year,
#'   company). An additional column (name controlled by `sheet_id` paramater)
#'   tracks the name of each sheet in the returned tibble.
#'
#'   It takes all the parameters of read_excel EXCEPT 'sheet', the list of
#'   sheets is obtained through a call to `readxl::excel_sheets()`. Future
#'   versions of this function should include a `sheets` parameter to allow the
#'   user to specify explicitly which sheets to read.
#'
#'   Stackoverflow for help building the function:
#'   \code{\link{https://stackoverflow.com/q/58243710/3373438}}
#'
#' @param path 	Path to the xls/xlsx file
#' @param sheet_id default = ".sheet": name of column that contains the sheet_name
#' @param ... all arguments to readxl::read_excel() EXCEPT sheet. If you try to
#'   explicitly use a sheet param the function will break.
#'
#' @return a tibble
#' @seealso \code{\link[readxl]{read_excel}}
#' @export
#'
#' @examples
#' read_excel_all(readxl::readxl_example("datasets.xlsx"), n_max = 2)
read_excel_all <- function(path, ..., sheet_id = ".sheet") {

  sheets <- readxl::excel_sheets(path )
  sheets <- purrr::set_names(sheets)
  purrr::map_df(sheets, readxl::read_excel, path = path, ..., .id = sheet_id)
}




