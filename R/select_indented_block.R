# Select the current indented block of code.

#' select_indented_block
#'
#' @importFrom rstudioapi insertText getSourceEditorContext primary_selection
#'     document_position setSelectionRanges setSelectionRanges
#' @export
select_indented_block <- function() {
    f.is_indented <- function(row) {
        return(isTRUE(grep("^\\s{2,}", rstudioapi::getSourceEditorContext()$contents[row]) == 1))
    }
    cur_row <- rstudioapi::primary_selection(rstudioapi::getSourceEditorContext()$selection)$range$start[["row"]]
    for (row in cur_row:1) {
        if (!f.is_indented(row)) {
            indented_block <- c(row, cur_row)
            break
        }
    }
    start <- rstudioapi::document_position(indented_block[1], 0)
    end <- rstudioapi::document_position(indented_block[2], nchar(rstudioapi::getSourceEditorContext()$contents[indented_block[2]])+1)
    rstudioapi::setSelectionRanges(rstudioapi::document_range(start, end))
}

