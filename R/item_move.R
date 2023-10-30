#' Move item from one folder to another
#'
#' @export
#' @template manipulate_item
#' @param id_new Folder/item to move \code{id} to. A ScienceBase ID or something 
#' that can be coerced to a SB item ID by \code{\link{as.sbitem}} 
#'
#' @return An object of class \code{sbitem}. Same as \code{id}, but with new 
#' parent id
#' 
#' @examples \dontrun{
#' # create 1st folder
#' (fold1 <- folder_create(user_id(), "bear123"))
#' (res <- item_create(fold1, "item-to-move"))
#' 
#' # create 2nd folder
#' (fold2 <- folder_create(user_id(), "bear456"))
#' 
#' # move item in 1st folder to 2nd folder
#' (res2 <- item_move(res, fold2))
#' 
#' # test identical
#' identical(res2$parentId, fold2$id)
#' }
item_move <- function(sb_id, id_new, ...) {
	id <- as.sbitem(sb_id)
	id_new <- as.sbitem(id_new)
	item_update(id, list(parentId = id_new$id), ...)
}
