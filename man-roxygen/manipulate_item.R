#'@param sb_id An \code{\link{sbitem}} object or a character ScienceBase ID corresponding to the item
#'
#'@param sb_parent An \code{\link{sbitem}} object or character ScienceBase ID corresponding to the 
#'parent item (folder) 
#'
#'@param ... Additional parameters are passed on to \code{\link[httr]{GET}}, \code{\link[httr]{POST}},
#'\code{\link[httr]{HEAD}}, \code{\link[httr]{PUT}}, or \code{\link[httr]{DELETE}}
#'
#'@param session Session object from \code{\link{authenticate_sb}}. Defaults to anonymous or 
#'last authenticated session
#'
