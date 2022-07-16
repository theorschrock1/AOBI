#' @import sUtils
#' @import glue
#' @import stringr
#' @import checkmate
#' @import rlang
#' @import sDataTable
#' @import exprTools
#' @import AOunits
#' @import shiny
#' @import htmltools
#' @import ShinyReboot
#' @import bslib
#' @import shinyAce
#' @import shinyjs
#' @import zeallot
#' @import data.table
#' @import R6

# This script is intended for imports and .onLoad/.onAttach functions only.  Do not add other functions to this file or remove existing.

.onLoad <- function(libname, pkgname) {
    bslib::bs_global_theme()
}
