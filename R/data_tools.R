#' @export
#' @title Import des donnees "spam" (.txt).
#' @description La fonction est just prevue pour "spam".
#'
#' Data input is path of file.
#' @param file Character containing path and name of file "spam".
#' @importFrom utils read.delim
#' @return Data frame object.

# import ----
read_spam <- function(file){
 read.delim(file=file, header = TRUE, sep = "\t", dec = ".",
            encoding="UTF-8")
}



#' @export
#' @title Reommage des variables (statique).
#' @description Les noms de colonnes deviennent "wf_" et "cf_".
#'
#' Data input is data frame.
#' @param data_frame Data frame des donnees "SPAM".
#' @return Le data frame mis a jour.

# rename ----
rename_static <- function(data_frame){
 #récupération des noms de colonnes
 nom.col = colnames(data_frame) #nom des variables
 #les noms de variables non bugés
 liste.col.char = c("cf_comma", "cf_bracket", "cf_sqbracket", "cf_exclam", "cf_dollar", "cf_hash", "spam")
 #repérage des indices des variables caractères
 ind <- grep("^char_freq|SPAM", nom.col)

 #renommage test sur variable
 nom.col[ind] = liste.col.char

 #renommage descolonnes du df principal
 colnames(data_frame)[ind] = liste.col.char

 #changement des noms de colonnes
 #word_freq -> wf_415
 nom_col_achanger = colnames(data_frame) ; nom_col_achanger
 new_nom_col = sub("word_freq","wf",colnames(data_frame)) ; new_nom_col

 colnames(data_frame)=sub("word_freq","wf",colnames(data_frame))

 data_frame

}




