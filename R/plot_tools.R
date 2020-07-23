# declaration variables globales
globalVariables(c("count", "prop"))

#' @export
#' @title Analyse descriptive avec geom_tile().
#' @description Visualiser rapidement des concentrations (scale).
#'
#' Data input is "tidy" data frame.
#' @import ggplot2
#' @param tidy_df Tidy data frame (id, nom_col, values).
#' @param x_plot Object, name of column of your tidy df to plot on x axis.
#' @param y_plot Object, name of column of your tidy df to plot on y axis.
#' @param fill_plot Object, name of column of your tidy df to scale.
#' @param labs_x Character label.
#' @param labs_y Character label.
#' @param labs_capt  Character label.
#' @param labs_title Character label.
#' @return ggplot/geom_tile/theme_minimal/them/labs.

# graph tile ----
gg_tile <- function(tidy_df,
                    x_plot, y_plot, fill_plot, # noms objets ds le df_tidy
                    labs_x, labs_y, labs_capt, labs_title){ # chaine de caracteres
 ggplot(data=tidy_df,
        mapping = aes(x=!!x_plot, y=!!y_plot))+
  geom_tile(aes(fill = !!fill_plot), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_equal()+
  theme_minimal() +
  theme(
   plot.title = element_text(face = "bold", hjust = 0.5, size=8, vjust = 40),
   legend.position = "bottom",
   legend.title=element_text(face = "bold", hjust = 0.5, size=8),
   legend.text=element_text(size=8,vjust =1,  hjust = 1),
   axis.text.x = element_text(vjust=1, angle=90, hjust = 1, size = 8, colour = "gray40"),
   axis.title.x=element_text(size=8),
   axis.title.y=element_text(size=8)
  )+
  labs(x = labs_x, y = labs_y,
       caption = labs_capt,
       title = labs_title
  )

}

# graph bar ----

#' @export
#' @title Proportion Email/Spam.
#' @description Barplot volume + poucentage.
#'
#' Data input is data frame.
#' @import ggplot2
#' @param df Data frame object.
#' @param y_plot Object, name of column of your data frame.
#' @param labs_capt  Character label.
#' @param labs_title Character label.
#' @return ggplot/geom_bar/theme_minimal/theme/labs.

gg_bar <- function(df, y_plot, labs_capt, labs_title){
  ggplot(df)+
    geom_bar(aes(x=!!y_plot, fill= ifelse(!!y_plot=="0", "Email", "Spam") ))+
    geom_text(aes(x=!!y_plot, label=stat(count)),
              stat='count',
              vjust=-0.9
    )+
    geom_text(
      aes(x=!!y_plot, label=paste0(sprintf("%.f",stat(prop)*100),"%"), group=1),
      stat='count',
      vjust=10
    )+
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size=12),
      legend.title=element_blank(), #element_text(face = "bold", hjust = 0.5, size=8),
      legend.text=element_text(size=8,vjust =1,  hjust = 1),
      axis.text.x = element_text(vjust=1, angle=0, hjust = 1, size = 8, colour = "gray40"),
      axis.title.x=element_text(size=8),
      axis.title.y=element_text(size=8)
    )+
    labs(x =  element_blank(), y = element_blank(),
         caption = labs_capt ,
         title = labs_title
    )

}


# graph bar capital letters ----

#' @export
#' @title Average bar plot by group Email/Spam.
#' @description Barplot, numbers of capital letters (average, longest, total).
#'
#' Data (tidy format) input is data frame.
#' @import ggplot2
#' @param tidy_df Data frame object.
#' @param x_plot Object, name of column of your data frame.
#' @param y_plot Object, name of column of your data frame (response variable).
#' @param fill_plot Object, name of column of your data frame (value).
#' @param labs_capt  Character label.
#' @param labs_title Character label.
#' @return ggplot/geom_bar/theme_minimal/theme/labs.

gg_bar_group <- function(tidy_df,
                         x_plot, y_plot, fill_plot, # noms objets ds le df_tidy
                         labs_capt, labs_title){

  ggplot(tidy_df)+
    geom_bar(aes(fill=!!y_plot, x=!!x_plot, y=!!fill_plot),
             position =  position_dodge(width = 0.5), width = 0.4,
             stat='identity')+
    geom_text(aes(x=!!x_plot,
                  y=!!fill_plot,
                  label=paste0(round(!!fill_plot,3)),
                  group=!!y_plot),
              stat = "identity",
              position = position_dodge(width = 0.5),
              vjust=-0.5)+
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size=12),
      legend.title=element_blank(), #element_text(face = "bold", hjust = 0.5, size=8),
      legend.text=element_text(size=8,vjust =1,  hjust = 1),
      axis.text.x = element_text(vjust=1, angle=0, hjust = 1, size = 8, colour = "gray40"),
      axis.title.x=element_text(size=8),
      axis.title.y=element_text(size=8)
    )+
    labs(x =  element_blank(), y = element_blank(),
         caption = labs_capt ,
         title = labs_title
    )
}
