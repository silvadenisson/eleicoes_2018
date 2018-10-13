tema_br_dados <- function (tam.fonte = 16, fonte = "sans", angle.x = 360, leg_pos = "bottom"){
  
  theme_bw(base_size = tam.fonte, base_family = fonte) + 
     theme(axis.text = element_text(colour = "#042971", size = rel(0.9)), 
           axis.text.y = element_text(hjust = 1), 
           axis.text.x = element_text(vjust = 1, angle = angle.x), 
           axis.title = element_text(colour = "#042971"),
           axis.title.y = element_text(angle = 90, vjust = 1.2),
           axis.title.x = element_text(vjust = -0.2), 
           axis.line.x = element_line(color = "#449847"),
           axis.line.y = element_line(color = "#449847"), 
           axis.ticks = element_blank(),
           legend.position = leg_pos, 
           legend.title=element_blank(), 
           strip.background = element_blank(), 
           panel.grid = element_blank(), 
           panel.border = element_blank(), 
           panel.background = element_blank(), 
           plot.background = element_blank(), 
           plot.title = element_text(lineheight = 0.9, color = "#449847", face = "bold", size = rel(1)),
           plot.subtitle = element_text(lineheight = 0.6, color = "#042971", face = "bold", size = rel(0.7)),
           plot.caption = element_text(color = "#FBDE4B", face = "bold")
           )
}
