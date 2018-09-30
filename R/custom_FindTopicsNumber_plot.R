custom_FindTopicsNumber_plot = function(values, b){
  
  library(extrafont)
  library(RColorBrewer)
  
  # normalize to [0,1]
  columns <- base::subset(values, select = 2:ncol(values))
  values <- base::data.frame(
    values["topics"],
    base::apply(columns, 2, function(column) {
      scales::rescale(column, to = c(0, 1), from = range(column))
    })
  )
  
  # melt
  values <- reshape2::melt(values, id.vars = "topics", na.rm = TRUE)
  
  # separate max-arg & min-arg metrics
  values$group <- values$variable %in% c("Griffiths2004", "Deveaud2014")
  values$group <- base::factor(
    values$group,
    levels = c(FALSE, TRUE),
    labels = c("Minimize", "Maximize")
  )
  
  values$variable = factor(values$variable,
                           levels = sort(unique(values$variable)))
  
  # standart plot
  
  p = ggplot(values, aes_string(x = "topics", y = "value", group = "variable", colour = "variable")) + 
    geom_line(size = 1) + 
    scale_colour_manual(name = "Metrics",
                        values = brewer.pal(n = 4, name = "Set1")) + 
    scale_x_continuous(breaks = b) + 
    labs(x = "Number of Topics", 
         y = "Score",
         title = "LDA Tuning") + 
    facet_grid(group ~ .)  + 
    theme(axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 16, face = "bold"),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 14),
          strip.text = element_text(size = 16, face = "bold"),
          text = element_text(family = "Cambria"))
  
  return(p)
  
}