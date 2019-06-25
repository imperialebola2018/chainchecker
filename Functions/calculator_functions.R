##################################
### ONSET CALCULATOR FUNCTIONS ###
##################################

### calculator logic steps ###
fun_get_onset = function(input,
                         default_to_death_date = TRUE){
  
  df = tibble("id" = input$id)
  
  if(default_to_death_date){
    #then add other dates
    if(input$death_avail){ #if there is a death date
      
      df %<>% add_column(death_date = input$death_date,
                             onset_date = as.Date(input$death_date - input$days_onset_to_death),
                             reported_onset_date = input$reported_onset_date)
      
    } else { #no death date
      df %<>% add_column(death_date = input$death_date,
                             reported_onset_date = input$reported_onset_date)
      
      if(input$bleeding_at_reported_onset){ #with bleeding
        df %<>% add_column( onset_date = as.Date(df$reported_onset_date - 
                                                       input$days_onset_to_bleeding))
        
      } else if(input$diarrhea_at_reported_onset){ #with diarrhea
        df %<>% add_column( onset_date = as.Date(df$reported_onset_date - 
                                                       input$days_onset_to_diarrhea))
        
      } else { #no wet symptoms
        df %<>% add_column( onset_date = df$reported_onset_date)
      }
    }
  } else {
    df %<>% add_column( onset_date = input$reported_onset_date,
                            death_date = input$death_date,
                            reported_onset_date = input$reported_onset_date)
  }
  
  #calculate exposure period
  df %<>% add_column( exposure_date_min = as.Date(df$onset_date - input$max_incubation),
                          exposure_date_max = as.Date(df$onset_date - input$min_incubation))
  
  
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
### import and adjust ###
fun_import_adjust = function(input,
                             default_to_death_date = TRUE){
  
  #import and check
  df = check_line_upload(input$file_line)
  
  #format
  df %<>% add_column( days_onset_to_bleeding = input$days_onset_to_bleeding_all,
                          days_onset_to_diarrhea = input$days_onset_to_diarrhea_all,
                          max_incubation = input$max_incubation_all,
                          min_incubation = input$min_incubation_all,
                          days_onset_to_death = input$days_onset_to_death_all,
                          death_avail = !is.na(df$death_date))
  
  if(is.null(df$bleeding_at_reported_onset)){
    df %<>% add_column(bleeding_at_reported_onset = FALSE)
  } else if(is.na(all(df$bleeding_at_reported_onset))){
    df$bleeding_at_reported_onset = FALSE
  }
  if(is.null(df$diarrhea_at_reported_onset)){
    df %<>% add_column(diarrhea_at_reported_onset = FALSE)
  } else if(is.na(all(df$diarrhea_at_reported_onset))){
    df$diarrhea_at_reported_onset = FALSE
  }
  
  #get onset
  df_out = NULL
  for(i in 1:nrow(df)){
    df_out = rbind(df_out, fun_get_onset(df[i,], default_to_death_date))
  }
  
  df %<>% 
    add_column( onset_date = df_out$onset_date, 
                exposure_date_min = df_out$exposure_date_min, 
                exposure_date_max = df_out$exposure_date_max,
                .after = "reported_onset_date") 
  
  return(df)
}

#### ----------------------------------------------------------------------------------- ####
##function to create clusters and calculate size, then add to linelist###
cluster_add_func <- function(linelist, contacts, input) {

  
  x <- epicontacts::make_epicontacts(linelist, contacts)
  xClust <- subset(x, cs_min = 0, cs_max = 300)
  xClust <- thin(xClust, what = "contacts")
  xClust$directed <- F
  cGraph <- cluster_fast_greedy(as.igraph(xClust))
  cGraph <- as.data.frame(cbind(cGraph$names, cGraph$membership))
  x$linelist <- merge(x$linelist,
                      cGraph,
                      by.x = "id",
                      by.y = "V1",
                      all = T)
  
  
  x$linelist$V2 <- paste0("cl_", as.character(x$linelist$V2))
  x$linelist$V2[x$linelist$V2 == "cl_NA"] <- "cl_0"
  names(x$linelist)[names(x$linelist) == 'V2'] <- 'clMembership'
  clustSize <- as.data.frame(table(x$linelist$clMembership))
  colnames(clustSize) <- c("member", "clSize")
  x$linelist <-
    merge(x$linelist, clustSize, by.x = "clMembership", by.y = "member")
  x$linelist <- x$linelist[, c(2:ncol(x$linelist), 1)]
  degs <- as.data.frame(get_degree(x))
  degs$id <- rownames(degs)
  x$linelist <- merge(x$linelist,
                      degs,
                      by.x = "id",
                      by.y = "id",
                      all = T)
  names(x$linelist)[names(x$linelist) == 'get_degree(x)'] <-
    'degrees'
  x$linelist$degrees <-
    ifelse(is.na(x$linelist$degrees), 0, x$linelist$degrees)
  x$linelist$clMembership <-
    ifelse(x$linelist$degrees == 0, "cl_NA", x$linelist$clMembership)
  x <- x[!is.na(x$linelist$clMembership)]
  lookup <- as.data.frame(unique(x$linelist$clMembership))
  lookup$cluster_number <-
    c("cl_NA", paste0("cl_", c(1:(nrow(
      lookup
    ) - 1))))
  names(lookup)[names(lookup) == 'unique(x$linelist$clMembership)'] <-
    'clMembership'
  x$linelist <-
    merge(x$linelist, lookup, by.x = "clMembership", by.y = "clMembership")
  x$linelist <- x$linelist[, -1]
  
  #rename the NA so they have a number
  x$linelist$cluster_number[x$linelist$cluster_number == "cl_NA"] = paste0("cl_", length(unique(x$linelist$cluster_number)))
  
  
  return(x$linelist)
}

#### ----------------------------------------------------------------------------------- ####
### function to make tree if data is uploaded ###
fun_make_tree = function(input, type = "timeline"){
  
  linelist = fun_import_adjust(input,
                               default_to_death_date = input$adjust_tree)
  
  contacts = check_contacts_upload(input$file_contact)
  
  
  linelist = cluster_add_func(linelist,contacts, input)
  
  
 #covering extras for vis_epicontacts_ggplot
  if(is.null(linelist$name)){ linelist %<>% mutate(name = id)}
  if(is.null(linelist$code)){ linelist %<>% mutate(code = id)}
  
  #check links are feasible
  contacts = check_exposure_timeline(linelist, contacts, input)
  
  contacts[is.na(contacts)] = FALSE
  
  #adjust for epicontacts
  names(linelist)[names(linelist) == "onset_date"] = "onset"
  
  
  #make epicontacts
  x = epicontacts::make_epicontacts(linelist, contacts)
  
  if(type=="timeline"){
  #visualise
  p = vis_epicontacts_ggplot(x,
                             group = input$group, 
                             contactsgroup = input$groupcontact,
                             tooltip = unlist(strsplit(paste(input$tooltip), ","))) %>% 
    layout(height = 700)

  return(p)
  }
  
  #visualise
  if (type =="network") {
    #make epicontacts
    x1<- as.igraph(x)
    
    p =  ggnet2(x1,
                        node.color = "white",
                        size="degree",
                        alpha = 0.75, 
                        edge.alpha = 0.5,
                        legend.position  ="none")
    
    p$data<-merge(p$data,x$linelist,by.x="label",by.y="id")#ggnet is a pain, so need to add cluster degree size details manually to get a filled circle...
    p$data$size<-3+(p$data$size / max(p$data$size)) * 10##scale sizes so they are relative
    
    cols<-grDevices::rainbow(length(unique(p$data$cluster_number)))#get cluster names for colours
    colPal<-setNames(cols, unique(p$data$cluster_number))#set colour pallet
    colPal[]<-ifelse(names(colPal)=="cl_NA","lightgrey",colPal)#change cl_na to grey
    
    group_name=as.vector(p$data$cluster_number)
    id=as.vector(p$data$label)
    onset=as.vector(p$data$reported_onset_date)
    #HCW=as.vector(p$data$HCW)
    #sex=as.vector(p$data$Gender)
    #age=as.vector(p$data$Age)
    #caseId_source=as.vector(p$data$caseId_source)
    group=input$group
    
    p=p+geom_point(aes(x = p$data$x,
                       y = p$data$y,
                       fill=group_name,
                       id=id,
                       onset=onset), size = p$data$size, alpha = 0.7,color="black",shape=21)+
      scale_fill_manual(values = colPal)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    return(ggplotly(p,tooltip=c("id","onset","sex","age","caseId_source","fill","HCW",group)))
  }
  
  if (type =="table") {
    return(datatable(linelist[,c(names(linelist))],rownames = FALSE, extensions = c('Buttons'),
                     filter = 'top', options = list(
      dom = 'Bfrtip',
      scrollX = TRUE,
      buttons = I("colvis")
    )
    ) )
  }
  
}

#### ----------------------------------------------------------------------------------- ####
### function to plot exposure windows ###
fun_plot_exposure_windows = function(df, height){
  
  g = ggplot(df, aes(text = paste0("ID: ",id))) 
  
  
  g = g + geom_rect(aes(xmin = exposure_date_min,
                        xmax = exposure_date_max,
                        ymin = reorder(id, exposure_date_min), 
                        ymax = reorder(id, exposure_date_min),
                        color = "Exposure"),
                    size = 1.1) +
    geom_point( aes( x = death_date,
                     y = reorder(id, exposure_date_min),
                     color = "Death"),
                shape = ifelse(df$dates_in_correct_order != TRUE,
                               "square",
                               "circle"),
                size = 5) +
    geom_point( aes( x = exposure_date_min,
                     y = reorder(id, exposure_date_min),
                     color = "Exposure"), size = 0.1) +
    geom_point( aes( x = exposure_date_max,
                     y = reorder(id, exposure_date_min),
                     color = "Exposure"), size = 0.1) +
    geom_point(aes(x = onset_date,
                   y = reorder(id, exposure_date_min),
                   color = "Estimated onset"),
               size = 5) +
    geom_point(aes(x = reported_onset_date,
                   y = reorder(id, exposure_date_min),
                   color = "Reported onset"),
               size = 5, shape = 4, stroke = 2) +
    ylab("Identifier") +
    labs(colour = "Key", shape = NULL)+
    theme(panel.background = element_rect(fill = "white", colour = "grey50"),
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Date")
  
  p = plotly::ggplotly(g, height = height, tooltip = c("x", "text" )) 
  
  return(p)
}
