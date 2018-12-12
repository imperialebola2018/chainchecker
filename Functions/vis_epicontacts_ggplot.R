#' visualise epicontacts object using ggplot and plotly
#' 
#' @param x epicontacts object
#' @param group group to adjust colour of nodes by, defaults to NA
#' @param contactsgroup group to adjust colour of links by, defaults to NA
#' @param anon whether to show id on hover, defaults to TRUE
#' @param serial minimum serial interval for link highlighting
#' @param with_epicurve whether to plot epicurve as part of output, defaults to FALSE
#' 
#' @return plot of transmissin tree
#' @export

vis_epicontacts_ggplot = function(x,
                                  group = "onset",
                                  contactsgroup = NA,
                                  tooltip,
                                  with_epicurve = FALSE){

  
  
  # rank contacts
  out = fun_rank_contacts(x)
  rank_contacts = out$rank_contacts
  linelist = out$linelist
  
  #start plot
  g = ggplot()
  
  #adding line at zero
  g = g + geom_abline(slope = 0, 
                      intercept = -0.5, 
                      color = "grey", 
                      size = 3, 
                      alpha = 0.5)
  

  if(contactsgroup %in% names(rank_contacts)){ #highlight particular links
    g = g + geom_segment(data = rank_contacts,
                         aes( x= from_onset,
                              xend = from_onset,
                              y = to,
                              yend = from),
                         colour = ifelse(rank_contacts[,contactsgroup], "orange", "black"),
                         size = ifelse(rank_contacts[,contactsgroup], 2, 0.5),
                         alpha = ifelse(rank_contacts[,contactsgroup], 0.8, 1))
    
    g = g + geom_segment(data = rank_contacts,
                         aes( x= to_onset,
                              xend = from_onset,
                              y = to,
                              yend = to),
                         colour = ifelse(rank_contacts[,contactsgroup], "orange", "black"),
                         size = ifelse(rank_contacts[,contactsgroup], 2, 0.5),
                         alpha = ifelse(rank_contacts[,contactsgroup], 0.8, 1))
  }
  #add contact lines
  g = g + geom_segment(data = rank_contacts,
                       aes( x= to_onset,
                            xend = from_onset,
                            y = to,
                            yend = to))
  
  
  g = g + geom_segment(data = rank_contacts,
                       aes( x= from_onset,
                            xend = from_onset,
                            y = to,
                            yend = from))
  
  
  
  #add points
  g = g + geom_point(data = linelist,
                     aes_string(x = "onset",
                                y = "rank",
                                fill = group,
                                text1 = tooltip[1],
                                text2 = tooltip[2],
                                text3 = tooltip[3],
                                text4 = tooltip[4],
                                text5 = tooltip[5]), 
                     size = 5, 
                     shape = 21)
  
  #change the appearance
  g = g + xlab("Symptom onset date") + 
    ylab("") +
    labs(fill = "")+
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") 
  
  
  g = ggplotly(g, tooltip = tooltip) 
  
  #add epicurve
  m = ggplot(linelist) + 
    geom_histogram( aes_string(x = "onset", 
                               y = "..count..",
                               fill = group) ) +
    theme(axis.ticks.y = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.text.x = element_blank()) 
  
  
  m = ggplotly(m, tooltip = c(group))
  
  if(with_epicurve){
    subplot(m ,g, nrows = 2, heights = c(0.2, 0.8), shareX = TRUE)
  } else {
    g
  }
}


#-------------------------------------------------------------------------------------------------#
#' swap contact ids for 'ranking' (the order with which to plot nodes on the y axis)
#' 
#' @param x epicontacts object
#' 
#' @return rank_contacts and ordered linelist
#' @export

fun_rank_contacts = function(x){
  
  #add clusters
  df = fun_get_clusters(x)
  
  #add trees
  df = fun_get_trees(df)
  
  #link id_to_cluster and linelist and order
  x = fun_link_linelist_cluster(x, df)
  
  #add a rank based on the ordering, distributed by tree
  x = fun_rank_linelist(x)
  
  #use this rank instead of id
  rc = x$contacts
  
  rc = rc %>% 
    mutate(to = x$linelist$rank[match(rc$to, x$linelist$id)],
           from = x$linelist$rank[match(rc$from , x$linelist$id)]) %>%
    #add rank onsets
    add_column(to_onset = x$linelist$onset[match(rc$to, x$linelist$id)],
               from_onset = x$linelist$onset[match(rc$from , x$linelist$id)])
  
  rc = rc %>% filter(!is.na(to_onset) & !is.na(from_onset))
  
  return(list(rank_contacts = rc, linelist = x$linelist))
}


#-------------------------------------------------------------------------------------------------#
#' add extra column stating which cluster each individual is in
#' 
#' @param x epicontacts object including both linelist and contacts
#' 
#' @return the contacts with attached onset and cluster
#' @export

fun_get_clusters = function(x){
  
  #create df and order
  df = x$contacts %>% 
    add_column(onset = x$linelist$onset[match(x$contacts$from, 
                                              x$linelist$id)],
               cluster = NA) %>%
    arrange(onset)
  
  #get Cluster Index Cases
  cic = unique(df$from)
  
  #assign clusters
  for(i in  1 : length(cic) ){
    df = df %>% mutate(cluster = replace(cluster,
                                         from %in% cic[i] | to %in% cic[i],
                                         i))
  }
  
  return(df)
}


#-------------------------------------------------------------------------------------------------#
#' add extra column stating which tree each individual is in
#' 
#' @param df output of fun_get_cluster
#' 
#' @return the contacts with attached onset, cluster and tree
#' @export

fun_get_trees = function(df){
  
  #check we won't have any infinite loops
  df_out = df[!duplicated(t(apply(df[c("from", "to")], 1, sort))),]
  if(nrow(df_out)<nrow(df)){
    ind1 = which(!duplicated(t(apply(df[c("from", "to")], 1, sort))) == "FALSE")
    ind2 = which(!duplicated(t(apply(df[c("from", "to")], 1, sort)), fromLast = TRUE) == "FALSE")
    stop(paste0("There were contact links defined twice (A->B and B->A). Please check rows ", ind1,
                " and ", ind2,
                " in the contacts."))
  }
  
  #add column
  df = df %>% add_column(tree = NA)
  
  #find index cases for each tree
  ic = unique( df$from[ !df$from %in% df$to ])
  
  
  #trace from every index case - does not work for multiple sources
  for(t in 1:length(ic)){
    
    tree_from = ic[t]
    
    while(length(tree_from)>0){
      # primary, secondary etc. infections
      
      df$tree[ df$from %in% tree_from] = t 
      
      #just in case there are multiple sources
      df$tree[ df$to %in% tree_from] = t 
      
      tree_from = df$to[df$from %in% tree_from]
      
    }
    
  }
  
  return(df)
}

#-------------------------------------------------------------------------------------------------#
#' join cluster and trees to linelist and order
#' 
#' @param x epicontacts object
#' @param df contacts with onset, cluster and trees
#' 
#' @return x with an ordered linelist
#' @export

fun_link_linelist_cluster = function(x, df){
  
  #order linelist by onset
  x$linelist = x$linelist %>% arrange( desc(onset) ) 
  
  ### CLUSTERS ###
  
  #link to clusters in df
  x$linelist = x$linelist %>% 
    add_column(cluster = df$cluster[match( x$linelist$id, df$to)])
  
  
  #unconnected cases will be NA
  x$linelist = x$linelist %>% 
    mutate(cluster = replace(cluster, 
                             is.na(cluster) & !id %in% x$contacts$from,  
                             max(cluster, na.rm=TRUE) + 1))
  
  #cases that are only index cases will also be NA- these need to be linked to their cluster
  x$linelist = x$linelist %>% 
    mutate(cluster = replace(cluster, 
                             is.na(cluster) & id %in% x$contacts$from,  
                             df$cluster[id %in% df$from]))
  
  ### TREES ###
  
  #Link to trees in df
  x$linelist = x$linelist %>% 
    add_column(tree = df$tree[match( x$linelist$id, df$to)])
  
  #index cases
  missing_ind = which(is.na(x$linelist$tree) & x$linelist$id %in% x$contacts$from)
  for(i in missing_ind){
    x$linelist$tree[i] = df$tree[match(x$linelist$id[i], df$from)]
  }
  
  #cases outside trees will be NA
  x$linelist = x$linelist %>% 
    mutate(tree = replace(tree,
                          is.na(tree), 
                          c( (max(tree, na.rm=TRUE) + 1) : 
                               (max(tree, na.rm=TRUE) + length(tree)) ))
    ) %>%
    arrange(cluster, 
            tree)
  
  return(x)
}

#-------------------------------------------------------------------------------------------------#
#' rank based on the trees
#' 
#' @param x epicontacts object
#' 
#' @return x with rankings
#' @export

fun_rank_linelist = function(x){
  
  # distribute by tree
  max_space = 100 # top of the plot (doesn't really matter what this is)
  
  n_trees = max(x$linelist$tree)
  
  #declare
  x$linelist = x$linelist %>% add_column(rank = NA)
  
  for(t in 1:n_trees){
    
    #get number of individuals in tree
    tree_size = nrow( filter(x$linelist, tree == t))
    
    #previous points
    tree_previous_size = nrow( filter(x$linelist, tree < t))
    
    #get area to distribute points over
    tree_area = c((max_space/n_trees) * tree_previous_size, 
                  (max_space/n_trees) * (tree_size + tree_previous_size) )
    
    if(tree_size == 1){
      ranked_tree = -1
    } else if(tree_size==2){
      ranked_tree = mean(tree_area)
    } else {
      ranked_tree = seq(from = tree_area[1]+1,
                        to = tree_area[2], 
                        length.out = tree_size)
    }
    
    x$linelist = x$linelist %>% mutate(rank = replace(rank, tree == t, ranked_tree))
    
  }
  
  #for the unconnected cases, we need to stack by onset date- this works because they are ordered by onset
  if(min(x$linelist$rank)<0){
    
    unconnected = which(x$linelist$rank == -1)
    
    unconnected_onset = x$linelist$onset[unconnected]
    
    for(u in 2:length(unconnected)){
      
      if( x$linelist$onset[unconnected[u]] == x$linelist$onset[unconnected[u-1]] &
          !anyNA(x$linelist$onset[unconnected[c(u, u-1)]]) ){
        
        x$linelist$rank[unconnected[u]] = x$linelist$rank[unconnected[u-1]] - max_space/nrow(x$linelist)
      }
    }
    
  }
  
  return(x)
}
