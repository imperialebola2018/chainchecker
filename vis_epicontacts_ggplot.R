#' visualise epicontacts object using ggplot and plotly
#' 
#' @param x epicontacts object
#' @param group group to adjust colour of nodes by, defaults to NA
#' 
#' @return plot of transmissin tree
#' @export

vis_epicontacts_ggplot = function(x,
                                  group = "onset",
                                  contactsgroup = NA,
                                  anon = TRUE,
                                  serial = 4){
  if(anon){
    tooltip = c("onset",  "code", group)
  } else {
    tooltip = c("onset", "id", "fullname", "code", group)
  }
  
  
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
  
  #add contact lines
  g = g + geom_segment(data = rank_contacts,
                       aes( x= to_onset,
                            xend = from_onset,
                            y = to,
                            yend = to),
                       colour = ifelse(rank_contacts$from_onset-rank_contacts$to_onset>0, 
                                       "red", 
                                       ifelse(rank_contacts$from_onset-rank_contacts$to_onset>-serial,
                                              "darkorange3",
                                              "black")))
  if(is.na(contactsgroup) | is.null(contactsgroup) | !contactsgroup %in% names(rank_contacts)){
    g = g + geom_segment(data = rank_contacts,
                         aes( x= from_onset,
                              xend = from_onset,
                              y = to,
                              yend = from))
  } else {
    g = g + geom_segment(data = rank_contacts,
                         aes( x= from_onset,
                              xend = from_onset,
                              y = to,
                              yend = from),
                         colour = ifelse(rank_contacts[,contactsgroup], "blue", "black"),
                         size = ifelse(rank_contacts[,contactsgroup], 2, 0.5),
                         alpha = ifelse(rank_contacts[,contactsgroup], 0.5, 1))
  }
  #add points
  
  g = g + geom_point(data = linelist,
                     aes_string(x = "onset",
                                y = "rank",
                                fill = group,
                                id = "id",
                                fullname = "name",
                                code = "code"), 
                     size = 5, 
                     shape = 21)
  
  #change the apperance
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
  
  g
  # plotly::subplot(m ,g, nrows = 2, heights = c(0.2, 0.8), 
  #                 shareX = TRUE)
}



#' swap contact ids for 'ranking' (the order with which to plot nodes on the y axis)
#' 
#' @param x epicontacts object
#' 
#' @return rank_contacts and ordered linelist
#' @export

fun_rank_contacts = function(x){
  
  #add clusters
  id_to_cluster = fun_get_clusters(x)
  
  #add trees
  id_to_cluster = fun_get_trees(id_to_cluster)
  
  #link id_to_cluster and linelist and order
  x = fun_link_linelist_cluster(x, id_to_cluster)
  
  #add a rank based on the ordering, distributed by tree
  x = fun_rank_linelist(x)
  
  #use this rank instead of id
  rank_contacts = x$contacts
  
  contact_order_to = match(rank_contacts$to, x$linelist$id)
  rank_contacts$to = x$linelist$rank[contact_order_to]
  
  contact_order_from = match(rank_contacts$from , x$linelist$id)
  rank_contacts$from = x$linelist$rank[contact_order_from]
  
  #add rank onsets
  rank_contacts$to_onset = x$linelist$onset[contact_order_to]
  rank_contacts$from_onset = x$linelist$onset[contact_order_from]
  
  return(list(rank_contacts = rank_contacts, linelist = x$linelist))
}



#' add extra column stating which cluster each individual is in
#' 
#' @param x epicontacts object including both linelist and contacts
#' 
#' @return the contacts with attached onset and cluster
#' @export

fun_get_clusters = function(x){
  
  
  #add onset dates of "from"
  id_to_cluster = cbind(x$contacts, 
                        x$linelist$onset[match(x$contacts$from, 
                                               x$linelist$id)])
  names(id_to_cluster)[ncol(id_to_cluster)] = "onset"
  
  #order by onset of "from"
  id_to_cluster = dplyr::arrange(id_to_cluster, onset )
  
  #get index cases of each cluster
  cluster_index_case = unique(id_to_cluster$from)
  
  #define cluster
  id_to_cluster$cluster = rep(NA, nrow(id_to_cluster))
  for(i in  1 : length(cluster_index_case) ){
    
    from_ind = which(id_to_cluster$from %in% cluster_index_case[i])
    to_ind = which(id_to_cluster$to %in% cluster_index_case[i])
    
    id_to_cluster$cluster[from_ind] = i
    id_to_cluster$cluster[to_ind] = i
    
  }
  
  return(id_to_cluster)
}



#' add extra column stating which tree each individual is in
#' 
#' @param id_to_contacts output of fun_get_cluster
#' 
#' @return the contacts with attached onset, cluster and tree
#' @export

fun_get_trees = function(id_to_cluster){
  
  #find index cases
  index_cases = unique( id_to_cluster$from[ which(!id_to_cluster$from %in% id_to_cluster$to)])
  
  n_trees = length(index_cases)
  
  id_to_cluster$tree = rep(NA, nrow(id_to_cluster))
  for(t in 1:n_trees){
    
    tree_from = index_cases[t]
    
    while(length(tree_from)>0){
      # primary, secondary etc. infections
      
      id_to_cluster$tree[ which(id_to_cluster$from %in% tree_from)] = t 
      
      tree_from = id_to_cluster$to[which(id_to_cluster$from %in% tree_from)]
      
    }
    
  }
  
  return(id_to_cluster)
}

#' join cluster and trees to linelist and order
#' 
#' @param x epicontacts object
#' @param id_to_cluster contacts with onset, cluster and trees
#' 
#' @return x with an ordered linelist
#' @export

fun_link_linelist_cluster = function(x, id_to_cluster){
  
  #order linelist by onset
  x$linelist = dplyr::arrange(x$linelist, desc(onset) )
  
  #link to clusters in id_to_cluster
  x$linelist$cluster = id_to_cluster$cluster[match( x$linelist$id, id_to_cluster$to)]
  
  #cases outside clusters will be NA
  x$linelist$cluster[is.na(x$linelist$cluster) & 
                       !x$linelist$id %in% x$contacts$from] = 
    max(x$linelist$cluster, na.rm=TRUE)+1
  
  #cases that are only index cases will also be NA- these need to be linked to their cluster
  missing_ind = which(is.na(x$linelist$cluster) & x$linelist$id %in% x$contacts$from)
  for(i in missing_ind){
    x$linelist$cluster[i] = id_to_cluster$cluster[match(x$linelist$id[i], id_to_cluster$from)]
  }
  
  
  #Link to trees in id_to_cluster
  x$linelist$tree = id_to_cluster$tree[match( x$linelist$id, id_to_cluster$to)]
  
  #index cases 
  missing_ind = which(is.na(x$linelist$tree) & x$linelist$id %in% x$contacts$from)
  for(i in missing_ind){
    x$linelist$tree[i] = id_to_cluster$tree[match(x$linelist$id[i], id_to_cluster$from)]
  }
  
  #cases outside trees will be NA
  x$linelist$tree[is.na(x$linelist$tree) ] = 
    c( (max(x$linelist$tree, na.rm=TRUE) + 1) : 
         (max(x$linelist$tree, na.rm=TRUE) + length(x$linelist$tree[is.na(x$linelist$tree) ])) )
  
  #order by cluster
  x$linelist = dplyr::arrange(x$linelist, (cluster))
  
  #order by tree
  x$linelist = dplyr::arrange(x$linelist, (tree))
  
  return(x)
}

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
  
  n_points = nrow(x$linelist)
  
  #declare
  x$linelist$rank = rep(NA, nrow(x$linelist))
  for(t in 1:n_trees){
    #get number of individuals in tree
    tree_size = nrow( dplyr::filter(x$linelist, tree == t))
    
    #previous points
    tree_previous_size = nrow( dplyr::filter(x$linelist, tree < t))
    
    
    #get area to distribute points over
    tree_area = c((max_space/n_trees)*tree_previous_size, 
                  (max_space/n_trees)*(tree_size + tree_previous_size) )
    
    if(tree_size == 1){
      x$linelist$rank[ x$linelist$tree == t] = -1
    } else {
      #if only two points, put in the middle
      if(tree_size==2){
        x$linelist$rank[ x$linelist$tree == t] = mean(tree_area)
      } else {
        x$linelist$rank[ x$linelist$tree == t] = seq(from = tree_area[1]+1, 
                                                     to = tree_area[2], #so there is a buffer
                                                     length.out = tree_size)
      }
    }
  }
  
  #for the unconnected cases, we need to stack by onset date
  if(min(x$linelist$rank)>-1){
    
  } else{
    unconnected = which(x$linelist$rank == -1)
    no_unconnected = length(unconnected)
    for(u in 2:no_unconnected){
      if( x$linelist$onset[unconnected[u]]==x$linelist$onset[unconnected[u-1]] &
          !is.na(x$linelist$onset[unconnected[u]]) & 
          !is.na(x$linelist$onset[unconnected[u-1]])){
        x$linelist$rank[unconnected[u]] = x$linelist$rank[unconnected[u-1]] - 100/nrow(x$linelist)
      }
    }
  }
  return(x)
}
