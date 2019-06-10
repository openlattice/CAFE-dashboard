get_node_table <- function(cafename, apis) {
    master_all_entsets <- apis$master$edmApi$get_all_entity_sets()
    
    # get master data
    
    entsetnames <-
        master_all_entsets %>% filter(str_detect(name, paste0("CAFE_.{0,10}", cafename, "$"))) %>% pull(name)
    
    entsets <- tibble()
    
    for (entsetname in entsetnames) {
        # load data
        entid <- apis$master$edmApi$get_entity_set_id(entsetname)
        dat <- apis$master$dataApi$load_entity_set_data(entid)
        
        if (length(dat$"openlattice.@id") == 1) {
            dat <-
                dat %>% lapply(function(x) {
                    x <- gsub("NULL", NA, paste(x))
                }) %>% as_tibble()
            dat['study'] <- entsetname
        } else if (length(dat$"openlattice.@id") == 0) {
            dat <- tibble()
        } else{
            dat <-
                dat %>% sapply(function(x) {
                    x <- gsub("NULL", NA, paste(x))
                }) %>% as_tibble()
            dat['study'] <- entsetname
        }
        
        
        # bind to table
        
        entsets <- bind_rows(entsets, dat)
    }
    
    return(entsets)
    
}


# transform_edges <- function(table) {
#     # with the entity key ids to link data tables
#     
#     table <- table[['neighborDetails']]
#     
#     if (length(table$"openlattice.@id") == 1) {
#         table <-
#             table %>% lapply(function(x) {
#                 x <- gsub("NULL", NA, as.character(x))
#             }) %>% as_tibble()
#     } else {
#         table <-
#             table %>% sapply(function(x) {
#                 x <- gsub("NULL", NA, as.character(x))
#             }) %>% as_tibble()
#     }
#     
#     newtable <- table %>% select('openlattice.@id')
#     return (newtable)
# }

get_edge_table <- function(cafeedge, datasets, apis) {
    master_all_entsets <- apis$master$edmApi$get_all_entity_sets()
    
    # get entity set ids for this edge
    
    entsetids <- list()
    for (part in c("src", 'edge', 'dst')) {
        entsetnames <-
            master_all_entsets %>% filter(str_detect(name, paste0("CAFE_.{0,10}", cafeedge[part], "$"))) %>% pull(name)
        entsetids[[part]] <-
            entsetnames %>% map_chr(apis$master$edmApi$get_entity_set_id)
    }
    
    if (length(entsetids$src)==0 || length(entsetids$edge)==0 || length(entsetids$dst)==0) {
        return (tibble())
    }
    
    # get entity keys for source and filter
    
    if (!'openlattice.@id' %in% names(datasets[[cafeedge$src]])) {
        return (tibble())
    }
    
    src_entkeys <-
        datasets[[cafeedge$src]] %>% pull('openlattice.@id')
    
    filter = NeighborSearchFilter$new(
        entityKeyIds = src_entkeys,
        edge = entsetids[['edge']],
        dst = entsetids[['dst']],
        src = entsetids[['src']]
    )
    
    # get edges
    
    edges_table <- tibble()
    
    for (src_id in entsetids[['src']]) {
        edges_cafeedge <-
            apis$master$searchApi$execute_filtered_entity_neighbor_id_search(src_id, filter)
        if (!is.null(edges_cafeedge$response$status_code)) {
            print(paste0("Error for search src_id ", src_id))
            next
        }
        
        if (length(edges_cafeedge) == 0){
            entset = apis$master$edmApi$get_entity_set(src_id)
            print(
                        paste0(
                            "No edges for dataset ",
                            entset$name,
                            " in ",
                            cafeedge['src'],
                            " --> ",
                            cafeedge['edge'],
                            " --> ",
                            cafeedge['dst']
                        )
            )
            next
        }

        edges_trans = edges_cafeedge %>% 
            enframe('srcId', 'tbl') %>%
            unnest(tbl, .preserve = srcId) %>% 
            unnest(tbl, .preserve = srcId) %>% 
            unnest() %>%
            select(-c("src")) %>%
            rename(src = srcId, dst = neighborId, edge = associationId) %>%
            select(src, dst)
        
        # bind_rows(edges_cafeedge)  %>% rename() %>% mutate(srcId = unnest())
        # 
        # edges_cafeedge %>% 
        #     enframe('src_id', 'tbl') %>%
        #     unnest(tbl, .preserve = src_id) %>% 
        #     unnest(tbl, .preserve = src_id)
        # 
        # # edges_trans <-
        # #     edges_cafeedge %>% map(transform_edges) %>% map2_dfr(names(edges_cafeedge), ~ mutate(.x, name =
        # #                                                                                              .y))
        # edges_trans <- edges_cafeedge %>% 
        #     map2_dfr(names(edges_cafeedge), ~ mutate(.x, src = .y)) %>%
        #     select(src, dstEntityKeyId) %>%
        #     rename(dst = dstEntityKeyId)
        
        # if (dim(edges_trans)[1] == 0) {
        #     entset = apis$master$edmApi$get_entity_set(src_id)
        #     print(
        #         paste0(
        #             "No edges for dataset ",
        #             entset$name,
        #             " in ",
        #             cafeedge['src'],
        #             " --> ",
        #             cafeedge['edge'],
        #             " --> ",
        #             cafeedge['dst']
        #         )
        #     )
        #     next
        # }
        
        edges_table <- bind_rows(edges_table, edges_trans)
        
    }
    
    return (edges_table)
}
