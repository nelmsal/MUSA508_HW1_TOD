multi_ring_buffer


# ********************************

multiringbuffers_bymatrix = function(
  input.points, # dataframe
  buffer_distance=.5,
  buffer_max=5,
  id_field = 'stop_id'
){
  # BUFFER DISTANCES VECTOR
  buffer_distances = seq(
    from=buffer_distance, to=buffer_max,by=buffer_distance)
  
  # POINT ID VECTOR
  input.id = input.points[[id_field]]
  # POINT GEOMETRY VECTOR
  input.geoms = input.points$geometry

  # FUNCTION TO BUFFER AND CREATE RINGS
  ring_buffer_points = function(input.geom, buffer.distance){
    diff_buff = buffer.distance-buffer_distance
    
    input.difference =
      input.geom %>% st_buffer(diff_buff*mile) %>%
      st_union()
    input.buffer =
      input.geom %>% st_buffer(buffer.distance*mile) %>%
      st_difference(input.difference) %>% st_union()
    return(input.buffer)
  }

  # POINT GEOMETRY AND BUFFER DISTANCE VECTORS FEED INTO A RING BUFFER FUNCTION
  # CREATING A MATRIX OF DISTANCE COLUMNS BY POINT ID ROWS
  input.buffers = outer(
    input.geoms, 
    buffer_distances,
    FUN=Vectorize(ring_buffer_points))
  
  # CREATE MATRIX COLUMN & ROW NAMES
  rownames(input.buffers) = input.id
  colnames(input.buffers) = buffer_distances
  
  # UNION ROW
  ALL =
    apply(input.buffers, 2, function(geom_row)
      geom_row %>% st_sfc() %>% st_union()) %>% as.vector()
  input.buffers = rbind(input.buffers, ALL) %>% data.frame()

  # REFORMAT MATRIX TO PLACE ALL GEOMETRIES INTO ONE COLUMN
  input.transpose =
    input.buffers %>%
    gather(key="distance", value="geometry")
  input.id.union = c(input.id, c("ALL"))
  input.transpose[[id_field]] =
    rep.int(input.id.union, length(buffer_distances))
  
  # FIXES DISTANCE FIELD THAT HAS A RANDOM X AFTER GATHER
  input.transpose$distance = gsub('X', '', input.transpose$distance)
  input.transpose$distance = as.numeric(input.transpose$distance)
  
  return(input.transpose)
}

input.points = BART.stops
id_field = 'stop_id'

buffer_distance=.5
buffer_max=5
buffer_distances = seq(
  from=buffer_distance, to=buffer_max,by=buffer_distance)

input.id = input.points[[id_field]]
input.geoms = input.points$geometry

ring_buffer_points = function(input.geom, buffer.distance){
  diff_buff = buffer.distance-buffer_distance
  
  input.difference =
    input.geom %>% st_buffer(diff_buff*mile) %>%
    st_union()
  input.buffer =
    input.geom %>% st_buffer(buffer.distance*mile) %>%
    st_difference(input.difference) %>% st_union()
  return(input.buffer)
}

input.buffers = outer(
  input.geoms, 
  buffer_distances,
  FUN=Vectorize(ring_buffer_points))

rownames(input.buffers) = input.id
colnames(input.buffers) = buffer_distances
ALL =
  apply(input.buffers, 2, function(geom_row)
    geom_row %>% st_sfc() %>% st_union()) %>% as.vector()
input.buffers = rbind(input.buffers, ALL) %>% data.frame()

input.transpose =
  input.buffers %>%
  gather(key="distance", value="geometry")
input.id.union = c(input.id, c("ALL"))
input.transpose[[id_field]] =
  rep.int(input.id.union, length(buffer_distances))
input.transpose$distance = gsub('X', '', input.transpose$distance)
input.transpose$distance = as.numeric(input.transpose$distance)

# ********************************

qBr <- function(
  df, variable, round=3, probs=c(.01,.2,.4,.6,.8)
                ) {
  field = df[[variable]]
  quants = quantile(
    field, probs = probs, 
    na.rm=T) %>% 
    round(., digits=round)
  return(as.vector(quants))
}

labeling = function(breaks){return(
  format(as.list(breaks),
         digits=ifelse(min(breaks)>=1, 0, 2),
         big.mark=",",scientific=FALSE))}

label_cut = function(breaks_cut){
  
  return(labels)
}
