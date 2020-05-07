# this function is developed for sanity checks
# it reconstruct historical lineplots from contact matrices
# @author: Yang Liu
# @date: 12/03/2020

extract_ts <- function(CNTY_CODE, dir, matrix){
  c_no <- which(colnames(matrix$connect[[1]]) == CNTY_CODE)
  r_no <- which(matrix$connect[[1]]$from == CNTY_CODE)
  
  if(dir == "from"){
    sapply(1:nrow(matrix), function(x){
      matrix$connect[[x]][r_no,] %>% 
        unlist() %>% 
        as.numeric %>% 
        .[-1] %>% 
        sum(., na.rm = T)
    }) -> tmp
  }
  
  if(dir == "to"){
    sapply(1:nrow(matrix), function(x){
      matrix$connect[[x]][,c_no] %>% 
        sum(., na.rm = T)
    }) -> tmp
  }
  return(tmp)
}