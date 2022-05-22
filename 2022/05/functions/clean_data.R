clean_data = function(x = read_csv("~/project/2022/05/data/train.csv")) {
  
  require(tidyverse)
  
  get_f_27_char = function(pos = 1,
                           f_27_data = x) {
    
    append_char_at_pos = f_27_data %>% 
      select(id,
             f_27) %>%
      mutate(pos = pos) %>% 
      mutate(char_at_pos = str_sub(string = f_27,
                                   start = pos,
                                   end = pos)) %>% 
      as.data.frame() %>% 
      mutate(char_at_pos = factor(char_at_pos, levels = LETTERS)) %>% 
      mutate(char_at_pos = as.numeric(char_at_pos))
    
    return(append_char_at_pos)
    
  }
  
  pos_len = 1:10
  
  f_27_split_long = pos_len %>% 
    map(.f = get_f_27_char,
        f_27_data = x) %>% 
    bind_rows()
  
  f_27_split = f_27_split_long %>% 
    select(id,
           pos,
           char_at_pos) %>% 
    mutate(pos = paste0("f_27_",pos)) %>% 
    pivot_wider(names_from = pos,
                values_from = char_at_pos) %>% 
    as.data.frame()
  
  with_f_27_split = x %>% 
    inner_join(f_27_split, 
               by = "id") %>% 
    select(-f_27) 
  
  return(with_f_27_split)
  
}