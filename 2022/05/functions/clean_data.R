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
      mutate(char_at_pos = as.numeric(char_at_pos) - 1) #Linear relationships in some features (Except A)
    
    return(append_char_at_pos)
    
  }
  
  pos_len = 1:10
  
  f_27_split_long = pos_len %>% 
    map(.f = get_f_27_char,
        f_27_data = x) %>% 
    bind_rows()
  
  f_27_split = f_27_split_long %>%
    group_by(id) %>% 
    mutate(f_27_n_char = n_distinct(char_at_pos)) %>% 
    as.data.frame() %>% 
    select(id,
           f_27_n_char,
           pos,
           char_at_pos) %>% 
    mutate(pos = paste0("f_27_",pos)) %>% 
    pivot_wider(names_from = pos,
                values_from = char_at_pos) %>% 
    as.data.frame() %>% 
    mutate(f_27_5_8_9_10 = f_27_5 + f_27_8 + f_27_9 + f_27_10)
  
  with_f_27_split = x %>% 
    inner_join(f_27_split, 
               by = "id") %>% 
    select(-f_27) 
  
  return(with_f_27_split)
  
}