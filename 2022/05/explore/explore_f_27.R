library(tidyverse)

train = read_csv(file = "~/project/2022/05/data/train.csv") %>% 
  mutate(target = case_when(target == 1 ~ "One",
                           target == 0 ~ "Zero")) %>% 
  mutate(target = factor(target))

test = read_csv(file = "~/project/2022/05/data/test.csv")


# Letter in each slot -----------------------------------------------------


get_f_27_char = function(pos = 1,
                         x = train) {
  
  append_char_at_pos = train %>% 
    select(id,
           f_27,
           target) %>%
    mutate(pos = pos) %>% 
    mutate(char_at_pos = str_sub(string = f_27,
                                 start = pos,
                                 end = pos)) %>% 
    as.data.frame()
  
  return(append_char_at_pos)
  
}

pos_len = 1:10

f_27_split_long = pos_len %>% 
  map(.f = get_f_27_char,
      x = train) %>% 
  bind_rows()

# f_27_split_long %>% 
#   group_by(pos,
#            char_at_pos) %>% 
#   summarize(pct = mean(target),
#             rows = n(),
#             .groups = "keep") %>% 
#   arrange(desc(pct)) %>% 
#   filter(rows >= 10) %>% 
#   as.data.frame()

f_27_split = f_27_split_long %>% 
  select(id,
         pos,
         char_at_pos) %>% 
  mutate(pos = paste0("f_27_",pos)) %>% 
  pivot_wider(names_from = pos,
              values_from = char_at_pos) %>% 
  as.data.frame()






# In train and test -------------------------------------------------------

group_f_27 = function(x = train) {
  
  f_27_summary_long = x %>% 
    group_by(f_27,
             target) %>% 
    summarize(rows = n(),
              .groups = "keep")
  
  f_27_summary = f_27_summary_long %>% 
    pivot_wider(names_from = target,
                values_from = rows,
                values_fill = list(Rows = 0)) %>% 
    mutate(pct_one = One/(One + Zero)) %>% 
    mutate(rows = One + Zero) %>% 
    arrange(desc(pct_one)) %>% 
    mutate(f_27_group = case_when(pct_one == 1 ~ "all_ones",
                                  pct_one == 0 ~ "all_zero",
                                  T ~ "mixed")) %>% 
    select(f_27, rows, f_27_group)
  
  return(f_27_summary)
  
}

train_grouped = group_f_27(train)

test_grouped = group_f_27(test)

unique_train = train %>% 
  select(f_27) %>% 
  unique() %>% 
  mutate(Train = 1)

unique_test = test %>% 
  select(f_27) %>% 
  unique() %>% 
  mutate(Test = 1)

full_f_27_join = unique_train %>% 
  full_join(unique_test, 
            by = "f_27")

full_f_27_join %>% 
  nrow()

full_f_27_join %>% 
  filter(Train == 1,
         Test == 1) %>% 
  nrow()


# Old ---------------------------------------------------------------------

f_27_summary_long = train %>% 
  group_by(f_27,
           target) %>% 
  summarize(Rows = n(),
            .groups = "keep")

f_27_summary = f_27_summary_long %>% 
  pivot_wider(names_from = target,
              values_from = Rows,
              values_fill = list(Rows = 0)) %>% 
  mutate(PctOne = One/(One + Zero)) %>% 
  mutate(Rows = One + Zero) %>% 
  arrange(desc(PctOne)) 

f_27_summary %>% 
  arrange(desc(One)) %>% 
  as.data.frame() %>% 
  head(100)

f_27_summary %>% 
  filter(Zero + One > 5) %>% 
  arrange(f_27) %>% 
  as.data.frame()

f_27_summary %>% 
  filter(PctOne == 1) %>% 
  arrange(f_27) %>% 
  head(25) %>% 
  as.data.frame()

f_27_summary %>% 
  # filter(PctOne < 1 & PctOne > 0) %>% 
  arrange(f_27) %>% 
  as.data.frame() %>% 
  nrow()


