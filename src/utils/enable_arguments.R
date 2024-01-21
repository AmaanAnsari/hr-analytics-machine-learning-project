# remove elements from list ls() that start with 'args'
if(args_default_list[['args_clean_workspace']]){
  rm(list = ls()[!grepl("^args", ls())])
}

# define default args
for (argument in names(args_default_list)) {
  if (exists(argument) == F) {
    assign(argument, args_default_list[[argument]])
  }
}

rm(argument, args_default_list, args_clean_workspace)