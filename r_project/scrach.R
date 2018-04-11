##
train_y <- fread(train_path, select = "is_attributed")
train_ip <- fread(train_path, select = "ip")
test_ip <- fread(test_path, select = "ip")


##
print(object.size(train_y), units = "auto")
print(object.size(train_ip), units = "auto")
print(object.size(test_ip), units = "auto")



##
which_one <- which(train_y == 1)
which_zero <- which(train_y == 0)
which_same_ip <- which(train_ip %in% test_ip)



##
fread_selected <- 
function(num_row) {
  fread(train_path, skip = 1+num_row, nrows = 1, header = F)
}




map_df(which(train_y == 1),
       fread_selected)
