@base_width = 200
@base_height = 180

list = {
  inventory_location = inventory_location1
  list_name = main
  X = 0
  Y = 0
  W = @base_width / 10
  H = @base_height / 10
}

@elems = [infiniminer, minecraft, minetest]

textlist = {
  X = 10
  Y = 10
  W = @base_width / 5
  H = @base_height / 5
  name = textlist1
  listelems = @elems
}

dropdown = {
  X = 10
  Y = 10
  W = @base_width + 10
  H = @base_height + 20
  name = dropdown1
  items = @elems
  selected_idx = 0
}
