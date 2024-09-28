# Author: Jaehyun Yeom
#
# Description: Merge sort algorithm

merge_sort = exports? and @ or @linked_list = {}

# Merges two sorted array
merge_sort.merge = (array1, array2) ->
  array3 = []
  until array1.length == 0 or array2.length == 0
    if array1[0] < array2[0]
      array3.push array1[0]
      array1 = array1[1..]
    else
      array3.push array2[0]
      array2 = array2[1..]
  array3.concat array1.concat array2

# Merge sort the array
merge_sort.mergeSort = (array) ->
  if array.length > 1
    mid = array.length / 2
    left_array = merge_sort.mergeSort array[0...mid]
    right_array = merge_sort.mergeSort array[mid..]
    merge_sort.merge left_array, right_array
  else
    array
