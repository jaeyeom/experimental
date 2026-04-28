# Author: Jaehyun Yeom
#
# Description:
#   Linked list implementation.

linked_list = exports? and @ or @linked_list = {}

# Linked list class. To create new list with 1 data:
#   newList = new LinkedList data, null
class linked_list.LinkedList
  constructor: (@data, @next) ->

# Returns list with appended data at the end
linked_list.append = (list, data) ->
  if list?
    list.next = linked_list.append list.next, data
    list
  else
    new linked_list.LinkedList data, null

# Returns equivalent array of the list
linked_list.toArray = (list) ->
  array = []
  toArrayInner = (list) ->
    if list?
      array.push list.data
      toArrayInner list.next
  toArrayInner list
  array

# Returns nth data of the list
linked_list.nth = (list, n) ->
  if list?
    if n == 0
      list.data
    else
      if n > 0 and list.next?
        linked_list.nth list.next, n - 1

# Returns copy of reversed list
linked_list.reverse = (list) ->
  acc = null
  reverseInner = (list) ->
    if list?
      acc = new linked_list.LinkedList list.data, acc
      reverseInner list.next
    else
      acc
  reverseInner list, null

# Returns length of the list
linked_list.length = (list) ->
  if list? then 1 + linked_list.length list.next else 0

# Returns mapped list with results of func with each element of the list
linked_list.map = (list, func) ->
  acc = null
  mapInner = (list, func) ->
    if list?
      mapped = func list.data
      acc = mapInner list.next, func
      acc = new linked_list.LinkedList mapped, acc
    else
      null
  mapInner list, func

# Returns reduced value with two-argument function func across the list
linked_list.reduce = (list, func) ->
  if list?
    if list.next?
      acc = list.data
      acc = func acc, list.data while list = list.next
      acc
    else
      list.data

# Returns copy of sorted list
linked_list.insertionSort = (list) ->
  acc = null
  insert = (list, data) ->
    if list?
      if data < list.data
        new linked_list.LinkedList(data, list)
      else
        list.next = insert list.next, data
        list
    else
      new linked_list.LinkedList(data, null)
  insertAcc = (data) ->
    acc = insert acc, data
  linked_list.map list, insertAcc
  acc

# Whether two lists equal or not. This does not work for object lists.
linked_list.isEqual = (list1, list2) ->
  if list1?
    list2? and list1.data == list2.data and linked_list.isEqual list1.next, list2.next
  else
    not list2?
