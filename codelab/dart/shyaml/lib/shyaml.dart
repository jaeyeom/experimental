/// Spreadsheet-style YAML parser.
///
/// This parser is designed to parse YAML files that are formatted like a
/// spreadsheet. The first column is the key and the second column is the value.

/// Returns the indentation level of the given row.
///
/// This function returns the index of the first non-empty cell in the row.
int findRowIndent(List<String> row) {
  for (var i = 0; i < row.length; i++) {
    if (row[i] != '') {
      return i;
    }
  }
  return -1;
}

/// Gets the cell content in the given row.
String getCell(List<String> row, int index) {
  if (index < row.length) {
    return row[index];
  }
  return '';
}

/// Parses the rows starting from the given index with the given indent level.
///
/// It returns key value pairs in a map.
dynamic parseRows(List<List<String>> rows, {int index = 0, int indent = 0}) {
  // print('rows: $rows, index: $index, indent: $indent');
  var result = <String, dynamic>{};
  var previousKey = '';
  // Loop through the rows with the index.
  for (var i = index; i < rows.length; i++) {
    var row = rows[i];
    var rowIndent = findRowIndent(row);
    if (i > index && 0 <= rowIndent && rowIndent < indent) {
      break;
    }
    if (rowIndent > indent) {
      continue;
    }
    if (row[indent] == '-') {
      if (result[previousKey] == null) {
        result[previousKey] = <dynamic>[];
      }
      result[previousKey].add(parseRows(rows, index: i, indent: indent + 1));
      continue;
    }
    // Get key and value if the index is within the row length.
    var key = getCell(row, indent);
    var value = getCell(row, indent + 1);
    if (value == '') {
      // Either a single value or an array.
      result[key] = [];
      previousKey = key;
      continue;
    }

    result[key] = value;
  }
  // print('result: $result');
  if (result.containsKey('')) {
    return result[''];
  }
  if (result.length == 1 && result.values.first.isEmpty) {
    return result.keys.first;
  }
  return result;
}
