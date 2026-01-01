/// Spreadsheet-style YAML parser.
///
/// This parser is designed to parse YAML files that are formatted like a
/// spreadsheet. The first column is the key and the second column is the value.

/// Exception thrown when parsing fails.
class ShyamlParseException implements Exception {
  /// The error message describing what went wrong.
  final String message;

  /// The row index (0-based) where the error occurred.
  final int row;

  /// The column index (0-based) where the error occurred, if applicable.
  final int? column;

  ShyamlParseException(this.message, {required this.row, this.column});

  @override
  String toString() {
    if (column != null) {
      return 'ShyamlParseException: $message (row: ${row + 1}, column: ${column! + 1})';
    }
    return 'ShyamlParseException: $message (row: ${row + 1})';
  }
}

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
///
/// Throws [ShyamlParseException] if the input is malformed.
dynamic parseRows(List<List<String>> rows, {int index = 0, int indent = 0}) {
  var result = <String, dynamic>{};
  var previousKey = '';
  // Loop through the rows with the index.
  for (var i = index; i < rows.length; i++) {
    var row = rows[i];
    var rowIndent = findRowIndent(row);

    // Skip empty rows.
    if (rowIndent == -1) {
      continue;
    }

    if (i > index && 0 <= rowIndent && rowIndent < indent) {
      break;
    }
    if (rowIndent > indent) {
      continue;
    }

    // Validate row has enough cells for the indent level.
    if (row.length <= indent) {
      throw ShyamlParseException(
        'Row has insufficient cells for indent level $indent',
        row: i,
        column: indent,
      );
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

    // Validate that key is not empty when it should have content.
    if (key == '' && rowIndent == indent) {
      throw ShyamlParseException(
        'Expected a key or array marker at indent level $indent',
        row: i,
        column: indent,
      );
    }

    if (value == '') {
      // Either a single value or an array.
      result[key] = [];
      previousKey = key;
      continue;
    }

    result[key] = value;
  }
  if (result.containsKey('')) {
    return result[''];
  }
  if (result.length == 1 && result.values.first.isEmpty) {
    return result.keys.first;
  }
  return result;
}
