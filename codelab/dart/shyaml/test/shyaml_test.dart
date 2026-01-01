import 'package:shyaml/shyaml.dart';
import 'package:test/test.dart';

void main() {
  group('ShyamlParseException', () {
    test('toString includes row number (1-based)', () {
      var exception = ShyamlParseException('Test error', row: 0);
      expect(exception.toString(), contains('row: 1'));
      expect(exception.toString(), contains('Test error'));
    });

    test('toString includes column when provided', () {
      var exception = ShyamlParseException('Test error', row: 2, column: 3);
      expect(exception.toString(), contains('row: 3'));
      expect(exception.toString(), contains('column: 4'));
    });
  });

  test('parseRows', () {
    // Return an empty dictionary for empty rows.
    expect(parseRows([]), {});
    expect(
      parseRows([
        ['key', 'value'],
      ]),
      {
        'key': 'value',
      },
    );
    expect(
      parseRows([
        ['key', 'value'],
        ['key2', 'value2'],
      ]),
      {
        'key': 'value',
        'key2': 'value2',
      },
    );
    expect(
      parseRows([
        ['arrayKey', ''],
        ['-', 'v1'],
        ['-', 'v2'],
      ]),
      {
        'arrayKey': ['v1', 'v2'],
      },
    );
    expect(
      parseRows([
        ['key', 'value'],
        ['arrayKey', ''],
        ['-', 'v1'],
        ['-', 'v2'],
      ]),
      {
        'key': 'value',
        'arrayKey': ['v1', 'v2'],
      },
    );
    expect(
      parseRows([
        ['arrayKey', '', ''],
        ['-', 'name', 'Tom'],
        ['-', 'name', 'Jerry'],
        ['key', 'value'],
      ]),
      {
        'arrayKey': [
          {'name': 'Tom'},
          {'name': 'Jerry'},
        ],
        'key': 'value',
      },
    );
    expect(
      parseRows([
        ['arrayKey', '', ''],
        ['-', 'name', 'Tom'],
        ['', 'age', '10'],
        ['-', 'name', 'Jerry'],
        ['', 'age', '12'],
        ['key', 'value'],
      ]),
      {
        'arrayKey': [
          {'name': 'Tom', 'age': '10'},
          {'name': 'Jerry', 'age': '12'},
        ],
        'key': 'value',
      },
    );
    expect(
      parseRows([
        ['-', 'hello'],
        ['-', 'world'],
      ]),
      ['hello', 'world'],
    );
    expect(
      parseRows([
        ['-', 'name', 'Tom'],
        ['-', 'name', 'Jerry'],
      ]),
      [
        {'name': 'Tom'},
        {'name': 'Jerry'},
      ],
    );
    expect(
      parseRows([
        ['-', 'name', 'Tom'],
        ['', 'age', '10'],
        ['-', 'name', 'Jerry'],
        ['', 'age', '12'],
      ]),
      [
        {'name': 'Tom', 'age': '10'},
        {'name': 'Jerry', 'age': '12'},
      ],
    );
  });

  group('parseRows error handling', () {
    test('skips empty rows', () {
      expect(
        parseRows([
          ['key1', 'value1'],
          [], // Empty row should be skipped
          ['key2', 'value2'],
        ]),
        {'key1': 'value1', 'key2': 'value2'},
      );
    });

    test('skips rows with only empty strings', () {
      expect(
        parseRows([
          ['key1', 'value1'],
          ['', ''], // Row with empty strings should be skipped
          ['key2', 'value2'],
        ]),
        {'key1': 'value1', 'key2': 'value2'},
      );
    });

    test('throws on insufficient cells for indent level', () {
      expect(
        () => parseRows([
          ['arrayKey', ''],
          ['-'], // Only one cell, but needs indent level 1
        ]),
        throwsA(isA<ShyamlParseException>().having(
          (e) => e.message,
          'message',
          contains('insufficient cells'),
        )),
      );
    });

    test('exception contains correct row number', () {
      try {
        parseRows([
          ['arrayKey', ''],
          ['-'], // Error at row index 1
        ]);
        fail('Expected ShyamlParseException');
      } on ShyamlParseException catch (e) {
        expect(e.row, 1); // 0-based index
        expect(e.toString(), contains('row: 2')); // 1-based in message
      }
    });
  });
}
