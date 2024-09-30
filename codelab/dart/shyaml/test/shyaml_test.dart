import 'package:shyaml/shyaml.dart';
import 'package:test/test.dart';

void main() {
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
}
