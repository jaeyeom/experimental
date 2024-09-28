import 'package:flutter_test/flutter_test.dart';
import 'package:namer_app/main.dart';

void main() {
  test('getNext changes the current word pair', () {
    // Create an instance of MyAppState.
    final myAppState = MyAppState();

    // Get the default word pair from the MyApp's context.
    final defaultWordPair = myAppState.current;

    // Verify that the BigCard contains the default word pair.
    expect(defaultWordPair.asLowerCase, isNotNull);

    // Press the button with "Next" text.
    myAppState.getNext();

    // Check if the new current word and the default one are not equal.
    expect(myAppState.current, isNot(equals(defaultWordPair)));
  });

  group('tests toggleFavorite', () {
    test('toggleFavorite adds the current word pair to favorites', () {
      final myAppState = MyAppState();
      final defaultWordPair = myAppState.current;

      // Verify that the default word pair is not in favorites.
      expect(defaultWordPair.asLowerCase, isNotNull);
      expect(myAppState.favorites, isNot(contains(defaultWordPair)));

      myAppState.toggleFavorite();

      // Check if the word pair is favorited.
      expect(myAppState.favorites, contains(defaultWordPair));
    });

    test('toggleFavorite removes the current word pair from favorites', () {
      final myAppState = MyAppState();
      final defaultWordPair = myAppState.current;

      // Verify that the default word pair is not in favorites.
      expect(defaultWordPair.asLowerCase, isNotNull);
      expect(myAppState.favorites, isNot(contains(defaultWordPair)));

      myAppState.toggleFavorite();

      // Check if the word pair is favorited.
      expect(myAppState.favorites, contains(defaultWordPair));

      myAppState.toggleFavorite();

      // Check if the word pair is favorited.
      expect(myAppState.favorites, isNot(contains(defaultWordPair)));
    });

    test('toggleFavorite can add multiple word pairs', () {
      final myAppState = MyAppState();
      final favorites = myAppState.favorites;

      // Verify that favorites is empty initially.
      expect(favorites.length, 0);

      myAppState.toggleFavorite();

      expect(favorites.length, 1);

      myAppState.getNext();
      myAppState.toggleFavorite();

      expect(favorites.length, 2);

      myAppState.getNext();
      myAppState.toggleFavorite();

      expect(favorites.length, 3);

      myAppState.toggleFavorite();

      expect(favorites.length, 2);
    });
  });
}
