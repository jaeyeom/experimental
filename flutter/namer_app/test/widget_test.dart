import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:provider/provider.dart';

import 'package:namer_app/main.dart';

void main() {
  testWidgets('Next button changes the BigCard text', (WidgetTester tester) async {
    // Create an instance of MyAppState.
    final myAppState = MyAppState();

    // Build our app and trigger a frame.
    await tester.pumpWidget(
      ChangeNotifierProvider<MyAppState>(
        create: (context) => myAppState,
        child: MaterialApp(home: MyHomePage()),
      ),
    );

    // Get the default word pair from the MyApp's context.
    final defaultWordPair = myAppState.current;

    // Verify that the BigCard contains the default word pair.
    expect(find.text(defaultWordPair.asLowerCase), findsOneWidget);
    expect(myAppState.current, equals(defaultWordPair));

    // Press the next button.
    await tester.tap(find.byKey(Key('nextWordPairButtonKey')));

    // Check if the new current word and the default one are not equal.
    expect(myAppState.current, isNot(equals(defaultWordPair)));

    final nextWordPair = myAppState.current;
    expect(myAppState.current, equals(nextWordPair));
  });
}
