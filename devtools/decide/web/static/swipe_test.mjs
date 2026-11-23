import test from 'node:test';
import assert from 'node:assert/strict';
import { swipeDecision } from './swipe.js';
import { customUntil, PRESETS } from './snooze_sheet.js';

test('swipe right archives, left snoozes, past the threshold', () => {
  assert.equal(swipeDecision(100, 5), 'archive');
  assert.equal(swipeDecision(-100, -5), 'snooze');
  assert.equal(swipeDecision(79, 0), null);
  assert.equal(swipeDecision(-79, 0), null);
});

test('mostly vertical drags are scrolls, not swipes', () => {
  assert.equal(swipeDecision(100, 90), null);
  assert.equal(swipeDecision(-100, 120), null);
});

test('snooze presets use the server preset names', () => {
  assert.deepEqual(PRESETS.map((p) => p.preset), ['1h', '3h', 'tomorrow9', 'nextmon9']);
});

test('custom wake time must be a valid future local time', () => {
  const now = new Date(2026, 0, 1, 12, 0).getTime();
  assert.deepEqual(customUntil('', now), { error: 'Pick a date and time.' });
  assert.deepEqual(customUntil('nope', now), { error: 'Pick a date and time.' });
  assert.deepEqual(customUntil('2026-01-01T11:00', now), { error: 'Pick a time in the future.' });
  const ok = customUntil('2026-01-02T09:00', now);
  assert.equal(ok.until, new Date(2026, 0, 2, 9, 0).toISOString());
});
