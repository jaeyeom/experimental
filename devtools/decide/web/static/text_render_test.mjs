import { test } from 'node:test';
import assert from 'node:assert/strict';
import { safeHref, linkify, parseBlocks } from './text_render.js';

test('safeHref accepts only http and https', () => {
  assert.equal(safeHref('https://example.com/a?b=1'), 'https://example.com/a?b=1');
  assert.equal(safeHref('http://example.com'), 'http://example.com/');
  for (const bad of ['javascript:alert(1)', 'JaVaScRiPt:alert(1)', 'data:text/html,<b>x</b>',
    'vbscript:x', 'file:///etc/passwd', 'ftp://x.example', '//evil.example', 'example.com', '',
    ' javascript:alert(1)', 'https://', 'blob:https://x/1']) {
    assert.equal(safeHref(bad), null, bad);
  }
  assert.equal(safeHref(null), null);
  assert.equal(safeHref(undefined), null);
});

test('linkify splits text and links, trimming trailing punctuation', () => {
  assert.deepEqual(linkify('see https://a.example/x, ok'), [
    { type: 'text', text: 'see ' },
    { type: 'link', text: 'https://a.example/x', href: 'https://a.example/x' },
    { type: 'text', text: ', ok' },
  ]);
  assert.deepEqual(linkify('(https://a.example/p)'), [
    { type: 'text', text: '(' },
    { type: 'link', text: 'https://a.example/p', href: 'https://a.example/p' },
    { type: 'text', text: ')' },
  ]);
  const wiki = linkify('https://a.example/w_(x)');
  assert.equal(wiki[0].text, 'https://a.example/w_(x)');
});

test('linkify leaves non-http schemes and html as plain text', () => {
  for (const s of ['javascript:alert(1)', '<a href="javascript:x">y</a>', '<script>alert(1)</script>',
    'data:text/html;base64,AAAA', '<img src=x onerror=alert(1)>']) {
    assert.deepEqual(linkify(s), [{ type: 'text', text: s }], s);
  }
});

test('linkify does not link a javascript url smuggled after http text', () => {
  const segs = linkify('http://x.example/ javascript:alert(1)');
  assert.equal(segs.filter((s) => s.type === 'link').length, 1);
  assert.equal(segs.map((s) => s.text).join(''), 'http://x.example/ javascript:alert(1)');
});

test('linkify keeps html metacharacters inside link text, only as text', () => {
  const segs = linkify('https://a.example/?q=<b>&x="y"');
  assert.equal(segs.map((s) => s.text).join(''), 'https://a.example/?q=<b>&x="y"');
  for (const s of segs) if (s.type === 'link') assert.match(s.href, /^https:\/\//);
});

test('parseBlocks handles fenced code', () => {
  const b = parseBlocks('before\n```sh\nls -l\n\n  echo hi\n```\nafter');
  assert.deepEqual(b, [
    { type: 'text', text: 'before' },
    { type: 'code', text: 'ls -l\n\n  echo hi', lang: 'sh' },
    { type: 'text', text: 'after' },
  ]);
});

test('parseBlocks: unterminated fence runs to the end', () => {
  assert.deepEqual(parseBlocks('```\na\nb'), [{ type: 'code', text: 'a\nb', lang: '' }]);
});

test('parseBlocks: indented command block after blank line', () => {
  const b = parseBlocks('Run this:\n\n    git push\n    git status\n\nthen wait');
  assert.deepEqual(b, [
    { type: 'text', text: 'Run this:' },
    { type: 'code', text: 'git push\ngit status', lang: '' },
    { type: 'text', text: 'then wait' },
  ]);
});

test('parseBlocks: indented line inside a paragraph is not code', () => {
  assert.deepEqual(parseBlocks('a\n    b'), [{ type: 'text', text: 'a\n    b' }]);
});

test('parseBlocks: plain, empty and html text stay verbatim', () => {
  assert.deepEqual(parseBlocks(''), []);
  assert.deepEqual(parseBlocks('<b>x</b> & y'), [{ type: 'text', text: '<b>x</b> & y' }]);
  assert.deepEqual(parseBlocks('a\n\nb'), [{ type: 'text', text: 'a\n\nb' }]);
});

test('parseBlocks: CRLF is normalised', () => {
  assert.deepEqual(parseBlocks('a\r\n```\r\nx\r\n```'), [
    { type: 'text', text: 'a' }, { type: 'code', text: 'x', lang: '' }]);
});
