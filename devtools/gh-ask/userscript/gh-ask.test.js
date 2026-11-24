// Tests for the pure parts of gh-ask.user.js. Run with:
//   node --test devtools/gh-ask/userscript/
const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const { parse, byAttr } = require("./testdom.js");
const G = require("./gh-ask.user.js");

const A = "diff-99d875210072b234cb9c80abb7d462206b77696d5450874b204d32fa61b2dca4";
const B = "diff-c444f711e9191b53952edb65bfd8c644419fc7695c62611dc0fb304b4fb197d6";

function fixture(name) {
  return parse(fs.readFileSync(path.join(__dirname, "testdata", name), "utf8"));
}

const views = [
  {
    name: "classic files view",
    file: "files-view.html",
    href: "https://github.com/octo/demo/pull/42/files",
    head: "0123456789abcdef0123456789abcdef01234567",
  },
  {
    name: "changes view",
    file: "changes-view.html",
    href: "https://github.com/octo/demo/pull/42/changes",
    head: "89abcdef0123456789abcdef0123456789abcdef",
  },
];

for (const v of views) {
  test(`${v.name}: head sha`, () => {
    assert.equal(G.findHeadSha(fixture(v.file), v.href), v.head);
  });

  test(`${v.name}: line info for code and number cells`, () => {
    const root = fixture(v.file);
    const cases = [
      ["ctx8", "RIGHT", 8],
      ["del9", "LEFT", 9],
      ["numL9", "LEFT", 9],
      ["add10", "RIGHT", 10],
      ["ctx12", "RIGHT", 12],
    ];
    for (const [id, side, line] of cases) {
      const info = G.lineInfo(byAttr(root, "data-fixture", id));
      assert.ok(info, id);
      assert.deepEqual(
        [info.path, info.side, info.line, info.anchor],
        ["pkg/a.go", side, line, A],
        id,
      );
    }
  });

  test(`${v.name}: hovered line payload uses the hunk text`, () => {
    const root = fixture(v.file);
    const el = byAttr(root, "data-fixture", "add10");
    const got = G.buildPayload({
      href: `${v.href}?w=1#top`,
      headSha: v.head,
      start: el,
      end: el,
      selection: "",
      question: "Why log?",
      deep: true,
    });
    assert.deepEqual(got, {
      payload: {
        repo: "octo/demo",
        pr: 42,
        head_sha: v.head,
        path: "pkg/a.go",
        side: "RIGHT",
        start_line: 10,
        end_line: 10,
        selection: [
          "@@ -8,5 +8,7 @@ package a",
          " func Retry() error {",
          "-\tfor i := 0; i < 2; i++ {",
          "+\tfor i := 0; i < 3; i++ {",
          "+\t\tlog.Print(i)",
          " \t\tif err := try(); err == nil {",
          " \t\t\treturn nil",
        ].join("\n"),
        question: "Why log?",
        deep: true,
        page_url: `${v.href}#${A}R10`,
      },
    });
  });

  test(`${v.name}: selection range takes the end side and the selected text`, () => {
    const root = fixture(v.file);
    const got = G.buildPayload({
      href: v.href,
      headSha: v.head,
      // Selected bottom-up: the order must not matter.
      start: byAttr(root, "data-fixture", "ctx12"),
      end: byAttr(root, "data-fixture", "ctx8"),
      selection: "func Retry() error {\n...",
      question: "q",
      deep: false,
    });
    assert.equal(got.error, undefined);
    const p = got.payload;
    assert.deepEqual(
      [p.side, p.start_line, p.end_line, p.selection, p.page_url],
      ["RIGHT", 8, 12, "func Retry() error {\n...", `${v.href}#${A}R8-R12`],
    );
  });

  test(`${v.name}: deleted lines range on the left side`, () => {
    const root = fixture(v.file);
    const del = byAttr(root, "data-fixture", "del9");
    const got = G.buildPayload({
      href: v.href,
      headSha: v.head,
      start: byAttr(root, "data-fixture", "ctx8"),
      end: del,
      selection: "x",
      question: "q",
      deep: false,
    });
    const p = got.payload;
    assert.deepEqual(
      [p.side, p.start_line, p.end_line, p.page_url],
      ["LEFT", 8, 9, `${v.href}#${A}L8-L9`],
    );
  });
}

// changes-view-captured.html is markup GitHub served (see its header).
const C = "diff-640d823e223a4641034666098d26a6f04d83692043100600dad8ae98effad277";
const CAPTURED_HEAD = "fedcba9876543210fedcba9876543210fedcba98";
const CAPTURED_HREF = "https://github.com/octo/demo/pull/42/changes";
const CAPTURED_HUNK = [
  "@@ -0,0 +1,6 @@",
  "+package greet",
  "+",
  "+import (",
  '+\t"context"',
  "+",
  '+\t"strings"',
].join("\n");

function capturedAsk(root, start, end, selection = "") {
  return G.buildPayload({
    href: CAPTURED_HREF,
    headSha: CAPTURED_HEAD,
    start,
    end,
    selection,
    question: "q",
    deep: false,
  });
}

test("captured changes view: head sha", () => {
  assert.equal(G.findHeadSha(fixture("changes-view-captured.html"), CAPTURED_HREF), CAPTURED_HEAD);
});

test("captured changes view: code and number cells of a new file", () => {
  const root = fixture("changes-view-captured.html");
  for (const el of [
    byAttr(root, "data-line-anchor", `${C}R4`),
    byAttr(root, "data-grid-cell-id", `${C}-empty-4-1`),
  ]) {
    const info = G.lineInfo(el);
    assert.ok(info);
    assert.deepEqual(
      [info.path, info.side, info.line, info.anchor],
      ["pkg/greet/greet.go", "RIGHT", 4, C],
    );
  }
});

test("captured changes view: hovered line sends the hunk's code", () => {
  const root = fixture("changes-view-captured.html");
  const el = byAttr(root, "data-line-anchor", `${C}R1`);
  const p = capturedAsk(root, el, el).payload;
  assert.deepEqual(
    [p.side, p.start_line, p.end_line, p.selection, p.page_url],
    ["RIGHT", 1, 1, CAPTURED_HUNK, `${CAPTURED_HREF}#${C}R1`],
  );
});

test("captured changes view: a range with no selected text sends the whole hunk", () => {
  const root = fixture("changes-view-captured.html");
  const p = capturedAsk(
    root,
    byAttr(root, "data-grid-cell-id", `${C}-empty-1-1`),
    byAttr(root, "data-line-anchor", `${C}R6`),
  ).payload;
  assert.deepEqual([p.start_line, p.end_line, p.selection], [1, 6, CAPTURED_HUNK]);
});

test("code that cannot be read from the page is sent as no selection", () => {
  const html = fs
    .readFileSync(path.join(__dirname, "testdata", "changes-view-captured.html"), "utf8")
    .replace(/(data-diff-text-inner="true">)(?!@@).*?(<\/div><\/code>)/g, "$1 $2");
  const root = parse(html);
  const el = byAttr(root, "data-line-anchor", `${C}R3`);
  assert.equal(capturedAsk(root, el, el).payload.selection, "");
});

test("classic split view: side comes from the code cell", () => {
  const root = fixture("files-view.html");
  const left = G.lineInfo(byAttr(root, "data-fixture", "splitL5"));
  const right = G.lineInfo(byAttr(root, "data-fixture", "splitR6"));
  assert.deepEqual([left.path, left.side, left.line, left.anchor], ["cmd/main.go", "LEFT", 5, B]);
  assert.deepEqual(
    [right.path, right.side, right.line, right.anchor],
    ["cmd/main.go", "RIGHT", 6, B],
  );
  const got = G.buildPayload({
    href: "https://github.com/octo/demo/pull/42/files",
    headSha: "0123456",
    start: byAttr(root, "data-fixture", "splitR5"),
    end: byAttr(root, "data-fixture", "splitR5"),
    selection: "",
    question: "q",
    deep: false,
  });
  assert.equal(
    got.payload.selection,
    ["@@ -5,2 +5,2 @@ func main() {", "-\trun(false)", "+\trun(true)", " }"].join("\n"),
  );
});

test("a range across two files is refused", () => {
  const root = fixture("files-view.html");
  const got = G.buildPayload({
    href: "https://github.com/octo/demo/pull/42/files",
    headSha: "0123456",
    start: byAttr(root, "data-fixture", "ctx8"),
    end: byAttr(root, "data-fixture", "splitR5"),
    selection: "x",
    question: "q",
    deep: false,
  });
  assert.match(got.error, /one file/);
});

test("hunk header rows and non-diff elements have no line", () => {
  const root = fixture("files-view.html");
  assert.equal(G.lineInfo(byAttr(root, "data-fixture", "hunk1")), null);
  assert.equal(G.lineInfo(byAttr(root, "id", "files")), null);
});

test("parsePrUrl", () => {
  assert.deepEqual(G.parsePrUrl("https://github.com/octo/demo/pull/42/files?diff=split#diff-1"), {
    repo: "octo/demo",
    pr: 42,
    page: "https://github.com/octo/demo/pull/42/files",
  });
  assert.deepEqual(G.parsePrUrl("https://github.com/o/r.js/pull/7/changes/abc1234..def5678"), {
    repo: "o/r.js",
    pr: 7,
    page: "https://github.com/o/r.js/pull/7/changes/abc1234..def5678",
  });
  for (const bad of [
    "https://github.com/octo/demo/pull/42",
    "https://github.com/octo/demo/issues/42/files",
    "http://github.com/octo/demo/pull/42/files",
    "https://github.com.evil.example/octo/demo/pull/42/files",
    "https://gist.github.com/octo/demo/pull/42/files",
  ]) {
    assert.equal(G.parsePrUrl(bad), null, bad);
  }
});

test("findHeadSha prefers a commit range in the URL", () => {
  const root = fixture("files-view.html");
  assert.equal(
    G.findHeadSha(root, "https://github.com/octo/demo/pull/42/files/1111111..abcdef1"),
    "abcdef1",
  );
  assert.equal(
    G.findHeadSha(root, "https://github.com/octo/demo/pull/42/files/fedcba9"),
    "fedcba9",
  );
  assert.equal(
    G.findHeadSha(parse("<div></div>"), "https://github.com/octo/demo/pull/42/files"),
    "",
  );
});

test("buildPayload errors", () => {
  const root = fixture("files-view.html");
  const el = byAttr(root, "data-fixture", "ctx8");
  const base = {
    href: "https://github.com/octo/demo/pull/42/files",
    headSha: "0123456",
    start: el,
    end: el,
    selection: "x",
    question: "q",
    deep: false,
  };
  assert.match(
    G.buildPayload({ ...base, href: "https://github.com/octo/demo" }).error,
    /pull request/,
  );
  assert.match(G.buildPayload({ ...base, headSha: "" }).error, /head commit/);
  assert.match(G.buildPayload({ ...base, headSha: "xyz1234" }).error, /head commit/);
  assert.match(G.buildPayload({ ...base, question: "  " }).error, /question/);
  assert.match(G.buildPayload({ ...base, question: "é".repeat(1025) }).error, /question/);
  assert.match(G.buildPayload({ ...base, start: root }).error, /line/);
});

test("selection is cut to 4 KB on a character boundary", () => {
  const root = fixture("files-view.html");
  const el = byAttr(root, "data-fixture", "ctx8");
  const got = G.buildPayload({
    href: "https://github.com/octo/demo/pull/42/files",
    headSha: "0123456",
    start: el,
    end: el,
    selection: `a${"€".repeat(2000)}`,
    question: "q",
    deep: false,
  });
  const bytes = Buffer.byteLength(got.payload.selection);
  assert.ok(bytes <= 4096 && bytes > 4090, String(bytes));
  assert.ok(!got.payload.selection.includes("�"));
});

test("truncateUtf8 keeps surrogate pairs whole", () => {
  assert.equal(G.truncateUtf8("ab😀c", 5), "ab");
  assert.equal(G.truncateUtf8("ab😀c", 6), "ab😀");
  assert.equal(G.truncateUtf8("abc", 3), "abc");
});

test("serverUrl only builds loopback URLs", () => {
  assert.equal(G.serverUrl(8765, "/ask"), "http://127.0.0.1:8765/ask");
  assert.equal(
    G.serverUrl("9000", G.statusPath("a/b c")),
    "http://127.0.0.1:9000/status/a%2Fb%20c",
  );
  for (const bad of [0, 65536, "80; evil", "1e3", "", null, "8765@evil.example", -1]) {
    assert.equal(G.serverUrl(bad, "/ask"), null, String(bad));
  }
});

test("askError maps server statuses", () => {
  assert.match(G.askError(401), /token/);
  assert.match(G.askError(403), /forbidden/);
  assert.match(G.askError(413), /too large/);
  assert.match(G.askError(429), /rate limit/);
  assert.match(G.askError(502), /command failed/);
  assert.match(G.askError(0), /server down/);
  assert.match(G.askError(500), /500/);
  assert.match(G.askError(-1), /userscript menu/);
  assert.equal(G.askError(400, "question is empty"), "bad request (question is empty)");
});

test("status labels and chip text", () => {
  assert.equal(G.statusLabel({ state: "x", answered: false, closed: false }), "waiting");
  assert.equal(G.statusLabel({ state: "x", answered: true, closed: false }), "answered");
  assert.equal(G.statusLabel({ state: "x", answered: true, closed: true }), "closed");
  assert.equal(G.chipText("t-1", "waiting"), "Asked in t-1 · waiting");
});

// fakeClock runs setTimeout callbacks when advance passes their time.
function fakeClock() {
  let now = 0;
  let timers = [];
  return {
    now: () => now,
    setTimeout(fn, ms) {
      const t = { at: now + ms, fn };
      timers.push(t);
      return t;
    },
    clearTimeout(t) {
      timers = timers.filter((x) => x !== t);
    },
    advance(ms) {
      const end = now + ms;
      for (;;) {
        timers.sort((a, b) => a.at - b.at);
        const t = timers[0];
        if (!t || t.at > end) {
          break;
        }
        timers.shift();
        now = t.at;
        t.fn();
      }
      now = end;
    },
    pending: () => timers.length,
  };
}

function poller(clock, opts) {
  const calls = [];
  const updates = [];
  let visible = true;
  const results = opts.results ?? [];
  const stop = G.startPolling({
    now: clock.now,
    setTimeout: clock.setTimeout,
    clearTimeout: clock.clearTimeout,
    isVisible: () => visible,
    fetchStatus(done) {
      calls.push(clock.now());
      done(
        results.shift() ?? { status: 200, body: { state: "open", answered: false, closed: false } },
      );
    },
    onUpdate: (u) => updates.push(u),
  });
  return { calls, updates, stop, setVisible: (b) => (visible = b) };
}

test("polling: every 20 s while visible, until answered", () => {
  const clock = fakeClock();
  const p = poller(clock, {
    results: [
      { status: 200, body: { state: "open", answered: false, closed: false } },
      { status: 200, body: { state: "done", answered: true, closed: false } },
    ],
  });
  clock.advance(19_999);
  assert.deepEqual(p.calls, []);
  clock.advance(1);
  assert.deepEqual(p.calls, [20_000]);
  p.setVisible(false);
  clock.advance(60_000);
  assert.deepEqual(p.calls, [20_000]);
  p.setVisible(true);
  clock.advance(20_000);
  assert.deepEqual(p.calls, [20_000, 100_000]);
  assert.deepEqual(p.updates, [{ label: "waiting" }, { label: "answered" }]);
  assert.equal(clock.pending(), 0);
});

test("polling: stops on closed", () => {
  const clock = fakeClock();
  const p = poller(clock, {
    results: [{ status: 200, body: { state: "x", answered: false, closed: true } }],
  });
  clock.advance(20_000);
  assert.deepEqual(p.updates, [{ label: "closed" }]);
  assert.equal(clock.pending(), 0);
});

test("polling: gives up after 30 minutes", () => {
  const clock = fakeClock();
  const p = poller(clock, {});
  clock.advance(60 * 60_000);
  assert.equal(p.calls.length, 90);
  assert.ok(p.calls.at(-1) <= 30 * 60_000);
  assert.deepEqual(p.updates.at(-1), { label: "waiting", error: "stopped polling after 30 min" });
  assert.equal(clock.pending(), 0);
});

test("polling: auth failure stops, a server error keeps polling", () => {
  const clock = fakeClock();
  const p = poller(clock, { results: [{ status: 0 }, { status: 502 }, { status: 401 }] });
  clock.advance(10 * 60_000);
  assert.equal(p.calls.length, 3);
  assert.deepEqual(
    p.updates.map((u) => u.error),
    [G.askError(0), G.askError(502), G.askError(401)],
  );
  assert.equal(clock.pending(), 0);
});

test("polling: stop() cancels the timer", () => {
  const clock = fakeClock();
  const p = poller(clock, {});
  p.stop();
  clock.advance(60_000);
  assert.deepEqual(p.calls, []);
});

test("the script only connects to loopback", () => {
  const src = fs.readFileSync(path.join(__dirname, "gh-ask.user.js"), "utf8");
  const connects = [...src.matchAll(/^\/\/ @connect\s+(\S+)/gm)].map((m) => m[1]);
  assert.deepEqual(connects.sort(), ["127.0.0.1", "localhost"]);
  assert.doesNotMatch(src, /XMLHttpRequest\(|\.innerHTML\b|insertAdjacentHTML|\beval\(/);
  assert.doesNotMatch(src, /\bfetch\(\s*["'`]https?:/);
  assert.match(src, /^\/\/ @version\s+\d+\.\d+\.\d+$/m);
});

// placeInView takes the desired top-left in viewport coordinates (what
// place() copies today), the measured box, and the viewport. It returns
// {left, top, width}. A fit that is already on screen stays put.
const POPOVER_W = 360;
const MARGIN = 8;

test("userscript version is 0.1.13", () => {
  const src = fs.readFileSync(path.join(__dirname, "gh-ask.user.js"), "utf8");
  assert.match(src, /^\/\/ @version\s+0\.1\.13$/m);
});

const FILES_HEAD = "0123456789abcdef0123456789abcdef01234567";
const CONVERSATION = "https://github.com/octo/demo/pull/42";
const HEAD_FINDING = "finding the head commit…";
const HEAD_MISSING = "couldn't find the head commit; open Files changed and ask there";

test("conversation view has no head commit marker", () => {
  assert.equal(G.findHeadSha(fixture("conversation-view.html"), CONVERSATION), "");
});

function panelAskOn(root, href, fetchText, cache, requests, statuses) {
  return G.askPanel({
    href,
    root,
    question: "why?",
    deep: false,
    fetchText,
    parseHtml: (html) => parse(html),
    cache,
    onStatus: (msg) => statuses.push(msg),
    request: (payload) => requests.push(payload),
  });
}

test("panel ask loads the head commit from the files page and caches it", async () => {
  const filesHtml = fs.readFileSync(path.join(__dirname, "testdata", "files-view.html"), "utf8");
  const cache = new Map();
  const requests = [];
  const statuses = [];
  let fetches = 0;
  const fetchText = async (url) => {
    fetches += 1;
    assert.equal(url, "/octo/demo/pull/42/files");
    return filesHtml;
  };
  const root = fixture("conversation-view.html");
  const first = await panelAskOn(root, CONVERSATION, fetchText, cache, requests, statuses);
  assert.equal(first.error, undefined);
  assert.equal(first.payload.head_sha, FILES_HEAD);
  assert.equal(first.payload.repo, "octo/demo");
  assert.equal(first.payload.pr, 42);
  assert.equal(fetches, 1);
  assert.deepEqual(statuses, [HEAD_FINDING]);
  assert.equal(requests.length, 1);
  assert.equal(requests[0].head_sha, FILES_HEAD);

  const second = await panelAskOn(root, CONVERSATION, fetchText, cache, requests, statuses);
  assert.equal(second.payload.head_sha, FILES_HEAD);
  assert.equal(fetches, 1);
  assert.equal(requests.length, 2);
  assert.deepEqual(statuses, [HEAD_FINDING]);
});

test("panel ask reports a missing head and does not call the local server", async () => {
  const requests = [];
  const got = await panelAskOn(
    fixture("conversation-view.html"),
    CONVERSATION,
    async () => {
      throw new Error("network");
    },
    new Map(),
    requests,
    [],
  );
  assert.equal(got.error, HEAD_MISSING);
  assert.deepEqual(requests, []);

  const empty = await panelAskOn(
    fixture("conversation-view.html"),
    CONVERSATION,
    async () => "<div>no commit here</div>",
    new Map(),
    requests,
    [],
  );
  assert.equal(empty.error, HEAD_MISSING);
  assert.deepEqual(requests, []);
});

const SOFT_HEAD = "cafebabecafebabecafebabecafebabecafebabe";
const FILES_HREF = "https://github.com/octo/demo/pull/42/files";

test("soft-nav files view reads the layout head commit", () => {
  assert.equal(G.findHeadSha(fixture("files-soft-nav.html"), FILES_HREF), SOFT_HEAD);
  assert.equal(
    G.findHeadSha(fixture("files-soft-nav.html"), `${FILES_HREF}/1111111..abcdef1`),
    "abcdef1",
  );
});

test("a bad layout head commit is ignored and the next source is used", () => {
  const input = `<input name="head_sha" value="${FILES_HEAD}">`;
  const cases = [
    JSON.stringify({
      payload: { pullRequestsLayoutRoute: { pullRequest: { headSha: "zzzzzzz" } } },
    }),
    "{",
    JSON.stringify({ payload: {} }),
  ];
  for (const json of cases) {
    const root = parse(
      `<script type="application/json" data-target="react-app.embeddedData">${json}</script>${input}`,
    );
    assert.equal(G.findHeadSha(root, FILES_HREF), FILES_HEAD, json);
  }
});

test("line ask uses the cache, then the files page, and keeps the question", async () => {
  const start = byAttr(fixture("files-view.html"), "data-fixture", "add10");
  const page = parse("<div></div>");
  const filesHtml = fs.readFileSync(path.join(__dirname, "testdata", "files-view.html"), "utf8");
  const cachedSha = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
  const cache = new Map([["octo/demo#42", cachedSha]]);
  const requests = [];
  const statuses = [];
  let fetches = 0;
  const fetchText = async (url) => {
    fetches += 1;
    assert.equal(url, "/octo/demo/pull/42/files");
    return filesHtml;
  };
  const opts = {
    href: FILES_HREF,
    root: page,
    start,
    end: start,
    selection: "",
    question: "why this line?",
    deep: false,
    parseHtml: (html) => parse(html),
    onStatus: (msg) => statuses.push(msg),
    request: (payload) => requests.push(payload),
  };
  const cached = await G.askLine({ ...opts, fetchText, cache });
  assert.equal(fetches, 0);
  assert.equal(cached.payload.head_sha, cachedSha);
  assert.equal(requests[0].head_sha, cachedSha);
  assert.equal(requests[0].path, "pkg/a.go");
  assert.equal(requests[0].start_line, 10);

  cache.delete("octo/demo#42");
  const fetched = await G.askLine({ ...opts, fetchText, cache });
  assert.equal(fetches, 1);
  assert.equal(fetched.payload.head_sha, FILES_HEAD);
  assert.deepEqual(statuses, [HEAD_FINDING]);
  assert.equal(requests[1].head_sha, FILES_HEAD);

  const before = requests.length;
  const failed = await G.askLine({
    ...opts,
    cache: new Map(),
    fetchText: async () => {
      throw new Error("network");
    },
  });
  const shown = G.popoverAskResult("why this line?", failed);
  assert.equal(shown.question, "why this line?");
  assert.equal(shown.error, HEAD_MISSING);
  assert.equal(shown.payload, null);
  assert.equal(requests.length, before);
});

test("files and changes views do not fetch the files page", async () => {
  for (const v of views) {
    let fetches = 0;
    const requests = [];
    const got = await panelAskOn(
      fixture(v.file),
      v.href,
      async () => {
        fetches += 1;
        return "";
      },
      new Map(),
      requests,
      [],
    );
    assert.equal(fetches, 0, v.name);
    assert.equal(got.payload.head_sha, v.head, v.name);
    assert.equal(requests.length, 1, v.name);
  }
});

test("a url change or a page load drops the cached head commit", () => {
  assert.equal(
    G.headNavDropsCache(
      "https://github.com/octo/demo/pull/42",
      "https://github.com/octo/demo/pull/42",
      "turbo:render",
    ),
    false,
  );
  assert.equal(
    G.headNavDropsCache(
      "https://github.com/octo/demo/pull/42",
      "https://github.com/octo/demo/pull/42/files",
      "turbo:render",
    ),
    true,
  );
  assert.equal(
    G.headNavDropsCache(
      "https://github.com/octo/demo/pull/42",
      "https://github.com/octo/demo/pull/42",
      "turbo:load",
    ),
    true,
  );
  assert.equal(G.headNavDropsCache("", "/o/r/pull/1", "gh-ask:navigate"), false);
  assert.equal(G.headNavDropsCache("/o/r/pull/1", "/o/r/pull/1/files", "gh-ask:navigate"), true);
});

function fakeDoc() {
  function createElement(tag) {
    return {
      tag,
      className: "",
      style: {},
      textContent: "",
      children: [],
      append(...kids) {
        this.children.push(...kids);
      },
    };
  }
  return { createElement };
}

function leafText(el) {
  if (!el.children || el.children.length === 0) {
    return el.textContent ? [el.textContent] : [];
  }
  return el.children.flatMap(leafText);
}

function tags(el) {
  return [el.tag, ...(el.children || []).flatMap(tags)];
}

test("message text is literal, with no element created from it", () => {
  const text = '<img onerror="alert(1)"><a href="https://evil.example">x</a>';
  const node = G.renderMessage(fakeDoc(), {
    role: "agent",
    time: "2026-11-14T10:00:00Z",
    text,
  });
  const body = node.children.find((el) => el.className === "gh-ask-msg-text");
  assert.equal(body.textContent, text);
  assert.equal(body.style.whiteSpace, "pre-wrap");
  assert.deepEqual(
    tags(node).filter((tag) => tag === "img" || tag === "a"),
    [],
  );
  assert.match(node.children[0].textContent, /^Agent · /);
  assert.equal(
    G.formatMessageTime("2026-11-14T10:00:00Z"),
    new Date("2026-11-14T10:00:00Z").toLocaleString(),
  );
});

test("thread lookup shows the thread, and a 404 is the empty state", () => {
  assert.equal(G.threadPath("octo/demo", 42), "/thread?repo=octo%2Fdemo&pr=42");
  const messages = [
    { role: "you", time: "2026-11-14T10:00:00Z", text: "q" },
    { role: "note", time: "2026-11-14T10:00:30Z", text: "later" },
  ];
  const hit = G.panelView(
    { status: 200, body: { id: "aaa" } },
    { state: "answered", answered: true, closed: true, messages },
  );
  assert.equal(hit.empty, false);
  assert.equal(hit.id, "aaa");
  assert.equal(hit.closed, true);
  assert.deepEqual(
    hit.messages.map((m) => m.text),
    ["q", "later"],
  );
  const shown = G.renderPanel(fakeDoc(), hit);
  const leaves = leafText(shown);
  assert.deepEqual(
    leaves.filter((t) => t === "q" || t === "later" || t === "closed"),
    ["closed", "q", "later"],
  );
  assert.ok(leaves.includes("Read-only. Nothing here is posted to GitHub."));

  const miss = G.panelView({ status: 404, body: { error: "not found" } }, null);
  assert.equal(miss.empty, true);
  const empty = leafText(G.renderPanel(fakeDoc(), miss));
  assert.ok(empty.includes("No question yet"));
  assert.equal(empty.includes("closed"), false);
});

test("panel polls 20s after you, 60s otherwise, then stops and refresh restarts", () => {
  assert.equal(G.panelPollInterval([{ role: "you", text: "q" }]), 20_000);
  assert.equal(
    G.panelPollInterval([
      { role: "you", text: "q" },
      { role: "agent", text: "a" },
    ]),
    60_000,
  );
  assert.equal(G.panelPollInterval([]), 60_000);

  const clock = fakeClock();
  const you = [{ role: "you", time: "2026-11-14T10:00:00Z", text: "q" }];
  const answered = [
    { role: "you", time: "2026-11-14T10:00:00Z", text: "q" },
    { role: "agent", time: "2026-11-14T10:00:20Z", text: "a" },
  ];
  const queued = [you, answered];
  const calls = [];
  const updates = [];
  const handle = G.startPanelPolling({
    now: clock.now,
    setTimeout: clock.setTimeout,
    clearTimeout: clock.clearTimeout,
    isVisible: () => true,
    messages: you,
    fetchStatus(done) {
      calls.push(clock.now());
      const messages = queued.length > 1 ? queued.shift() : queued[0];
      done({
        status: 200,
        body: { state: "working", answered: false, closed: false, messages },
      });
    },
    onUpdate: (u) => updates.push(u),
  });

  clock.advance(19_999);
  assert.deepEqual(calls, []);
  clock.advance(1);
  assert.deepEqual(calls, [20_000]);
  clock.advance(20_000);
  assert.deepEqual(calls, [20_000, 40_000]);
  clock.advance(59_999);
  assert.deepEqual(calls, [20_000, 40_000]);
  clock.advance(1);
  assert.deepEqual(calls, [20_000, 40_000, 100_000]);

  clock.advance(60 * 60_000);
  assert.equal(calls.at(-1), 40_000 + 30 * 60_000);
  assert.equal(updates.at(-1).error, "stopped polling after 30 min");
  assert.equal(clock.pending(), 0);
  const stopped = calls.length;

  handle.refresh();
  const refreshedAt = clock.now();
  clock.advance(59_999);
  assert.equal(calls.length, stopped);
  clock.advance(1);
  assert.equal(calls.length, stopped + 1);
  assert.equal(calls.at(-1), refreshedAt + 60_000);
});

test("panel follow-up sends the whole PR, or the line form from a chip", () => {
  const headSha = "a".repeat(40);
  const whole = G.panelAsk({
    href: "https://github.com/octo/demo/pull/42",
    headSha,
    question: "why?",
    deep: false,
  });
  assert.deepEqual(whole.payload, {
    repo: "octo/demo",
    pr: 42,
    head_sha: headSha,
    question: "why?",
    deep: false,
    page_url: "https://github.com/octo/demo/pull/42",
  });

  const fromChip = G.panelAsk({
    href: "https://github.com/octo/demo/pull/42/files",
    headSha,
    question: "why this line?",
    deep: true,
    line: {
      path: "pkg/a.go",
      side: "RIGHT",
      start_line: 10,
      end_line: 12,
      selection: "for i := 0",
      page_url: "https://github.com/octo/demo/pull/42/files#diff-abcR10-R12",
    },
  });
  assert.equal(fromChip.error, undefined);
  assert.equal(fromChip.payload.path, "pkg/a.go");
  assert.equal(fromChip.payload.side, "RIGHT");
  assert.equal(fromChip.payload.start_line, 10);
  assert.equal(fromChip.payload.end_line, 12);
  assert.equal(fromChip.payload.selection, "for i := 0");
  assert.equal(fromChip.payload.question, "why this line?");
  assert.equal(fromChip.payload.deep, true);
  assert.equal(
    fromChip.payload.page_url,
    "https://github.com/octo/demo/pull/42/files#diff-abcR10-R12",
  );
});

test("the panel stays up across PR tabs", () => {
  const clock = fakeClock();
  const location = { pathname: "/o/r/pulls" };
  const listeners = new Map();
  const window = {
    Event: class Event {
      constructor(type) {
        this.type = type;
      }
    },
    addEventListener(type, fn) {
      const list = listeners.get(type) ?? [];
      list.push(fn);
      listeners.set(type, list);
    },
    removeEventListener(type, fn) {
      listeners.set(
        type,
        (listeners.get(type) ?? []).filter((f) => f !== fn),
      );
    },
    dispatchEvent(event) {
      for (const fn of listeners.get(event.type) ?? []) {
        fn(event);
      }
    },
  };
  const history = {
    pushState(_state, _title, url) {
      location.pathname = String(url);
    },
    replaceState(_state, _title, url) {
      location.pathname = String(url);
    },
  };
  const panels = [];
  const buttons = [];
  function track(list, text) {
    const node = {
      text,
      removed: false,
      remove() {
        this.removed = true;
      },
    };
    list.push(node);
    return () => node.remove();
  }
  G.bindPage({
    location,
    history,
    window,
    setTimeout: (fn, ms) => clock.setTimeout(fn, ms),
    clearTimeout: (t) => clock.clearTimeout(t),
    activate: () => track(buttons, "Ask agent"),
    activatePanel: () => track(panels, "gh-ask"),
  });
  const live = (list) => list.filter((node) => !node.removed);
  clock.advance(G.navDebounceMs);
  assert.equal(live(panels).length, 0);

  history.pushState({}, "", "/o/r/pull/1");
  clock.advance(G.navDebounceMs);
  assert.equal(live(panels).length, 1);
  assert.equal(live(buttons).length, 0);

  history.pushState({}, "", "/o/r/pull/1/files");
  clock.advance(G.navDebounceMs);
  assert.equal(live(panels).length, 1);
  assert.equal(live(buttons).length, 1);

  history.pushState({}, "", "/o/r/pull/1/commits");
  clock.advance(G.navDebounceMs);
  assert.equal(live(panels).length, 1);
  assert.equal(live(buttons).length, 0);

  history.pushState({}, "", "/o/r/pull/2");
  clock.advance(G.navDebounceMs);
  assert.equal(live(panels).length, 1);
  assert.equal(panels.filter((node) => node.removed).length, 1);

  history.pushState({}, "", "/o/r/pulls");
  clock.advance(G.navDebounceMs);
  assert.equal(live(panels).length, 0);
});

function keyEvent(extra) {
  return {
    key: "Enter",
    ctrlKey: false,
    metaKey: false,
    altKey: false,
    shiftKey: false,
    prevented: false,
    stopped: false,
    preventDefault() {
      this.prevented = true;
    },
    stopPropagation() {
      this.stopped = true;
    },
    ...extra,
  };
}

test("ctrl+enter and meta+enter send once from the question box", () => {
  for (const extra of [{ ctrlKey: true }, { metaKey: true }, { ctrlKey: true, metaKey: true }]) {
    const sends = [];
    const event = keyEvent(extra);
    let page = 0;
    const popover = () => {
      G.onPopoverKey(event, {
        question: "Why?",
        sending: false,
        send: () => sends.push("send"),
        close: () => sends.push("close"),
      });
    };
    popover();
    if (!event.stopped) {
      page += 1;
    }
    assert.deepEqual(sends, ["send"]);
    assert.equal(event.prevented, true);
    assert.equal(page, 0);
  }
});

test("plain enter does not send", () => {
  const event = keyEvent();
  let sends = 0;
  G.onPopoverKey(event, {
    question: "Why?",
    sending: false,
    send: () => {
      sends += 1;
    },
    close() {},
  });
  assert.equal(sends, 0);
  assert.equal(event.prevented, false);
  assert.equal(event.stopped, false);
});

test("an empty question or a send in flight does not send", () => {
  for (const ctx of [
    { question: "  ", sending: false },
    { question: "Why?", sending: true },
  ]) {
    const event = keyEvent({ ctrlKey: true });
    let sends = 0;
    G.onPopoverKey(event, {
      ...ctx,
      send: () => {
        sends += 1;
      },
      close() {},
    });
    assert.equal(sends, 0);
    assert.equal(event.stopped, true);
  }
});

test("ctrl+enter does nothing while the popover is closed", () => {
  const src = fs.readFileSync(path.join(__dirname, "gh-ask.user.js"), "utf8");
  const main = src.slice(src.indexOf("function main("));
  assert.match(main, /popover\.addEventListener\("keydown"/);
  assert.doesNotMatch(
    main,
    /(?:document|window)\.addEventListener\("keydown"[\s\S]{0,200}(?:ctrlKey|metaKey)/,
  );
});

test("send button names the shortcut", () => {
  const src = fs.readFileSync(path.join(__dirname, "gh-ask.user.js"), "utf8");
  assert.match(src, /Send \(⌘↵ \/ Ctrl\+↵\)/);
});

function headerToggles(root) {
  const out = [];
  const stack = [root];
  while (stack.length > 0) {
    const el = stack.pop();
    if ((el.getAttribute?.("class") || "").includes("gh-ask-header")) {
      out.push(el);
    }
    for (const kid of el.children || []) {
      stack.push(kid);
    }
  }
  return out;
}

function adopt(parent, el) {
  el.parentElement = parent;
  parent.childNodes.push(el);
}

function freshToggle() {
  return parse('<button class="gh-ask-btn gh-ask-header" data-fixture="toggle">gh-ask</button>')
    .children[0];
}

test("panel toggle prefers the sticky header, then the title", () => {
  const toggle = freshToggle();
  const sticky = fixture("sticky-header.html");
  assert.equal(G.mountPanelToggle(sticky, toggle), true);
  const copy = byAttr(sticky, "data-fixture", "title-copy");
  const metaKids = copy.parentElement.children;
  assert.equal(metaKids[indexOf(metaKids, copy) + 1], toggle);
  assert.equal(headerToggles(sticky).length, 1);
  assert.equal(G.mountPanelToggle(sticky, toggle), true);
  assert.equal(headerToggles(sticky).length, 1);

  const actionsOnly = parse(
    '<div class="miniHeaderInner"><div class="metadata"><span class="number">#1</span></div><div class="actions" data-fixture="actions"><button data-fixture="merge">Merge</button></div></div>',
  );
  const second = freshToggle();
  assert.equal(G.mountPanelToggle(actionsOnly, second), true);
  assert.equal(byAttr(actionsOnly, "data-fixture", "actions").children[0], second);

  const titleOnly = fixture("pr-header-titlearea.html");
  const third = freshToggle();
  assert.equal(G.mountPanelToggle(titleOnly, third), true);
  assert.equal(third.parentElement, byAttr(titleOnly, "data-fixture", "pr-title").parentElement);
  assert.equal(headerToggles(titleOnly).length, 1);
});

function indexOf(list, node) {
  return Array.prototype.indexOf.call(list, node);
}

test("panel toggle is moved once when the header is replaced or appears later", () => {
  const page = parse('<div id="page"></div>').children[0];
  const toggle = freshToggle();
  const first = fixture("sticky-header.html").children[0];
  adopt(page, first);
  assert.equal(G.mountPanelToggle(page, toggle), true);
  assert.equal(headerToggles(page).length, 1);

  page.childNodes.splice(indexOf(page.childNodes, first), 1);
  const again = fixture("sticky-header.html").children[0];
  adopt(page, again);
  assert.equal(G.mountPanelToggle(page, toggle), true);
  assert.equal(headerToggles(page).length, 1);
  const copy = byAttr(page, "data-fixture", "title-copy");
  assert.equal(copy.parentElement.children[indexOf(copy.parentElement.children, copy) + 1], toggle);
  assert.equal(G.mountPanelToggle(page, toggle), true);
  assert.equal(headerToggles(page).length, 1);

  const late = parse('<h1 class="js-issue-title" data-fixture="pr-title">Fix retry</h1>')
    .children[0];
  const plain = parse('<div id="plain"></div>').children[0];
  adopt(plain, late);
  const moved = freshToggle();
  assert.equal(G.mountPanelToggle(plain, moved), true);
  assert.equal(moved.parentElement, late.parentElement);
  const sticky = fixture("sticky-header.html").children[0];
  adopt(plain, sticky);
  assert.equal(G.mountPanelToggle(plain, moved), true);
  assert.equal(headerToggles(plain).length, 1);
  assert.equal(late.parentElement.children.includes(moved), false);
  const title = byAttr(plain, "data-fixture", "title-copy");
  assert.equal(
    title.parentElement.children[indexOf(title.parentElement.children, title) + 1],
    moved,
  );
});

test("TitleArea header is the h1", () => {
  const root = fixture("pr-header-titlearea.html");
  const header = G.prHeader(root);
  assert.ok(header);
  assert.equal(header.tagName, "H1");
  assert.equal(header.getAttribute("data-fixture"), "pr-title");
  const direct = parse(
    '<div data-component="TitleArea"><h1 data-fixture="pr-title">Fix the widget</h1></div>',
  );
  assert.equal(G.prHeader(direct).getAttribute("data-fixture"), "pr-title");
});

test("TitleArea without an h1 is not a header", () => {
  const root = parse('<div data-component="TitleArea"><div>Fix the widget</div></div>');
  assert.equal(G.prHeader(root), null);
  const deep = parse(
    '<div data-component="TitleArea"><div><div><h1 data-fixture="pr-title">Fix the widget</h1></div></div></div>',
  );
  assert.equal(G.prHeader(deep), null);
  const classic = parse(
    '<bdi class="js-issue-title" data-fixture="pr-title">Old</bdi><div data-component="TitleArea"><div><h1>New</h1></div></div>',
  );
  assert.equal(G.prHeader(classic).getAttribute("data-fixture"), "pr-title");
});

test("a panel question with no line is a whole-PR ask", () => {
  const src = fs.readFileSync(path.join(__dirname, "gh-ask.user.js"), "utf8");
  assert.doesNotMatch(src, /Ask about this PR/);
  assert.doesNotMatch(src, /Follow-up for the agent/);
  assert.match(src, /Question for the agent/);
  for (const v of views) {
    const got = G.panelAsk({
      href: `${v.href}?w=1#diff-abcR10`,
      headSha: v.head,
      question: "Is the design right?",
      deep: true,
    });
    assert.equal(got.error, undefined, v.name);
    assert.deepEqual(got.payload, {
      repo: "octo/demo",
      pr: 42,
      head_sha: v.head,
      question: "Is the design right?",
      deep: true,
      page_url: v.href,
    });
    for (const key of ["path", "side", "start_line", "end_line", "selection"]) {
      assert.equal(Object.hasOwn(got.payload, key), false, `${v.name} ${key}`);
    }
  }
});

test("far right anchor keeps the popover inside the viewport", () => {
  const viewport = { width: 1280, height: 800 };
  const size = { width: POPOVER_W, height: 140 };
  const anchor = { left: 1210, top: 120 };
  const got = G.placeInView(anchor, size, viewport);
  assert.equal(got.width, POPOVER_W);
  assert.equal(got.left, viewport.width - POPOVER_W - MARGIN);
  assert.equal(got.top, anchor.top);
  assert.ok(got.left >= MARGIN);
  assert.ok(got.left + got.width <= viewport.width - MARGIN);
  assert.ok(got.top + size.height <= viewport.height - MARGIN);
});

test("diff paths are files and changes views", () => {
  for (const path of [
    "/o/r/pull/1/files",
    "/o/r/pull/1/changes",
    "/o/r/pull/1/files/abcdef",
    "/o/r/pull/1/files/",
  ]) {
    assert.equal(G.isDiffPath(path), true, path);
  }
  for (const path of ["/o/r/pulls", "/o/r/pull/1", "/o/r/pull/1/commits", "/pull/1/files"]) {
    assert.equal(G.isDiffPath(path), false, path);
  }
});

function diffPage(pathname) {
  const clock = fakeClock();
  const location = { pathname };
  const listeners = new Map();
  const window = {
    Event: class Event {
      constructor(type) {
        this.type = type;
      }
    },
    addEventListener(type, fn) {
      const list = listeners.get(type) ?? [];
      list.push(fn);
      listeners.set(type, list);
    },
    removeEventListener(type, fn) {
      listeners.set(
        type,
        (listeners.get(type) ?? []).filter((f) => f !== fn),
      );
    },
    dispatchEvent(event) {
      for (const fn of listeners.get(event.type) ?? []) {
        fn(event);
      }
    },
  };
  const history = {
    pushState(_state, _title, url) {
      location.pathname = String(url);
    },
    replaceState(_state, _title, url) {
      location.pathname = String(url);
    },
  };
  const nodes = [];
  function mount() {
    const made = ["Ask agent"].map((text) => {
      const node = {
        text,
        removed: false,
        remove() {
          this.removed = true;
        },
      };
      nodes.push(node);
      return node;
    });
    return () => {
      for (const node of made) {
        node.remove();
      }
    };
  }
  const binding = G.bindPage({
    location,
    history,
    window,
    setTimeout: (fn, ms) => clock.setTimeout(fn, ms),
    clearTimeout: (t) => clock.clearTimeout(t),
    activate: mount,
  });
  return {
    location,
    history,
    window,
    clock,
    binding,
    buttons: () => nodes.filter((node) => !node.removed),
  };
}

function settle(page) {
  page.clock.advance(G.navDebounceMs);
}

test("soft navigation activates only on a diff and does not duplicate buttons", () => {
  const page = diffPage("/o/r/pulls");
  settle(page);
  assert.equal(page.buttons().length, 0);

  page.history.pushState({}, "", "/o/r/pull/1/files");
  settle(page);
  assert.deepEqual(
    page.buttons().map((b) => b.text),
    ["Ask agent"],
  );

  page.history.pushState({}, "", "/o/r/pull/1");
  settle(page);
  assert.equal(page.buttons().length, 0);

  page.history.pushState({}, "", "/o/r/pull/1/files");
  settle(page);
  assert.deepEqual(
    page.buttons().map((b) => b.text),
    ["Ask agent"],
  );

  page.history.pushState({}, "", "/o/r/pull/2/files");
  settle(page);
  assert.deepEqual(
    page.buttons().map((b) => b.text),
    ["Ask agent"],
  );
});

test("turbo:load activates a diff view", () => {
  const page = diffPage("/o/r/pulls");
  settle(page);
  page.location.pathname = "/o/r/pull/1/files";
  page.window.dispatchEvent(new page.window.Event("turbo:load"));
  settle(page);
  assert.equal(page.buttons().length, 1);

  page.window.dispatchEvent(new page.window.Event("turbo:load"));
  settle(page);
  assert.equal(page.buttons().length, 1);
});

test("userscript matches all of github.com", () => {
  const src = fs.readFileSync(path.join(__dirname, "gh-ask.user.js"), "utf8");
  assert.match(src, /^\/\/ @match\s+https:\/\/github\.com\/\*$/m);
  assert.doesNotMatch(src, /@match\s+https:\/\/github\.com\/\*\/\*\/pull/);
});

test("anchor near the bottom opens the popover above", () => {
  const viewport = { width: 800, height: 600 };
  const size = { width: POPOVER_W, height: 180 };
  const anchor = { left: 200, top: 520 };
  const got = G.placeInView(anchor, size, viewport);
  assert.equal(got.top, anchor.top - size.height);
  assert.ok(got.top + size.height <= anchor.top);
  assert.ok(got.top >= MARGIN);
  assert.equal(got.left, anchor.left);
  assert.equal(got.width, POPOVER_W);
});

test("narrow viewport shrinks the popover and keeps it inside", () => {
  const height = 120;
  // 400 - 2*8 is still wider than 360, so the box stays 360 and slides in.
  const at400 = G.placeInView(
    { left: 360, top: 80 },
    { width: POPOVER_W, height },
    { width: 400, height: 600 },
  );
  assert.equal(at400.width, Math.min(POPOVER_W, 400 - 2 * MARGIN));
  assert.ok(at400.left >= MARGIN);
  assert.ok(at400.left + at400.width <= 400 - MARGIN);
  assert.ok(at400.top >= MARGIN);
  assert.ok(at400.top + height <= 600 - MARGIN);

  // Narrower than the popover plus margins: the width itself shrinks.
  const at300 = G.placeInView(
    { left: 280, top: 80 },
    { width: POPOVER_W, height },
    { width: 300, height: 600 },
  );
  assert.equal(at300.width, 300 - 2 * MARGIN);
  assert.ok(at300.width < POPOVER_W);
  assert.equal(at300.left, MARGIN);
  assert.ok(at300.left + at300.width <= 300 - MARGIN);
});

test("middle anchor stays where it is today", () => {
  const viewport = { width: 1280, height: 800 };
  const anchor = { left: 420, top: 240 };
  const size = { width: POPOVER_W, height: 150 };
  const got = G.placeInView(anchor, size, viewport);
  // place() today sets left to max(0, anchor.left) and keeps the top.
  assert.deepEqual(got, {
    left: Math.max(0, anchor.left),
    top: anchor.top,
    width: POPOVER_W,
  });
});

test("a button near the right edge uses the same clamp", () => {
  const viewport = { width: 1280, height: 800 };
  const got = G.placeInView({ left: 1270, top: 300 }, { width: 72, height: 22 }, viewport);
  assert.equal(got.width, 72);
  assert.equal(got.left, viewport.width - 72 - MARGIN);
  assert.equal(got.top, 300);
  assert.ok(got.left + got.width <= viewport.width - MARGIN);
});

// split-view-captured.html is the classic split diff GitHub served for a
// public pull request (see its header): context lines 510-512, line 513
// deleted on the left and added on the right, and context line 514.
const SP = "diff-35cadb9bdc1334ec5640179c4a8ea8dde57326f4c430717bd13bef62f67aa3c1";
const SPLIT_HREF = "https://github.com/jaeyeom/experimental/pull/402/files";
const SPLIT_HEAD = "a68e2bcc224ccb1b0c337a9aed149143d36b07ce";

function splitCell(root, side, n) {
  const num = byAttr(root, "id", `${SP}${side[0]}${n}`);
  const row = num.parentElement;
  return row.children.find((c) => c.getAttribute("data-split-side") === side.toLowerCase());
}

function rects(map) {
  return (node) => map.get(node);
}

function splitAsk(start, end = start) {
  return G.buildPayload({
    href: SPLIT_HREF,
    headSha: SPLIT_HEAD,
    start,
    end,
    selection: "",
    question: "q",
    deep: false,
  });
}

test("split view: a deleted left line asks on the LEFT side", () => {
  const root = fixture("split-view-captured.html");
  const cell = splitCell(root, "LEFT", 513);
  const info = G.lineInfo(cell);
  assert.deepEqual([info.side, info.line, info.sideCell], ["LEFT", 513, cell]);
  const p = splitAsk(cell).payload;
  assert.deepEqual([p.side, p.start_line, p.end_line], ["LEFT", 513, 513]);
  assert.equal(p.page_url, `${SPLIT_HREF}#${SP}L513`);
});

test("split view: a left context line asks on the LEFT side", () => {
  const root = fixture("split-view-captured.html");
  const cell = splitCell(root, "LEFT", 511);
  const p = splitAsk(cell).payload;
  assert.deepEqual([p.side, p.start_line, p.end_line], ["LEFT", 511, 511]);
});

test("split view: a right line is unchanged", () => {
  const root = fixture("split-view-captured.html");
  const cell = splitCell(root, "RIGHT", 514);
  const info = G.lineInfo(cell);
  assert.deepEqual([info.side, info.line], ["RIGHT", 514]);
  assert.equal(info.sideCell, cell);
  const p = splitAsk(cell).payload;
  assert.deepEqual([p.side, p.start_line], ["RIGHT", 514]);
});

test("split view: the empty left cell of an added line has no line", () => {
  const root = fixture("split-view-captured.html");
  const empty = byAttr(root, "id", `${SP}R514`).parentElement.children.find(
    (c) => c.getAttribute("data-split-side") === "left",
  );
  assert.equal(G.lineInfo(empty), null);
});

test("split view: the hover button sits inside the hovered side's cell", () => {
  const root = fixture("split-view-captured.html");
  const left = splitCell(root, "LEFT", 513);
  const right = splitCell(root, "RIGHT", 513);
  const row = left.parentElement;
  const rectOf = rects(
    new Map([
      [row, { top: 100, left: 0, right: 800 }],
      [left, { top: 100, left: 40, right: 400 }],
      [right, { top: 100, left: 440, right: 800 }],
    ]),
  );
  const l = G.hoverPlacement(G.lineInfo(left), 80, rectOf);
  assert.ok(l.left >= 40 && l.left + 80 <= 400, `left button at ${l.left}`);
  assert.equal(l.top, 100);
  const r = G.hoverPlacement(G.lineInfo(right), 80, rectOf);
  assert.equal(r.left, 800 - 80 - 4);
});

test("unified view: the hover button stays at the row's right edge", () => {
  const root = fixture("changes-view-captured.html");
  const info = G.lineInfo(byAttr(root, "data-line-anchor", `${C}R1`));
  assert.equal(info.sideCell, null);
  const rectOf = rects(new Map([[info.row, { top: 5, left: 0, right: 900 }]]));
  assert.deepEqual(G.hoverPlacement(info, 80, rectOf), { top: 5, left: 816 });
});

test("split view: moving from a left cell to its button keeps the LEFT target", () => {
  const root = fixture("split-view-captured.html");
  const left = splitCell(root, "LEFT", 513);
  const first = G.nextHover(null, left, false);
  assert.equal(G.lineInfo(first.start).side, "LEFT");
  // The pointer reaches the button, and even the empty right cell
  // beside a deleted line, without dropping the target.
  assert.equal(G.nextHover(first, left, true), first);
  const empty = byAttr(root, "id", `${SP}R514`).parentElement.children.find(
    (c) => c.getAttribute("data-split-side") === "left",
  );
  assert.equal(G.nextHover(null, empty, false), null);
});

test("split view: chips attach to the hovered side's cell", () => {
  const root = fixture("split-view-captured.html");
  const left = splitCell(root, "LEFT", 513);
  const right = splitCell(root, "RIGHT", 513);
  assert.equal(G.chipHost(G.lineInfo(left)), left);
  assert.equal(G.chipHost(G.lineInfo(right)), right);
  const uni = G.lineInfo(byAttr(fixture("changes-view-captured.html"), "data-line-anchor", `${C}R1`));
  assert.equal(G.chipHost(uni), uni.row.lastElementChild);
});

function createBrowserEnv(root, pathname = "/octo/demo/pull/42") {
  const clock = fakeClock();
  const gmStyles = [];
  const headChildren = [];
  const bodyChildren = [];
  const winListeners = new Map();
  const docListeners = new Map();

  function addListener(map, type, fn) {
    const list = map.get(type) ?? [];
    list.push(fn);
    map.set(type, list);
  }
  function removeListener(map, type, fn) {
    map.set(
      type,
      (map.get(type) ?? []).filter((f) => f !== fn),
    );
  }
  function makeNode(tag) {
    const node = {
      nodeType: 1,
      tagName: tag.toUpperCase(),
      attrs: {},
      parentElement: null,
      childNodes: [],
      style: {},
      hidden: false,
      textContent: "",
      get children() {
        return this.childNodes.filter((n) => n.nodeType === 1);
      },
      get className() {
        return this.attrs.class || "";
      },
      set className(v) {
        this.attrs.class = String(v);
      },
      getAttribute(name) {
        return Object.hasOwn(this.attrs, name) ? this.attrs[name] : null;
      },
      append(...items) {
        for (const item of items) {
          if (typeof item === "string") {
            this.childNodes.push({ nodeType: 3, textContent: item, parentElement: this });
          } else {
            item.parentElement = this;
            this.childNodes.push(item);
          }
        }
      },
      remove() {
        if (!this.parentElement) {
          return;
        }
        const list = this.parentElement.childNodes;
        const i = list.indexOf(this);
        if (i >= 0) {
          list.splice(i, 1);
        }
        this.parentElement = null;
      },
      contains(other) {
        for (let e = other; e; e = e.parentElement) {
          if (e === this) {
            return true;
          }
        }
        return false;
      },
      addEventListener() {},
      removeEventListener() {},
    };
    return node;
  }

  const head = {
    childNodes: headChildren,
    get children() {
      return headChildren;
    },
    append(...items) {
      for (const item of items) {
        item.parentElement = this;
        headChildren.push(item);
      }
    },
  };
  const body = {
    childNodes: bodyChildren,
    get children() {
      return bodyChildren;
    },
    append(...items) {
      for (const item of items) {
        item.parentElement = this;
        bodyChildren.push(item);
      }
    },
  };
  const location = {
    pathname,
    get href() {
      return `https://github.com${this.pathname}`;
    },
  };
  const history = {
    pushState(_state, _title, url) {
      location.pathname = String(url);
    },
    replaceState(_state, _title, url) {
      location.pathname = String(url);
    },
  };
  const document = {
    documentElement: root,
    head,
    body,
    visibilityState: "visible",
    createElement: makeNode,
    addEventListener: (type, fn) => addListener(docListeners, type, fn),
    removeEventListener: (type, fn) => removeListener(docListeners, type, fn),
    querySelectorAll: () => [],
  };
  const window = {
    location,
    history,
    innerWidth: 1280,
    innerHeight: 800,
    scrollX: 0,
    scrollY: 0,
    Event: class Event {
      constructor(type) {
        this.type = type;
      }
    },
    addEventListener: (type, fn) => addListener(winListeners, type, fn),
    removeEventListener: (type, fn) => removeListener(winListeners, type, fn),
    dispatchEvent(event) {
      for (const fn of winListeners.get(event.type) ?? []) {
        fn(event);
      }
    },
    setTimeout: (fn, ms) => clock.setTimeout(fn, ms),
    clearTimeout: (t) => clock.clearTimeout(t),
  };
  return {
    document,
    window,
    clock,
    gmStyles,
    GM_addStyle: (css) => gmStyles.push(css),
    MutationObserver: class {
      observe() {}
      disconnect() {}
    },
  };
}

function allStyles(env) {
  return [...env.gmStyles, ...env.document.head.children.map((el) => el.textContent)];
}

test("conversation fixture with only activatePanel() includes shared button and chip rules", () => {
  const root = fixture("conversation-view.html");
  const env = createBrowserEnv(root, "/octo/demo/pull/42");
  const stop = G.activatePanel(env);
  try {
    assert.equal(headerToggles(root).length, 1);
    const shared = env.gmStyles.filter((s) => s.includes(".gh-ask-btn"));
    assert.equal(shared.length, 1);
    assert.match(shared[0], /\.gh-ask-btn\.gh-ask-header\b/);
    assert.match(shared[0], /\.gh-ask-chip\b/);
    assert.equal(
      env.gmStyles.some((s) => s.includes(".gh-ask-panel")),
      true,
    );
    assert.equal(
      env.document.head.children.some((el) => el.textContent.includes(".gh-ask-pop")),
      false,
    );
  } finally {
    stop();
  }
});

test("soft navigation from conversation to files adds shared rules once and keeps diff-only rules", () => {
  const root = fixture("conversation-view.html");
  const env = createBrowserEnv(root, "/octo/demo/pull/42");
  const binding = G.bindPage({
    location: env.window.location,
    history: env.window.history,
    window: env.window,
    setTimeout: (fn, ms) => env.clock.setTimeout(fn, ms),
    clearTimeout: (t) => env.clock.clearTimeout(t),
    activate: () => G.main(env),
    activatePanel: () => G.activatePanel(env),
  });
  try {
    env.clock.advance(G.navDebounceMs);
    assert.equal(allStyles(env).filter((s) => s.includes(".gh-ask-btn")).length, 1);
    assert.equal(
      env.document.head.children.some((el) => el.textContent.includes(".gh-ask-pop")),
      false,
    );

    env.window.history.pushState({}, "", "/octo/demo/pull/42/files");
    env.clock.advance(G.navDebounceMs);
    assert.equal(allStyles(env).filter((s) => s.includes(".gh-ask-btn")).length, 1);
    assert.equal(
      env.document.head.children.some((el) => el.textContent.includes(".gh-ask-pop")),
      true,
    );

    env.window.history.pushState({}, "", "/octo/demo/pull/42/commits");
    env.clock.advance(G.navDebounceMs);
    assert.equal(allStyles(env).filter((s) => s.includes(".gh-ask-btn")).length, 1);
    assert.equal(
      env.document.head.children.some((el) => el.textContent.includes(".gh-ask-pop")),
      false,
    );

    env.window.history.pushState({}, "", "/octo/demo/pull/42/changes");
    env.clock.advance(G.navDebounceMs);
    assert.equal(allStyles(env).filter((s) => s.includes(".gh-ask-btn")).length, 1);
    assert.equal(
      env.document.head.children.some((el) => el.textContent.includes(".gh-ask-pop")),
      true,
    );
  } finally {
    binding.stop();
  }
});

test("loading files view first adds shared rules once even after activatePanel()", () => {
  const root = fixture("files-view.html");
  const env = createBrowserEnv(root, "/octo/demo/pull/42/files");
  const stopMain = G.main(env);
  const stopPanel = G.activatePanel(env);
  try {
    assert.equal(allStyles(env).filter((s) => s.includes(".gh-ask-btn")).length, 1);
    assert.equal(
      env.document.head.children.some((el) => el.textContent.includes(".gh-ask-pop")),
      true,
    );
  } finally {
    stopMain();
    stopPanel();
  }
});
