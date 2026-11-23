// Touch swipe on a row: right archives, left snoozes. Swipes are only a
// shortcut; every row also has Archive and Snooze buttons and keys.

const THRESHOLD = 80;

// swipeDecision maps a drag to an action. A mostly vertical drag is a
// scroll and does nothing.
export function swipeDecision(dx, dy, threshold = THRESHOLD) {
  if (Math.abs(dx) < threshold || Math.abs(dx) < Math.abs(dy) * 1.5) return null;
  return dx > 0 ? 'archive' : 'snooze';
}

// attachSwipe wires pointer swipes on row. onArchive and onSnooze may be
// null to disable a direction. The row snaps back after every gesture; the
// handlers decide what changes.
export function attachSwipe(row, { onArchive, onSnooze }) {
  let start = null;
  let moved = false;
  const reset = () => { row.style.transform = ''; start = null; };
  row.addEventListener('pointerdown', (e) => {
    if (e.pointerType === 'mouse' || !e.isPrimary) return;
    start = { x: e.clientX, y: e.clientY };
    moved = false;
  });
  row.addEventListener('pointermove', (e) => {
    if (!start) return;
    const dx = e.clientX - start.x;
    const dy = e.clientY - start.y;
    if (Math.abs(dx) > 10 && Math.abs(dx) > Math.abs(dy)) {
      moved = true;
      const allowed = dx > 0 ? onArchive : onSnooze;
      row.style.transform = allowed ? `translateX(${Math.max(-140, Math.min(140, dx))}px)` : '';
    }
  });
  row.addEventListener('pointerup', (e) => {
    if (!start) return;
    const act = swipeDecision(e.clientX - start.x, e.clientY - start.y);
    reset();
    if (act === 'archive' && onArchive) onArchive();
    else if (act === 'snooze' && onSnooze) onSnooze();
  });
  row.addEventListener('pointercancel', reset);
  // A swipe that ends on the link must not also open the thread.
  row.addEventListener('click', (e) => {
    if (moved) { e.preventDefault(); e.stopPropagation(); moved = false; }
  }, true);
}
