// Typed fetch wrapper for /api/v1. Errors are values, never thrown for
// HTTP status: callers inspect {status, data}.

export class ApiError extends Error {
  constructor(status, data) {
    super(`api ${status}`);
    this.status = status;
    this.data = data;
  }
}

export const unauthorized = (e) => e instanceof ApiError && e.status === 401;

// onUnauthorized is called on any 401 so the app can show the login view.
let onUnauthorized = () => {};
export function setUnauthorizedHandler(fn) { onUnauthorized = fn; }

export async function call(method, path, body) {
  const init = {
    method,
    credentials: 'same-origin',
    headers: { 'X-Decide-Client': '1', 'Content-Type': 'application/json' },
  };
  if (body !== undefined) init.body = JSON.stringify(body);
  const res = await fetch('/api/v1' + path, init); // network errors reject
  let data = null;
  try { data = await res.json(); } catch { /* empty body */ }
  if (res.status === 401) onUnauthorized();
  if (!res.ok) throw new ApiError(res.status, data);
  return data;
}

export const get = (path) => call('GET', path);
export const post = (path, body) => call('POST', path, body ?? {});
export const authStatus = () => get('/auth/status');
export const login = (token) => post('/auth/login', { token });
export const logout = () => post('/auth/logout');
