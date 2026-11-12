// Diagnostic banner injected into the exported Shinylive index.html (see export_shinylive.R).
// Shinylive's host page fails silently on phones; this prints the first errors on screen so a
// screenshot is enough to diagnose. Shows nothing when everything works.
(function () {
  // Cross-origin isolation: Shinylive's service worker adds COOP/COEP headers only when the page
  // is opened with ?coi=1. With them, webR gets SharedArrayBuffer and uses its native channel
  // instead of the PostMessage fallback, whose stack unwinding overflowed the call stack on iOS
  // Safari ("RangeError: Maximum call stack size exceeded"). Send every visit there.
  try {
    if (!/[?&]coi=1(&|$)/.test(location.search) && !/_shinylive-mode=/.test(location.search)) {
      var q = location.search ? location.search + '&coi=1' : '?coi=1';
      location.replace(location.pathname + q + location.hash);
      return;
    }
  } catch (e) {}
  var lines = [], box = null, started = Date.now(), appSeen = false;
  function show(msg) {
    lines.push(msg); if (lines.length > 10) lines.shift();
    if (!box) { box = document.createElement('pre');
      box.style.cssText = 'position:fixed;left:0;right:0;bottom:0;max-height:45vh;overflow:auto;margin:0;padding:8px 10px;' +
        'background:#fff3cd;color:#5c3b00;border-top:2px solid #e0a800;font:12px/1.4 -apple-system,Menlo,monospace;' +
        'white-space:pre-wrap;z-index:2147483647';
      (document.body || document.documentElement).appendChild(box); }
    box.textContent = 'DPR explorer diagnostics\n' + navigator.userAgent + '\n' + lines.join('\n');
  }
  window.addEventListener('error', function (e) { show('Error: ' + (e.message || e) + (e.filename ? ' @ ' + e.filename.split('/').pop() + ':' + e.lineno : '')); });
  window.addEventListener('unhandledrejection', function (e) { var r = e.reason || {};
    show('Rejection: ' + (r.name || '') + ' ' + (r.message || (typeof r === 'string' ? r : '')) + '\n[last stage: ' + lastStage + ']\n' +
      String(r.stack || '').split('\n').slice(0, 14).map(function (l) { return l.replace(/@wasm-function/g, '').replace(/^\s*(\d+)\[\1\]$/, '$1'); }).join(' > ')); });
  var lastStage = '(none yet)';
  var cl = console.log; console.log = function () {
    try { var t = Array.prototype.slice.call(arguments).join(' '); var m = t.match(/DPR-STAGE:\s*(.*)$/); if (m) lastStage = m[1]; } catch (x) {}
    cl.apply(console, arguments); };
  var ce = console.error; console.error = function () {
    try { var t = Array.prototype.slice.call(arguments).join(' ');
      // webR routes R's stderr through console.error; only surface lines that look like real errors
      var body = t.replace(/^preload error:\s*/, '');
      var fromR = /^preload error:/.test(t);
      if (body && (!fromR || /\b(Error|error in|cannot|failed|no package|unable)\b/.test(body))) show('console.error: ' + body.slice(0, 300)); } catch (x) {}
    ce.apply(console, arguments); };
  if (!('serviceWorker' in navigator)) show('No service worker support in this browser context. Shinylive needs one: open the link directly in Safari or Chrome (not inside Messages, Instagram, Facebook, etc.) and not in Private mode.');
  if (typeof WebAssembly === 'undefined') show('WebAssembly is not available in this browser.');
  if (typeof SharedArrayBuffer === 'undefined') lines.push('(info) SharedArrayBuffer unavailable (crossOriginIsolated=' + self.crossOriginIsolated + '): webR is on the PostMessage fallback channel.');
  if (navigator.deviceMemory) lines.push('(info) deviceMemory ≈ ' + navigator.deviceMemory + ' GB');
  // watchdog: if the app iframe never shows content, say so
  var t = setInterval(function () {
    try { var f = document.querySelector('iframe'); var d = f && f.contentDocument;
      if (d && d.body && d.body.children.length > 0) { appSeen = true; clearInterval(t); } } catch (x) {}
    if (!appSeen && Date.now() - started > 90000) { clearInterval(t); show('Still no app after 90 s. Likely out of memory or a blocked download. Try: close other tabs, reload once, or open in Safari.'); }
  }, 3000);
})();
