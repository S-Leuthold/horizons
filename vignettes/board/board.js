/* The pipeline board: builds the cards from the embedded JSON, wires the diagram to them, and the lane toggle. */
(function () {
  var root = document.getElementById('hz-board');
  if (!root) return;
  var data = JSON.parse(document.getElementById('hz-data').textContent);
  var cards = document.getElementById('hz-cards');

  function el(tag, cls, html) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (html !== undefined) e.innerHTML = html;
    return e;
  }
  function esc(s) {
    return String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }
  /* `code` spans in Rd text arrive as backticks */
  function md(s) { return esc(s).replace(/`([^`]+)`/g, '<code>$1</code>'); }
  function src(p) { return p ? '<span class="hz-src hz-mono">' + esc(p) + '</span>' : ''; }
  function list(items, fmt, ordered) {
    if (!items || !items.length) return '';
    var tag = ordered ? 'ol' : 'ul';
    return '<' + tag + '>' + items.map(function (it) { return '<li>' + fmt(it) + '</li>'; }).join('') + '</' + tag + '>';
  }

  function lanesAttr(v) { return v.lanes.length === 2 ? 'both' : v.lanes[0]; }

  function verbCard(v) {
    var card = el('details', 'hz-card');
    card.setAttribute('data-lanes', lanesAttr(v));
    card.id = 'hz-card-' + v.verb;

    var badges = v.lanes.map(function (l) { return '<span class="hz-badge hz-badge-' + l + '">' + l + '</span>'; }).join('');
    if (v.design_ahead && v.design_ahead.length) badges += '<span class="hz-badge hz-badge-flag">design ahead of code</span>';
    if (v.optional) badges += '<span class="hz-badge hz-badge-flag">optional</span>';

    /* the one-line summary lives inside <summary> so it stays visible when the card is closed */
    var head = el('summary', 'hz-card-head',
      '<span class="hz-head-row"><span class="hz-verb hz-mono">' + esc(v.display || v.verb) + '()</span>' +
      (v.display && v.display !== v.verb ? '<span class="hz-class hz-mono">' + esc(v.verb) + '</span>' : '') +
      '<span class="hz-class">' + esc(v.class_in) + ' → ' + esc(v.class_out) + '</span>' +
      '<span class="hz-badges">' + badges + '</span></span>' +
      '<span class="hz-card-summary">' + md(v.summary) + '</span>');
    card.appendChild(head);

    var body = el('div', 'hz-card-body');
    var h = '';

    if (v.design_ahead && v.design_ahead.length) {
      h += '<div class="hz-flag"><strong>Design ahead of code.</strong> The card shows what is built. Pending: ' +
           list(v.design_ahead, function (d) { return md(d.text || d) + src(d.source); }) + '</div>';
    }

    h += '<h4>Signature</h4><pre class="hz-sig hz-mono">' + esc(v.signature) + '</pre>';
    h += '<h4>Arguments</h4><table class="hz-args"><thead><tr><th>argument</th><th>default</th><th>what it is</th><th>when to touch it</th></tr></thead><tbody>';
    v.args.forEach(function (a) {
      h += '<tr><td class="hz-name hz-mono">' + esc(a.name) + '</td>' +
           '<td class="hz-default hz-mono">' + (a.default == null ? '<em>required</em>' : esc(a.default)) + '</td>' +
           '<td>' + md(a.text) + '</td>' +
           '<td>' + md(a.touch) + src(a.source) + '</td></tr>';
    });
    h += '</tbody></table>';

    if (v.mechanics.length) h += '<h4>Behind the scenes</h4>' + list(v.mechanics, function (m) { return md(m.step) + src(m.source); }, true);
    if (v.records.length)   h += '<h4>What it records</h4>' + list(v.records, function (r) { return '<code>' + esc(r.field) + '</code>: ' + md(r.text) + src(r.source); });
    if (v.refuses.length)   h += '<h4>What it refuses or warns about</h4>' + list(v.refuses, function (r) { return '<em>when</em> ' + md(r.when) + ' <em>it</em> ' + md(r.does) + src(r.source); });
    if (v.evidence.length)  h += '<h4>Evidence behind the defaults</h4><div class="hz-evidence">' + list(v.evidence, function (e) { return md(e.claim) + (e.value ? ' (' + esc(e.value) + ')' : '') + src(e.where); }) + '</div>';
    if (v.issues.length)    h += '<h4>Open issues</h4>' + list(v.issues, function (i) { return '<span class="hz-mono">#' + esc(i.id) + '</span> ' + md(i.text) + src(i.source); });
    if (v.notes.length)     h += '<h4>Notes</h4>' + list(v.notes, function (n) { return md(n.text) + src(n.source); });

    body.innerHTML = h;
    card.appendChild(body);
    return card;
  }

  function divergenceCard(d) {
    var card = el('div', 'hz-diverge');
    card.id = 'hz-div-' + d.id;
    card.innerHTML =
      '<strong>' + esc(d.title) + '</strong> <span class="hz-src">after ' + esc(d.after) + '()</span>' +
      '<div class="hz-two">' +
        '<div class="hz-d-local"><h5>Local: ' + esc(d.local.verb) + '()</h5><p><em>trained on</em> ' + md(d.local.training_set) + '</p><p><em>targets</em> ' + md(d.local.targets) + '</p></div>' +
        '<div class="hz-d-library"><h5>Library: ' + esc(d.library.verb) + '()</h5><p><em>trained on</em> ' + md(d.library.training_set) + '</p><p><em>targets</em> ' + md(d.library.targets) + '</p></div>' +
      '</div>' +
      '<p>' + md(d.differs) + src(d.source) + '</p>';
    return card;
  }

  /* cards in pipeline order, with each divergence placed after the verb it follows */
  var byOrder = data.verbs.slice().sort(function (a, b) { return a.order - b.order; });
  byOrder.forEach(function (v) {
    cards.appendChild(verbCard(v));
    data.divergences.forEach(function (d) { if (d.after === v.verb) cards.appendChild(divergenceCard(d)); });
  });

  /* diagram -> card */
  root.querySelectorAll('.hz-diagram .node').forEach(function (n) {
    n.addEventListener('click', function () {
      var c = document.getElementById('hz-card-' + n.getAttribute('data-verb'));
      if (!c) return;
      c.open = true;
      c.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });
  });
  root.querySelectorAll('.hz-diagram .divergence').forEach(function (n) {
    n.addEventListener('click', function () {
      var c = document.getElementById('hz-div-' + n.getAttribute('data-divergence'));
      if (c) c.scrollIntoView({ behavior: 'smooth', block: 'start' });
    });
  });

  /* lane toggle */
  root.querySelectorAll('.hz-lane-btn').forEach(function (b) {
    b.addEventListener('click', function () {
      var lane = b.getAttribute('data-lane');
      root.querySelectorAll('.hz-lane-btn').forEach(function (x) { x.setAttribute('aria-pressed', x === b ? 'true' : 'false'); });
      if (lane === 'both') root.removeAttribute('data-focus'); else root.setAttribute('data-focus', lane);
    });
  });
})();
