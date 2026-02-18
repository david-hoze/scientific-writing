// ─── Type-driven renderers for qec-notebook ───
// Each function takes (data, msg) and returns a DOM element.

const RENDERERS = {
  css_code:           renderCSSCode,
  code_construction:  renderCodeConstruction,
  cat_qubit_params:   renderCatParams,
  pauli_channel:      renderPauliChannel,
  sim_result:         renderSimResult,
  threshold_plot:     renderThresholdPlot,
  sweep_progress:     renderThresholdPlot,
  resource_estimate:  renderResourceEstimate,
  resource_comparison: renderResourceComparison,
  bin_matrix:         renderBinMatrix,
  error:              renderError,
};

// ── Utilities ──

function sci(x, digits = 2) {
  if (x === 0) return '0';
  const exp = Math.floor(Math.log10(Math.abs(x)));
  const mantissa = x / Math.pow(10, exp);
  if (exp === 0) return mantissa.toFixed(digits);
  const supDigits = '⁰¹²³⁴⁵⁶⁷⁸⁹';
  const supMinus = '⁻';
  let expStr = '';
  const absExp = Math.abs(exp);
  const expDigits = String(absExp);
  for (const ch of expDigits) expStr += supDigits[parseInt(ch)];
  if (exp < 0) expStr = supMinus + expStr;
  return mantissa.toFixed(digits) + '×10' + expStr;
}

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

function numFmt(n) {
  return n.toLocaleString();
}

// ── Main dispatcher ──

function renderCellOutput(msg) {
  if (msg.status === 'decl_ok') {
    return renderDeclOk(msg);
  }
  if (msg.status === 'error') {
    const renderer = RENDERERS[msg.render_as];
    if (renderer && msg.data) return renderer(msg.data, msg);
    return renderError(msg.data || { message: 'Unknown error' }, msg);
  }
  const renderer = RENDERERS[msg.render_as];
  if (renderer && msg.data) {
    return renderer(msg.data, msg);
  }
  return renderFallback(msg.show_text, msg.haskell_type, msg);
}

function renderDeclOk(msg) {
  const d = el('div', 'decl-ok');
  d.textContent = '✓';
  if (msg.elapsed_ms > 0) {
    d.textContent += ` (${msg.elapsed_ms} ms)`;
  }
  return d;
}

function renderFallback(showText, haskellType, msg) {
  const container = el('div');
  if (haskellType) {
    container.appendChild(el('div', 'output-type', ':: ' + haskellType));
  }
  container.appendChild(el('pre', 'output-fallback', showText || ''));
  if (msg && msg.elapsed_ms) {
    container.appendChild(el('span', 'elapsed', msg.elapsed_ms + ' ms'));
  }
  return container;
}

// ── css_code ──

function renderCSSCode(data, msg) {
  const card = el('div', 'code-card');

  card.appendChild(el('div', 'code-label', data.label));

  const props = [
    ['n', data.n],
    ['k', data.k],
    ['d', data.d],
    ['Rate', (data.rate || 0).toFixed(3)],
    ['H_X rows', data.hx_rows],
    ['H_Z rows', data.hz_rows],
    ['H_Z weights', (data.hz_weights || []).join(', ')],
    ['H_X weights', (data.hx_weights || []).join(', ')],
  ];

  for (const [name, value] of props) {
    card.appendChild(el('span', 'prop-name', name));
    card.appendChild(el('span', 'prop-value', String(value)));
  }

  if (data.hx_rows === 0) {
    card.appendChild(el('div', 'note', 'Z-sector only (cat qubit regime)'));
  }

  // Tanner graph for small codes
  if (data.tanner && data.n <= 200 && data.tanner.edges) {
    const tannerEl = renderTannerGraph(data.tanner);
    if (tannerEl) {
      tannerEl.style.gridColumn = '1 / -1';
      tannerEl.style.marginTop = '12px';
      card.appendChild(tannerEl);
    }
  }

  return card;
}

function renderTannerGraph(tanner) {
  const width = 400, height = 300;
  const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
  svg.setAttribute('width', width);
  svg.setAttribute('height', height);
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
  svg.style.background = '#0d1117';
  svg.style.borderRadius = '6px';
  svg.style.border = '1px solid #30363d';

  const nc = tanner.num_checks;
  const nq = tanner.num_qubits;

  // Place checks on left, qubits on right
  const checkX = 60, qubitX = width - 60;
  const pad = 20;

  const checkY = (i) => pad + (i / Math.max(1, nc - 1)) * (height - 2 * pad);
  const qubitY = (i) => pad + (i / Math.max(1, nq - 1)) * (height - 2 * pad);

  // Edges
  for (const edge of tanner.edges) {
    const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    line.setAttribute('x1', checkX);
    line.setAttribute('y1', checkY(edge.check));
    line.setAttribute('x2', qubitX);
    line.setAttribute('y2', qubitY(edge.qubit));
    line.setAttribute('stroke', '#30363d');
    line.setAttribute('stroke-width', '0.5');
    svg.appendChild(line);
  }

  // Checks (squares)
  for (let i = 0; i < nc; i++) {
    const rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
    const s = 6;
    rect.setAttribute('x', checkX - s / 2);
    rect.setAttribute('y', checkY(i) - s / 2);
    rect.setAttribute('width', s);
    rect.setAttribute('height', s);
    rect.setAttribute('fill', '#f85149');
    svg.appendChild(rect);
  }

  // Qubits (circles)
  for (let i = 0; i < nq; i++) {
    const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    circle.setAttribute('cx', qubitX);
    circle.setAttribute('cy', qubitY(i));
    circle.setAttribute('r', 3);
    circle.setAttribute('fill', '#58a6ff');
    svg.appendChild(circle);
  }

  return svg;
}

// ── code_construction ──

function renderCodeConstruction(data, msg) {
  if (data.status === 'valid') {
    return renderCSSCode(data.code, msg);
  }
  const errEl = el('div', 'output-error');
  errEl.appendChild(el('div', 'error-kind', data.error?.kind || 'error'));
  errEl.appendChild(el('div', 'error-message', data.error?.message || 'Invalid code construction'));
  return errEl;
}

// ── cat_qubit_params ──

function renderCatParams(data, msg) {
  const card = el('div', 'params-card');

  // Left: physical params
  const left = el('div', 'params-section');
  left.appendChild(el('h3', null, 'Physical Parameters'));
  const physical = [
    ['|α|²', data.alpha_sq],
    ['κ₁', sci(data.kappa1) + ' Hz'],
    ['κ₂', sci(data.kappa2) + ' Hz'],
    ['T_cycle', (data.t_cycle_ns || 0).toFixed(0) + ' ns'],
    ['γ', data.gamma],
  ];
  for (const [name, value] of physical) {
    const row = el('div', 'params-row');
    row.appendChild(el('span', 'param-name', name));
    row.appendChild(el('span', 'param-value', String(value)));
    left.appendChild(row);
  }
  card.appendChild(left);

  // Right: derived channel
  const right = el('div', 'params-section');
  right.appendChild(el('h3', null, 'Pauli Channel'));
  const derived = [
    ['p_X', sci(data.p_x)],
    ['p_Z', sci(data.p_z)],
    ['p_Y', sci(data.p_y)],
    ['κ₁/κ₂', sci(data.kappa_ratio)],
  ];
  for (const [name, value] of derived) {
    const row = el('div', 'params-row');
    row.appendChild(el('span', 'param-name', name));
    row.appendChild(el('span', 'param-value', String(value)));
    right.appendChild(row);
  }
  card.appendChild(right);

  // Bias hero
  const hero = el('div', 'bias-hero');
  hero.appendChild(el('div', 'label', 'Noise bias η = p_Z / p_X'));
  hero.appendChild(el('div', 'value', 'η = ' + sci(data.bias)));
  card.appendChild(hero);

  return card;
}

// ── pauli_channel ──

function renderPauliChannel(data, msg) {
  const container = el('div', 'pauli-channel');
  const items = [
    ['p_X', data.p_x],
    ['p_Z', data.p_z],
    ['p_Y', data.p_y],
    ['η', data.bias],
  ];
  for (const [label, value] of items) {
    const item = el('span', 'pc-item');
    item.appendChild(el('span', 'pc-label', label + ' = '));
    item.appendChild(el('span', 'pc-value', sci(value)));
    container.appendChild(item);
  }
  return container;
}

// ── sim_result ──

function renderSimResult(data, msg) {
  const container = el('div', 'sim-result');
  container.appendChild(el('span', 'sr-main', 'p_L = ' + sci(data.logical_rate)));
  container.appendChild(el('span', 'sr-detail',
    `(${numFmt(data.logical_errors)} / ${numFmt(data.total_trials)} trials)`));
  container.appendChild(el('span', 'sr-detail',
    `95% CI: [${sci(data.ci_lower)}, ${sci(data.ci_upper)}]`));
  return container;
}

// ── threshold_plot ──

function renderThresholdPlot(data, msg) {
  const container = el('div', 'threshold-plot-container');

  // Okabe-Ito color palette
  const colors = ['#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2',
                  '#D55E00', '#CC79A7', '#999999', '#000000'];

  if (!data.series || data.series.length === 0) {
    container.appendChild(el('div', 'output-fallback', 'No data to plot'));
    return container;
  }

  // Build Vega-Lite spec
  const values = [];
  data.series.forEach((s, si) => {
    for (const p of s.points) {
      values.push({
        code: s.label,
        x: p.x,
        y: Math.max(1e-15, p.y),
        y_lower: Math.max(1e-15, p.y_lower),
        y_upper: p.y_upper,
        trials: p.trials,
        errors: p.errors,
      });
    }
  });

  // p_L = p_Z diagonal reference
  const xMin = Math.min(...values.map(v => v.x));
  const xMax = Math.max(...values.map(v => v.x));
  const diagValues = [];
  for (let i = 0; i <= 20; i++) {
    const x = xMin + (xMax - xMin) * i / 20;
    diagValues.push({ x, y: x, code: 'p_L = p_Z' });
  }

  const spec = {
    $schema: 'https://vega.github.io/schema/vega-lite/v5.json',
    width: 'container',
    height: 380,
    background: '#161b22',
    config: {
      axis: { labelColor: '#8b949e', titleColor: '#c9d1d9', gridColor: '#21262d', domainColor: '#30363d' },
      legend: { labelColor: '#c9d1d9', titleColor: '#8b949e' },
      view: { stroke: '#30363d' },
    },
    layer: [
      // Reference diagonal
      {
        data: { values: diagValues },
        mark: { type: 'line', strokeDash: [6, 4], opacity: 0.4 },
        encoding: {
          x: { field: 'x', type: 'quantitative' },
          y: { field: 'y', type: 'quantitative' },
          color: { value: '#8b949e' },
        },
      },
      // Error bars
      {
        data: { values },
        mark: { type: 'errorbar' },
        encoding: {
          x: { field: 'x', type: 'quantitative', title: data.x_label || 'Physical p_Z' },
          y: { field: 'y_lower', type: 'quantitative', title: data.y_label || 'Logical p_L', scale: { type: 'log' } },
          y2: { field: 'y_upper' },
          color: { field: 'code', type: 'nominal', scale: { range: colors }, title: 'Code' },
        },
      },
      // Lines + points
      {
        data: { values },
        mark: { type: 'line', point: { size: 40 } },
        encoding: {
          x: { field: 'x', type: 'quantitative' },
          y: { field: 'y', type: 'quantitative', scale: { type: 'log' } },
          color: { field: 'code', type: 'nominal', scale: { range: colors } },
          tooltip: [
            { field: 'code', title: 'Code' },
            { field: 'x', title: 'p_Z', format: '.4f' },
            { field: 'y', title: 'p_L', format: '.2e' },
            { field: 'trials', title: 'Trials' },
            { field: 'errors', title: 'Errors' },
          ],
        },
      },
    ],
  };

  // Embed asynchronously
  if (typeof vegaEmbed !== 'undefined') {
    vegaEmbed(container, spec, { actions: { export: true, source: false, compiled: false, editor: false } })
      .catch(err => {
        container.appendChild(el('pre', 'output-fallback', 'Plot error: ' + err.message));
      });
  } else {
    container.appendChild(el('pre', 'output-fallback', 'Vega-Lite not loaded'));
  }

  return container;
}

// ── resource_estimate ──

function renderResourceEstimate(data, msg) {
  const card = el('div', 'resource-card');

  const total = data.total_qubits || (data.data_qubits + data.syndrome_qubits + data.routing_qubits + data.factory_qubits);
  card.appendChild(el('div', 'hero', numFmt(total) + ' total cat qubits'));

  // Stacked bar
  const segments = [
    { label: 'Data', value: data.data_qubits, color: '#58a6ff' },
    { label: 'Syndrome', value: data.syndrome_qubits, color: '#3fb950' },
    { label: 'Routing', value: data.routing_qubits, color: '#d29922' },
    { label: 'Factory', value: data.factory_qubits, color: '#f85149' },
  ];
  const bar = el('div', 'bar-container');
  for (const seg of segments) {
    const pct = (seg.value / total * 100);
    if (pct < 1) continue;
    const s = el('div', 'bar-seg');
    s.style.width = pct + '%';
    s.style.backgroundColor = seg.color;
    s.textContent = pct > 8 ? seg.label + ' ' + numFmt(seg.value) : '';
    s.title = seg.label + ': ' + numFmt(seg.value) + ' (' + pct.toFixed(1) + '%)';
    bar.appendChild(s);
  }
  card.appendChild(bar);

  const details = [
    ['Code family', data.code_family],
    ['Code distance', data.code_distance],
    ['Runtime', (data.runtime_seconds || 0).toFixed(1) + ' s'],
    ['Factories', data.num_factories],
    ['p_L/cycle', sci(data.logical_error_per_cycle)],
  ];
  for (const [name, value] of details) {
    card.appendChild(el('span', 'prop-name', name));
    card.appendChild(el('span', 'prop-value', String(value)));
  }

  return card;
}

// ── resource_comparison ──

function renderResourceComparison(data, msg) {
  const container = el('div');

  if (!data.rows || data.rows.length === 0) {
    container.appendChild(el('div', 'output-fallback', 'No data'));
    return container;
  }

  const table = el('table', 'resource-table');

  // Header
  const thead = el('thead');
  const headerRow = el('tr');
  const columns = data.columns || Object.keys(data.rows[0]);
  const colLabels = {
    code_family: 'Code Family',
    total_qubits: 'Total Qubits',
    data_qubits: 'Data',
    syndrome_qubits: 'Syndrome',
    routing_qubits: 'Routing',
    factory_qubits: 'Factory',
    code_distance: 'Distance',
    runtime_seconds: 'Runtime (s)',
    num_factories: 'Factories',
    logical_error_per_cycle: 'p_L/cycle',
  };
  for (const col of columns) {
    const th = el('th', null, colLabels[col] || col);
    th.addEventListener('click', () => sortTable(table, columns.indexOf(col)));
    headerRow.appendChild(th);
  }
  thead.appendChild(headerRow);
  table.appendChild(thead);

  // Body
  const tbody = el('tbody');
  for (const row of data.rows) {
    const tr = el('tr');
    for (const col of columns) {
      const val = row[col];
      const td = el('td');
      if (col === 'total_qubits') {
        td.className = 'highlight';
        td.textContent = numFmt(val);
      } else if (typeof val === 'number') {
        td.textContent = val < 0.01 ? sci(val) : numFmt(val);
      } else {
        td.textContent = String(val);
      }
      tr.appendChild(td);
    }
    tbody.appendChild(tr);
  }
  table.appendChild(tbody);
  container.appendChild(table);

  return container;
}

function sortTable(table, colIdx) {
  const tbody = table.querySelector('tbody');
  const rows = Array.from(tbody.querySelectorAll('tr'));
  rows.sort((a, b) => {
    const aVal = a.children[colIdx].textContent;
    const bVal = b.children[colIdx].textContent;
    const aNum = parseFloat(aVal.replace(/,/g, ''));
    const bNum = parseFloat(bVal.replace(/,/g, ''));
    if (!isNaN(aNum) && !isNaN(bNum)) return bNum - aNum;
    return aVal.localeCompare(bVal);
  });
  for (const row of rows) tbody.appendChild(row);
}

// ── bin_matrix ──

function renderBinMatrix(data, msg) {
  const container = el('div');

  if (data.display) {
    const grid = el('div', 'bin-matrix-grid');
    grid.style.gridTemplateColumns = `repeat(${data.cols}, 8px)`;
    for (const row of data.display) {
      for (const bit of row) {
        grid.appendChild(el('div', 'bit ' + (bit ? 'one' : 'zero')));
      }
    }
    container.appendChild(grid);
  }

  const summary = el('div', 'matrix-summary');
  summary.innerHTML = `${data.rows}×${data.cols} &nbsp; rank ${data.rank} &nbsp; density ${(data.density * 100).toFixed(1)}%`;
  summary.style.marginTop = '8px';
  container.appendChild(summary);

  return container;
}

// ── error ──

function renderError(data, msg) {
  const errEl = el('div', 'output-error');
  errEl.appendChild(el('div', 'error-kind', data.error_type || 'error'));
  errEl.appendChild(el('pre', 'error-message', data.message || msg?.show_text || 'Unknown error'));
  return errEl;
}
