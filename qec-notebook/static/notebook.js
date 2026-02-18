// ─── qec-notebook client ───
// Cell management, WebSocket communication, keyboard shortcuts.

(function () {
  'use strict';

  // ── State ──
  let ws = null;
  let cellCounter = 0;
  const cells = new Map();  // cellId -> { element, input, output, state }

  // ── WebSocket ──

  function connect() {
    setStatus('connecting');
    const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';
    ws = new WebSocket(proto + '//' + location.host + '/ws');

    ws.onopen = () => {
      console.log('[ws] connected');
    };

    ws.onmessage = (event) => {
      let msg;
      try { msg = JSON.parse(event.data); } catch { return; }
      handleServerMessage(msg);
    };

    ws.onclose = () => {
      setStatus('disconnected');
      console.log('[ws] disconnected, reconnecting in 3s...');
      setTimeout(connect, 3000);
    };

    ws.onerror = () => {
      setStatus('disconnected');
    };
  }

  function send(msg) {
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify(msg));
    }
  }

  function setStatus(state) {
    const ind = document.getElementById('status-indicator');
    const txt = document.getElementById('status-text');
    ind.className = 'status ' + state;
    const labels = { connected: 'Connected', disconnected: 'Disconnected', connecting: 'Connecting...' };
    txt.textContent = labels[state] || state;
  }

  // ── Server message handler ──

  function handleServerMessage(msg) {
    switch (msg.type) {
      case 'ready':
        setStatus('connected');
        console.log('[ws] server ready, version', msg.version);
        break;

      case 'result':
        handleResult(msg);
        break;

      case 'progress':
        handleProgress(msg);
        break;

      default:
        console.log('[ws] unknown message type:', msg.type);
    }
  }

  function handleResult(msg) {
    const cell = cells.get(msg.cell_id);
    if (!cell) return;

    cell.state = msg.status === 'error' ? 'error' : 'complete';
    cell.element.classList.remove('running');
    if (msg.status === 'error') cell.element.classList.add('error');
    else cell.element.classList.remove('error');

    // Render output
    const outputEl = cell.output;
    outputEl.innerHTML = '';
    outputEl.classList.add('has-content');

    const rendered = renderCellOutput(msg);
    if (rendered) outputEl.appendChild(rendered);

    // Elapsed time
    if (msg.elapsed_ms > 0 && msg.status !== 'error') {
      const elapsed = document.createElement('div');
      elapsed.className = 'elapsed';
      elapsed.textContent = msg.elapsed_ms + ' ms';
      elapsed.style.marginTop = '8px';
      outputEl.appendChild(elapsed);
    }
  }

  function handleProgress(msg) {
    const cell = cells.get(msg.cell_id);
    if (!cell) return;

    const outputEl = cell.output;
    outputEl.innerHTML = '';
    outputEl.classList.add('has-content');

    const renderer = RENDERERS[msg.render_as];
    if (renderer && msg.data) {
      outputEl.appendChild(renderer(msg.data, msg));
    }

    const elapsed = document.createElement('div');
    elapsed.className = 'elapsed';
    elapsed.textContent = (msg.elapsed_ms / 1000).toFixed(1) + 's (running...)';
    outputEl.appendChild(elapsed);
  }

  // ── Cell management ──

  function createCell(source, insertAfterEl) {
    cellCounter++;
    const cellId = 'c-' + String(cellCounter).padStart(3, '0');

    const cellEl = document.createElement('div');
    cellEl.className = 'cell';
    cellEl.id = 'cell-' + cellId;
    cellEl.dataset.cellId = cellId;

    // Header
    const header = document.createElement('div');
    header.className = 'cell-header';

    const label = document.createElement('span');
    label.className = 'cell-label';
    label.textContent = 'In [' + cellCounter + ']';

    const actions = document.createElement('div');
    actions.className = 'cell-actions';

    const btnRun = document.createElement('button');
    btnRun.textContent = '▶ Run';
    btnRun.addEventListener('click', () => runCell(cellId));

    const btnUp = document.createElement('button');
    btnUp.textContent = '↑';
    btnUp.title = 'Move up';
    btnUp.addEventListener('click', () => moveCell(cellId, -1));

    const btnDown = document.createElement('button');
    btnDown.textContent = '↓';
    btnDown.title = 'Move down';
    btnDown.addEventListener('click', () => moveCell(cellId, 1));

    const btnDelete = document.createElement('button');
    btnDelete.textContent = '✕';
    btnDelete.title = 'Delete cell';
    btnDelete.addEventListener('click', () => deleteCell(cellId));

    actions.append(btnRun, btnUp, btnDown, btnDelete);
    header.append(label, actions);

    // Input textarea
    const input = document.createElement('textarea');
    input.className = 'cell-input';
    input.placeholder = 'Haskell expression...';
    input.spellcheck = false;
    input.value = source || '';
    input.addEventListener('keydown', (e) => handleCellKeydown(e, cellId));
    input.addEventListener('input', () => {
      autoResize(input);
      markStale(cellId);
    });

    // Output area
    const output = document.createElement('div');
    output.className = 'cell-output';

    cellEl.append(header, input, output);

    // Insert into DOM
    const container = document.getElementById('cells-container');
    if (insertAfterEl && insertAfterEl.nextSibling) {
      container.insertBefore(cellEl, insertAfterEl.nextSibling);
    } else {
      container.appendChild(cellEl);
    }

    cells.set(cellId, {
      element: cellEl,
      input: input,
      output: output,
      state: 'idle',
    });

    autoResize(input);
    input.focus();

    return cellId;
  }

  function runCell(cellId) {
    const cell = cells.get(cellId);
    if (!cell) return;

    const source = cell.input.value.trim();
    if (!source) return;

    cell.state = 'running';
    cell.element.classList.add('running');
    cell.element.classList.remove('error', 'stale');
    cell.output.innerHTML = '<span class="spinner"></span> Running...';
    cell.output.classList.add('has-content');

    send({ type: 'eval', cell_id: cellId, source: source });
  }

  function deleteCell(cellId) {
    const cell = cells.get(cellId);
    if (!cell) return;
    if (cells.size <= 1) return;  // keep at least one cell

    cell.element.remove();
    cells.delete(cellId);

    // Focus previous cell
    const remaining = Array.from(cells.values());
    if (remaining.length > 0) {
      remaining[remaining.length - 1].input.focus();
    }
  }

  function moveCell(cellId, direction) {
    const cell = cells.get(cellId);
    if (!cell) return;
    const container = document.getElementById('cells-container');
    const sibling = direction < 0 ? cell.element.previousElementSibling : cell.element.nextElementSibling;
    if (!sibling) return;
    if (direction < 0) {
      container.insertBefore(cell.element, sibling);
    } else {
      container.insertBefore(sibling, cell.element);
    }
  }

  function markStale(cellId) {
    const cell = cells.get(cellId);
    if (!cell) return;
    if (cell.state === 'complete' || cell.state === 'error') {
      cell.element.classList.add('stale');
    }
  }

  function autoResize(textarea) {
    textarea.style.height = 'auto';
    textarea.style.height = Math.max(60, textarea.scrollHeight) + 'px';
  }

  // ── Keyboard shortcuts ──

  function handleCellKeydown(e, cellId) {
    // Shift+Enter: run cell, move to next
    if (e.key === 'Enter' && e.shiftKey && !e.ctrlKey) {
      e.preventDefault();
      runCell(cellId);
      // Move to next cell or create one
      const cell = cells.get(cellId);
      const next = cell.element.nextElementSibling;
      if (next && next.classList.contains('cell')) {
        const nextId = next.dataset.cellId;
        const nextCell = cells.get(nextId);
        if (nextCell) nextCell.input.focus();
      } else {
        createCell('', cell.element);
      }
      return;
    }

    // Ctrl+Enter: run cell, stay
    if (e.key === 'Enter' && e.ctrlKey && !e.shiftKey) {
      e.preventDefault();
      runCell(cellId);
      return;
    }

    // Tab: insert 2 spaces
    if (e.key === 'Tab' && !e.shiftKey) {
      e.preventDefault();
      const ta = e.target;
      const start = ta.selectionStart;
      const end = ta.selectionEnd;
      ta.value = ta.value.substring(0, start) + '  ' + ta.value.substring(end);
      ta.selectionStart = ta.selectionEnd = start + 2;
      autoResize(ta);
      return;
    }

    // Escape: cancel
    if (e.key === 'Escape') {
      e.preventDefault();
      send({ type: 'cancel', cell_id: cellId });
      return;
    }
  }

  // Global keyboard shortcuts
  document.addEventListener('keydown', (e) => {
    // Ctrl+Shift+N: new cell
    if (e.ctrlKey && e.shiftKey && e.key === 'N') {
      e.preventDefault();
      // Find focused cell and insert after it
      const focused = document.activeElement?.closest('.cell');
      createCell('', focused);
      return;
    }

    // Ctrl+Shift+D: delete current cell
    if (e.ctrlKey && e.shiftKey && e.key === 'D') {
      e.preventDefault();
      const focused = document.activeElement?.closest('.cell');
      if (focused) deleteCell(focused.dataset.cellId);
      return;
    }

    // Ctrl+Shift+ArrowUp: move cell up
    if (e.ctrlKey && e.shiftKey && e.key === 'ArrowUp') {
      e.preventDefault();
      const focused = document.activeElement?.closest('.cell');
      if (focused) moveCell(focused.dataset.cellId, -1);
      return;
    }

    // Ctrl+Shift+ArrowDown: move cell down
    if (e.ctrlKey && e.shiftKey && e.key === 'ArrowDown') {
      e.preventDefault();
      const focused = document.activeElement?.closest('.cell');
      if (focused) moveCell(focused.dataset.cellId, 1);
      return;
    }

    // Ctrl+S: save (prevent default)
    if (e.ctrlKey && !e.shiftKey && e.key === 's') {
      e.preventDefault();
      saveNotebook();
      return;
    }
  });

  // ── Save / Load ──

  function saveNotebook() {
    const container = document.getElementById('cells-container');
    const cellEls = container.querySelectorAll('.cell');
    const cellData = [];
    for (const el of cellEls) {
      const id = el.dataset.cellId;
      const cell = cells.get(id);
      if (cell) {
        cellData.push({ cell_id: id, source: cell.input.value });
      }
    }
    const nb = {
      version: '0.1.0',
      title: 'Untitled',
      created: new Date().toISOString(),
      cells: cellData,
    };
    // Download as file
    const blob = new Blob([JSON.stringify(nb, null, 2)], { type: 'application/json' });
    const a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = 'notebook.qec';
    a.click();
    URL.revokeObjectURL(a.href);
  }

  function loadNotebook() {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.qec,.json';
    input.addEventListener('change', (e) => {
      const file = e.target.files[0];
      if (!file) return;
      const reader = new FileReader();
      reader.onload = () => {
        try {
          const nb = JSON.parse(reader.result);
          if (nb.cells && Array.isArray(nb.cells)) {
            // Clear existing cells
            document.getElementById('cells-container').innerHTML = '';
            cells.clear();
            cellCounter = 0;
            // Hide welcome
            const welcome = document.getElementById('welcome');
            if (welcome) welcome.style.display = 'none';
            // Create cells
            for (const c of nb.cells) {
              createCell(c.source || '');
            }
          }
        } catch (err) {
          alert('Failed to load notebook: ' + err.message);
        }
      };
      reader.readAsText(file);
    });
    input.click();
  }

  // ── Button handlers ──

  document.getElementById('btn-add-cell').addEventListener('click', () => {
    const focused = document.activeElement?.closest('.cell');
    createCell('', focused);
    const welcome = document.getElementById('welcome');
    if (welcome) welcome.style.display = 'none';
  });

  document.getElementById('btn-reset').addEventListener('click', () => {
    send({ type: 'reset' });
    // Clear all outputs
    for (const cell of cells.values()) {
      cell.output.innerHTML = '';
      cell.output.classList.remove('has-content');
      cell.element.classList.remove('running', 'error', 'stale');
      cell.state = 'idle';
    }
  });

  document.getElementById('btn-save').addEventListener('click', saveNotebook);
  document.getElementById('btn-load').addEventListener('click', loadNotebook);

  // ── Init ──

  function init() {
    // Create initial cell
    createCell('');
    // Connect WebSocket
    connect();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

})();
