const API = 'http://localhost:3000';
let gameId = null;
let selectedDice = new Set(); // indices actuales seleccionados para conservar

const el = (id) => document.getElementById(id);
const show = (id) => el(id).classList.remove('hidden');
const hide = (id) => el(id).classList.add('hidden');
const setMsg = (m) => el('message').textContent = m || '';

// Crear juego
el('game-form').addEventListener('submit', async (e) => {
  e.preventDefault();
  const raw = el('players').value.trim();
  if (!raw) return;
  const players = raw.split(',').map(s => s.trim()).filter(Boolean);
  try {
    const res = await fetch(API + '/game', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ players })
    });
    const data = await res.json();
    if (data.gameId) {
      gameId = data.gameId;
      el('game-id').textContent = gameId;
      show('game-info');
      show('turn');
      show('combinations');
      show('scores');
      refreshState();
    } else {
      setMsg('Error creando juego');
    }
  } catch (err) {
    setMsg('Error de red');
  }
});

async function refreshState() {
  if (!gameId) return;
  try {
    const res = await fetch(`${API}/game/${gameId}`);
    const st = await res.json();
    if (st.error) { setMsg(st.error); return; }
    renderState(st);
  } catch (err) {
    setMsg('Error obteniendo estado');
  }
}

function renderState(st) {
  setMsg('');
  el('current-player').textContent = st.currentPlayer;
  el('rolls-used').textContent = st.rollsUsed;
  renderDice(st.dice, st.kept);
  renderCombinations(st.available);
  renderScores(st.scores, st.available, st.currentPlayer);
  // Update roll indicator
  const ri = el('roll-indicator');
  ri.innerHTML = '';
  const title = document.createElement('span'); title.className = 'title'; title.textContent = 'GIRA';
  ri.appendChild(title);
  for (let i=1;i<=3;i++){ const s = document.createElement('span'); s.className = 'step' + (i<=st.rollsUsed? ' active':'' ); s.textContent = String(i); ri.appendChild(s); }
  el('roll-btn').disabled = st.rollsUsed >= 3;
  el('keep-btn').disabled = st.dice.length === 0 || st.rollsUsed === 0;
}

function renderDice(dice, kept) {
  selectedDice.clear();
  const dc = el('dice-container');
  const kc = el('kept-container');
  dc.innerHTML = '<h3>Dados</h3>';
  kc.innerHTML = '<h3>Conservados</h3>';
  dice.forEach((val, i) => {
    const div = document.createElement('div');
    div.className = 'die v' + String(val);
    div.setAttribute('aria-label', 'Dado ' + String(val));
    div.addEventListener('click', () => {
      if (div.classList.contains('kept')) return; // ya conservado
      if (selectedDice.has(i)) { selectedDice.delete(i); div.classList.remove('selected'); }
      else { selectedDice.add(i); div.classList.add('selected'); }
    });
    dc.appendChild(div);
  });
  kept.forEach((val) => {
    const div = document.createElement('div');
    div.className = 'die kept v' + String(val);
    div.setAttribute('aria-label', 'Dado ' + String(val));
    kc.appendChild(div);
  });
}

function renderCombinations(list) {
  const ul = el('combo-list');
  ul.innerHTML = '';
  list.forEach(c => {
    const li = document.createElement('li');
    li.textContent = c;
    li.addEventListener('click', () => chooseCombination(c));
    ul.appendChild(li);
  });
}

function renderScores(scoresArr, available = [], currentPlayer = null) {
  // Build scoreboard similar to reference: labels + per-player cells + bonus
  const container = el('scoreboard');
  container.innerHTML = '';
  if (!Array.isArray(scoresArr)) return;

  const COMBO_ORDER = ['unos','doses','treses','cuatro','cinco','seis','trio','cuarteto','fullhouse','pequenaescalera','granescalera','yatzy','chance'];
  const LABELS = {
    unos: {type:'pip', v:1}, doses:{type:'pip', v:2}, treses:{type:'pip', v:3}, cuatro:{type:'pip', v:4}, cinco:{type:'pip', v:5}, seis:{type:'pip', v:6},
    trio: {type:'text', t:'3X'}, cuarteto:{type:'text', t:'4X'}, fullhouse:{type:'text', t:'🏠'},
    pequenaescalera:{type:'text', t:'SMALL'}, granescalera:{type:'text', t:'LARGE'}, yatzy:{type:'text', t:'YATZY'}, chance:{type:'text', t:'?'}
  };

  // Map scores per player
  const players = scoresArr.map(([jug]) => jug);
  const scoreMap = new Map(scoresArr.map(([jug, list]) => [jug, new Map(list.map(([comb, val]) => [String(comb).toLowerCase(), val]))]));

  const sb = document.createElement('div');
  sb.className = 'scoreboard';

  // Top header: player names and VS badge
  const top = document.createElement('div');
  top.className = 'sb-top';
  const left = document.createElement('div'); left.className = 'pname'; left.textContent = players[0] || 'P1';
  const mid = document.createElement('div'); mid.className = 'vs'; mid.textContent = 'VS';
  const right = document.createElement('div'); right.className = 'pname right'; right.textContent = players[1] || players[0] || 'P2';
  top.append(left, mid, right);
  sb.appendChild(top);

  const grid = document.createElement('div');
  grid.className = 'sb-grid';

  // Helper to create label cell
  const mkLabel = (key) => {
    const c = document.createElement('div');
    c.className = 'sb-cell label';
    const L = LABELS[key];
    if (!L) { c.textContent = key; return c; }
    if (L.type === 'pip') { const p = document.createElement('div'); p.className = 'pip v'+L.v; c.appendChild(p); }
    else { c.textContent = L.t; }
    return c;
  };

  const lowerAvailable = new Set((available||[]).map(s=>String(s).toLowerCase()));

  // Each row: label + per player cell
  COMBO_ORDER.forEach((key) => {
    const row = document.createElement('div'); row.className = 'sb-row';
    grid.appendChild(mkLabel(key));
    players.forEach((jug) => {
      const cell = document.createElement('div');
      const val = (scoreMap.get(jug) || new Map()).get(key);
      if (typeof val === 'number') { cell.className = 'sb-cell score'; cell.textContent = String(val); }
      else {
        cell.className = 'sb-cell play';
        if (jug === currentPlayer && lowerAvailable.has(key)) {
          cell.classList.add('clickable');
          cell.addEventListener('click', () => chooseCombination(key));
          cell.textContent = '';
        }
      }
      grid.appendChild(cell);
    });
  });

  // Bonus row for upper section
  const bonus = document.createElement('div'); bonus.className = 'sb-bonus';
  const blabel = document.createElement('div'); blabel.className = 'sb-cell label bonus-label'; blabel.textContent = 'BONUS +35'; bonus.appendChild(blabel);
  players.forEach((jug) => {
    const m = scoreMap.get(jug) || new Map();
    const upperKeys = ['unos','doses','treses','cuatro','cinco','seis'];
    const sum = upperKeys.reduce((acc,k)=> acc + (typeof m.get(k)==='number'? m.get(k):0), 0);
    const wrap = document.createElement('div'); wrap.className = 'sb-cell score'; wrap.textContent = `${sum}/63`;
    bonus.appendChild(wrap);
  });

  sb.appendChild(grid);
  sb.appendChild(bonus);
  container.appendChild(sb);
}

// Acciones
el('roll-btn').addEventListener('click', async () => {
  if (!gameId) return;
  setMsg('Tirando dados...');
  const res = await fetch(`${API}/game/${gameId}/roll`, { method: 'POST' });
  const data = await res.json();
  if (data.error) setMsg(data.error); else renderState(data);
});

el('keep-btn').addEventListener('click', async () => {
  if (!gameId || selectedDice.size === 0) { setMsg('Selecciona dados.'); return; }
  setMsg('Conservando...');
  const indices = Array.from(selectedDice).map(i => i + 1); // backend espera 1-based
  const res = await fetch(`${API}/game/${gameId}/keep`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ indices })
  });
  const data = await res.json();
  if (data.error) setMsg(data.error); else renderState(data);
});

async function chooseCombination(comb) {
  if (!gameId) return;
  setMsg('Eligiendo combinación...');
  const res = await fetch(`${API}/game/${gameId}/choose`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ combination: comb })
  });
  const data = await res.json();
  if (data.error) { setMsg(data.error); } else { renderState(data); }
}

// Auto refresh cada 5s para ver cambios (opcional)
setInterval(() => { if (gameId) refreshState(); }, 5000);
