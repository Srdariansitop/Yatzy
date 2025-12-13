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
  renderScores(st.scores);
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
    div.className = 'die';
    div.textContent = val;
    div.addEventListener('click', () => {
      if (div.classList.contains('kept')) return; // ya conservado
      if (selectedDice.has(i)) { selectedDice.delete(i); div.classList.remove('selected'); }
      else { selectedDice.add(i); div.classList.add('selected'); }
    });
    dc.appendChild(div);
  });
  kept.forEach((val) => {
    const div = document.createElement('div');
    div.className = 'die kept';
    div.textContent = val;
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

function renderScores(scoresArr) {
  // scoresArr es array de arrays: [[jugador, [[comb, punt], ...]], ...]
  const container = el('scores-table');
  container.innerHTML = '';
  if (!Array.isArray(scoresArr)) return;
  const table = document.createElement('table');
  const header = document.createElement('tr');
  header.innerHTML = '<th>Jugador</th><th>Combinaciones</th>';
  table.appendChild(header);
  scoresArr.forEach(entry => {
    const [jug, combos] = entry;
    const tr = document.createElement('tr');
    const tdJug = document.createElement('td');
    tdJug.textContent = jug;
    const tdComb = document.createElement('td');
    tdComb.style.textAlign = 'left';
    tdComb.innerHTML = combos.map(c => `${c[0]}: ${c[1]}`).join('<br>');
    tr.appendChild(tdJug); tr.appendChild(tdComb);
    table.appendChild(tr);
  });
  container.appendChild(table);
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
