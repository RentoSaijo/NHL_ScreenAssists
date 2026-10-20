// Shared data -----------------------------------------------------------

// Load public snapshot and keep replay requests compact.
const $ = (selector, root = document) => root.querySelector(selector);
const $$ = (selector, root = document) => [...root.querySelectorAll(selector)];
const number = new Intl.NumberFormat('en-US');
const decimal = value => Number.isFinite(value) ? value.toFixed(2) : '—';
const percent = value => `${(100 * value).toFixed(2)}%`;
const seasonName = value => `${String(value).slice(0, 4)}–${String(value).slice(6)}`;
const escapeHTML = value => String(value ?? '').replace(/[&<>"']/g, char => ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[char]));
const fetchJSON = async path => {
  const response = await fetch(path);
  if (!response.ok) throw new Error(`Could not load ${path} (${response.status}).`);
  return response.json();
};
const state = {data: null, players: new Map(), teams: new Map(), goals: null, shards: new Map(), page: 0, sort: 'conservative', descending: true, filtered: [], clip: null, frame: 0, playing: false, lastTime: 0, player: null};
const playerName = id => state.players.get(Number(id))?.name || `Player ${id}`;
const teamCode = id => state.teams.get(Number(id))?.code || String(id);
const goalDate = value => new Date(`${String(value).slice(0, 10)}T12:00:00`).toLocaleDateString('en-US', {month: 'short', day: 'numeric', year: 'numeric'});

// Rankings --------------------------------------------------------------

// Aggregate credits and exposure with identical filters.
function aggregateRows() {
  const season = $('#season').value;
  const phase = $('#phase').value;
  const team = $('#team').value;
  const query = $('#search').value.trim().toLocaleLowerCase();
  const definition = $('#definition').value;
  const metric = $('#metric').value;
  const minimum = Math.max(0, Number($('#minimum').value) || 0);
  const totals = new Map();
  for (const row of state.data.rows) {
    if (season !== 'all' && row.season !== Number(season)) continue;
    if (phase !== 'all' && row.type !== Number(phase)) continue;
    if (team !== 'all' && row.team !== Number(team)) continue;
    if (!totals.has(row.player)) totals.set(row.player, {player: row.player, name: playerName(row.player), teams: new Set(), games: 0, seconds: 0, conservative: 0, broader: 0});
    const total = totals.get(row.player);
    total.teams.add(row.team);
    for (const key of ['games', 'seconds', 'conservative', 'broader']) total[key] += row[key];
  }
  return [...totals.values()].map(row => ({
    ...row, team: [...new Set([...row.teams].map(teamCode))].sort().join(' / '),
    per_game: row.games > 0 ? row[definition] / row.games : null,
    per60: row.seconds > 0 ? 3600 * row[definition] / row.seconds : null
  })).filter(row => row.name.toLocaleLowerCase().includes(query) &&
    (metric !== 'per_game' || (row.games > 0 && row.games >= minimum)) &&
    (metric !== 'per60' || (row.seconds > 0 && row.seconds / 60 >= minimum)));
}

// Render stable ranks, preserving ties in selected metric.
function renderRankings() {
  const rows = aggregateRows();
  rows.sort((a, b) => {
    const x = a[state.sort], y = b[state.sort];
    const diff = typeof x === 'string' ? x.localeCompare(y) : (x ?? -Infinity) - (y ?? -Infinity);
    return (state.descending ? -diff : diff) || a.name.localeCompare(b.name);
  });
  rows.forEach((row, index) => { row.rank = index && row[state.sort] === rows[index - 1][state.sort] ? rows[index - 1].rank : index + 1; });
  state.filtered = rows;
  state.page = Math.min(state.page, Math.max(0, Math.ceil(rows.length / 50) - 1));
  const visible = rows.slice(state.page * 50, (state.page + 1) * 50);
  const cell = (key, content, extra = '') => `<td class="${[extra, key === state.sort ? 'sorted' : ''].filter(Boolean).join(' ')}">${content}</td>`;
  $('#rows').innerHTML = visible.length ? visible.map(row => `<tr>
    <td>${row.rank}</td>${cell('name', `<button class="player-link" data-player="${row.player}">${escapeHTML(row.name)}</button>`)}
    ${cell('team', escapeHTML(row.team), 'team')}${cell('conservative', row.conservative)}${cell('broader', row.broader)}
    ${cell('games', row.games)}${cell('seconds', decimal(row.seconds / 60))}${cell('per_game', decimal(row.per_game))}${cell('per60', decimal(row.per60))}</tr>`).join('') : '<tr><td colspan="9" class="loading">No players match these filters.</td></tr>';
  $('#page-number').textContent = `${rows.length ? state.page + 1 : 0} / ${Math.ceil(rows.length / 50)}`;
  $('#previous').disabled = state.page === 0;
  $('#next').disabled = (state.page + 1) * 50 >= rows.length;
  $('#download').disabled = !rows.length;
  $$('th[aria-sort]').forEach(th => th.removeAttribute('aria-sort'));
  $(`[data-sort="${state.sort}"]`)?.closest('th').setAttribute('aria-sort', state.descending ? 'descending' : 'ascending');
}

// Set exposure defaults from selected competition and snapshot season.
function setMinimum() {
  const cutoff = state.data.cutoff;
  const year = Number(cutoff.slice(0, 4)) - (Number(cutoff.slice(5, 7)) < 7 ? 1 : 0);
  const currentSeason = String(year * 10000 + year + 1);
  const ongoing = $('#season').value === currentSeason;
  const playoffs = $('#phase').value === '3';
  $('#minimum').value = playoffs ? 0 : $('#metric').value === 'per60' ? (ongoing ? 60 : 250) : (ongoing ? 5 : 25);
}

// Change ranking measure and initialize its exposure control.
function setMetric(metric) {
  const changed = $('#metric').value !== metric;
  $('#metric').value = metric;
  $('#minimum-control').hidden = metric === 'count';
  $('#minimum-label').textContent = metric === 'per60' ? 'Minimum minutes' : 'Minimum games';
  if (changed) setMinimum();
  state.sort = metric === 'count' ? $('#definition').value : metric;
  state.descending = true;
  state.page = 0;
}

// Download exactly the filtered rows shown by ranking controls.
function downloadRows() {
  const headers = ['rank', 'player_id', 'player', 'teams', 'games', 'minutes', 'conservative', 'broader', 'rate_definition', 'assists_per_game', 'assists_per_60'];
  const quote = value => `"${String(value ?? '').replace(/"/g, '""')}"`;
  const rows = state.filtered.map(row => [row.rank, row.player, row.name, row.team, row.games, decimal(row.seconds / 60), row.conservative, row.broader, $('#definition').value, decimal(row.per_game), decimal(row.per60)]);
  const blob = new Blob([[headers, ...rows].map(row => row.map(quote).join(',')).join('\n')], {type: 'text/csv;charset=utf-8'});
  const url = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = url;
  link.download = `screen-assists-${$('#season').value}-${$('#phase').value}-${$('#team').value}.csv`;
  link.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
}

// Offer team identities represented in selected season and competition.
function populateTeams() {
  const selection = $('#team').value;
  const ids = new Set(state.data.rows.filter(row =>
    ($('#season').value === 'all' || row.season === Number($('#season').value)) &&
    ($('#phase').value === 'all' || row.type === Number($('#phase').value))).map(row => row.team));
  $('#team').innerHTML = '<option value="all">All teams</option>';
  state.data.teams.filter(team => ids.has(team.id)).sort((a, b) => a.code.localeCompare(b.code)).forEach(team => {
    const duplicate = state.data.teams.some(other => other.id !== team.id && other.code === team.code);
    $('#team').add(new Option(duplicate ? `${team.code} · ${team.name}` : team.code, team.id));
  });
  $('#team').value = selection === 'all' || ids.has(Number(selection)) ? selection : 'all';
}

// Connect ranking controls.
function initializeRankings() {
  populateTeams();
  setMinimum();
  for (const id of ['season', 'phase', 'team']) $(`#${id}`).addEventListener('change', () => {
    if (id !== 'team') { populateTeams(); setMinimum(); }
    state.page = 0;
    renderRankings();
  });
  $('#minimum').addEventListener('input', () => { state.page = 0; renderRankings(); });
  $('#search').addEventListener('input', () => { state.page = 0; renderRankings(); });
  $('#definition').addEventListener('change', () => { if ($('#metric').value === 'count') state.sort = $('#definition').value; state.page = 0; renderRankings(); });
  $('#metric').addEventListener('change', () => { setMetric($('#metric').value); setMinimum(); renderRankings(); });
  $$('[data-sort]').forEach(button => button.addEventListener('click', () => {
    const sort = button.dataset.sort;
    const descending = state.sort === sort ? !state.descending : sort !== 'name';
    if (['conservative', 'broader'].includes(sort)) { $('#definition').value = sort; setMetric('count'); }
    if (['per_game', 'per60'].includes(sort) && $('#metric').value !== sort) setMetric(sort);
    state.sort = sort;
    state.descending = descending;
    state.page = 0;
    renderRankings();
  }));
  $('#rows').addEventListener('click', event => { const button = event.target.closest('[data-player]'); if (button) openPlayer(Number(button.dataset.player)); });
  $('#previous').addEventListener('click', () => { state.page--; renderRankings(); });
  $('#next').addEventListener('click', () => { state.page++; renderRankings(); });
  $('#download').addEventListener('click', downloadRows);
  renderRankings();
}

// Replay explorer -------------------------------------------------------

// Build accessible explorer shared by rankings and methodology.
function createExplorer() {
  const dialog = document.createElement('dialog');
  dialog.id = 'explorer';
  dialog.setAttribute('aria-labelledby', 'explorer-title');
  dialog.innerHTML = `<div class="dialog-head"><div><h2 id="explorer-title"></h2><p id="explorer-summary"></p></div><button class="close" aria-label="Close explorer">×</button></div>
    <div class="explorer"><div class="goal-list" id="goal-list" aria-label="Credited goals"></div><div class="replay" id="replay">
      <h3 id="replay-title">Select a goal</h3><p class="replay-subtitle" id="replay-subtitle"></p>
      <div class="canvas-frame"><canvas id="rink" width="1000" height="650" aria-label="Animated rink replay with player identities, puck, and estimated sightline">Animated hockey tracking replay. Use the original NHL replay link for an alternative view.</canvas></div>
      <div class="controls"><button id="play" disabled>Play</button><input id="seek" type="range" min="0" max="1" step="1" value="0" aria-label="Replay frame" disabled><select id="speed" aria-label="Playback speed"><option value="0.5">0.5×</option><option value="1" selected>1×</option><option value="2">2×</option></select></div>
      <div class="replay-options"><span class="replay-time" id="replay-time"></span><label><input id="names" type="checkbox" checked> Player names</label><label><input id="uncertainty" type="checkbox" checked> Sightline band</label></div>
      <div class="legend"><span><i class="selected"></i>Selected player</span><span><i></i>Attacker</span><span><i class="defender"></i>Defender</span><span><i class="puck"></i>Puck</span></div>
      <p class="replay-caption" id="replay-credits"></p>
      <p class="replay-caption">0.00 s marks estimated release. Tracking is sampled at 0.10 s. Dots show shoulder sensors; the shaded band illustrates a 1.50 ft effective tolerance around the goalie-to-puck segment. It does not reveal body pose or the goalie’s actual field of view.</p>
      <div class="replay-links"><a id="nhl-replay" target="_blank" rel="noopener">Open NHL tracking replay ↗</a><a id="nhl-video" target="_blank" rel="noopener" hidden>Watch goal video ↗</a></div>
    </div></div>`;
  document.body.append(dialog);
  $('.close', dialog).addEventListener('click', () => dialog.close());
  dialog.addEventListener('close', () => { state.playing = false; state.clip = null; state.selectedKey = null; });
  dialog.addEventListener('click', event => { if (event.target === dialog) dialog.close(); });
  $('#play').addEventListener('click', () => {
    if (!state.clip) return;
    if (state.frame >= state.clip.time.length - 1) state.frame = 0;
    state.playing = !state.playing;
    state.lastTime = 0;
    $('#play').textContent = state.playing ? 'Pause' : 'Play';
    if (state.playing) requestAnimationFrame(animate);
  });
  $('#seek').addEventListener('input', () => { state.playing = false; $('#play').textContent = 'Play'; state.frame = Number($('#seek').value); drawRink(); });
  for (const id of ['names', 'uncertainty']) $(`#${id}`).addEventListener('change', drawRink);
  window.addEventListener('resize', () => { if (state.clip) drawRink(); });
}

// Read public goal index only when an explorer is opened.
async function loadGoals() {
  if (!state.goals) state.goals = await fetchJSON('data/goals.json');
  return state.goals;
}

// Match explorer goals to identical leaderboard scope.
async function openPlayer(id, explicitKey = null) {
  state.player = id;
  state.playing = false;
  state.clip = null;
  $('#explorer-title').textContent = playerName(id);
  $('#explorer-summary').textContent = 'Loading credited goals…';
  $('#goal-list').innerHTML = '<p class="loading">Loading goals…</p>';
  $('#replay').hidden = true;
  if (!$('#explorer').open) $('#explorer').showModal();
  try {
    const index = await loadGoals();
    if (state.player !== id || !$('#explorer').open) return;
    let goals = Object.values(index.events).filter(goal => goal.credits.some(credit => credit.player === id));
    if (!explicitKey && document.body.dataset.page === 'rankings') goals = goals.filter(goal =>
      ($('#season').value === 'all' || goal.season === Number($('#season').value)) &&
      ($('#phase').value === 'all' || goal.type === Number($('#phase').value)) &&
      ($('#team').value === 'all' || goal.team === Number($('#team').value)));
    if (explicitKey && !goals.some(goal => goal.key === explicitKey)) goals.push(index.events[explicitKey]);
    goals.sort((a, b) => b.game - a.game || b.event - a.event);
    const conservative = goals.filter(goal => goal.credits.some(c => c.player === id && c.conservative)).length;
    $('#explorer-summary').textContent = `${conservative} conservative · ${goals.length} broader credits${explicitKey ? ' across snapshot' : ' under selected filters'}`;
    $('#goal-list').innerHTML = goals.length ? goals.map(goal => {
      const credit = goal.credits.find(c => c.player === id);
      return `<button class="goal-item" data-key="${goal.key}" aria-pressed="false"><strong>${teamCode(goal.away)} at ${teamCode(goal.home)} · ${goalDate(goal.date)}</strong><span>${escapeHTML(playerName(goal.scorer))} · ${escapeHTML(goal.shot || 'Shot')} · P${goal.period}</span><span class="badge${credit?.conservative ? '' : ' broad'}">${credit?.conservative ? 'Conservative + broader' : 'Broader only'}</span></button>`;
    }).join('') : '<p class="loading">No detected screen assists under these filters. This does not establish that the player never screens.</p>';
    $$('.goal-item').forEach(button => button.addEventListener('click', () => selectGoal(button.dataset.key)));
    if (goals.length) await selectGoal(explicitKey || goals[0].key);
  } catch (error) {
    $('#goal-list').innerHTML = `<p class="error">${escapeHTML(error.message)} Please close and try again.</p>`;
  }
}

// Load selected short replay shard and reset playback.
async function selectGoal(key) {
  state.playing = false;
  state.clip = null;
  const goal = state.goals.events[key];
  $('#replay').hidden = false;
  $('#replay-title').textContent = `${playerName(goal.scorer)} scores`;
  $('#replay-subtitle').textContent = `${teamCode(goal.away)} at ${teamCode(goal.home)} · ${goalDate(goal.date)} · ${seasonName(goal.season)} ${goal.type === 3 ? 'playoffs' : 'regular season'}`;
  $('#replay-credits').textContent = `Credited teammates: ${goal.credits.map(credit => `${playerName(credit.player)} (${credit.conservative ? 'conservative + broader' : 'broader only'})`).join('; ')}.`;
  $('#play').disabled = true;
  $('#play').textContent = 'Loading';
  $$('.goal-item').forEach(button => button.setAttribute('aria-pressed', String(button.dataset.key === key)));
  state.selectedKey = key;
  try {
    if (!state.shards.has(goal.file)) state.shards.set(goal.file, await fetchJSON(`data/${goal.file}`));
    if (state.selectedKey !== key) return;
    state.clip = state.shards.get(goal.file)[key];
    if (!state.clip) throw new Error('Replay segment is unavailable.');
    state.frame = 0;
    $('#seek').max = state.clip.time.length - 1;
    $('#seek').disabled = false;
    $('#play').disabled = false;
    $('#play').textContent = 'Play';
    $('#nhl-replay').href = goal.replay_url;
    const validVideo = typeof goal.video_url === 'string' && /^https:\/\//.test(goal.video_url);
    $('#nhl-video').hidden = !validVideo;
    if (validVideo) $('#nhl-video').href = goal.video_url;
    drawRink();
  } catch (error) {
    $('#play').textContent = 'Unavailable';
    $('#replay-subtitle').textContent = error.message;
  }
}

// Draw normalized rink and finite sensor-to-puck sightline.
function drawRink() {
  const clip = state.clip;
  if (!clip) return;
  const canvas = $('#rink');
  const bounds = canvas.getBoundingClientRect();
  const ratio = window.devicePixelRatio || 1;
  canvas.width = Math.round(bounds.width * ratio);
  canvas.height = Math.round(bounds.height * ratio);
  const ctx = canvas.getContext('2d');
  ctx.scale(ratio, ratio);
  const w = bounds.width, h = bounds.height;
  const minPuck = Math.min(...clip.puck.filter(p => p.every(Number.isFinite)).map(p => p[0]));
  const xmin = Math.max(-100, Math.min(25, minPuck - 10));
  const scale = Math.min((w - 30) / (104 - xmin), (h - 55) / 91);
  const left = (w - (104 - xmin) * scale) / 2;
  const top = (h - 85 * scale) / 2 + 9;
  const xy = p => [left + (p[0] - xmin) * scale, top + (42.5 - p[1]) * scale];
  const valid = p => Array.isArray(p) && p.length === 2 && p.every(Number.isFinite);
  const line = (a, b, color, width = 1) => {
    const x = xy(a), y = xy(b);
    ctx.beginPath(); ctx.moveTo(...x); ctx.lineTo(...y); ctx.strokeStyle = color; ctx.lineWidth = width; ctx.stroke();
  };
  ctx.clearRect(0, 0, w, h);
  ctx.fillStyle = '#f8faf9'; ctx.fillRect(0, 0, w, h);
  const topLeft = xy([xmin, 42.5]), bottomRight = xy([100, -42.5]);
  ctx.fillStyle = '#fff'; ctx.fillRect(topLeft[0], topLeft[1], bottomRight[0] - topLeft[0], bottomRight[1] - topLeft[1]);
  ctx.strokeStyle = '#b7c6ce'; ctx.lineWidth = 1;
  ctx.strokeRect(topLeft[0], topLeft[1], bottomRight[0] - topLeft[0], bottomRight[1] - topLeft[1]);
  line([89, -42.5], [89, 42.5], '#d7aaa5');
  if (xmin <= 25) line([25, -42.5], [25, 42.5], '#b0c4d9', 2);
  if (xmin < 0) line([0, -42.5], [0, 42.5], '#d7aaa5', 2);
  for (const y of [-22, 22]) {
    const c = xy([69, y]); ctx.beginPath(); ctx.arc(...c, 15 * scale, 0, Math.PI * 2); ctx.strokeStyle = '#eddddd'; ctx.stroke();
    ctx.beginPath(); ctx.arc(...c, 1.5 * scale, 0, Math.PI * 2); ctx.fillStyle = '#d9a6a0'; ctx.fill();
  }
  const crease = xy([89, 0]); ctx.beginPath(); ctx.arc(...crease, 6 * scale, Math.PI / 2, 3 * Math.PI / 2); ctx.closePath(); ctx.fillStyle = '#e6f2f7'; ctx.fill(); ctx.strokeStyle = '#c3d9e4'; ctx.stroke();
  line([89, -3], [93, -3], '#bc655b', 2); line([93, -3], [93, 3], '#bc655b', 2); line([93, 3], [89, 3], '#bc655b', 2);
  const frame = Math.round(state.frame), puck = clip.puck[frame], goalie = clip.players.find(p => p.id === clip.goalie)?.xy[frame];
  if (valid(puck) && valid(goalie)) {
    const active = clip.time[frame] <= clip.arrival && (clip.time[frame] <= 0 || puck[0] <= goalie[0] + 1);
    if ($('#uncertainty').checked && active) line(goalie, puck, '#00796c24', 3 * scale);
    line(goalie, puck, active ? '#00796c' : '#a7b5bc', 1.1);
    for (let i = Math.max(1, frame - 5); i <= frame; i++) if (valid(clip.puck[i - 1]) && valid(clip.puck[i])) line(clip.puck[i - 1], clip.puck[i], '#142c3f35', 1.5);
  }
  const labels = [];
  const ordered = clip.players.slice().sort((a, b) => Number(a.id === state.player) - Number(b.id === state.player));
  for (const player of ordered) {
    const p = player.xy[frame];
    if (!valid(p) || p[0] < xmin || Math.abs(p[1]) > 45) continue;
    const point = xy(p), selected = player.id === state.player;
    const color = selected ? '#b2671e' : player.id === clip.goalie ? '#142c3f' : player.team === clip.team ? '#00796c' : '#85959f';
    if (selected) { ctx.beginPath(); ctx.arc(...point, 8, 0, Math.PI * 2); ctx.strokeStyle = '#b2671e66'; ctx.lineWidth = 2; ctx.stroke(); }
    ctx.beginPath(); ctx.arc(...point, selected ? 5 : 4.5, 0, Math.PI * 2); ctx.fillStyle = color; ctx.fill(); ctx.strokeStyle = '#fff'; ctx.lineWidth = 1.2; ctx.stroke();
    if ($('#names').checked || selected || player.id === clip.scorer || player.id === clip.goalie) labels.push({point, text: playerName(player.id).split(' ').slice(1).join(' ') || playerName(player.id), color, selected});
  }
  const used = [];
  ctx.font = '11px -apple-system, BlinkMacSystemFont, sans-serif';
  labels.sort((a, b) => Number(b.selected) - Number(a.selected)).forEach(label => {
    const width = ctx.measureText(label.text).width;
    let x = Math.min(w - width - 8, Math.max(6, label.point[0] + 8));
    let y = Math.max(20, label.point[1] - 7);
    for (let attempt = 0; attempt < 6 && used.some(box => x < box.x + box.w && x + width > box.x && Math.abs(y - box.y) < 13); attempt++) y += 13;
    y = Math.min(h - 10, y);
    ctx.fillStyle = '#ffffffdc'; ctx.fillRect(x - 2, y - 10, width + 4, 13);
    ctx.fillStyle = label.selected ? '#9d5010' : '#334e5e'; ctx.fillText(label.text, x, y);
    used.push({x, y, w: width});
  });
  if (valid(puck)) { ctx.beginPath(); ctx.arc(...xy(puck), 3.3, 0, Math.PI * 2); ctx.fillStyle = '#101b23'; ctx.fill(); ctx.strokeStyle = '#fff'; ctx.lineWidth = .8; ctx.stroke(); }
  const time = clip.time[frame];
  ctx.fillStyle = '#142c3f'; ctx.font = '600 12px -apple-system, BlinkMacSystemFont, sans-serif';
  ctx.fillText(`${time < 0 ? 'Before release' : time < clip.arrival ? 'Shot in flight' : 'Goal-plane arrival'} · ${time > 0 ? '+' : ''}${time.toFixed(2)} s`, 15, 20);
  $('#seek').value = frame;
  $('#replay-time').textContent = `${time > 0 ? '+' : ''}${time.toFixed(2)} s from release`;
}

// Animate recorded frames at selected playback speed.
function animate(timestamp) {
  if (!state.playing || !state.clip) return;
  if (!state.lastTime) state.lastTime = timestamp;
  const elapsed = (timestamp - state.lastTime) / 1000 * Number($('#speed').value);
  const delta = state.clip.time[Math.min(state.frame + 1, state.clip.time.length - 1)] - state.clip.time[state.frame];
  if (elapsed >= Math.max(.01, delta)) { state.frame++; state.lastTime = timestamp; }
  if (state.frame >= state.clip.time.length - 1) { state.frame = state.clip.time.length - 1; state.playing = false; $('#play').textContent = 'Play'; }
  drawRink();
  if (state.playing) requestAnimationFrame(animate);
}

// Research and methodology ---------------------------------------------

// Populate measured findings directly from public research aggregates.
async function initializeResearch() {
  const [validation, outcome] = await Promise.all([fetchJSON('data/validation.json'), fetchJSON('data/outcome.json')]);
  const final = validation.metrics['20252026'];
  const values = {
    'screen-rate': percent(outcome.adjusted.screened_rate.estimate),
    'clear-rate': percent(outcome.adjusted.clear_rate.estimate),
    'risk-difference': `${(100 * outcome.adjusted.risk_difference.estimate).toFixed(2)} pp`,
    'odds-ratio': `${outcome.adjusted.odds_ratio.estimate.toFixed(2)} (95% interval ${outcome.adjusted.odds_ratio.lower.toFixed(2)}–${outcome.adjusted.odds_ratio.upper.toFixed(2)})`,
    'shot-count': `${number.format(outcome.screened_shots)} screened and ${number.format(outcome.clear_shots)} clear-sight shots`,
    'average-precision': final.average_precision.toFixed(3),
    'calibration-brier': final.calibration?.brier.toFixed(4) || 'Not a probability model',
    'evaluated-goals': number.format(state.data.evaluated),
    'total-goals': number.format(state.data.goals)
  };
  $$('[data-value]').forEach(element => { if (values[element.dataset.value]) element.textContent = values[element.dataset.value]; });
  if ($('#validation-rows')) $('#validation-rows').innerHTML = ['conservative', 'broader'].flatMap(definition => ['player', 'event'].map(level => {
    const m = final[definition][level];
    return `<tr><td>${definition === 'conservative' ? 'Conservative' : 'Broader'}</td><td>${level === 'player' ? 'Player attribution' : 'Goal detection'}</td><td>${percent(m.precision)}</td><td>${percent(m.recall)}</td><td>${number.format(m.n)}</td></tr>`;
  })).join('');
  if ($('#quality-rows')) $('#quality-rows').innerHTML = [20232024, 20242025, 20252026, 20262027].map(season => {
    const data = state.data.quality.filter(row => row.season === season);
    const count = status => data.filter(row => row.status === status).reduce((s, row) => s + row.goals, 0);
    const total = data.reduce((s, row) => s + row.goals, 0);
    return `<tr><td>${seasonName(season)}</td><td>${number.format(total)}</td><td>${number.format(count('evaluated'))}</td><td>${number.format(count('empty_net'))}</td><td>${number.format(total - count('evaluated') - count('empty_net'))}</td></tr>`;
  }).join('');
  if ($('#examples')) {
    const goals = await loadGoals();
    const keys = Object.values(goals.examples);
    $('#examples').innerHTML = keys.map(key => { const g = goals.events[key]; return `<button data-example="${key}">${seasonName(g.season)} · ${teamCode(g.away)} at ${teamCode(g.home)} ↗</button>`; }).join('');
    $$('[data-example]').forEach(button => button.addEventListener('click', () => {
      const goal = goals.events[button.dataset.example];
      const credit = goal.credits.slice().sort((a, b) => b.score - a.score)[0];
      openPlayer(credit.player, goal.key);
    }));
  }
}

// Start page with visible error handling.
(async () => {
  try {
    state.data = await fetchJSON('data/rankings.json');
    state.players = new Map(state.data.players.map(player => [player.id, player]));
    state.teams = new Map(state.data.teams.map(team => [team.id, team]));
    createExplorer();
    if (document.body.dataset.page === 'rankings') initializeRankings();
    else await initializeResearch();
  } catch (error) {
    const target = $('#load-status');
    if (target) { target.textContent = `${error.message} Reload to try again.`; target.classList.add('error'); }
    if ($('#rows')) $('#rows').innerHTML = '<tr><td colspan="9" class="error">Rankings could not be loaded.</td></tr>';
  }
})();
