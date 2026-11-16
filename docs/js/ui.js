// Small DOM toolkit: element builder plus the handful of controls the app
// needs (select, chip multi-select, segmented control, combobox, popover,
// list rail, context strip, tooltip, paged table).

export function el(tag, attrs = {}, ...children) {
  const [name, ...classes] = tag.split(".");
  const node = document.createElement(name || "div");
  if (classes.length) node.className = classes.join(" ");
  for (const [k, v] of Object.entries(attrs || {})) {
    if (v == null || v === false) continue;
    if (k === "class") node.className += (node.className ? " " : "") + v;
    else if (k === "style" && typeof v === "object") Object.assign(node.style, v);
    else if (k.startsWith("on") && typeof v === "function") node.addEventListener(k.slice(2), v);
    else if (k === "text") node.textContent = v;
    else node.setAttribute(k, v === true ? "" : v);
  }
  append(node, children);
  return node;
}

function append(node, children) {
  for (const c of children.flat(Infinity)) {
    if (c == null || c === false) continue;
    node.append(c instanceof Node ? c : document.createTextNode(String(c)));
  }
}

export function setChildren(node, ...children) {
  node.replaceChildren();
  append(node, children);
  return node;
}

// A labelled control. Single form controls get a <label>; button groups get
// role="group" with an accessible name instead (a <label> would name only
// their first button).
export function field(label, control, help) {
  const single = control.matches && control.matches("select, input, .combo");
  const box = single
    ? el("label.field", {}, el("span.label", { text: label }), control)
    : el("div.field", { role: "group", "aria-label": label }, el("span.label", { text: label }), control);
  if (help) box.append(el("p.help", { text: help }));
  return box;
}

// ---- <select> --------------------------------------------------------------
// options: [{value, label, disabled}] or {group: [{value,label}]} for optgroups.

export function select({ options, value, onChange, ariaLabel }) {
  const node = el("select", { "aria-label": ariaLabel });
  const api = {
    el: node,
    setOptions(opts, keep = true) {
      const prev = node.value;
      node.replaceChildren();
      const add = (parent, list) => {
        for (const o of list) {
          parent.append(el("option", { value: o.value, disabled: o.disabled || null, text: o.label }));
        }
      };
      if (Array.isArray(opts)) add(node, opts);
      else {
        for (const [group, list] of Object.entries(opts)) {
          const g = el("optgroup", { label: group });
          add(g, list);
          node.append(g);
        }
      }
      if (keep && [...node.options].some((o) => o.value === prev)) node.value = prev;
    },
    get value() { return node.value; },
    set value(v) { if (v != null) node.value = String(v); },
  };
  api.setOptions(options, false);
  if (value != null) node.value = String(value);
  node.addEventListener("change", () => onChange && onChange(node.value));
  return api;
}

// ---- toggle chips (multi-select) -------------------------------------------

export function chips({ options, value = [], onChange, min = 1 }) {
  const node = el("div.chips", { role: "group" });
  let current = [...value];
  let opts = options;
  const render = () => {
    node.replaceChildren(...opts.map((o) => el("button.chip", {
      type: "button",
      "aria-pressed": current.includes(o.value) ? "true" : "false",
      title: o.title || null,
      text: o.label,
      onclick: () => {
        const on = current.includes(o.value);
        if (on && current.length <= min) return;
        current = on ? current.filter((v) => v !== o.value) : opts.map((x) => x.value).filter((v) => v === o.value || current.includes(v));
        render();
        onChange && onChange([...current]);
      },
    })));
  };
  render();
  return {
    el: node,
    get value() { return [...current]; },
    set value(v) { current = [...v]; render(); },
    setOptions(o, v) { opts = o; if (v) current = [...v]; render(); },
  };
}

// ---- segmented control (single choice) -------------------------------------

export function seg({ options, value, onChange, ariaLabel }) {
  const node = el("div.seg", { role: "group", "aria-label": ariaLabel });
  let current = value;
  const render = () => {
    node.replaceChildren(...options.map((o) => el("button", {
      type: "button",
      "aria-pressed": o.value === current ? "true" : "false",
      text: o.label,
      onclick: () => { if (o.value !== current) { current = o.value; render(); onChange && onChange(current); } },
    })));
  };
  render();
  return { el: node, get value() { return current; }, set value(v) { current = v; render(); } };
}

// ---- combobox (type to search a long list) ---------------------------------

export function combo({ placeholder, getItems, onPick, max = 50 }) {
  const input = el("input", { type: "search", placeholder, autocomplete: "off", spellcheck: "false" });
  const list = el("div.combo-list", { role: "listbox", hidden: true });
  const node = el("div.combo", {}, input, list);
  let shown = [];
  let active = -1;

  const close = () => { list.hidden = true; active = -1; };
  const highlight = (i) => {
    active = i;
    [...list.children].forEach((c, k) => c.classList.toggle("is-active", k === i));
    if (i >= 0 && list.children[i]) list.children[i].scrollIntoView({ block: "nearest" });
  };
  const pick = (item) => { input.value = item.label; close(); onPick(item); input.blur(); };
  const open = () => {
    const q = input.value.trim().toLowerCase();
    const items = getItems();
    const starts = [], contains = [];
    for (const it of items) {
      const l = it.label.toLowerCase();
      if (!q || l.startsWith(q)) starts.push(it);
      else if (l.includes(q)) contains.push(it);
      if (starts.length >= max) break;
    }
    shown = starts.concat(contains).slice(0, max);
    list.replaceChildren(...(shown.length ? shown.map((it, i) => el("div.combo-item", {
      role: "option", text: it.label, title: it.label,
      onmousedown: (e) => { e.preventDefault(); pick(it); },
      onmousemove: () => highlight(i),
    })) : [el("div.combo-empty", { text: "No matches" })]));
    list.hidden = false;
    active = -1;
  };
  input.addEventListener("input", open);
  input.addEventListener("focus", () => { input.select(); open(); });
  input.addEventListener("blur", () => setTimeout(close, 120));
  input.addEventListener("keydown", (e) => {
    if (e.key === "ArrowDown") { e.preventDefault(); if (list.hidden) open(); highlight(Math.min(active + 1, shown.length - 1)); }
    else if (e.key === "ArrowUp") { e.preventDefault(); highlight(Math.max(active - 1, 0)); }
    else if (e.key === "Enter") { e.preventDefault(); const it = shown[active >= 0 ? active : 0]; if (it) pick(it); }
    else if (e.key === "Escape") { close(); input.blur(); }
  });
  return { el: node, set text(v) { input.value = v || ""; } };
}

// ---- popover anchored to a trigger button ----------------------------------

let openPop = null;
document.addEventListener("pointerdown", (e) => {
  if (openPop && !openPop.pop.contains(e.target) && !openPop.trigger.contains(e.target)) closePop();
});
document.addEventListener("keydown", (e) => { if (e.key === "Escape") closePop(); });
function closePop() { if (openPop) { openPop.pop.remove(); openPop.trigger.setAttribute("aria-expanded", "false"); openPop = null; } }

export function popover(trigger, title, content) {
  trigger.setAttribute("aria-expanded", "false");
  trigger.addEventListener("click", () => {
    if (openPop && openPop.trigger === trigger) { closePop(); return; }
    closePop();
    const pop = el("div.pop", { role: "dialog", "aria-label": title }, el("div.pop-title", { text: title }), content);
    document.body.append(pop);
    const r = trigger.getBoundingClientRect();
    const w = pop.offsetWidth;
    pop.style.left = `${Math.max(8, Math.min(r.left, innerWidth - w - 8))}px`;
    pop.style.top = `${Math.min(r.bottom + 6, innerHeight - pop.offsetHeight - 8)}px`;
    trigger.setAttribute("aria-expanded", "true");
    openPop = { pop, trigger };
    const focusable = pop.querySelector("select, input, button");
    if (focusable) focusable.focus();
  });
}

// ---- list rail -------------------------------------------------------------
// items: [{id, label, swatch?, num?}]

export function rail({ onPick }) {
  const node = el("div.rail-list", { role: "listbox" });
  let activeId = null;
  const api = {
    el: node,
    setItems(items) {
      node.replaceChildren(...items.map((it) => el("div.rail-row", {
        role: "option", tabindex: "0", "data-id": String(it.id), title: it.title || it.label,
        onclick: () => onPick(it.id),
        onkeydown: (e) => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); onPick(it.id); } },
      },
      el("span.rail-row-label", {}, it.swatch ? el("span.rail-swatch", { style: { background: it.swatch } }) : null, el("span", { text: it.label })),
      it.num != null ? el("span.rail-num", { text: it.num }) : null)));
      api.setActive(activeId, false);
    },
    setActive(id, scroll = true) {
      activeId = id;
      let hit = null;
      for (const row of node.children) {
        const on = id != null && row.dataset.id === String(id);
        row.classList.toggle("is-active", on);
        row.setAttribute("aria-selected", on ? "true" : "false");
        if (on) hit = row;
      }
      if (hit && scroll) hit.scrollIntoView({ block: "nearest" });
    },
  };
  return api;
}

// ---- context strip ---------------------------------------------------------

export function ctxStrip({ map = false, idLabel = "Subbasin", stats = [], action = null }) {
  const refs = {};
  refs.idLabel = el("div.ctx-label", { text: idLabel });
  refs.id = el("div.ctx-id-value", { text: "—" });
  refs.sub = el("div.ctx-id-sub", { text: "" });
  refs.map = map ? el("div.ctx-map", { "aria-hidden": "true" }) : null;
  refs.stats = {};
  const statEls = stats.map(([key, label]) => {
    const v = el("div.ctx-value", { text: "—" });
    refs.stats[key] = v;
    return el("div.ctx-stat", {}, el("div.ctx-label", { text: label }), v);
  });
  refs.el = el("div.ctx", {},
    refs.map,
    el("div.ctx-id", {}, refs.idLabel, refs.id, refs.sub),
    el("div.ctx-stats", {}, statEls),
    action ? el("div.ctx-act", {}, action) : null);
  refs.setId = (text) => {
    refs.id.textContent = text;
    refs.id.classList.toggle("is-long", String(text).length > 20);
    refs.id.title = text;
  };
  return refs;
}

export const link = (text, onclick) => el("a", { href: "#", text, onclick: (e) => { e.preventDefault(); onclick(); } });

// ---- tooltip ---------------------------------------------------------------

const tip = () => document.getElementById("tooltip");

export function showTip(x, y, content) {
  const t = tip();
  setChildren(t, content);
  t.hidden = false;
  const w = t.offsetWidth, h = t.offsetHeight;
  let left = x + 14, top = y + 14;
  if (left + w > innerWidth - 8) left = x - w - 14;
  if (top + h > innerHeight - 8) top = y - h - 14;
  t.style.left = `${Math.max(8, left)}px`;
  t.style.top = `${Math.max(8, top)}px`;
}

export function hideTip() { tip().hidden = true; }

// A short stroke in the series colour, used as the key in tooltips/legends.
export function lineKey(color, dash, width = 18) {
  const ns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(ns, "svg");
  svg.setAttribute("width", width); svg.setAttribute("height", 10); svg.setAttribute("class", "tt-key");
  const l = document.createElementNS(ns, "line");
  l.setAttribute("x1", 1); l.setAttribute("x2", width - 1); l.setAttribute("y1", 5); l.setAttribute("y2", 5);
  l.setAttribute("stroke", color); l.setAttribute("stroke-width", 2);
  if (dash) l.setAttribute("stroke-dasharray", dash);
  svg.append(l);
  return svg;
}

export function swatchKey(color, opacity = 1) {
  const ns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(ns, "svg");
  svg.setAttribute("width", 12); svg.setAttribute("height", 10); svg.setAttribute("class", "tt-key");
  const r = document.createElementNS(ns, "rect");
  r.setAttribute("x", 0); r.setAttribute("y", 0); r.setAttribute("width", 12); r.setAttribute("height", 10);
  r.setAttribute("fill", color); r.setAttribute("fill-opacity", opacity);
  svg.append(r);
  return svg;
}

export function dotKey(color) {
  const ns = "http://www.w3.org/2000/svg";
  const svg = document.createElementNS(ns, "svg");
  svg.setAttribute("width", 10); svg.setAttribute("height", 10); svg.setAttribute("class", "tt-key");
  const c = document.createElementNS(ns, "circle");
  c.setAttribute("cx", 5); c.setAttribute("cy", 5); c.setAttribute("r", 4); c.setAttribute("fill", color);
  svg.append(c);
  return svg;
}

// ---- paged table -----------------------------------------------------------

export function pagedTable(columns, rows, { pageSize = 100, compact = false } = {}) {
  const wrap = el("div.table-wrap");
  const pager = el("div.pager");
  let page = 0;
  const pages = Math.max(1, Math.ceil(rows.length / pageSize));
  const render = () => {
    const slice = rows.slice(page * pageSize, (page + 1) * pageSize);
    const table = el(`table.data${compact ? ".compact" : ""}`, {},
      el("thead", {}, el("tr", {}, columns.map((c) => el("th", { text: c.label })))),
      el("tbody", {}, slice.map((r) => el("tr", {}, columns.map((c) => el("td", { text: c.format ? c.format(r[c.key], r) : (r[c.key] ?? "") }))))));
    wrap.replaceChildren(table);
    wrap.scrollTop = 0;
    setChildren(pager,
      el("span", { text: `${(page * pageSize + 1).toLocaleString("en-GB")}–${Math.min(rows.length, (page + 1) * pageSize).toLocaleString("en-GB")} of ${rows.length.toLocaleString("en-GB")}` }),
      el("button", { type: "button", text: "‹ Prev", disabled: page === 0 || null, onclick: () => { page--; render(); } }),
      el("button", { type: "button", text: "Next ›", disabled: page >= pages - 1 || null, onclick: () => { page++; render(); } }));
  };
  render();
  return el("div", { style: { display: "flex", flexDirection: "column", flex: "1 1 auto", minHeight: "0" } }, wrap, rows.length > pageSize ? pager : null);
}

// ---- file save -------------------------------------------------------------

export function saveFile(name, data, type = "application/octet-stream") {
  const blob = data instanceof Blob ? data : new Blob([data], { type });
  const url = URL.createObjectURL(blob);
  const a = el("a", { href: url, download: name });
  document.body.append(a);
  a.click();
  a.remove();
  setTimeout(() => URL.revokeObjectURL(url), 4000);
}

// Notice used wherever a dataset has not been published to the site.
export function unpublished(what) {
  return el("div.notice", {},
    el("b", { text: `${what} not published. ` },),
    "The source data was not available when this site was built. Run scripts/build_site.py on a machine that has it and redeploy.");
}
