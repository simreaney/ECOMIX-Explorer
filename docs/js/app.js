// ECOMIX Explorer - static web app. Boots the data, builds every page, and
// routes between them (the selected tab and selection live in the URL hash).

import { init, D, isChemVar } from "./data.js";
import { state, set, on, startRouting } from "./state.js";
import { el } from "./ui.js";
import * as mapPage from "./pages/map.js";
import * as sitePage from "./pages/site.js";
import * as hydroPage from "./pages/hydro.js";
import * as chemPage from "./pages/chem.js";
import * as ecoPage from "./pages/eco.js";
import * as spatialPage from "./pages/spatial.js";
import * as downloadPage from "./pages/download.js";
import * as helpPage from "./pages/help.js";

const MODULES = [mapPage, sitePage, hydroPage, chemPage, ecoPage, spatialPage, downloadPage, helpPage];

async function boot() {
  const meta = await init();

  // Defaults that depend on what was published.
  const H = meta.hype;
  const scen = ["Baseline", "SSP585"].filter((s) => H.scenarios.includes(s));
  state.scenarios = scen.length ? scen : H.scenarios.slice(0, 1);
  state.periods = H.periods.slice(-1);
  if (!meta.datasets.hype_proj) {
    const first = meta.chem.keys.slice().sort((a, b) => meta.chem.labels[meta.chem.keys.indexOf(a)].localeCompare(meta.chem.labels[meta.chem.keys.indexOf(b)]))[0];
    state.variable = `chem_${first}`;
  }
  if (meta.d3d.chemicals.length) state.d3dChem = meta.d3d.chemicals[0].key;

  const pagesEl = document.getElementById("pages");
  const tabsEl = document.getElementById("tabs");
  const pages = MODULES.map((m) => m.create());
  const byId = new Map(pages.map((p) => [p.id, p]));
  for (const p of pages) {
    p.el.dataset.page = p.id;
    p.dirty = true;
    p.started = false;
    pagesEl.append(p.el);
    tabsEl.append(el("a", { href: `#tab=${p.id}`, "data-tab": p.id, text: p.title, onclick: (e) => { e.preventDefault(); set({ tab: p.id }); } }));
    if (p.deps) {
      on(p.deps, () => {
        if (state.tab === p.id && p.started) p.update();
        else p.dirty = true;
      });
    }
  }
  document.getElementById("boot").remove();

  async function show() {
    const page = byId.get(state.tab) || pages[0];
    for (const p of pages) p.el.classList.toggle("is-active", p === page);
    for (const a of tabsEl.children) a.classList.toggle("is-active", a.dataset.tab === page.id);
    document.title = page.id === "map" ? "ECOMIX Explorer" : `${page.title} · ECOMIX Explorer`;
    if (!page.started) {
      page.started = true;
      if (page.init) await page.init();
    }
    if (page.onShow) page.onShow();
    if (page.dirty && page.update) { page.dirty = false; page.update(); }
  }

  // Registered after the pages, so their own subscriptions settle first.
  on(["tab"], () => show().catch(fail));
  startRouting();
  await show();
}

function fail(err) {
  console.error(err);
  const boot = document.getElementById("boot");
  const msg = `ECOMIX Explorer could not start.\n\n${err && err.message ? err.message : err}\n\nIf you opened index.html straight from disk, serve the folder over HTTP instead (see README).`;
  if (boot) { boot.classList.add("is-error"); boot.textContent = msg; }
}

boot().catch(fail);

// Exposed for debugging in the browser console.
window.ECOMIX = { state, set, D, isChemVar };
