#!/usr/bin/env python3
"""Record a short walkthrough video of the ECOMIX Explorer Shiny app.

Starts the app locally, drives it with Playwright (a synthetic cursor dot
and on-screen captions make the recording self-explanatory without audio),
and saves the result as an .mp4.

The tour hits every tab and shows one or two examples of each kind of
control (a search box, a checkbox layer, a popover, a marker click on the
map, ...) rather than exhaustively cycling every dropdown option or every
site - see the README section this script's module docstring is paired
with for the full list of tabs.

Usage:
    cd ECOMIX-Explorer
    source .venv/bin/activate       # needs the `playwright` package
    python3 -m playwright install chromium   # once, if not already done
    python3 scripts/record_walkthrough.py

Output:
    walkthrough_output/ecomix_walkthrough.mp4   (falls back to .webm if
    ffmpeg isn't on PATH)
"""

from __future__ import annotations

import shutil
import socket
import subprocess
import sys
import time
import traceback
from contextlib import closing
from pathlib import Path

from playwright.sync_api import Locator, Page, sync_playwright

ROOT = Path(__file__).resolve().parent.parent
OUT_DIR = ROOT / "walkthrough_output"
VIDEO_SIZE = {"width": 1600, "height": 1000}

CAPTION_MS = 2600  # how long each caption stays on screen
PAUSE_MS = 700  # short beat after most actions
SETTLE_MS = 1800  # longer pause after something re-renders (plot/map/table)

CURSOR_AND_CAPTION_JS = """
() => {
  if (document.getElementById('__wt_cursor')) return;
  const dot = document.createElement('div');
  dot.id = '__wt_cursor';
  Object.assign(dot.style, {
    position: 'fixed', top: '-50px', left: '-50px', width: '20px', height: '20px',
    borderRadius: '50%', background: 'rgba(214, 64, 58, 0.85)',
    border: '2px solid white', boxShadow: '0 1px 6px rgba(0,0,0,.5)',
    zIndex: 2147483647, pointerEvents: 'none', transform: 'translate(-50%, -50%)',
  });
  document.body.appendChild(dot);
  window.addEventListener('mousemove', (e) => {
    dot.style.left = e.clientX + 'px';
    dot.style.top = e.clientY + 'px';
  });

  const cap = document.createElement('div');
  cap.id = '__wt_caption';
  Object.assign(cap.style, {
    position: 'fixed', left: '50%', bottom: '30px', transform: 'translateX(-50%)',
    background: 'rgba(20,20,20,0.90)', color: '#fff', padding: '11px 22px',
    borderRadius: '8px', fontSize: '17px', fontFamily: 'Arial, Helvetica, sans-serif',
    zIndex: 2147483647, maxWidth: '78%', textAlign: 'center', opacity: '0',
    transition: 'opacity 0.25s ease', pointerEvents: 'none',
  });
  document.body.appendChild(cap);
}
"""


# --------------------------------------------------------------------------
# Shiny process management
# --------------------------------------------------------------------------

def free_port() -> int:
    with closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def wait_for_http(url: str, proc: subprocess.Popen, timeout: float = 120) -> None:
    import urllib.request

    deadline = time.time() + timeout
    while time.time() < deadline:
        if proc.poll() is not None:
            raise RuntimeError(
                f"Shiny process exited early (code {proc.returncode}). "
                f"Check {LOG_PATH} for details."
            )
        try:
            urllib.request.urlopen(url, timeout=2)
            return
        except Exception:
            time.sleep(1)
    raise TimeoutError(f"App never came up at {url} within {timeout}s")


LOG_PATH = ROOT / "walkthrough_output" / "shiny_server.log"


def start_shiny(port: int) -> subprocess.Popen:
    OUT_DIR.mkdir(exist_ok=True)
    log_file = open(LOG_PATH, "w")
    # ECOMIX_AUTORUN=0 stops app.R's own bottom-of-file runApp() call from
    # firing a second, differently-ported server out from under this one -
    # see the "### 3. Execution" block at the end of app.R.
    r_expr = (
        f"options(shiny.host='127.0.0.1'); "
        f"shiny::runApp('.', port={port}, launch.browser=FALSE)"
    )
    proc = subprocess.Popen(
        ["Rscript", "-e", r_expr],
        cwd=str(ROOT),
        stdout=log_file,
        stderr=subprocess.STDOUT,
        env={**__import__("os").environ, "ECOMIX_AUTORUN": "0"},
    )
    return proc


# --------------------------------------------------------------------------
# Playwright helpers
# --------------------------------------------------------------------------

def inject_overlay(page: Page) -> None:
    page.evaluate(CURSOR_AND_CAPTION_JS)


def say(page: Page, text: str, ms: int = CAPTION_MS) -> None:
    page.evaluate(
        "(t) => { const c = document.getElementById('__wt_caption'); "
        "c.textContent = t; c.style.opacity = '1'; }",
        text,
    )
    page.wait_for_timeout(ms)
    page.evaluate(
        "() => { const c = document.getElementById('__wt_caption'); "
        "if (c) c.style.opacity = '0'; }"
    )
    page.wait_for_timeout(300)


def move_to(page: Page, locator: Locator) -> None:
    box = locator.bounding_box()
    if box is None:
        return
    x, y = box["x"] + box["width"] / 2, box["y"] + box["height"] / 2
    page.mouse.move(x, y, steps=20)


def click(page: Page, locator: Locator, pause: int = PAUSE_MS) -> None:
    locator.scroll_into_view_if_needed()
    move_to(page, locator)
    page.wait_for_timeout(150)
    locator.click()
    page.wait_for_timeout(pause)


def nav_to(page: Page, label: str) -> None:
    tab = page.locator("ul.navbar-nav").first.get_by_text(label, exact=True)
    click(page, tab, pause=SETTLE_MS)


def toggle_layer(page: Page, value: str, pause: int = PAUSE_MS) -> None:
    cb = page.locator(f"#map_layers input[value={value}]")
    click(page, cb, pause=pause)


def pick_named_option(page: Page, input_id: str, option_text: str) -> None:
    """For small fixed-choice selectize dropdowns: open it, click the option
    with this exact visible text. These controls show their whole list on
    open (they don't filter as you type), so no search text is needed."""
    box = page.locator(f"#{input_id}-selectized")
    click(page, box, pause=350)
    opt = page.locator(
        ".selectize-dropdown-content .option:visible", has_text=option_text
    ).first
    opt.wait_for(state="visible", timeout=4000)
    click(page, opt, pause=SETTLE_MS)


def pick_first_match(page: Page, input_id: str, search_text: str) -> None:
    """For big searchable selectize dropdowns (subbasin/water body/site
    pickers): type a filter and take whichever match comes first - the
    exact one chosen doesn't matter for a demo."""
    box = page.locator(f"#{input_id}-selectized")
    click(page, box, pause=300)
    page.keyboard.type(search_text, delay=70)
    page.wait_for_timeout(900)  # server-side selectize search: debounce + a round trip
    opt = page.locator(".selectize-dropdown-content .option:visible").first
    opt.wait_for(state="visible", timeout=8000)
    click(page, opt, pause=SETTLE_MS)


def rail_click_first(page: Page, container_id: str) -> None:
    row = page.locator(f"#{container_id} .rail-row").first
    row.wait_for(state="visible", timeout=4000)
    click(page, row, pause=SETTLE_MS)


def count_map_paths(page: Page) -> int:
    return page.evaluate(
        "document.querySelectorAll('#basemap path.leaflet-interactive').length"
    )


def click_new_marker(page: Page, before_count: int) -> bool:
    """Click the first map marker added since `before_count`, returning
    whether one was found. New addCircleMarkers()/addPolygons() layers
    append fresh <path> elements after whatever was already on the map, so
    diffing the count is a reliable way to grab "a marker from the layer I
    just switched on" without needing exact pixel coordinates."""
    pt = page.evaluate(
        f"""
        () => {{
          const paths = document.querySelectorAll('#basemap path.leaflet-interactive');
          const p = paths[{before_count}];
          if (!p) return null;
          const r = p.getBoundingClientRect();
          return {{x: r.x + r.width / 2, y: r.y + r.height / 2}};
        }}
        """
    )
    if pt is None:
        return False
    page.mouse.move(pt["x"], pt["y"], steps=15)
    page.wait_for_timeout(150)
    page.mouse.click(pt["x"], pt["y"])
    page.wait_for_timeout(PAUSE_MS)
    return True


def click_popup_link(page: Page, text_substring: str) -> bool:
    link = page.get_by_text(text_substring, exact=False)
    if link.count() == 0:
        return False
    click(page, link.first, pause=SETTLE_MS)
    return True


def require(ok: bool, message: str) -> None:
    if not ok:
        raise RuntimeError(message)


def safe(label: str, fn) -> None:
    """Run one interaction step; log and move on if it doesn't pan out
    rather than losing the rest of the recording to one bad selector."""
    try:
        fn()
    except Exception as exc:  # noqa: BLE001 - deliberately broad, this is a best-effort demo
        print(f"[warn] step '{label}' failed, continuing: {exc}", file=sys.stderr)


# --------------------------------------------------------------------------
# The tour itself
# --------------------------------------------------------------------------

def run_tour(page: Page, base_url: str) -> None:
    page.set_default_timeout(8000)
    page.goto(base_url, wait_until="networkidle", timeout=60_000)
    page.wait_for_timeout(9000)  # first render: big sf/leaflet geometries
    inject_overlay(page)

    say(page, "ECOMIX Explorer — a quick tour of the dashboard", ms=3200)

    # ---------------- Map ----------------
    say(page, "Map: search for a subbasin by ID…")
    safe("subbasin search", lambda: pick_first_match(page, "subbasin_search", "11235"))

    say(page, "…or turn on a layer, like measured sites, and click one")
    before = count_map_paths(page)
    safe("measured_sites layer", lambda: toggle_layer(page, "measured_sites", pause=2200))

    def marker_to_site_details():
        require(click_new_marker(page, before), "no new marker appeared on the map")
        require(click_popup_link(page, "Open in Site Details"), "popup link not found")

    safe("measured site marker -> Site Details", marker_to_site_details)

    # ---------------- Site Details ----------------
    # Guaranteed to land here even if the marker click above didn't take.
    nav_to(page, "Site Details")
    say(page, "Site Details: chemical occurrence grid and time series for the site", ms=3000)

    say(page, "Switch to a modelled water body instead of a measured site")
    safe(
        "toggle to modelled water body",
        lambda: click(page, page.locator("#site_entity_type").get_by_text("Modelled", exact=True)),
    )
    safe("pick a water body from the rail", lambda: rail_click_first(page, "modelled_rail"))

    say(page, "Choose which determinands appear in the time series")

    def choose_panel_1():
        click(page, page.locator("#site_ts_opts"))
        box = page.locator("#site_ts_chemical_1-selectized")
        click(page, box, pause=350)
        opt = page.locator(".selectize-dropdown-content .option:visible").nth(1)
        click(page, opt, pause=SETTLE_MS)
        page.keyboard.press("Escape")

    safe("choose time-series panel", choose_panel_1)

    # ---------------- Hydro Explorer ----------------
    nav_to(page, "Hydro Explorer")
    say(page, "Hydro Explorer: climate, hydrology and future projections for that subbasin", ms=3000)

    say(page, "Pick a model variable…")
    safe(
        "pick model variable",
        lambda: pick_named_option(page, "prediction_variable", "Inorganic Nitrogen"),
    )

    say(page, "…and flip between yearly, monthly and distribution views")

    def flip_projection_tabs():
        tabs = page.locator("#projection_tabs")
        click(page, tabs.get_by_text("Monthly", exact=True), pause=SETTLE_MS)
        click(page, tabs.get_by_text("Distributions", exact=True), pause=SETTLE_MS)

    safe("flip projection tabs", flip_projection_tabs)

    say(page, "Or switch the whole page to a modelled water body")
    # Not searching a specific water body here: this selectize instance is
    # bound while its pill pane is hidden, and (independent of this script)
    # never receives search results once shown - a pre-existing app quirk,
    # not something to paper over with more clicking.
    safe(
        "switch Hydro Explorer to water body",
        lambda: click(page, page.locator("#de_entity_type").get_by_text("Water body", exact=True)),
    )

    # ---------------- Chemical Explorer ----------------
    nav_to(page, "Chemical Explorer")
    say(page, "Chemical Explorer: daily modelled concentrations against risk thresholds", ms=3000)

    say(page, "Search for a different site…")
    safe("pick a different D3D site", lambda: pick_first_match(page, "chem_d3d_site", "2"))

    say(page, "…or aggregate to weekly / monthly percentiles")

    def change_grouping():
        click(page, page.locator("#chem_d3d_grouping").get_by_text("Weekly", exact=True))
        p90 = page.locator("#chem_d3d_group_stat input[value=p90]")
        click(page, p90, pause=SETTLE_MS)

    safe("change D3D grouping/stat", change_grouping)

    # ---------------- Ecological Risk (reached from the Map) ----------------
    nav_to(page, "Map")
    say(page, "The Map's Ecological risk layer flags acute/chronic exposure by site")
    before = count_map_paths(page)
    # measured_sites is still on from earlier, so this redraws two groups at
    # once (see the clearGroup/addPolygons chain in app.R's map_layers
    # observer) - give it extra time to fully settle before diffing again.
    safe("eco_risk layer", lambda: toggle_layer(page, "eco_risk", pause=3600))

    def marker_to_eco_risk():
        require(click_new_marker(page, before), "no new marker appeared on the map")
        require(click_popup_link(page, "ecological risk detail"), "popup link not found")

    safe("eco-risk marker -> Ecological Risk detail", marker_to_eco_risk)

    nav_to(page, "Ecological Risk")
    say(page, "Ecological Risk: full detail for the selected chemical and site", ms=3000)

    # ---------------- Spatial Datasets ----------------
    nav_to(page, "Spatial Datasets")
    say(page, "Spatial Datasets: any modelled variable as a coloured map layer", ms=3000)
    safe(
        "pick spatial variable",
        lambda: pick_named_option(page, "prediction_variable_spatial", "Fipronil"),
    )

    def switch_basemap():
        click(page, page.locator("#sp_basemap-selectized"), pause=350)
        esri = page.locator(".selectize-dropdown-content .option:visible", has_text="Satellite").first
        click(page, esri, pause=SETTLE_MS)

    safe("switch basemap", switch_basemap)

    # ---------------- Download ----------------
    nav_to(page, "Download")
    say(page, "Download: build a custom extract and preview it before saving", ms=3000)
    safe("pick download variable", lambda: pick_named_option(page, "dl_variable", "Discharge"))

    # ---------------- Help ----------------
    nav_to(page, "Help")
    say(page, "Help page, and that's the tour — thanks for watching", ms=3400)


# --------------------------------------------------------------------------
# Entry point
# --------------------------------------------------------------------------

def main() -> int:
    OUT_DIR.mkdir(exist_ok=True)
    port = free_port()
    base_url = f"http://127.0.0.1:{port}/"
    print(f"Starting Shiny app on {base_url} (log: {LOG_PATH}) ...")
    shiny_proc = start_shiny(port)

    video_dir = OUT_DIR / "_video_tmp"
    video_dir.mkdir(exist_ok=True)

    exit_code = 0
    try:
        wait_for_http(base_url, shiny_proc, timeout=120)
        print("App is up. Launching the recorder...")

        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True)
            context = browser.new_context(
                viewport=VIDEO_SIZE,
                record_video_dir=str(video_dir),
                record_video_size=VIDEO_SIZE,
            )
            page = context.new_page()
            try:
                run_tour(page, base_url)
            except Exception:
                print("Tour hit an error partway through - saving what was recorded:")
                traceback.print_exc()
                exit_code = 1
            finally:
                video = page.video
                context.close()
                # video.path() needs the Playwright driver still running, so
                # it must be read before falling out of the `with` block.
                webm_path = Path(video.path()) if video else None
                browser.close()

        if webm_path and webm_path.exists():
            final_webm = OUT_DIR / "ecomix_walkthrough.webm"
            shutil.move(str(webm_path), final_webm)
            if shutil.which("ffmpeg"):
                final_mp4 = OUT_DIR / "ecomix_walkthrough.mp4"
                result = subprocess.run(
                    [
                        "ffmpeg", "-y", "-i", str(final_webm),
                        "-c:v", "libx264", "-crf", "16", "-preset", "slow",
                        "-pix_fmt", "yuv420p", "-vf", "scale=1600:1000",
                        str(final_mp4),
                    ],
                    capture_output=True, text=True,
                )
                if result.returncode == 0:
                    final_webm.unlink()
                    print(f"Saved video to {final_mp4}")
                else:
                    print("ffmpeg conversion failed, keeping the .webm:")
                    print(result.stderr[-2000:])
                    print(f"Saved video to {final_webm}")
            else:
                print("ffmpeg not found on PATH - keeping the raw .webm:")
                print(f"Saved video to {final_webm}")
        else:
            print("No video was produced - check the traceback above.")
            exit_code = 1
    finally:
        shutil.rmtree(video_dir, ignore_errors=True)
        shiny_proc.terminate()
        try:
            shiny_proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            shiny_proc.kill()

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
