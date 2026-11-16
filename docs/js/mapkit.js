// Leaflet helpers shared by the Map, Spatial Datasets and context-strip maps.

import { EC } from "./theme.js";
import { el } from "./ui.js";

const L = () => window.L;

export const CENTRE = [53.75, -1.16];

const TILES = {
  osm: ["https://tile.openstreetmap.org/{z}/{x}/{y}.png",
    { maxZoom: 19, attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors' }],
  esri_satellite: ["https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
    { maxZoom: 19, attribution: "Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community" }],
  esri_gray: ["https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    { maxZoom: 16, attribution: "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ" }],
};

export const BASEMAPS = [
  { value: "osm", label: "Streets (OSM)" },
  { value: "esri_satellite", label: "Satellite (Esri)" },
  { value: "esri_gray", label: "Light gray (Esri)" },
];

export function setBasemap(map, key) {
  if (map._ecBase) map.removeLayer(map._ecBase);
  const [url, opts] = TILES[key] || TILES.osm;
  map._ecBase = L().tileLayer(url, opts).addTo(map);
}

// Leaflet needs to be told when its container changes size (tabs are
// display:none until opened), so every map watches its own box.
function watchSize(map, node, onShow) {
  let wasHidden = true;
  new ResizeObserver(() => {
    const visible = node.clientWidth > 0 && node.clientHeight > 0;
    if (visible) {
      // Default pan keeps the centre fixed (a map built while hidden would
      // otherwise keep its top-left corner and lose the study area off-screen).
      map.invalidateSize({ animate: false });
      if (wasHidden && onShow) onShow();
    }
    wasHidden = !visible;
  }).observe(node);
}

export function makeMap(node, { zoom = 8.5 } = {}) {
  const map = L().map(node, { zoomSnap: 0.25, preferCanvas: true, zoomControl: true }).setView(CENTRE, zoom);
  // One canvas renderer for every vector layer: separate canvases would
  // block each other's clicks. Draw order is managed with bringToFront().
  map._ecRenderer = L().canvas({ padding: 0.5, tolerance: 3 });
  setBasemap(map, "osm");
  watchSize(map, node);
  return map;
}

export function miniMap(node) {
  const map = L().map(node, {
    zoomControl: false, attributionControl: false, dragging: false, scrollWheelZoom: false,
    doubleClickZoom: false, boxZoom: false, keyboard: false, touchZoom: false,
  }).setView(CENTRE, 7);
  L().tileLayer(TILES.osm[0], { maxZoom: 19 }).addTo(map);
  let shape = null;
  let view = null;   // re-applied when the strip becomes visible (a hidden map has no size to fit to)
  const apply = () => {
    if (!view || !node.clientWidth) return;
    if (view.bbox) map.fitBounds([[view.bbox[1], view.bbox[0]], [view.bbox[3], view.bbox[2]]], { maxZoom: 12, padding: [6, 6], animate: false });
    else map.setView(view.centre, view.zoom, { animate: false });
  };
  watchSize(map, node, apply);
  return {
    map,
    showFeature(feature, bbox) {
      if (shape) map.removeLayer(shape);
      shape = feature ? L().geoJSON(feature, { interactive: false, style: { color: EC.accent, weight: 2, fillColor: EC.accent, fillOpacity: 0.6 } }).addTo(map) : null;
      view = bbox ? { bbox } : null;
      apply();
    },
    showPoint(lat, lon) {
      if (shape) map.removeLayer(shape);
      shape = L().circleMarker([lat, lon], { radius: 6, color: EC.accent, weight: 2, fillColor: EC.accent, fillOpacity: 1, interactive: false }).addTo(map);
      view = { centre: [lat, lon], zoom: 10 };
      apply();
    },
    clear() { if (shape) map.removeLayer(shape); shape = null; view = null; },
  };
}

// GeoJSON layer from a GeoPack, one feature per index. style(i) -> path
// options; each feature gets properties {i}.
export function packLayer(map, pack, indices, style, opts = {}) {
  const features = indices.map((i) => pack.feature(i, { i }));
  return L().geoJSON({ type: "FeatureCollection", features }, {
    renderer: map._ecRenderer,
    style: (f) => style(f.properties.i),
    ...opts,
  });
}

export function fitBbox(map, bbox, maxZoom = 12) {
  map.fitBounds([[bbox[1], bbox[0]], [bbox[3], bbox[2]]], { maxZoom, padding: [20, 20] });
}

// Popup body: a bold title, lines of text, then action links.
export function popupBody(title, lines = [], links = []) {
  return el("div", {},
    el("strong", { text: title }),
    lines.filter(Boolean).map((l) => [el("br"), typeof l === "string" ? document.createTextNode(l) : l]),
    links.filter(Boolean).map(([text, fn]) => [el("br"), el("a", { href: "#", text, onclick: (e) => { e.preventDefault(); fn(); } })]));
}

export function legendControl(position = "bottomleft") {
  const ctl = L().control({ position });
  ctl.onAdd = () => {
    const div = el("div.legend");
    L().DomEvent.disableClickPropagation(div);
    ctl._div = div;
    return div;
  };
  return ctl;
}
