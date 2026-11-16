// Design tokens (Modernist) - mirrored in css/app.css; keep the two in step.

export const EC = {
  ground: "#f3f2f2",
  surface: "#eae9e9",
  ink: "#201e1d",
  inkMuted: "#444141",
  inkDim: "#605d5d",
  rule: "#201e1d",
  ruleLight: "#d7d3d3",
  accent: "#ec3013",
  accentText: "#ae1800",
  accentTint: "#ffe0d9",
  accentInk: "#7c1405",
};

export const FONT = '"Archivo", system-ui, sans-serif';

// Categorical series colours. The interface is mono + one accent, so charts
// use the accent first and add just two more hues; this trio is checked for
// colour-vision-deficiency separation in every pairing. A series keeps its
// colour whatever else is selected.
export const SERIES = { red: "#ec3013", blue: "#2a78d6", aqua: "#1baf7a" };

// Emission scenarios: the baseline is ink (the reference everything else is
// read against); high emissions is the accent.
const SCENARIO_HUE = {
  SSP126: ["#a9cdf5", "#1c5cab"],
  SSP245: ["#9fe3c9", "#0d7a53"],
  SSP585: ["#f8b3a6", "#d92a0f"],
  "UKCP18 RCP8.5": ["#f8b3a6", "#d92a0f"],
};
const SPARE_HUES = [["#d6c8f5", "#5a45c9"], ["#f3cfa0", "#a36400"]];

// Colour for a scenario x period series: hue says which scenario, lightness
// which decade (earlier = lighter), so a later period is never mistaken for
// a different scenario.
export function scenarioColour(ssp, period, periods) {
  if (ssp === "Baseline") return EC.ink;
  let ramp = SCENARIO_HUE[ssp];
  if (!ramp) {
    const extra = Object.keys(scenarioColour.extra);
    if (!(ssp in scenarioColour.extra)) scenarioColour.extra[ssp] = SPARE_HUES[extra.length % SPARE_HUES.length];
    ramp = scenarioColour.extra[ssp];
  }
  const i = periods ? periods.indexOf(period) : -1;
  const t = i < 0 || periods.length < 2 ? 1 : i / (periods.length - 1);
  return window.d3.interpolateLab(ramp[0], ramp[1])(0.25 + 0.75 * t);
}
scenarioColour.extra = {};

export const STAT_COLOUR = { median: SERIES.red, mean: SERIES.blue, "90th": SERIES.aqua };
export const STAT_LABEL = { mean: "Mean", median: "Median", "90th": "90th percentile" };

// Percentile line styles: the median is solid; lower percentiles use dashes
// that get shorter away from it, upper percentiles dots/dash-dots likewise.
export const PCT_DASH = {
  p01: "1 5", p05: "3 5", p10: "6 4", p25: "11 4", p50: null,
  p75: "11 3 2 3", p90: "6 3 2 3", p95: "3 3 1 3", p99: "1 3",
};

export const PCT_LABEL = {
  p01: "Extreme low (1st percentile)", p05: "Very low (5th percentile)",
  p10: "Low (10th percentile)", p25: "Lower quartile (25th percentile)",
  p50: "Average (50th percentile)", p75: "Upper quartile (75th percentile)",
  p90: "High (90th percentile)", p95: "Very high (95th percentile)",
  p99: "Extreme high (99th percentile)",
};

export const PCT_PROB = { p01: 0.01, p05: 0.05, p10: 0.1, p25: 0.25, p50: 0.5, p75: 0.75, p90: 0.9, p95: 0.95, p99: 0.99 };

// Heat ramp for the occurrence grids (one hue, light -> dark).
export const HEAT = [EC.ruleLight, "#ffc4b8", "#ff563c", EC.accentText];

export const RISK_COLOUR = { None: EC.ruleLight, Acute: EC.accentTint, Chronic: EC.accent, Both: EC.accentInk };
export const RISK_LEVELS = ["None", "Acute", "Chronic", "Both"];

export function prettyUnit(u) {
  return { "m3/s": "m³/s", "ug/l": "µg/l", degC: "°C" }[u] || u || "";
}

// ---- number / date formatting ----------------------------------------------

export function fmtNum(v, digits = 3) {
  if (v == null || !Number.isFinite(v)) return "–";
  if (v === 0) return "0";
  const a = Math.abs(v);
  if (a >= 1e6 || a < 1e-4) return v.toExponential(Math.max(digits - 1, 0)).replace("e+", "e").replace("e-", "e−").replace(/^-/, "−");
  const s = Number(v.toPrecision(digits));
  return s.toLocaleString("en-GB", { maximumSignificantDigits: Math.max(digits, 1), maximumFractionDigits: 20 }).replace(/^-/, "−");
}

export function fmtInt(v) {
  return v == null || !Number.isFinite(v) ? "–" : Math.round(v).toLocaleString("en-GB");
}

const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
export const monthName = (m) => MONTHS[m - 1];

export const DAY = 86400000;
export const dayToDate = (d) => new Date(d * DAY);
export const dateToDay = (d) => Math.round(d.getTime() / DAY);

export function fmtDate(d, style = "dmy") {
  if (!(d instanceof Date)) d = new Date(d);
  const day = String(d.getUTCDate()).padStart(2, "0");
  const mon = MONTHS[d.getUTCMonth()];
  const yr = d.getUTCFullYear();
  if (style === "my") return `${mon} ${yr}`;
  if (style === "iso") return `${yr}-${String(d.getUTCMonth() + 1).padStart(2, "0")}-${day}`;
  return `${day} ${mon} ${yr}`;
}

// R's quantile(type = 7) on an unsorted array (NaN ignored).
export function quantile(values, p) {
  const v = Float64Array.from(values).filter((x) => !Number.isNaN(x)).sort();
  if (!v.length) return NaN;
  const h = (v.length - 1) * p;
  const lo = Math.floor(h);
  const hi = Math.min(lo + 1, v.length - 1);
  return v[lo] + (h - lo) * (v[hi] - v[lo]);
}

export function quantilesSorted(sorted, probs) {
  const n = sorted.length;
  return probs.map((p) => {
    if (!n) return NaN;
    const h = (n - 1) * p;
    const lo = Math.floor(h);
    const hi = Math.min(lo + 1, n - 1);
    return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
  });
}
