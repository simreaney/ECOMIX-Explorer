// File writers for the Download page, all running in the browser: CSV, XLSX,
// Parquet for tables; GeoPackage, Shapefile (zip), GeoParquet and GeoJSON
// for spatial layers. Spatial exports are in British National Grid
// (EPSG:27700), as the Shiny app's were.

import { BNG_WKT1, BNG_PRJ, BNG_PROJJSON } from "./crs.js";

// ---- column typing ---------------------------------------------------------

function columnsOf(rows) {
  const names = [];
  const seen = new Set();
  for (const r of rows) for (const k of Object.keys(r)) if (!seen.has(k)) { seen.add(k); names.push(k); }
  return names.map((name) => {
    let numeric = true, integer = true, any = false;
    for (const r of rows) {
      const v = r[name];
      if (v == null || (typeof v === "number" && Number.isNaN(v))) continue;
      any = true;
      if (typeof v !== "number") { numeric = false; integer = false; break; }
      if (!Number.isInteger(v) || Math.abs(v) > 2 ** 31 - 1) integer = false;
    }
    return { name, type: !any ? "string" : numeric ? (integer ? "int" : "double") : "string" };
  });
}

const isNull = (v) => v == null || (typeof v === "number" && !Number.isFinite(v));

// ---- CSV -------------------------------------------------------------------

export function toCSV(rows) {
  const cols = columnsOf(rows).map((c) => c.name);
  const cell = (v) => {
    if (isNull(v)) return "NA";
    if (typeof v === "number") return String(v);
    const s = String(v);
    return /[",\n\r]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
  };
  const lines = [cols.map((c) => cell(c)).join(",")];
  for (const r of rows) lines.push(cols.map((c) => cell(r[c])).join(","));
  return lines.join("\n") + "\n";
}

// ---- XLSX (one sheet, inline strings) --------------------------------------

const xmlEsc = (s) => String(s).replace(/[<>&"]/g, (c) => ({ "<": "&lt;", ">": "&gt;", "&": "&amp;", '"': "&quot;" }[c])).replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f]/g, "");

function colRef(i) {
  let s = "";
  for (i += 1; i > 0; i = Math.floor((i - 1) / 26)) s = String.fromCharCode(65 + ((i - 1) % 26)) + s;
  return s;
}

export function toXLSX(rows, sheetName = "data") {
  const cols = columnsOf(rows);
  const out = [];
  const head = cols.map((c, j) => `<c r="${colRef(j)}1" t="inlineStr"><is><t>${xmlEsc(c.name)}</t></is></c>`).join("");
  out.push(`<row r="1">${head}</row>`);
  rows.forEach((r, i) => {
    const n = i + 2;
    const cells = cols.map((c, j) => {
      const v = r[c.name];
      if (isNull(v)) return "";
      if (typeof v === "number") return `<c r="${colRef(j)}${n}"><v>${v}</v></c>`;
      return `<c r="${colRef(j)}${n}" t="inlineStr"><is><t xml:space="preserve">${xmlEsc(v)}</t></is></c>`;
    }).join("");
    out.push(`<row r="${n}">${cells}</row>`);
  });
  const enc = new TextEncoder();
  const files = {
    "[Content_Types].xml": `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>`,
    "_rels/.rels": `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>`,
    "xl/workbook.xml": `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="${xmlEsc(sheetName)}" sheetId="1" r:id="rId1"/></sheets></workbook>`,
    "xl/_rels/workbook.xml.rels": `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>`,
    "xl/worksheets/sheet1.xml": `<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>${out.join("")}</sheetData></worksheet>`,
  };
  const zipped = window.fflate.zipSync(Object.fromEntries(Object.entries(files).map(([k, v]) => [k, enc.encode(v)])), { level: 6 });
  return new Blob([zipped], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
}

// ---- Parquet ---------------------------------------------------------------

let parquetLib = null;
const parquet = () => (parquetLib = parquetLib || import("../vendor/hyparquet-writer.min.js"));

function parquetColumns(rows, cols) {
  return cols.map((c) => {
    const data = rows.map((r) => (isNull(r[c.name]) ? null : r[c.name]));
    if (c.type === "int") return { name: c.name, data, type: "INT32" };
    if (c.type === "double") return { name: c.name, data, type: "DOUBLE" };
    return { name: c.name, data: data.map((v) => (v == null ? null : String(v))), type: "STRING" };
  });
}

export async function toParquet(rows) {
  const { parquetWriteBuffer } = await parquet();
  const buf = parquetWriteBuffer({ columnData: parquetColumns(rows, columnsOf(rows)) });
  return new Blob([buf], { type: "application/vnd.apache.parquet" });
}

// ---- geometry encoding -----------------------------------------------------
// A feature here is {props: {...}, polygons: [[ring, ...], ...]} with rings of
// [x, y] (closed), in EPSG:27700.

function bboxOf(polygons) {
  let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
  for (const poly of polygons) for (const ring of poly) for (const [x, y] of ring) {
    if (x < x0) x0 = x; if (x > x1) x1 = x; if (y < y0) y0 = y; if (y > y1) y1 = y;
  }
  return [x0, y0, x1, y1];
}

function wkbMultiPolygon(polygons) {
  let size = 9;
  for (const poly of polygons) { size += 9; for (const ring of poly) size += 4 + ring.length * 16; }
  const buf = new ArrayBuffer(size);
  const dv = new DataView(buf);
  let o = 0;
  const u8 = (v) => { dv.setUint8(o, v); o += 1; };
  const u32 = (v) => { dv.setUint32(o, v, true); o += 4; };
  const f64 = (v) => { dv.setFloat64(o, v, true); o += 8; };
  u8(1); u32(6); u32(polygons.length);
  for (const poly of polygons) {
    u8(1); u32(3); u32(poly.length);
    for (const ring of poly) { u32(ring.length); for (const [x, y] of ring) { f64(x); f64(y); } }
  }
  return new Uint8Array(buf);
}

// ---- GeoPackage (via sql.js) -----------------------------------------------

let sqlPromise = null;
function loadSql() {
  if (!sqlPromise) {
    sqlPromise = new Promise((resolve, reject) => {
      const s = document.createElement("script");
      s.src = "vendor/sql-wasm.js";
      s.onload = () => window.initSqlJs({ locateFile: (f) => `vendor/${f}` }).then(resolve, reject);
      s.onerror = () => reject(new Error("could not load the SQLite engine"));
      document.head.append(s);
    });
    sqlPromise.catch(() => { sqlPromise = null; });
  }
  return sqlPromise;
}

function gpkgBlob(polygons, srsId) {
  const wkb = wkbMultiPolygon(polygons);
  const [x0, y0, x1, y1] = bboxOf(polygons);
  const out = new Uint8Array(8 + 32 + wkb.length);
  const dv = new DataView(out.buffer);
  out[0] = 0x47; out[1] = 0x50; out[2] = 0; out[3] = 0b00000011; // little-endian, [minx,maxx,miny,maxy] envelope
  dv.setInt32(4, srsId, true);
  dv.setFloat64(8, x0, true); dv.setFloat64(16, x1, true); dv.setFloat64(24, y0, true); dv.setFloat64(32, y1, true);
  out.set(wkb, 40);
  return out;
}

const sqlIdent = (s) => `"${String(s).replace(/"/g, '""')}"`;

export async function toGeoPackage(features, layer = "ecomix_spatial") {
  const SQL = await loadSql();
  const db = new SQL.Database();
  const cols = columnsOf(features.map((f) => f.props));
  const srs = 27700;
  const all = bboxOf(features.flatMap((f) => f.polygons));
  db.run("PRAGMA application_id = 1196444487");
  db.run("PRAGMA user_version = 10300");
  db.run(`CREATE TABLE gpkg_spatial_ref_sys (srs_name TEXT NOT NULL, srs_id INTEGER NOT NULL PRIMARY KEY, organization TEXT NOT NULL,
    organization_coordsys_id INTEGER NOT NULL, definition TEXT NOT NULL, description TEXT)`);
  const srsInsert = db.prepare("INSERT INTO gpkg_spatial_ref_sys VALUES (?, ?, ?, ?, ?, ?)");
  srsInsert.run(["Undefined cartesian SRS", -1, "NONE", -1, "undefined", "undefined cartesian coordinate reference system"]);
  srsInsert.run(["Undefined geographic SRS", 0, "NONE", 0, "undefined", "undefined geographic coordinate reference system"]);
  srsInsert.run(["WGS 84 geodetic", 4326, "EPSG", 4326, 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AXIS["Latitude",NORTH],AXIS["Longitude",EAST],AUTHORITY["EPSG","4326"]]', "longitude/latitude coordinates in decimal degrees on the WGS 84 spheroid"]);
  srsInsert.run(["OSGB36 / British National Grid", srs, "EPSG", srs, BNG_WKT1, "British National Grid"]);
  srsInsert.free();
  db.run(`CREATE TABLE gpkg_contents (table_name TEXT NOT NULL PRIMARY KEY, data_type TEXT NOT NULL, identifier TEXT UNIQUE, description TEXT DEFAULT '',
    last_change DATETIME NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')), min_x DOUBLE, min_y DOUBLE, max_x DOUBLE, max_y DOUBLE, srs_id INTEGER,
    CONSTRAINT fk_gc_r_srs_id FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys(srs_id))`);
  db.run(`CREATE TABLE gpkg_geometry_columns (table_name TEXT NOT NULL, column_name TEXT NOT NULL, geometry_type_name TEXT NOT NULL, srs_id INTEGER NOT NULL,
    z TINYINT NOT NULL, m TINYINT NOT NULL, CONSTRAINT pk_geom_cols PRIMARY KEY (table_name, column_name),
    CONSTRAINT fk_gc_tn FOREIGN KEY (table_name) REFERENCES gpkg_contents(table_name), CONSTRAINT fk_gc_srs FOREIGN KEY (srs_id) REFERENCES gpkg_spatial_ref_sys (srs_id))`);
  const colDefs = cols.map((c) => `${sqlIdent(c.name)} ${c.type === "int" ? "INTEGER" : c.type === "double" ? "REAL" : "TEXT"}`);
  db.run(`CREATE TABLE ${sqlIdent(layer)} (fid INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL, geom MULTIPOLYGON${colDefs.map((d) => `, ${d}`).join("")})`);
  db.run("INSERT INTO gpkg_contents (table_name, data_type, identifier, description, min_x, min_y, max_x, max_y, srs_id) VALUES (?, 'features', ?, '', ?, ?, ?, ?, ?)",
    [layer, layer, all[0], all[1], all[2], all[3], srs]);
  db.run("INSERT INTO gpkg_geometry_columns VALUES (?, 'geom', 'MULTIPOLYGON', ?, 0, 0)", [layer, srs]);
  const ins = db.prepare(`INSERT INTO ${sqlIdent(layer)} (geom${cols.map((c) => `, ${sqlIdent(c.name)}`).join("")}) VALUES (?${cols.map(() => ", ?").join("")})`);
  db.run("BEGIN");
  for (const f of features) ins.run([gpkgBlob(f.polygons, srs), ...cols.map((c) => (isNull(f.props[c.name]) ? null : f.props[c.name]))]);
  db.run("COMMIT");
  ins.free();
  const bytes = db.export();
  db.close();
  return new Blob([bytes], { type: "application/geopackage+sqlite3" });
}

// ---- Shapefile (zip of .shp/.shx/.dbf/.prj/.cpg) ----------------------------

function signedArea(ring) {
  let a = 0;
  for (let i = 0; i < ring.length - 1; i++) a += ring[i][0] * ring[i + 1][1] - ring[i + 1][0] * ring[i][1];
  return a / 2;
}

function dbfFields(rows) {
  const cols = columnsOf(rows);
  const used = new Set();
  return cols.map((c) => {
    let name = c.name.slice(0, 10);
    for (let k = 1; used.has(name.toUpperCase()); k++) name = `${c.name.slice(0, 10 - String(k).length)}${k}`;
    used.add(name.toUpperCase());
    if (c.type === "int") return { key: c.name, name, type: "N", len: 11, dec: 0 };
    if (c.type === "double") return { key: c.name, name, type: "N", len: 24, dec: 15 };
    const enc = new TextEncoder();
    const len = Math.min(254, Math.max(1, ...rows.map((r) => (isNull(r[c.name]) ? 0 : enc.encode(String(r[c.name])).length))));
    return { key: c.name, name, type: "C", len, dec: 0 };
  });
}

function fmtDbfNumber(v, f) {
  if (isNull(v)) return " ".repeat(f.len);
  let s = f.dec ? v.toFixed(f.dec) : String(Math.round(v));
  for (let d = f.dec; s.length > f.len && d > 0; d--) s = v.toFixed(d - 1);
  if (s.length > f.len) s = v.toExponential(f.len - 7);
  return s.padStart(f.len, " ");
}

function writeDbf(rows) {
  const fields = dbfFields(rows);
  const enc = new TextEncoder();
  const headerLen = 32 + 32 * fields.length + 1;
  const recLen = 1 + fields.reduce((a, f) => a + f.len, 0);
  const buf = new Uint8Array(headerLen + recLen * rows.length + 1);
  const dv = new DataView(buf.buffer);
  const now = new Date();
  buf[0] = 0x03; buf[1] = now.getFullYear() - 1900; buf[2] = now.getMonth() + 1; buf[3] = now.getDate();
  dv.setUint32(4, rows.length, true); dv.setUint16(8, headerLen, true); dv.setUint16(10, recLen, true);
  fields.forEach((f, i) => {
    const o = 32 + i * 32;
    buf.set(enc.encode(f.name).slice(0, 10), o);
    buf[o + 11] = f.type.charCodeAt(0); buf[o + 16] = f.len; buf[o + 17] = f.dec;
  });
  buf[headerLen - 1] = 0x0d;
  let o = headerLen;
  for (const r of rows) {
    buf[o++] = 0x20;
    for (const f of fields) {
      const v = r[f.key];
      let bytes;
      if (f.type === "N") bytes = enc.encode(fmtDbfNumber(v, f));
      else {
        const s = isNull(v) ? "" : String(v);
        let b = enc.encode(s);
        if (b.length > f.len) b = b.slice(0, f.len);
        bytes = new Uint8Array(f.len).fill(0x20);
        bytes.set(b);
      }
      buf.set(bytes, o);
      o += f.len;
    }
  }
  buf[o] = 0x1a;
  return buf;
}

export function toShapefileZip(features, base = "ecomix_spatial") {
  // Shapefile rings: outer clockwise, holes anticlockwise.
  const recs = features.map((f) => {
    const parts = [];
    for (const poly of f.polygons) {
      poly.forEach((ring, k) => {
        const cw = signedArea(ring) < 0;
        parts.push((k === 0) === cw ? ring : ring.slice().reverse());
      });
    }
    const nPoints = parts.reduce((a, p) => a + p.length, 0);
    return { parts, nPoints, bbox: bboxOf(f.polygons), bytes: 44 + 4 * parts.length + 16 * nPoints };
  });
  const all = bboxOf(features.flatMap((f) => f.polygons));
  const shpLen = 100 + recs.reduce((a, r) => a + 8 + r.bytes, 0);
  const shp = new DataView(new ArrayBuffer(shpLen));
  const shx = new DataView(new ArrayBuffer(100 + 8 * recs.length));
  const header = (dv, len) => {
    dv.setInt32(0, 9994, false); dv.setInt32(24, len / 2, false); dv.setInt32(28, 1000, true); dv.setInt32(32, 5, true);
    dv.setFloat64(36, all[0], true); dv.setFloat64(44, all[1], true); dv.setFloat64(52, all[2], true); dv.setFloat64(60, all[3], true);
  };
  header(shp, shpLen); header(shx, 100 + 8 * recs.length);
  let o = 100;
  recs.forEach((r, i) => {
    shx.setInt32(100 + i * 8, o / 2, false); shx.setInt32(104 + i * 8, r.bytes / 2, false);
    shp.setInt32(o, i + 1, false); shp.setInt32(o + 4, r.bytes / 2, false); o += 8;
    shp.setInt32(o, 5, true);
    shp.setFloat64(o + 4, r.bbox[0], true); shp.setFloat64(o + 12, r.bbox[1], true); shp.setFloat64(o + 20, r.bbox[2], true); shp.setFloat64(o + 28, r.bbox[3], true);
    shp.setInt32(o + 36, r.parts.length, true); shp.setInt32(o + 40, r.nPoints, true);
    let p = o + 44, start = 0;
    for (const part of r.parts) { shp.setInt32(p, start, true); p += 4; start += part.length; }
    for (const part of r.parts) for (const [x, y] of part) { shp.setFloat64(p, x, true); shp.setFloat64(p + 8, y, true); p += 16; }
    o += r.bytes;
  });
  const enc = new TextEncoder();
  const zipped = window.fflate.zipSync({
    [`${base}.shp`]: new Uint8Array(shp.buffer),
    [`${base}.shx`]: new Uint8Array(shx.buffer),
    [`${base}.dbf`]: writeDbf(features.map((f) => f.props)),
    [`${base}.prj`]: enc.encode(BNG_PRJ),
    [`${base}.cpg`]: enc.encode("UTF-8"),
  }, { level: 6 });
  return new Blob([zipped], { type: "application/zip" });
}

// ---- GeoParquet ------------------------------------------------------------

export async function toGeoParquet(features) {
  const { parquetWriteBuffer } = await parquet();
  const rows = features.map((f) => f.props);
  const columnData = parquetColumns(rows, columnsOf(rows));
  columnData.push({ name: "geometry", data: features.map((f) => wkbMultiPolygon(f.polygons)), type: "BYTE_ARRAY" });
  const geo = {
    version: "1.0.0", primary_column: "geometry",
    columns: { geometry: { encoding: "WKB", geometry_types: ["MultiPolygon"], crs: BNG_PROJJSON, bbox: bboxOf(features.flatMap((f) => f.polygons)) } },
  };
  const buf = parquetWriteBuffer({ columnData, kvMetadata: [{ key: "geo", value: JSON.stringify(geo) }] });
  return new Blob([buf], { type: "application/vnd.apache.parquet" });
}

// ---- GeoJSON ---------------------------------------------------------------

export function toGeoJSON(features) {
  const fc = {
    type: "FeatureCollection",
    // Legacy CRS member: coordinates are British National Grid, not WGS84.
    crs: { type: "name", properties: { name: "urn:ogc:def:crs:EPSG::27700" } },
    features: features.map((f) => ({
      type: "Feature",
      properties: Object.fromEntries(Object.entries(f.props).map(([k, v]) => [k, isNull(v) ? null : v])),
      geometry: { type: "MultiPolygon", coordinates: f.polygons },
    })),
  };
  return new Blob([JSON.stringify(fc)], { type: "application/geo+json" });
}
