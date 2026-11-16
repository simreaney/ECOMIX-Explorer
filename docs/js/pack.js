// Reader for the "pack" files written by scripts/build_site.py: a gzip stream
// holding  "ECXP" | uint32 header length | JSON header | array payload.
// Arrays come back as typed arrays carrying a `.shape`.

const TYPES = {
  f4: Float32Array, f8: Float64Array, i4: Int32Array, u4: Uint32Array,
  i2: Int16Array, u2: Uint16Array, i1: Int8Array, u1: Uint8Array,
};

async function gunzip(buf) {
  const bytes = new Uint8Array(buf);
  // A host that sets Content-Encoding for .gz files has already inflated it.
  if (!(bytes[0] === 0x1f && bytes[1] === 0x8b)) return buf;
  if (typeof DecompressionStream === "function") {
    const stream = new Blob([buf]).stream().pipeThrough(new DecompressionStream("gzip"));
    return new Response(stream).arrayBuffer();
  }
  return window.fflate.gunzipSync(bytes).buffer;
}

function unshuffle(bytes, size) {
  const n = bytes.length / size;
  const out = new Uint8Array(bytes.length);
  for (let b = 0; b < size; b++) {
    const plane = b * n;
    for (let i = 0; i < n; i++) out[i * size + b] = bytes[plane + i];
  }
  return out;
}

export function parsePack(buf) {
  const bytes = new Uint8Array(buf);
  if (String.fromCharCode(bytes[0], bytes[1], bytes[2], bytes[3]) !== "ECXP") {
    throw new Error("not an ECOMIX pack file");
  }
  const headerLen = new DataView(buf).getUint32(4, true);
  const header = JSON.parse(new TextDecoder().decode(bytes.subarray(8, 8 + headerLen)));
  const base = 8 + headerLen;
  const arrays = {};
  for (const [name, a] of Object.entries(header.arrays)) {
    const Type = TYPES[a.dtype];
    let raw = bytes.subarray(base + a.offset, base + a.offset + a.nbytes);
    // Copy either way: unshuffle makes a fresh buffer, slice() guarantees alignment.
    raw = a.shuffle ? unshuffle(raw, Type.BYTES_PER_ELEMENT) : raw.slice();
    const arr = new Type(raw.buffer, 0, raw.length / Type.BYTES_PER_ELEMENT);
    arr.shape = a.shape;
    arrays[name] = arr;
  }
  return { meta: header.meta, arrays };
}

export async function fetchPack(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${url}: HTTP ${res.status}`);
  return parsePack(await gunzip(await res.arrayBuffer()));
}

export async function fetchJSON(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${url}: HTTP ${res.status}`);
  return res.json();
}
