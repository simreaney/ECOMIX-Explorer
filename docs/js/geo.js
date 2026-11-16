// Polygon layers as written by scripts/build_site.py (pack_geometries):
// integer-quantised coordinates, delta-encoded ring after ring. Decoding is
// lazy per feature, so opening the 13,000-polygon "rest" layer to draw one
// subbasin does not build 13,000 GeoJSON objects.

export class GeoPack {
  constructor(pack) {
    const { meta, arrays } = pack;
    this.meta = meta;
    this.props = meta.props;
    this.n = meta.n;
    const { dx, dy, ring_len: ringLen, ring_ext: ringExt, feat_rings: featRings } = arrays;
    const x = new Float64Array(dx.length);
    const y = new Float64Array(dy.length);
    let ax = 0, ay = 0;
    for (let i = 0; i < dx.length; i++) {
      ax += dx[i]; ay += dy[i];
      x[i] = ax / meta.scale; y[i] = ay / meta.scale;
    }
    this.x = x; this.y = y; this.ringLen = ringLen; this.ringExt = ringExt;
    this.ringStart = new Int32Array(ringLen.length + 1);
    for (let r = 0; r < ringLen.length; r++) this.ringStart[r + 1] = this.ringStart[r] + ringLen[r];
    this.featStart = new Int32Array(featRings.length + 1);
    for (let f = 0; f < featRings.length; f++) this.featStart[f + 1] = this.featStart[f] + featRings[f];
    this._index = {};
  }

  // Position of the feature whose property `key` equals `value`.
  find(key, value) {
    if (!this._index[key]) this._index[key] = new Map(this.props[key].map((v, i) => [v, i]));
    const i = this._index[key].get(value);
    return i === undefined ? -1 : i;
  }

  // Polygons of feature i as [[ring, ring...], ...] with rings of [x, y]; the
  // first ring of each polygon is its exterior and rings are closed.
  polygons(i) {
    const polys = [];
    for (let r = this.featStart[i]; r < this.featStart[i + 1]; r++) {
      const a = this.ringStart[r], b = this.ringStart[r + 1];
      const ring = new Array(b - a + 1);
      for (let k = a; k < b; k++) ring[k - a] = [this.x[k], this.y[k]];
      ring[b - a] = [this.x[a], this.y[a]];
      if (this.ringExt[r] || !polys.length) polys.push([ring]);
      else polys[polys.length - 1].push(ring);
    }
    return polys;
  }

  feature(i, properties = {}) {
    return { type: "Feature", properties, geometry: { type: "MultiPolygon", coordinates: this.polygons(i) } };
  }

  // [minx, miny, maxx, maxy]
  bbox(i) {
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    for (let k = this.ringStart[this.featStart[i]]; k < this.ringStart[this.featStart[i + 1]]; k++) {
      const x = this.x[k], y = this.y[k];
      if (x < x0) x0 = x; if (x > x1) x1 = x;
      if (y < y0) y0 = y; if (y > y1) y1 = y;
    }
    return [x0, y0, x1, y1];
  }
}
