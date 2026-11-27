#!/usr/bin/env python3
"""Independent reference calculation for inst/extdata/lc_batch_control.csv.

Reads raw CSV bytes using only the Python standard library, verifies the file
SHA-256 digest, detects peaks by prominence, integrates peak areas above the
local linear baseline connecting left_base and right_base, fits an unweighted
OLS calibration curve on standard injections, applies dilution back-calculation,
and evaluates blank, QC, range, missing-peak, and interference outcomes.
"""

from __future__ import annotations

import csv
import hashlib
import json
import math
import pathlib
import sys


def detect_peaks_prominence(
    locations: list[float],
    values: list[float],
    *,
    min_prominence: float = 1.0,
) -> list[dict[str, float]]:
    """Prominence peak detector matching measure's local-minima boundary rule."""
    n = len(values)
    if n < 3:
        return []

    peaks_idx = [
        i
        for i in range(1, n - 1)
        if values[i] > values[i - 1] and values[i] > values[i + 1]
    ]
    if not peaks_idx:
        return []

    retained: list[tuple[int, float, int, int]] = []
    for pk in peaks_idx:
        pk_val = values[pk]
        left_idx = pk
        left_min = pk_val
        for j in range(pk - 1, -1, -1):
            if values[j] > pk_val:
                break
            if values[j] < left_min:
                left_min = values[j]
                left_idx = j

        right_idx = pk
        right_min = pk_val
        for j in range(pk + 1, n):
            if values[j] > pk_val:
                break
            if values[j] < right_min:
                right_min = values[j]
                right_idx = j

        prominence = pk_val - max(values[left_idx], values[right_idx])
        if prominence >= min_prominence:
            retained.append((pk, prominence, left_idx, right_idx))

    if not retained:
        return []

    # Clip bases to lowest sampled value between adjacent retained peaks
    # (ties pick the first index in acquisition order, matching R's which.min).
    k = len(retained)
    out: list[dict[str, float]] = []
    for idx, (pk, _prom, left_idx, right_idx) in enumerate(retained):
        if idx > 0:
            prev_pk = retained[idx - 1][0]
            valley_slice = values[prev_pk : pk + 1]
            valley_offset = min(range(len(valley_slice)), key=valley_slice.__getitem__)
            left_idx = prev_pk + valley_offset
        if idx < k - 1:
            next_pk = retained[idx + 1][0]
            valley_slice = values[pk : next_pk + 1]
            valley_offset = min(range(len(valley_slice)), key=valley_slice.__getitem__)
            right_idx = pk + valley_offset

        out.append(
            {
                "location": locations[pk],
                "height": values[pk],
                "left_base": locations[left_idx],
                "right_base": locations[right_idx],
                "left_idx": left_idx,
                "right_idx": right_idx,
            }
        )
    return out


def integrate_local_trapezoid(
    locations: list[float],
    values: list[float],
    left_base: float,
    right_base: float,
) -> float:
    """Integrate signal above the straight line joining the observed endpoints."""
    lo, hi = sorted((left_base, right_base))
    pts = sorted((x, y) for x, y in zip(locations, values) if lo <= x <= hi)
    if len(pts) < 2:
        return math.nan
    x0, y0 = pts[0]
    x1, y1 = pts[-1]
    span = x1 - x0
    corr = [
        max(0.0, y - (y0 if span == 0.0 else y0 + (y1 - y0) * (x - x0) / span))
        for x, y in pts
    ]
    area = 0.0
    for i in range(len(pts) - 1):
        area += 0.5 * (corr[i] + corr[i + 1]) * (pts[i + 1][0] - pts[i][0])
    return area


def ols_fit(xs: list[float], ys: list[float]) -> tuple[float, float, float]:
    """Return (intercept, slope, r_squared) for y = intercept + slope * x."""
    n = len(xs)
    x_bar = sum(xs) / n
    y_bar = sum(ys) / n
    sxx = sum((x - x_bar) ** 2 for x in xs)
    sxy = sum((x - x_bar) * (y - y_bar) for x, y in zip(xs, ys))
    slope = sxy / sxx
    intercept = y_bar - slope * x_bar
    ss_tot = sum((y - y_bar) ** 2 for y in ys)
    ss_res = sum((y - (intercept + slope * x)) ** 2 for x, y in zip(xs, ys))
    r_squared = 1.0 - ss_res / ss_tot
    return intercept, slope, r_squared


def main() -> None:
    repo_root = pathlib.Path(__file__).resolve().parents[2]
    data_path = repo_root / "inst" / "extdata" / "lc_batch_control.csv"
    manifest_path = repo_root / "inst" / "extdata" / "lc_batch_manifest.csv"

    raw_bytes = data_path.read_bytes()
    sha256 = hashlib.sha256(raw_bytes).hexdigest()

    curves: dict[str, dict[str, object]] = {}
    with data_path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        for row_num, row in enumerate(reader, start=1):
            inj = row["injection_id"]
            if inj not in curves:
                curves[inj] = {
                    "sample_id": row["sample_id"],
                    "curve_id": row["curve_id"],
                    "locations": [],
                    "values": [],
                    "source_rows": [],
                }
            curves[inj]["locations"].append(float(row["location"]))
            curves[inj]["values"].append(float(row["value"]))
            curves[inj]["source_rows"].append(row_num)

    manifest: list[dict[str, object]] = []
    with manifest_path.open("r", encoding="utf-8", newline="") as handle:
        for row in csv.DictReader(handle):
            manifest.append(
                {
                    "batch_order": int(row["batch_order"]),
                    "injection_id": row["injection_id"],
                    "injection_type": row["injection_type"],
                    "nominal_conc": None
                    if row["nominal_conc"] in ("", "NA")
                    else float(row["nominal_conc"]),
                    "dilution_factor": float(row["dilution_factor"]),
                }
            )

    window = (4.8, 5.6)
    min_prominence = 1.0
    qc_acceptance_pct = 15.0
    blank_max_pct_lloq = 20.0

    # Peak detection and window selection
    detected: list[dict[str, object]] = []
    for entry in manifest:
        inj = entry["injection_id"]
        curve = curves[inj]
        locs = curve["locations"]
        vals = curve["values"]
        src_rows = curve["source_rows"]
        peaks = detect_peaks_prominence(locs, vals, min_prominence=min_prominence)
        in_win = [p for p in peaks if window[0] <= p["location"] <= window[1]]
        item = {
            **entry,
            "sample_id": curve["sample_id"],
            "curve_id": curve["curve_id"],
            "source_row_start": min(src_rows),
            "source_row_end": max(src_rows),
            "peak_count": len(in_win),
            "peak_status": "missing"
            if len(in_win) == 0
            else ("unique" if len(in_win) == 1 else "interfered"),
            "peak_location": None,
            "left_base": None,
            "right_base": None,
            "area": None,
            "peak_source_row_start": None,
            "peak_source_row_end": None,
        }
        if len(in_win) == 1:
            pk = in_win[0]
            lo, hi = sorted((pk["left_base"], pk["right_base"]))
            area = integrate_local_trapezoid(
                locs, vals, pk["left_base"], pk["right_base"]
            )
            pk_rows = [r for x, r in zip(locs, src_rows) if lo <= x <= hi]
            item.update(
                {
                    "peak_location": pk["location"],
                    "left_base": pk["left_base"],
                    "right_base": pk["right_base"],
                    "area": area,
                    "peak_source_row_start": min(pk_rows),
                    "peak_source_row_end": max(pk_rows),
                }
            )
        detected.append(item)

    stds = [
        d
        for d in detected
        if d["injection_type"] == "standard" and d["peak_status"] == "unique"
    ]
    std_in_vial = [d["nominal_conc"] / d["dilution_factor"] for d in stds]
    intercept, slope, r_squared = ols_fit(
        std_in_vial,
        [d["area"] for d in stds],
    )
    lloq = min(c for c in std_in_vial if c > 0)
    uloq = max(std_in_vial)
    blank_max_area = (blank_max_pct_lloq / 100.0) * abs(intercept + slope * lloq)

    rows = []
    for d in detected:
        inj_type = d["injection_type"]
        p_stat = d["peak_status"]
        dil = d["dilution_factor"]
        nom = d["nominal_conc"]
        est = None
        rep = None
        acc = None
        dev = None

        if inj_type == "blank":
            if p_stat == "missing" or (
                p_stat == "unique" and d["area"] <= blank_max_area
            ):
                outcome, qc_status = "blank_ok", "pass"
            else:
                outcome, qc_status = "blank_interference", "fail"
        elif inj_type == "standard":
            if p_stat != "unique":
                outcome = "missing_peak" if p_stat == "missing" else "interfered_peak"
                qc_status = "fail"
            else:
                est = (d["area"] - intercept) / slope
                rep = est * dil
                acc = 100.0 * rep / nom
                dev = 100.0 * (rep - nom) / nom
                outcome = "calibrated"
                qc_status = "pass" if abs(dev) <= qc_acceptance_pct else "fail"
        elif inj_type == "qc":
            if p_stat != "unique":
                outcome = "missing_peak" if p_stat == "missing" else "interfered_peak"
                qc_status = "fail"
            else:
                est = (d["area"] - intercept) / slope
                rep = est * dil
                acc = 100.0 * rep / nom
                dev = 100.0 * (rep - nom) / nom
                if abs(dev) <= qc_acceptance_pct:
                    outcome, qc_status = "qc_pass", "pass"
                else:
                    outcome, qc_status = "qc_fail", "fail"
        else:
            if p_stat == "missing":
                outcome, qc_status = "missing_peak", "not_quantified"
            elif p_stat == "interfered":
                outcome, qc_status = "interfered_peak", "not_quantified"
            else:
                est = (d["area"] - intercept) / slope
                if est < lloq:
                    outcome, qc_status = "below_lloq", "out_of_range"
                elif est > uloq:
                    outcome, qc_status = "above_uloq", "out_of_range"
                else:
                    rep = est * dil
                    outcome, qc_status = "ok", "reported"

        rows.append(
            {
                **d,
                "estimated_conc": est,
                "reported_conc": rep,
                "accuracy_pct": acc,
                "deviation_pct": dev,
                "outcome": outcome,
                "qc_status": qc_status,
            }
        )

    payload = {
        "source_file": data_path.name,
        "source_sha256": sha256,
        "calibration": {
            "intercept": intercept,
            "slope": slope,
            "r_squared": r_squared,
            "lloq": lloq,
            "uloq": uloq,
            "blank_max_area": blank_max_area,
        },
        "rows": rows,
    }
    json.dump(payload, sys.stdout, indent=2)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
