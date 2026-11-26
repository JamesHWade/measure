"""Reproduce the measured infrared band in the guide with 60-digit arithmetic."""

from decimal import Decimal, getcontext
from hashlib import sha256
from pathlib import Path

getcontext().prec = 60
SOURCE = Path(__file__).resolve().parents[2] / "inst/extdata/SBO.jdx"
DIGEST = "e74d245d54aa3698babaa648172a422f1a2be48c7e2470544626a6f884e7abbd"
TOLERANCE = Decimal("1e-12")


def read_spectrum():
    """Decode the plain-number layout of the pinned SBO spectrum."""
    raw = SOURCE.read_bytes()
    assert sha256(raw).hexdigest() == DIGEST
    header, data = raw.decode("ascii").split("##XYDATA=(X++(Y..Y))")
    fields = dict(
        line[2:].split("=", 1) for line in header.splitlines() if line.startswith("##")
    )
    assert fields["XUNITS"] == "1/CM" and fields["YUNITS"] == "TRANSMITTANCE"
    first, last = Decimal(fields["FIRSTX"]), Decimal(fields["LASTX"])
    count = int(fields["NPOINTS"])
    step = (last - first) / (count - 1)
    xfactor, yfactor = Decimal(fields["XFACTOR"]), Decimal(fields["YFACTOR"])
    points = []
    for row in data.split("##END=")[0].strip().splitlines():
        anchor, *ordinates = map(Decimal, row.split())
        assert abs(anchor * xfactor - (first + len(points) * step)) < abs(step) / 2
        for ordinate in ordinates:
            x = first + len(points) * step
            points.append((x, -(ordinate * yfactor).log10()))
    assert len(points) == count
    return points


def band(points, low, high):
    selected = sorted((x, a) for x, a in points if low <= x <= high)
    if len(selected) < 2:
        raise ValueError("Fewer than two observed coordinates in the window")
    (x0, a0), (x1, a1) = selected[0], selected[-1]
    raw = sum((xj - xi) * (ai + aj) / 2 for (xi, ai), (xj, aj) in zip(selected, selected[1:]))
    # Integrate the straight baseline analytically over the observed interval.
    net = raw - (x1 - x0) * (a0 + a1) / 2
    return selected, raw, net


if __name__ == "__main__":
    points = read_spectrum()
    selected, raw, net = band(points, 1710, 1775)
    assert len(selected) == 34
    assert abs(raw - Decimal("6.659902808248615")) < TOLERANCE
    assert abs(net - Decimal("5.651018156303961")) < TOLERANCE
    assert band(reversed(points), 1710, 1775) == (selected, raw, net)

    apex, height = max(selected, key=lambda point: point[1])
    for limits in [(0, 1), (apex - Decimal("0.1"), apex + Decimal("0.1"))]:
        try:
            band(points, *limits)
        except ValueError:
            continue
        raise AssertionError(f"Insufficient observations in {limits} yielded an area")

    print(f"{len(selected)} points between {selected[0][0]:.9f} and {selected[-1][0]:.9f} cm^-1")
    print(f"Observed apex at {apex:.9f} cm^-1, absorbance {height:.9f}")
    print(f"Area before baseline correction: {raw:.12f} absorbance cm^-1")
    print(f"Area after baseline correction:  {net:.12f} absorbance cm^-1")
