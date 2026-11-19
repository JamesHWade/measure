"""Check a built site, or the same files after Pages publishes them.

Usage: python3 tools/pkgdown/check_favicons.py SITE ARTICLE
SITE is a directory or an https:// URL. ARTICLE is relative to SITE.
"""
import json
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urljoin, urlsplit
from urllib.request import urlopen
from xml.etree import ElementTree


class HeadLinks(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        rel = set(attrs.get("rel", "").lower().split())
        if tag == "link" and rel & {"icon", "apple-touch-icon", "manifest"}:
            self.links.append((attrs["href"], "manifest" in rel))


def check(site, article):
    root = (site if site.startswith("https://") else Path(site).resolve().as_uri()).rstrip("/") + "/"
    cache = {}

    def read(base, href):
        url = urljoin(base, href)
        if not url.startswith(root):
            raise ValueError(f"{base}: {href} is outside {root}")
        if url not in cache:
            with urlopen(url, timeout=20) as response:
                cache[url] = response.read()
        return url, cache[url]

    def icon(base, href):
        url, content = read(base, href)
        suffix = Path(urlsplit(url).path).suffix.lower()
        if suffix == ".svg":
            ok = ElementTree.fromstring(content).tag.rsplit("}", 1)[-1] == "svg"
        else:
            signature = {".ico": b"\0\0\1\0", ".png": b"\x89PNG\r\n\x1a\n"}.get(suffix)
            ok = signature and content.startswith(signature)
        if not ok:
            raise ValueError(f"Expected an icon at {url}")

    for page in ("index.html", article):
        url, content = read(root, page)
        links = HeadLinks()
        links.feed(content.decode("utf-8"))
        if {is_manifest for _, is_manifest in links.links} != {True, False}:
            raise ValueError(f"Expected an icon and manifest on {url}")
        for href, is_manifest in links.links:
            if not is_manifest:
                icon(url, href)
                continue
            manifest_url, body = read(url, href)
            manifest = json.loads(body)
            if not manifest.get("icons"):
                raise ValueError(f"No icons in {manifest_url}")
            for entry in manifest["icons"]:
                icon(manifest_url, entry["src"])
    print(f"Verified homepage, {article}, and {len(cache) - 2} linked assets.")


if __name__ == "__main__":
    check(*sys.argv[1:])
