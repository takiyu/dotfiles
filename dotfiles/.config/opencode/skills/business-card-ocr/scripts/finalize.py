"""Finalize parsed business-card data into CSV, a comparison HTML, and images.

Reads a JSON list of person records (name/organization/role/email plus the
source image basename) and writes under the output directory:
  - 名刺一覧.csv    (UTF-8 BOM, header: name,organization,role,email)
  - output.json     (UTF-8, ensure_ascii=false) for programmatic use
  - 確認用.html     (side-by-side review table with embedded thumbnails)
  - 画像/<組織名>_<名前>.jpg  (spaces removed) copied from the input dir

Usage:
    python finalize.py <input_dir> <output_dir> <people.json>
"""

import base64
import html as html_lib
import io
import json
import shutil
import sys
from pathlib import Path

from PIL import Image

CSV_HEADER = 'name,organization,role,email\n'
CSV_FILENAME = '名刺一覧.csv'
HTML_FILENAME = '確認用.html'
JSON_FILENAME = 'output.json'
IMAGES_DIR_NAME = '画像'


# -----------------------------------------------------------------------------
# -------------------------------- Main Flow ----------------------------------
# -----------------------------------------------------------------------------
def main() -> None:
    if len(sys.argv) != 4:
        print(f'Usage: {sys.argv[0]} <input_dir> <output_dir> <people.json>',
              file=sys.stderr)
        raise SystemExit(2)

    inp_dir = Path(sys.argv[1])
    out_dir = Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)

    people = json.loads(Path(sys.argv[3]).read_text(encoding='utf-8'))

    _write_json(people, out_dir)
    _write_csv(people, out_dir)
    _write_html(people, inp_dir, out_dir)
    _copy_images(people, inp_dir, out_dir)
    print(f'Done: {len(people)} records -> {out_dir}')


# -----------------------------------------------------------------------------
# -------------------------------- Output JSON --------------------------------
# -----------------------------------------------------------------------------
def _write_json(people: list[dict], out_dir: Path) -> None:
    output = [{
        'name': p.get('name', ''),
        'organization': p.get('organization', ''),
        'role': p.get('role', ''),
        'email': p.get('email', ''),
    } for p in people]
    (out_dir / JSON_FILENAME).write_text(
        json.dumps(output, ensure_ascii=False, indent=2) + '\n',
        encoding='utf-8')


# -----------------------------------------------------------------------------
# -------------------------------- Output CSV ---------------------------------
# -----------------------------------------------------------------------------
def _write_csv(people: list[dict], out_dir: Path) -> None:
    lines = [CSV_HEADER]
    for p in people:
        rows = [p.get('name', ''), p.get('organization', ''),
                p.get('role', ''), p.get('email', '')]
        lines.append(_csv_escape(rows))
    (out_dir / CSV_FILENAME).write_text(
        '\ufeff' + ''.join(lines), encoding='utf-8')


def _csv_escape(row: list[str]) -> str:
    cells = list()
    for cell in row:
        text = str(cell)
        if ',' in text or '"' in text or '\n' in text:
            text = '"' + text.replace('"', '""') + '"'
        cells.append(text)
    return ','.join(cells) + '\n'


# -----------------------------------------------------------------------------
# ------------------------------ Comparison HTML ------------------------------
# -----------------------------------------------------------------------------
def _write_html(people: list[dict], inp_dir: Path, out_dir: Path) -> None:
    rows: list[str] = list()
    for p in people:
        b64 = _thumbnail_b64(p, inp_dir)
        rows.append(_html_row(p, b64))
    document = _html_document(rows)
    (out_dir / HTML_FILENAME).write_text(document, encoding='utf-8')


def _thumbnail_b64(person: dict, inp_dir: Path) -> str:
    src_basename = person.get('image', '')
    src = inp_dir / src_basename
    if not src.is_file():
        return ''
    image = Image.open(src).convert('RGB')
    image.thumbnail((400, 620))
    buf = io.BytesIO()
    image.save(buf, 'JPEG', quality=75)
    return base64.b64encode(buf.getvalue()).decode()


def _html_row(person: dict, b64: str) -> str:
    name = html_lib.escape(str(person.get('name', '')))
    org = html_lib.escape(str(person.get('organization', '')))
    role = html_lib.escape(str(person.get('role', '')))
    email = html_lib.escape(str(person.get('email', '')))
    role_disp = role if role else '<span class="dim">—</span>'
    email_disp = email if email else '<span class="dim">—</span>'
    img_cell = (f'<img src="data:image/jpeg;base64,{b64}">'
                if b64 else '<span class="dim">画像なし</span>')
    return ('<tr>'
            f'<td>{img_cell}</td>'
            f'<td class="name">{name}</td>'
            f'<td class="org">{org}</td>'
            f'<td class="role">{role_disp}</td>'
            f'<td class="email">{email_disp}</td>'
            '</tr>')


def _html_document(rows: list[str]) -> str:
    return f'''<!doctype html>
<html lang="ja">
<head>
<meta charset="utf-8">
<title>名刺 OCR 確認</title>
<style>
body{{font-family:sans-serif;margin:20px;background:#f0f0f0}}
h1{{font-size:20px;text-align:center}}
table{{border-collapse:collapse;background:#fff;margin:0 auto}}
td,th{{border:2px solid #ccc;padding:12px;vertical-align:middle;
font-size:15px;text-align:center}}
th{{background:#e8e8e8;font-weight:bold}}
img{{width:300px;height:auto;display:block;margin:0 auto;border-radius:4px}}
.name{{font-weight:bold;font-size:17px}}
.org{{color:#444}}
.role{{color:#666}}
.email{{color:#225;font-size:14px}}
.dim{{color:#aaa}}
</style>
</head>
<body>
<h1>名刺 OCR 確認 ({len(rows)}枚)</h1>
<table>
<tr><th>画像</th><th>名前</th><th>組織</th><th>役職</th><th>Email</th></tr>
{chr(10).join(rows)}
</table>
</body>
</html>
'''


# -----------------------------------------------------------------------------
# ------------------------------- Copy Images ---------------------------------
# -----------------------------------------------------------------------------
def _copy_images(people: list[dict], inp_dir: Path, out_dir: Path) -> None:
    img_dir = out_dir / IMAGES_DIR_NAME
    img_dir.mkdir(parents=True, exist_ok=True)
    for p in people:
        src_basename = p.get('image', '')
        if not src_basename:
            continue
        src = inp_dir / src_basename
        if not src.is_file():
            print(f'Skip missing image: {src_basename}', file=sys.stderr)
            continue

        org = str(p.get('organization', '')).replace(' ', '')
        name = str(p.get('name', '')).replace(' ', '')
        stem = f'{org}_{name}'.strip('_')
        if not stem:
            stem = src.stem
        dst = img_dir / f'{stem}{src.suffix.lower()}'
        shutil.copyfile(src, dst)


# -----------------------------------------------------------------------------
# -------------------------------- Entry Point --------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    main()
