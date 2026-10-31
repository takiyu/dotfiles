"""Batch business-card OCR with multi-engine consensus + validation.

A single OCR engine/pass is unstable (ordering, kanji splitting, mis-reads).
This script runs several independent transcription sources over each image and
merges them into a per-line vote count so the parser can trust lines seen by
multiple sources and treat singletons as low confidence.

Engines (all optional; auto-detected, skipped if unavailable):
  - RapidOCR (built-in, PP-OCRv6): japan-small / japan-medium / en-small
  - Tesseract (system binary): jpn / eng
Each (engine, language, model) tuple is one "pass". Validation requires at
least two engines and three passes; a warning is printed otherwise.

Output layout in the work dir:
  - ocr_raw.json    merged: {image: {"lines": [...], "votes": {text: n}}}
  - ocr_passes.json per-pass raw lines, keyed by pass name, for reference

Usage:
    python ocr.py <input_dir> <output_dir>
"""

import json
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Callable, Optional

from rapidocr import LangRec, RapidOCR
from rapidocr.utils.typings import ModelType

IMAGE_EXTENSIONS = ('.jpg', '.jpeg', '.png', '.webp', '.tiff', '.bmp')

MIN_ENGINES = 2
MIN_PASSES = 3


# -----------------------------------------------------------------------------
# ------------------------------- Engine Specs --------------------------------
# -----------------------------------------------------------------------------
# Each pass is (name, ocr_function). The ocr_function maps an image path to a
# list of text lines and is supplied by _build_passes().
PassRunner = Callable[[Path], list[str]]


# -----------------------------------------------------------------------------
# -------------------------------- Main Flow ----------------------------------
# -----------------------------------------------------------------------------
def main() -> None:
    if len(sys.argv) != 3:
        print(f'Usage: {sys.argv[0]} <input_dir> <output_dir>', file=sys.stderr)
        raise SystemExit(2)

    inp_dir = Path(sys.argv[1])
    out_dir = Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)

    img_filenames = sorted(
        f for f in inp_dir.iterdir()
        if f.is_file() and f.suffix.lower() in IMAGE_EXTENSIONS
    )

    passes = _build_passes()
    if passes is None:
        raise SystemExit(1)
    _validate_passes(passes)

    merged: dict[str, dict] = dict()
    per_pass_log: dict[str, dict] = dict()
    for img in img_filenames:
        results = {name: runner(img) for name, runner in passes}
        per_pass_log[img.name] = results
        merged[img.name] = _merge_passes(list(results.values()))
        max_votes = max(merged[img.name]['votes'].values(), default=0)
        print(f'OCR {img.name}: {len(merged[img.name]["lines"])} lines '
              f'(max votes {max_votes}/{len(passes)})', file=sys.stderr)

    _write_json(out_dir / 'ocr_raw.json', merged)
    _write_json(out_dir / 'ocr_passes.json', per_pass_log)
    print(out_dir / 'ocr_raw.json')


# -----------------------------------------------------------------------------
# ------------------------------- Pass Building -------------------------------
# -----------------------------------------------------------------------------
def _build_passes() -> Optional[list[tuple[str, PassRunner]]]:
    passes: list[tuple[str, PassRunner]] = list()

    rapidocr_passes = _build_rapidocr_passes()
    if rapidocr_passes is not None:
        passes.extend(rapidocr_passes)
    else:
        print('RapidOCR unavailable', file=sys.stderr)

    tesseract_passes = _build_tesseract_passes()
    if tesseract_passes is not None:
        passes.extend(tesseract_passes)
    else:
        print('Tesseract unavailable (skipped)', file=sys.stderr)

    if not passes:
        print('No OCR engines available. Aborting.', file=sys.stderr)
        return None
    print(f'Engines ready: {len(passes)} passes -> '
          f'{", ".join(n for n, _ in passes)}', file=sys.stderr)
    return passes


def _build_rapidocr_passes() -> Optional[list[tuple[str, PassRunner]]]:
    try:
        engines = [
            ('rapidocr_japan_small',
             RapidOCR(params={'Rec.lang_type': LangRec.JAPAN,
                              'Rec.model_type': ModelType.SMALL})),
            ('rapidocr_japan_medium',
             RapidOCR(params={'Rec.lang_type': LangRec.JAPAN,
                              'Rec.model_type': ModelType.MEDIUM})),
            ('rapidocr_en_small',
             RapidOCR(params={'Rec.lang_type': LangRec.EN,
                              'Rec.model_type': ModelType.SMALL})),
        ]
    except Exception as e:
        print(f'RapidOCR init failed: {e}', file=sys.stderr)
        return None
    return [(name, _make_rapidocr_runner(engine))
            for name, engine in engines]


def _make_rapidocr_runner(engine: RapidOCR) -> PassRunner:
    def runner(img_path: Path) -> list[str]:
        try:
            res = engine(str(img_path))
        except Exception as e:
            print(f'RapidOCR failed {img_path.name}: {e}', file=sys.stderr)
            return list()
        if res is None or not hasattr(res, 'txts') or not res.txts:
            return list()
        return [str(t) for t in res.txts]
    return runner


def _build_tesseract_passes() -> Optional[list[tuple[str, PassRunner]]]:
    binary = shutil.which('tesseract')
    if binary is None:
        return None
    lang_pairs = [('jpn', 'jpn+eng'), ('eng', 'eng')]
    return [(f'tesseract_{name}', _make_tesseract_runner(binary, lang))
            for name, lang in lang_pairs]


def _make_tesseract_runner(binary: str, lang: str) -> PassRunner:
    def runner(img_path: Path) -> list[str]:
        try:
            proc = subprocess.run(
                [binary, str(img_path), 'stdout', '-l', lang],
                capture_output=True, text=True, timeout=120)
        except (OSError, subprocess.TimeoutExpired) as e:
            print(f'Tesseract failed {img_path.name}: {e}', file=sys.stderr)
            return list()
        return _tesseract_to_lines(proc.stdout)
    return runner


def _tesseract_to_lines(stdout: str) -> list[str]:
    lines: list[str] = list()
    for raw in stdout.splitlines():
        text = raw.strip()
        if text:
            lines.append(text)
    return lines


# -----------------------------------------------------------------------------
# ------------------------------ Pass Validation ------------------------------
# -----------------------------------------------------------------------------
def _validate_passes(passes: list[tuple[str, PassRunner]]) -> None:
    engine_count = len({name.split('_')[0] for name, _ in passes})
    if engine_count < MIN_ENGINES:
        print(f'WARNING: only {engine_count} engine(s) available; '
              f'{MIN_ENGINES}+ recommended for validation', file=sys.stderr)
    if len(passes) < MIN_PASSES:
        print(f'WARNING: only {len(passes)} pass(es); '
              f'{MIN_PASSES}+ recommended for majority vote', file=sys.stderr)


# -----------------------------------------------------------------------------
# ------------------------------ Consensus Merge ------------------------------
# -----------------------------------------------------------------------------
def _merge_passes(per_pass: list[list[str]]) -> dict:
    vote: dict[str, int] = dict()
    for lines in per_pass:
        for line in lines:
            text = line.strip()
            if text:
                vote[text] = vote.get(text, 0) + 1
    lines = _preserve_order(per_pass)
    return {'lines': lines, 'votes': vote}


def _preserve_order(per_pass: list[list[str]]) -> list[str]:
    # Union of lines ordered by first appearance (strongest pass first) so
    # vertically printed names keep their spatial reading order.
    seen: list[str] = list()
    for lines in per_pass:
        for line in lines:
            text = line.strip()
            if text and text not in seen:
                seen.append(text)
    return seen


# -----------------------------------------------------------------------------
# --------------------------------- Helpers -----------------------------------
# -----------------------------------------------------------------------------
def _write_json(path: Path, data: object) -> None:
    path.write_text(
        json.dumps(data, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')


# -----------------------------------------------------------------------------
# -------------------------------- Entry Point --------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    main()
