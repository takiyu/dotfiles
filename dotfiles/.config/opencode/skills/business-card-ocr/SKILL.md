---
name: business-card-ocr
description: Read business cards (名刺) from images in a directory and extract name, organization, role, and email into CSV/JSON plus renamed image copies. Trigger when the user asks to OCR business cards, extract 名刺 data, or convert card images into a contact list.
---

# Business Card OCR

Extract structured contact data from business card (名刺) images in a
directory. OCR uses multiple transcription engines (Japanese + English only)
and merges their outputs into a per-line vote count so the parser can validate
each line against independent sources. The agent only parses the OCR text into
fields; file output and image copying are deterministic scripts.

## Workflow

Run the two helper scripts from the **project directory** (the directory
containing `./data_inp`). Reference the scripts by their absolute path under
`~/.config/opencode/skills/business-card-ocr/scripts/`, or `cd` there and pass
absolute `<input_dir>`/`<output_dir>` paths. Do NOT pass bare `./data_inp`
after `cd`-ing into the scripts dir — that would resolve against the scripts
dir, not the project.

1. OCR all images with multi-engine consensus + validation:

   ```bash
   uv --project ~/.config/opencode/skills/business-card-ocr/scripts run \
     --python 3.11 python \
     ~/.config/opencode/skills/business-card-ocr/scripts/ocr.py \
     ./data_inp ./data_out
   ```

   `<input_dir>` defaults to `./data_inp` (fall back to `./raw_imgs` if the
   former is absent) and intermediate files are written under `./data_out`
   (created automatically). Always pass explicit paths and confirm the two
   directories exist relative to the project root.

   Engines (auto-detected, skipped if unavailable):
   - **RapidOCR** (built-in, PP-OCRv6) — passes: `japan-small`,
     `japan-medium`, `en-small`.
   - **Tesseract** (system binary) — passes: `jpn` (`jpn+eng`) and `eng`.
   Languages are restricted to **Japanese + English** (no `ch`, `latin`, etc.).

   Produces:
   - `./data_out/ocr_raw.json` — merged result keyed by image basename, each
     value `{"lines": [...], "votes": {text: N}}`. `votes` is the number of
     passes that agreed on a line; use it for validation.
   - `./data_out/ocr_passes.json` — per-pass raw lines keyed by pass name.

2. **Validate** before parsing: a line is trustworthy only if its vote count
   is high (majority of passes). Treat singletons (votes == 1) as noise or
   low-confidence; check them against the romanized name / email when they
   fall in the name position.

3. Parse each image's lines into four fields, in English keys:
   - `name` (名前) — 姓 + 名 (kanji name line).
   - `organization` (所属組織) — company / hospital name, plus the
     department / center (部署・センター名) where given.
   - `role` (役職) — the job title (役職名), e.g. 部長, 課長, 副院長,
     センター長. Include the title line even if long; leave `""` only when no
     title is printed.
   - `email` (メールアドレス) — the `...@...` address.

   Write the parsed records to `./data_out/people.json`, a JSON array where
   each object has `name`, `organization`, `role`, `email`, plus the source
   file name under `"image"` (e.g. `"IMG_20260907_125958.jpg"`).

4. Generate CSV/JSON, a visual review page, and rename/copy the images:

   ```bash
   uv --project ~/.config/opencode/skills/business-card-ocr/scripts run \
     --python 3.11 python \
     ~/.config/opencode/skills/business-card-ocr/scripts/finalize.py \
     ./data_inp ./data_out ./data_out/people.json
   ```

   This writes, under `./data_out` (created if missing):
   - `名刺一覧.csv` — tabular output (header: `name,organization,role,email`).
   - `output.json` — machine-readable JSON (UTF-8, ensure_ascii=false).
   - `確認用.html` — review table with embedded thumbnails to visually compare
     the extracted fields against each card image.
   - `画像/` — renamed image copies as `<組織名>_<名前>.jpg` (spaces removed).

## Conventions

- Never rewrite a name or organization. Example: a card printed
  「平成総合検診センター」 must stay as-is, never shortened to 「平成病院」.
- Preserve exact kanji/katakana; copy text faithfully.
- OCR may mis-read characters; normalize obvious noise but never invent words.
- If a field is absent, emit an empty string `""`.
- Keep the input images in place; output copies go to the output directory.
- `role` must hold a real job title (役職名). When the card prints a title
  (部長, 課長, 院長, センター長, 取締役, …), capture it — the field may be a
  compound title such as 取締役 統括営業部長.

## Multi-engine validation

Single-engine OCR is unstable, so `ocr_raw.json` carries a **vote count** per
line (how many of the independent passes agreed). Before committing a field:

- Prefer lines with a **majority vote** (>= 2, ideally >= 3 of 5 passes).
- Treat `votes == 1` lines as noise or low confidence. When such a line sits
  in the name position, reconcile it with the romanized name line and the
  email local-part (see "Name reconstruction" below).
- Tesseract `jpn` output is noisy for stylized cards (logos, rotated text);
  its lines commonly get low votes — do not trust a Tesseract-only reading.
- If engines disagree on a character, prefer the RapidOCR `japan` reading,
  then cross-check with the romanized name / email.

## Name reconstruction (critical)

Japanese names are often printed vertically (one kanji per OCR line), so the
OCR `txts` list can split a single name into several one-character lines, and
the reading order may not match the line order. When joining name lines, do
NOT simply concatenate in list order — verify against these signals before
finalizing a name:

1. **Romanized name line** (`NAME TARO` all-caps or `Taro Name`) gives the
   canonical order. Match its order to the kanji fragments.
2. **Email local-part** often preserves the reading: `taro.name@...`,
   `h_hamada@...`, `watanabe@...`. If the email leaks the given/family name,
   use it to reorder or decode the kanji fragments.
3. **Common surname knowledge**: known family names (佐藤, 鈴木, 高橋, 田中,
   渡辺, 上田, …) take priority; prefer a fragment order that forms a real
   surname + given name over the raw detection order.

Examples of the pattern this prevents (all fictional):
- OCR lines `田 / 中 / 太郎` + email `taro.tanaka` → `田中太郎` (not
  `田太郎中`).
- OCR lines `高 / 橋 / 花子` + email `hanako.takahashi` → `高橋花子` (not
  `高花子橋`).
- OCR lines `佐 / 藤` misread, but email `sato@…` resolves the surname →
  decode via email rather than trusting the misread fragment.

When a straight concatenation looks unnatural (missing/rare surname), always
cross-check the romanized name and email before committing the field.

## Directory defaults

- Default input: `./data_inp` (fall back to `./raw_imgs` if present).
- Default output: `./data_out`.

Both are relative to the **project directory** where the skill is invoked
(the directory that holds the input images). The outputs for `./data_inp` MUST
be written to `./data_out` at the same level, and `finalize.py` creates the
directory if it does not exist. If no explicit paths are given, always use
these defaults. The finished CSV/HTML/images must land under `./data_out`;
do not leave them in a temporary `work_dir`.
