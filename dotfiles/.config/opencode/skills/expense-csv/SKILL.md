---
name: expense-csv
description: Create an expense-reimbursement CSV from receipt images and PDFs in a directory. Trigger when the user asks to summarize receipts, create a keihi (経費) CSV, or process ryo-shu-sho (領収書).
allowed-tools: bash, glob, read, edit, write
---

# Expense CSV Creation from Receipts

Create a CSV for expense reimbursement by extracting date, amount, merchant,
and purpose from each receipt file in the target directory.

## Workflow

1. List receipt files (images and PDFs) in the directory.
2. For each file, extract text:
   - PDFs: use `pdftotext -layout`.
   - Images: use `pytesseract` with `lang='jpn'`.
3. Parse amounts, dates, merchant names, and item descriptions.
4. Build one CSV row per transaction. Columns:
   - 日付 (ISO date)
   - 分類 (expense category)
   - 会社名 (issuer/operating company)
   - 内容 (route, item, or description)
   - 金額（円） (integer)
   - 備考 (the renamed filename, `./領収書_<name>`)
5. Sort rows by date and append a total row.
6. Save as `経費申請.csv` with UTF-8 BOM encoding.
7. Rename each input file in place (same directory, no subfolder) with the
   prefix `領収書_` and the name
   `領収書_<日付>_<金額>_<分類>_<内容>.*`, where:
   - 日付 is the ISO date without dashes (YYYYMMDD, e.g. 20260903)
   - 金額 is the integer amount in yen followed by 円 (e.g. 29652円)
   - 分類 and 内容 are the CSV's 分類 and 内容 values
   - The extension is preserved.
   When one input file maps to multiple CSV rows (e.g. combined transit
   history), treat it like any other file: make a single row whose 日付 is
   the period covering all rows, written without dashes
   (`YYYYMMDD～YYYYMMDD`), 分類 and 内容 roughly summarized (e.g. 交通費 /
   乗車履歴（往復）), and 金額 as the total. Rename it in place using that
   single `領収書_`-prefixed name.

## Conventions

- 分類 (expense category) must be one of: 交通費, 宿泊費, 会議費, 消耗品費,
  印刷代. Airfare and transit both map to 交通費; meals map to 会議費;
  office consumables (tape, wet wipes, etc.) map to 消耗品費; convenience-store
  printing maps to 印刷代.
- Use the actual company name: 東海旅客鉄道株式会社, 東日本旅客鉄道株式会社,
  東京地下鉄株式会社, 小田急電鉄株式会社, 伊豆箱根鉄道株式会社, etc.
- For rail receipts, put only the route in 内容; omit train numbers and
  phrases like 利用履歴.
- For convenience-store print receipts, use the chain's operating company
  (e.g., セブン-イレブン・ジャパン株式会社, 株式会社ファミリーマート) and drop
  the branch name.
- Split combined transit history images into one row per chargeable segment;
  ignore top-up (チャージ) entries.
