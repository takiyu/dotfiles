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
   - 備考 (source filename)
5. Sort rows by date and append a total row.
6. Save as `経費申請.csv` with UTF-8 BOM encoding.

## Conventions

- Use the actual company name: 東海旅客鉄道株式会社, 東日本旅客鉄道株式会社,
  東京地下鉄株式会社, 小田急電鉄株式会社, 伊豆箱根鉄道株式会社, etc.
- For rail receipts, put only the route in 内容; omit train numbers and
  phrases like 利用履歴.
- For convenience-store print receipts, use the chain's operating company
  (e.g., セブン-イレブン・ジャパン株式会社, 株式会社ファミリーマート) and drop
  the branch name.
- Split combined transit history images into one row per chargeable segment;
  ignore top-up (チャージ) entries.
