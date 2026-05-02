.PHONY: all install lint test


# ------------------------------------------------------------------------------
# ----------------------------- Standard Commands ------------------------------
# ------------------------------------------------------------------------------
all: install lint test

install:
	poetry install

lint:
	poetry run isort *.py ./**/*.py
	poetry run autopep8 --in-place --recursive .
	poetry run pyright
	poetry run flake8 --exclude=.venv,*/.venv/* .

test:
	poetry run pytest tests/ -v
