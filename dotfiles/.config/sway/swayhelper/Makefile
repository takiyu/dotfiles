.PHONY: all install lint test daemon setup helper

PYTHON = .venv/bin/python


# ------------------------------------------------------------------------------
# ----------------------------- Standard Commands ------------------------------
# ------------------------------------------------------------------------------
all: install lint test

install:
	poetry install

lint:
	poetry run isort .
	poetry run autopep8 --in-place --recursive .
	poetry run pyright
	poetry run flake8 --exclude=.venv,*/.venv/* .

test:
	poetry run pytest tests/ -v


# ------------------------------------------------------------------------------
# --------------------------------- Sway Run -----------------------------------
# ------------------------------------------------------------------------------
# Kill old daemon and start new one (blocking; start_daemon.sh execs python)
daemon:
	./start_daemon.sh

# One-shot workspace/display setup
setup:
	$(PYTHON) -m swayhelper.helper setup

# Helper action: ACTION=<action> [OPT=<n>]
helper:
	$(PYTHON) -m swayhelper.helper $(ACTION)$(if $(OPT), --opt $(OPT))


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
