from swayhelper.constants import DISPLAY_CONFIGS


# -----------------------------------------------------------------------------
# ------------------------------- Implementation ------------------------------
# -----------------------------------------------------------------------------
def generate() -> str:
    lines = ['# Auto-generated from constants.py: do not edit by hand.']
    for name, res, pos, transform, scale in DISPLAY_CONFIGS:
        parts = [f'output {name}', f'resolution {res}', f'position {pos}']
        if transform:
            parts.append(f'transform {transform}')
        if scale:
            parts.append(f'scale {scale}')
        lines.append(' '.join(parts))
    return '\n'.join(lines)


# -----------------------------------------------------------------------------
# ----------------------------- Entry Point -----------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    print(generate())


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
