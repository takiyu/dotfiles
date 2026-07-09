#!/usr/bin/env python3
"""Generate TOTP codes from a secret key using only the standard library."""

import base64
import hashlib
import hmac
import struct
import sys
import time
from typing import Optional


# -----------------------------------------------------------------------------
# ------------------------------- Constants -----------------------------------
# -----------------------------------------------------------------------------
TOTP_TIME_STEP: int = 30  # Default time step in seconds (RFC 6238).
TOTP_CODE_DIGITS: int = 6  # Default number of digits in the TOTP code.


# -----------------------------------------------------------------------------
# ------------------------------- Section: Helpers ----------------------------
# -----------------------------------------------------------------------------
def decode_base32_secret(secret_key: str) -> bytes:
    """Decode a Base32-encoded secret key to raw bytes.

    Args:
        secret_key: Base32-encoded secret string (whitespace is ignored).

    Returns:
        Raw bytes of the decoded secret.

    Raises:
        ValueError: If the secret key is not valid Base32.
    """
    cleaned_key: str = secret_key.replace(' ', '').upper()
    padding_needed: int = (8 - len(cleaned_key) % 8) % 8
    padded_key: str = cleaned_key + ('=' * padding_needed)
    try:
        return base64.b32decode(padded_key)
    except Exception as exc:
        raise ValueError('Invalid Base32 secret key') from exc


def generate_hotp_counter(secret_bytes: bytes, counter: int) -> int:
    """Generate an HOTP value for the given counter (RFC 4226).

    Args:
        secret_bytes: Raw shared secret bytes.
        counter: Moving factor counter value.

    Returns:
        Integer HOTP value.
    """
    counter_bytes: bytes = struct.pack('>Q', counter)
    hash_value: bytes = hmac.new(
        secret_bytes, counter_bytes, hashlib.sha1
    ).digest()

    # Dynamic truncation (RFC 4226 section 5.3).
    offset: int = hash_value[-1] & 0x0F
    truncated: int = struct.unpack('>I', hash_value[offset:offset + 4])[0]
    truncated &= 0x7FFFFFFF

    return truncated


def generate_totp_code(secret_key: str, current_time: Optional[float] = None,
                       time_step: int = TOTP_TIME_STEP,
                       code_digits: int = TOTP_CODE_DIGITS) -> str:
    """Generate a TOTP code from a secret key (RFC 6238).

    Args:
        secret_key: Base32-encoded shared secret.
        current_time: Unix timestamp; defaults to the current system time.
        time_step: Time step window in seconds.
        code_digits: Number of digits in the returned code.

    Returns:
        Zero-padded TOTP code string.
    """
    secret_bytes: bytes = decode_base32_secret(secret_key)
    if current_time is None:
        current_time = time.time()

    counter: int = int(current_time // time_step)
    hotp_value: int = generate_hotp_counter(secret_bytes, counter)
    code: int = hotp_value % (10 ** code_digits)

    return str(code).zfill(code_digits)


# -----------------------------------------------------------------------------
# ------------------------------- Section: CLI --------------------------------
# -----------------------------------------------------------------------------
def print_usage_and_exit() -> None:
    """Print usage instructions and exit with an error code."""
    print('Usage: python gen_totp.py <BASE32_SECRET_KEY>')
    sys.exit(1)


def main(argv: list[str]) -> None:
    """Entry point for the TOTP generator CLI."""
    if len(argv) != 2:
        print_usage_and_exit()

    secret_key: str = argv[1]
    code: str = generate_totp_code(secret_key)
    print(code)


if __name__ == '__main__':
    main(sys.argv)
