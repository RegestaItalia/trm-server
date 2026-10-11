#!/usr/bin/env python3
"""Refresh the TLS certificate chain embedded in ztrm_installer.prog.abap."""

from __future__ import annotations

import argparse
import re
import subprocess
from pathlib import Path


HOST = "trmregistry.com"
PORT = 443
BEGIN_MARKER = "* BEGIN AUTO-GENERATED REGISTRY CERTIFICATES"
END_MARKER = "* END AUTO-GENERATED REGISTRY CERTIFICATES"
PEM_PATTERN = re.compile(
    r"-----BEGIN CERTIFICATE-----\s*(.*?)\s*-----END CERTIFICATE-----",
    re.DOTALL,
)


def read_chain() -> list[str]:
    command = [
        "openssl",
        "s_client",
        "-showcerts",
        "-verify_return_error",
        "-verify_hostname",
        HOST,
        "-connect",
        f"{HOST}:{PORT}",
        "-servername",
        HOST,
    ]
    result = subprocess.run(
        command,
        input="",
        text=True,
        capture_output=True,
        check=True,
    )
    certificates = [re.sub(r"\s+", "", body) for body in PEM_PATTERN.findall(result.stdout)]
    if not certificates:
        raise RuntimeError(f"No certificates returned by {HOST}:{PORT}")
    return certificates


def render_abap(certificates: list[str]) -> str:
    output: list[str] = []
    for index, certificate in enumerate(certificates, start=1):
        chunks = [certificate[pos : pos + 64] for pos in range(0, len(certificate), 64)]
        output.append(f"* Certificate {index} of {len(certificates)} from {HOST}:{PORT}")
        output.append("    CONCATENATE")
        output.extend(f"      '{chunk}'" for chunk in chunks)
        output.append("      INTO encoded_certificate.")
        output.append("    APPEND encoded_certificate TO encoded_certificates.")
        output.append("")
    return "\n".join(output).rstrip()


def update_report(report: Path, generated: str) -> bool:
    source = report.read_text(encoding="utf-8")
    if source.count(BEGIN_MARKER) != 1 or source.count(END_MARKER) != 1:
        raise RuntimeError("Expected exactly one registry certificate marker pair")

    before, remainder = source.split(BEGIN_MARKER, maxsplit=1)
    _, after = remainder.split(END_MARKER, maxsplit=1)
    updated = (
        before
        + BEGIN_MARKER
        + "\n"
        + generated
        + "\n"
        + END_MARKER
        + after
    )
    if updated == source:
        return False
    report.write_text(updated, encoding="utf-8")
    return True


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--report",
        type=Path,
        default=Path("ztrm_installer.prog.abap"),
        help="Path to the standalone installer report",
    )
    args = parser.parse_args()

    certificates = read_chain()
    changed = update_report(args.report, render_abap(certificates))
    state = "updated" if changed else "already current"
    print(f"{args.report}: {state} ({len(certificates)} certificates)")


if __name__ == "__main__":
    main()
