import os
import subprocess

def run_herbie(
    prgm_path: str,
    output_path: str,
    force: bool = False,
    log: bool = False,
):
    if log:
        print(f"\t{prgm_path}")
    if not force and os.path.exists(output_path):
        return
    
    cmd = [
        "racket",
        "-l",
        "herbie",
        "improve",
        prgm_path,
        output_path
    ]
    r = subprocess.run(cmd, capture_output=True)

    if log:
        print(r.stderr.decode("utf-8"))

    if r.returncode != 0:
        if not log:
            print(r.stderr.decode("utf-8"))
        print(f"Herbie failed for {prgm_path}")
        return

    with open(output_path, "r") as fd:
        prgm = fd.readlines()
    prgm = prgm[2:]
    with open(output_path, "w") as fd:
        fd.write("\n".join(prgm))
