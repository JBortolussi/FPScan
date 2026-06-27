import os

from . import Tiny
from ..report import Report

def generate_report(
    prgm_dir: str,
    prgm_name_list: list[str],
    extension: str,
    run_tiny: Tiny,
    report_dir: str,
    log=False,
    force_rebuild=False
) -> dict[str, Report]:
    
    N = len(prgm_name_list)
    n = 1
    report_dict = {}
    for prgm_name in prgm_name_list:
        if log:
            print(f"({n}/{N}) {prgm_name}")
        prgm_path = os.path.join(prgm_dir, prgm_name + extension)
        r = Report.build(
            prgm_name,
            prgm_path,
            report_dir,
            run_tiny=run_tiny,
            force_rebuild=force_rebuild
        )
        if r.time is None and log:
            print(f"\t --timeout")
        report_dict[prgm_name] = r
        n += 1
    return report_dict