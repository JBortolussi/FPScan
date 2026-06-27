import os
import tempfile
import shutil

from .sample import Sample
from ..report import Report
from .fpchecker import FPChecker

def generate_report(
    prgm_dir: str,
    prgm_name_list: list[str],
    extension: str,
    sample: Sample,
    report_dir: str,
    N_point: int,
    keep_working_dir: bool =False,
    force_rebuild: bool=False,
    log: bool = False
) -> dict[str, Report] :
    prgm_dir = os.path.abspath(prgm_dir)
    report_dir = os.path.abspath(report_dir)

    if not os.path.exists(report_dir):
        os.mkdir(report_dir)

    root_dir = tempfile.mkdtemp()

    N = len(prgm_name_list)
    n = 1
    fpc_report_dict: dict[str, Report] = {}
    for prgm_name in prgm_name_list:
        prgm_path = os.path.join(prgm_dir, prgm_name + extension)
        report_path = os.path.join(report_dir, prgm_name + "_report.yaml")
        if os.path.exists(report_path) and not force_rebuild:
            if log:
                print(f"({n}/{N}) {prgm_name}: use backup")
            fpc_report_dict[prgm_name] = Report.load_yaml(prgm_name, report_path)
        else:
            if log:
                print(f"({n}/{N}) {prgm_name}: rebuild")
            tmp_dir = os.path.join(root_dir, prgm_name)
            os.mkdir(tmp_dir)
            fpc = FPChecker(prgm_name, prgm_path, tmp_dir)
            fpc.explore(sample, N_point)
            r = fpc.mk_report()
            r.save_yaml(report_path)
            fpc_report_dict[prgm_name] = r
        n += 1
        


    if keep_working_dir:
        print(root_dir)
    else:
        shutil.rmtree(root_dir)
    return fpc_report_dict