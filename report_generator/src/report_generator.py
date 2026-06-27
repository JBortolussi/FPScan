import os

from .fpchecker.report_generator import generate_report as generate_fpc_report
from .fpchecker.sample import Sample
from .fpcore_prgm import get_fpcore_prgm
from .report import Report
from .tiny import Tiny
from .tiny.report_generator import generate_report as generate_tiny_report



def get_report(
    prgm_dir: str,
    extension: str,
    report_root_dir: str,
    timeout: int,
    N_point: int,
    prgm_name_list: None | list[str] = None,
    prgm_name_no_trigo_list: None | list[str] = None,
    rebuild_fpchecker: bool = False,
    rebuild_fpscan: bool = False,
    rebuild_bitblasting: bool = False,
    log: bool = False
) -> dict[str, dict[str, Report]]:
    prgm_dir = os.path.abspath(prgm_dir)
    report_root_dir = os.path.abspath(report_root_dir)

    if prgm_name_list is None or prgm_name_no_trigo_list is None:
        prgm_name_set, trigo_prgm_name_set = get_fpcore_prgm()
        prgm_name_set = set(map(lambda name: name.replace(extension, ""), prgm_name_set))
        trigo_prgm_name_set = set(map(lambda name: name.replace(extension, ""), trigo_prgm_name_set))

        if prgm_name_list is None:
            prgm_name_list = list(prgm_name_set)
        if prgm_name_no_trigo_list is None:
            prgm_name_no_trigo_list = list(prgm_name_set - trigo_prgm_name_set)

    run_tiny = Tiny("./fpscan.exe", timeout=timeout)

    # generate bitblast report
    print("Generating bitblast report")
    run_tiny.set_opt(["-bitblasting"])
    bitblast_report_dict = generate_tiny_report(
        prgm_dir,
        prgm_name_no_trigo_list,
        extension,
        run_tiny,
        os.path.join(report_root_dir, "report_bitblast"),
        log=log,
        force_rebuild=rebuild_bitblasting,
    )

    # generate standard report
    print("Generating FPScan report")
    run_tiny.set_opt([])
    fpscan_report_dict = generate_tiny_report(
        prgm_dir,
        prgm_name_list,
        extension,
        run_tiny,
        os.path.join(report_root_dir, "report"),
        log=log,
        force_rebuild=rebuild_fpscan,
    )

    if (N_point != 0):
        # generate fpc report
        print("Generating FPChecker report")
        fpc_report_dict = generate_fpc_report(
            prgm_dir,
            prgm_name_list,
            extension,
            Sample(),
            os.path.join(report_root_dir, f"report_fpc_{N_point}"),
            N_point,
            keep_working_dir=False,
            force_rebuild=rebuild_fpchecker,
            log=log
        )
    else:
        print("FPChecker Report Generation Skiped")
        fpc_report_dict = {}

    return {
        "bitblast": bitblast_report_dict,
        "fpc": fpc_report_dict,
        "fpscan": fpscan_report_dict
    }