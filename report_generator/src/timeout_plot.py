import os
import subprocess

import pandas as pd
import matplotlib.pyplot as plt

from .report_generator import get_report

def make_plot(
    timeout: int,
    N_point: int,
    save: bool = False,
    rebuild_fpscan: bool = False,
    rebuild_bitblasting: bool = False,
    rebuild_fpchecker: bool = False,
    log: bool = False
):
    fpcore_report_dicts = get_report(
        prgm_dir="fpcore_prgm",
        extension=".fpcore",
        report_root_dir="report",
        timeout=timeout,
        N_point=N_point,
        rebuild_fpscan=rebuild_fpscan,
        rebuild_bitblasting=rebuild_bitblasting,
        rebuild_fpchecker=rebuild_fpchecker,
        log=log
    )

    save_name = f"timout_bar_plot{N_point}"

    fpscan_report_dict = fpcore_report_dicts["fpscan"]
    fpchecker_report_dict = fpcore_report_dicts["fpc"]
    bitblast_report_dict = fpcore_report_dicts["bitblast"]

    prgm_name_list = []
    prgm_name_pitfall_count_fpc = {}
    prgm_name_pitfall_count_fps = {}
    for name, fpscan_r in fpscan_report_dict.items():
        if name not in bitblast_report_dict or bitblast_report_dict[name].time is None:
            # timeout
            prgm_name_pitfall_count_fpc[name] = len(fpchecker_report_dict[name].get_absorption()) + len(fpchecker_report_dict[name].get_cancellation())
            prgm_name_pitfall_count_fps[name] = len(fpscan_r.get_absorption()) + len(fpscan_r.get_cancellation())
            prgm_name_list.append(name)

    df = pd.DataFrame(
        {
            'Programs': prgm_name_list,
            'FPScan': [prgm_name_pitfall_count_fps[name] for name in prgm_name_list],
            'FPChecker': [prgm_name_pitfall_count_fpc[name] for name in prgm_name_list]
        }
    )
    fig, ax_bar_side = plt.subplots(figsize=(12, 4.5))
    ax_bar_side.set_yscale('log')
    ax_bar_side.set_axisbelow(True)
    ax_bar_side = df.plot(x="Programs", y=["FPScan", "FPChecker"], kind="bar", ax=ax_bar_side)

    ax_bar_side.set_ylabel("Number of Pitfalls")

    ax_bar_side.set_xticklabels(df["Programs"], rotation=45, ha='right', rotation_mode='anchor')

    if save:
        plt.savefig(save_name + ".svg")
        subprocess.run([
            "inkscape",
            f"{save_name}.svg",
            "-o", f"{save_name}.pdf",
            "--export-type", "pdf",
            "--export-area-drawing",
            "--vacuum-defs",
        ])
    else:
        plt.show()