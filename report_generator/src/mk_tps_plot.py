import os
import subprocess

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from .fpcore_prgm import get_fpcore_prgm
from .tiny import Tiny

from .fpchecker.sample import Sample, RandomSample
from .fpchecker.report_generator import generate_report as generate_fpc_report
from .tiny.report_generator import generate_report as generate_tiny_report

def mk_tps_plot(
    prgm_dir: str,
    extension: str,
    report_root_dir: str,
    timeout: int,
    N_point: int,
    save: bool,
    rebuild_fpchecker: bool = False,
    rebuild_fpscan: bool = False,
    rebuild_bitblasting: bool = False,
    bar: bool = True,
    log: bool = False
) -> None:
    prgm_dir = os.path.abspath(prgm_dir)
    report_root_dir = os.path.abspath(report_root_dir)

    prgm_name_set, trigo_prgm_name_set = get_fpcore_prgm()
    prgm_name_set = set(map(lambda name: name.replace(extension, ""), prgm_name_set))
    trigo_prgm_name_set = set(map(lambda name: name.replace(extension, ""), trigo_prgm_name_set))

    save_name = f"figure_7_{'bar' if bar else ''}"

    run_tiny = Tiny("./fpscan.exe", timeout=timeout)


    prgm_name_list = list(prgm_name_set)
    prgm_name_no_trigo_list = list(prgm_name_set - trigo_prgm_name_set)

    print(f"total prgm number: {len(prgm_name_list)}")
    print(f"trigo prgm: {len(trigo_prgm_name_set)}")

    # generate reports
    print("Generating FPChecker reports")
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

    print("Generating FPScan reports")
    run_tiny.set_opt([])
    std_report_dict = generate_tiny_report(
        prgm_dir,
        prgm_name_list,
        extension,
        run_tiny,
        os.path.join(report_root_dir, "report"),
        log=log,
        force_rebuild=rebuild_fpscan,
    )

    print("Generating Bitblast reports")
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

    timeout_prgm = [
        prgm for prgm in prgm_name_list if (
            prgm not in trigo_prgm_name_set
            and
            bitblast_report_dict[prgm].time is None
        )
    ]

    print(f"number of timeouts: {len(timeout_prgm)}")

    # data refinement

    def get_bitblasting_time(name):
        t = bitblast_report_dict[name].time if name in bitblast_report_dict else None
        if t is None:
            return timeout
        else:
            return t
    prgm_name_list.sort(key=get_bitblasting_time)

    # plot

    if not bar:
        # plot curves

        c_fig, c_ax = plt.subplots(figsize=[9,4])

        X = np.array(range(len(prgm_name_list)))

        Y_bitblast = np.array([
            get_bitblasting_time(name) for name in prgm_name_list 
        ])
        c_ax.plot(X, Y_bitblast, label="Bitblasting")

        Y_std = np.array([
            std_report_dict[name].time for name in prgm_name_list
        ])
        c_ax.plot(X, Y_std, label="Std")

        Y_fpc = np.array([
            fpc_report_dict[name].time for name in prgm_name_list
        ])
        c_ax.plot(X, Y_fpc, label="FPChecker")

        c_ax.set_yscale('log')
        c_ax.grid(which="minor", color="0.9")
        c_ax.legend(loc="upper left")
        c_ax.set_ylabel("time (s)")
        c_ax.set_title("Computational cost of each method")

    else:
        # plot bar side by side

        width_cluster = 0.7
        width = width_cluster/3

        # bar_side_fig, bar_side_ax = plt.subplots()

        # df = pd.DataFrame(
        # {
        #     'Programs': prgm_name_list,
        #     'Oracle': [(bitblast_report_dict[name].time if name in bitblast_report_dict else None) for name in prgm_name_list],
        #     # 'bitblast': [get_bitblasting_time(name) for name in prgm_name_list],
        #     'FPScan': [std_report_dict[name].time for name in prgm_name_list],
        #     "FPChecker": [fpc_report_dict[name].time for name in prgm_name_list]
        # })
        # fig, ax_bar_side = plt.subplots(figsize=(12, 4.5))
        # ax_bar_side.set_yscale('log')
        # # ax_bar_side.grid(which="minor", axis="y", color="0.95")
        # ax_bar_side.set_axisbelow(True)
        # ax_bar_side = df.plot(x="Programs", y=["Oracle", "FPScan", "FPChecker"], kind="bar", ax=ax_bar_side)
        # ax_bar_side.grid(which="major", visible=True, axis="y", color="0.85")
        # ax_bar_side.grid(which="minor", visible=True, axis="y", color="0.95")
        
        # # ax_bar_side.grid(which="minor", color="0.9")
        # ax_bar_side.legend(loc="upper left")
        # ax_bar_side.set_ylabel("Time (s)")
        # # ax_bar_side.set
        # ax_bar_side.set_xticklabels(df["Programs"], rotation=45, ha='right', rotation_mode='anchor')
        # # ax_bar_side.set_title("Computational cost of each method")

        df = {
            'Programs': prgm_name_list,
            'Bitblasting': [(bitblast_report_dict[name].time if name in bitblast_report_dict and bitblast_report_dict[name].time is not None else 0) for name in prgm_name_list],
            # 'bitblast': [get_bitblasting_time(name) for name in prgm_name_list],
            'FPScan': [std_report_dict[name].time for name in prgm_name_list],
            "FPChecker": [fpc_report_dict[name].time for name in prgm_name_list]
        }
        x = np.arange(len(df["Programs"]))*0.8
        fig, ax = plt.subplots(figsize=(16, 6))
        ax.bar(
            x-width,
            df["FPScan"],
            width,
            label="FPScan",
            color='#006BA4',
        )

        ax.bar(
            x,
            df["Bitblasting"],
            width,
            label="Bitblasting",
            color='#ABABAB',
        )

        ax.bar(
            x + width,
            df["FPChecker"],
            width,
            label="FPChecker",
            color='#FFBC79',
        )

        ax.set_yscale('log', base=10)
        ax.yaxis.grid(True, which="major")
        ax.yaxis.grid(True, which="minor", color="0.92")
        ax.set_axisbelow(True)

        plt.xticks(x, df["Programs"], rotation=45, ha="right", rotation_mode='anchor')
        ax.tick_params(axis='x', which='both', labelsize=11)
        ax.margins(x=0.01)

        plt.xlabel("Benchmarks", fontsize=14)
        plt.ylabel("Time (s)", fontsize=14)
        plt.legend()
        ax.legend(
            ncol=1,
            loc="upper left",
            # bbox_to_anchor=(0.5, 1.15),
            frameon=True,
            fontsize=15,
        )

    if save:
        plt.savefig(save_name + ".pdf", bbox_inches='tight')
        # plt.savefig(save_name + ".svg")
        # subprocess.run([
        #     "inkscape",
        #     f"{save_name}.svg",
        #     "-o", f"{save_name}.pdf",
        #     "--export-type", "pdf",
        #     "--export-area-drawing",
        #     "--vacuum-defs",
        # ])
    else:
        plt.show()