import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

from .tiny import Tiny

from .fpcore_prgm import get_fpcore_prgm

from .fpchecker.sample import Sample

from .fpchecker.report_generator import generate_report as generate_fpc_report
from .tiny.report_generator import generate_report as generate_tiny_report

def compare_fps_fpc(
    prgm_dir: str = "fpcore_prgm",
    extension: str= ".fpcore",
    report_root_dir: str= "report",
    timeout: int = 420,
    N_point: int = 1000,
    rebuild_fpchecker: bool = False,
    rebuild_fpscan: bool = False,
    rebuild_bitblasting: bool = False,
    log: bool = False
):
    prgm_order = [
        "triangle",
        "bspline3",
        "test06_sums4__sum1",
        "test06_sums4__sum2",
        "test05_nonlin1__test2",
        "test05_nonlin1__r4",
        "test04_dqmom9",
        "test03_nonlin2",
        "test02_sum8",
        "test01_sum3",
        "nonlin1",
        "nonlin2",
        "verhulst",
        "x_by_xy",
        "predatorPrey",
        "hypot32",
        "hypot",
        "sec4_example",
        "intro_example",
        "intro_example_mixed",
        # "smartRoot",
        "sqrt_add",
        "sqroot",
        "squareRoot3",
        "squareRoot3Invalid",
        "carbonGas",
        "turbine1",
        "turbine2",
        "turbine3",
        "delta4",
        "kepler0",
        "cav10",
        "floudas",
        "floudas1",
        "floudas3",
        "sum",
        "doppler1",
        "doppler2",
        "doppler3",
        "rigidBody1",
        "rigidBody2",
        "himmilbeau",
        "matrixDeterminant",
        "matrixDeterminant2",
    ]

    # generate data
    prgm_dir = os.path.abspath(prgm_dir)
    report_root_dir = os.path.abspath(report_root_dir)
    # pdf_path = os.path.abspath(pdf_path)

    prgm_name_set, trigo_prgm_name_set = get_fpcore_prgm()
    prgm_name_set = set(map(lambda name: name.replace(extension, ""), prgm_name_set))
    trigo_prgm_name_set = set(map(lambda name: name.replace(extension, ""), trigo_prgm_name_set))

    run_tiny = Tiny("./fpscan.exe", timeout=timeout)

    prgm_name_list = list(prgm_name_set)
    prgm_name_no_trigo_list = list(prgm_name_set - trigo_prgm_name_set)


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

    fd = open("test.csv", "w")

    max_op = 0
    min_op = 1000
    used_prgm = 0
    n_prgm = 0
    data = {}
    for name, std_r in fpscan_report_dict.items():
        fpc_r = fpc_report_dict[name]

        M = std_r.nb_potential_pitfall
        M_cancellation = M // 3

        fpc_cancellation = fpc_r.get_cancellation()
        fpc_cancellation_count = len(fpc_cancellation)

        std_cancellation = std_r.get_cancellation()
        std_cancellation_count = len(std_cancellation)

        n_prgm += 1
        if name not in bitblast_report_dict or bitblast_report_dict[name].time is None:
            continue
        else:
            used_prgm += 1
            n_op = std_r.nb_op
            if n_op > max_op:
                max_op = n_op
            if n_op < min_op:
                min_op = n_op

            if (log):
                print(f"{name}: {n_op}")

            bitblast_r = bitblast_report_dict[name]
            bitblast_cancellation = bitblast_r.get_cancellation()
            bitblast_cancellation_count = len(bitblast_cancellation)

        data[name] = {
            "d": M_cancellation-std_cancellation_count,
            "fps": std_cancellation_count,
            "fpc": fpc_cancellation_count,
            "fpc_miss": bitblast_cancellation_count - fpc_cancellation_count
        }

    print(f"prgm analysed: {n_prgm}")
    print(f"timout: {n_prgm - used_prgm}")
    print(f"used: {used_prgm}")

    col_name = [name for name in prgm_order if name in data]
    col_fps_imp = [data[name]["d"] for name in col_name]
    col_fps = [data[name]["fps"] for name in col_name]
    col_fpc = [data[name]["fpc"] for name in col_name]
    col_fpc_miss = [data[name]["fpc_miss"] for name in col_name]

    # # Read CSV
    # df = pd.read_csv(
    #     "data.csv",
    #     sep=";",
    #     header=None,
    #     names=["name", "d", "fps", "fpc", "fpc_miss"]
    # )

    df = pd.DataFrame({
        "name": col_name,
        "d": col_fps_imp,
        "fps": col_fps,
        "fpc": col_fpc,
        "fpc_miss": col_fpc_miss
    })

    # Clean whitespace
    df["name"] = df["name"].str.strip()

    for c in ["d", "fps", "fpc", "fpc_miss"]:
        df[c] = pd.to_numeric(df[c], errors="coerce")

    # X positions
    x = np.arange(len(df))*1
    width = 0.4

    # plt.figure(figsize=(14, 6))
    plt.figure(figsize=(9, 4))
    # plt.style.use('tableau-colorblind10')

    # Stacked bar (col2 + col3)
    plt.bar(
        x - width/2,
        df["fps"],
        width,
        label="FPScan: pitfalls",
        # hatch="///"
        color='#006BA4'
    )

    plt.bar(
        x - width/2,
        df["d"],
        width,
        bottom=df["fps"],
        label="FPScan: guaranteed impossible",
        # hatch="xxx"
        color='#ABABAB'
    )

    # Separate bar for col4
    plt.bar(
        x + width/2,
        df["fpc"],
        width,
        label="FPChecker: guaranteed possible",
        # hatch="\\\\"
        color='#5F9ED1'
    )

    plt.bar(
        x + width/2,
        df["fpc_miss"],
        width,
        bottom=df["fpc"],
        label="FPChecker: missed possible",
        # hatch="..."
        color='#FFBC79'
    )

    delta=0.3
    for i in range(len(x)):
        m = df["fps"][i] + df["d"][i]    
        plt.text(x[i] - width / 2, m + delta, f"{m}", ha="center", size="small")

    plt.xticks(x, df["name"], rotation=45, ha="right", rotation_mode='anchor')
    plt.xlabel("Benchmarks")
    plt.ylabel("Catastrophic Cancellations")
    # plt.title("Columns 2+3 Stacked, Column 4 Separate")
    plt.legend()

    ax = plt.gca()
    for label in ax.get_xticklabels():
        # if label.get_text() == "kepler0":
        if label.get_text() in ["test04_dqmom9", "test03_nonlin2", "cav10", "matrixDeterminant2", "matrixDeterminant"]:
            label.set_fontweight("bold")
    ax.margins(x=0.01)
    ax.set_ylim([0, 20])

    ax.legend(
        ncol=2,
        loc="upper left",
        # bbox_to_anchor=(0.5, 1.15),
        frameon=False
    )

    # plt.rcParams.update({
    #     "font.size": 8,
    #     "axes.labelsize": 8,
    #     "legend.fontsize": 8,
    #     "xtick.labelsize": 7,
    #     "ytick.labelsize": 7,
    # })

    plt.tight_layout()
    plt.savefig("figure_7.pdf", bbox_inches='tight')
    plt.show()