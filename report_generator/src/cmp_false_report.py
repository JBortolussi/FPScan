import os

from functools import cmp_to_key

from .report import Report
from .report_generator import get_report
from .table import Table, BOT

def get_stats(
    fpscan_r: Report,
    bb_r: Report
) -> dict[str, dict[str, int]]:
    M = fpscan_r.nb_potential_pitfall
    M_cancellation = M // 3
    M_absorption = M_cancellation * 2

    abs_FP = 0
    abs_FN = 0
    abs_TP = 0
    abs_TN = 0
    abs_tot_fps = 0
    abs_tot_bb = 0
    cancel_FP = 0
    cancel_FN = 0
    cancel_TP = 0
    cancel_TN = 0
    cancel_tot_fps = 0
    cancel_tot_bb = 0

    # absorption:
    for pitfall in fpscan_r.get_absorption():
        if bb_r.contains_pitfall(pitfall):
            # True positive
            abs_TP += 1
        else:
            # False positive
            abs_FP += 1
        abs_tot_fps += 1
    for pitfall in bb_r.get_absorption():
        if not fpscan_r.contains_pitfall(pitfall):
            # False negative
            abs_FN += 1
        abs_tot_bb += 1
    abs_TN = M_absorption - abs_FN - abs_FN - abs_FP

    # cancellation
    for pitfall in fpscan_r.get_cancellation():
        if bb_r.contains_pitfall(pitfall):
            # True positive
            cancel_TP += 1
        else:
            # False positive
            cancel_FP += 1
        cancel_tot_fps += 1
    for pitfall in bb_r.get_cancellation():
        if not fpscan_r.contains_pitfall(pitfall):
            # False negative
            cancel_FN += 1
        cancel_tot_bb += 1
    cancel_TN = M_cancellation - cancel_TP - cancel_FP - cancel_FN

    return {
        "abs": {
            "FP": abs_FP,
            "FN": abs_FN,
            "TP": abs_TP,
            "TN": abs_TN,
            "tot_fps": abs_tot_fps,
            "tot_bb": abs_tot_bb,
        },
        "cancel": {
            "FP": cancel_FP,
            "FN": cancel_FN,
            "TP": cancel_TP,
            "TN": cancel_TN,
            "tot_fps":  cancel_tot_fps,
            "tot_bb":   cancel_tot_bb,
        }
    }

def mk_plot(
    bitblast_report_dict: dict[str, Report],
    fpscan_report_dict: dict[str, Report],
    fpchecker_report_dict: dict[str, Report],
):
    

def mk_main_table(
    output_name: str,
    bitblast_report_dict: dict[str, Report],
    fpscan_report_dict: dict[str, Report]
):
    print("Create main table")
    main_table = Table("Comparison of FPScan with Bitblasting ground truth",[
        "\\textbf{Program}",
        ["\\textbf{FPS}", "r"],
        ["\\textbf{BB}", "l"],
        "\\textbf{FP}",
        "\\textbf{FN}",
        "\\textbf{TP}",
        "\\textbf{TN}",
        ["\\textbf{FPS}", "r"],
        ["\\textbf{BB}", "l"],
        "\\textbf{FP}",
        "\\textbf{FN}",
        "\\textbf{TP}",
        "\\textbf{TN}",
    ])
    main_table.add_top_header("\\textbf{Absorption}", 1, 6)
    main_table.add_top_header("\\textbf{Cancellation}", 7, 6)

    for name, fpscan_r in fpscan_report_dict.items():
        if name not in bitblast_report_dict or bitblast_report_dict[name].time is None:
            main_table.add_line([
                name,
                len(fpscan_r.get_absorption()),
                BOT,
                BOT,
                BOT,
                BOT,
                BOT,
                len(fpscan_r.get_cancellation()),
                BOT,
                BOT,
                BOT,
                BOT,
                BOT
            ])
            continue
        bb_r = bitblast_report_dict[name]
        
        stats_dict = get_stats(fpscan_r, bb_r)

        main_table.add_line([
            name,
            stats_dict["abs"]["tot_fps"],
            stats_dict["abs"]["tot_bb"],
            stats_dict["abs"]["FP"],
            stats_dict["abs"]["FN"],
            stats_dict["abs"]["TP"],
            stats_dict["abs"]["TN"],
            stats_dict["cancel"]["tot_fps"],
            stats_dict["cancel"]["tot_bb"],
            stats_dict["cancel"]["FP"],
            stats_dict["cancel"]["FN"],
            stats_dict["cancel"]["TP"],
            stats_dict["cancel"]["TN"],
        ])
        
    def cmp(x, y):
        x = x.get_val(2)
        y = y.get_val(2)
        if x == BOT:
            return 1
        elif y == BOT:
            return -1
        elif x > y:
            return 1
        elif x == y:
            return 0
        else:
            return -1

    main_table.sort(cmp_to_key(cmp))

    main_table.build_pdf(
        latex_dir=os.path.join("tex", output_name),
        pdf_path=f"{output_name}.pdf"
    )

def mk_summary_table(
    output_name: str,
    bitblast_report_dict: dict[str, Report],
    fpscan_report_dict: dict[str, Report]
):
    summary_table = Table("test", [
        "",
        ["Positive", "r"],
        ["Negative", "l"],
        ["Positive", "r"],
        ["Negative", "l"],
    ])

    summary_table.add_top_header("Absorption", 1, 2)
    summary_table.add_top_header("Cancellation", 3, 2)

    pitall_name = ["abs", "cancel"]
    val_name = ["FP", "FN", "TP", "TN", ]
    stats_tot_dict: dict[str, dict[str, int]] = {}
    for pitfall in pitall_name:
        stats_tot_dict[pitfall] = {}
        for val in val_name:
            stats_tot_dict[pitfall][val] = 0

    for name, fpscan_r in fpscan_report_dict.items():
        if name not in bitblast_report_dict or bitblast_report_dict[name].time is None:
            continue
        bb_r = bitblast_report_dict[name]

        stats_dict = get_stats(fpscan_r, bb_r)
        for pitfall in pitall_name:
            for val in val_name:
                stats_tot_dict[pitfall][val] += stats_dict[pitfall][val]


    summary_table.add_line([
        "False",
        stats_tot_dict["abs"]["FP"],
        stats_tot_dict["abs"]["FN"],
        stats_tot_dict["cancel"]["FP"],
        stats_tot_dict["cancel"]["FN"],
    ])
    summary_table.add_line([
        "True",
        stats_tot_dict["abs"]["TP"],
        stats_tot_dict["abs"]["TN"],
        stats_tot_dict["cancel"]["TP"],
        stats_tot_dict["cancel"]["TN"],
    ])

    summary_table.build_pdf(
        latex_dir=os.path.join("tex", output_name),
        pdf_path=f"{output_name}.pdf"
    )


def mk_tables(
    report_root_dir: str,
    timeout: int,
    rebuild_fpscan: bool = False,
    rebuild_bitblasting: bool = False,
    log: bool = False
) -> None:
    fpcore_dir = os.path.abspath("fpcore_prgm")
    fpscan_dir = os.path.abspath("fpscan")

    fpcore_report_dicts = get_report(
        prgm_dir=fpcore_dir,
        extension=".fpcore",
        report_root_dir=report_root_dir,
        timeout=timeout,
        N_point=0,
        rebuild_fpscan=rebuild_fpscan,
        rebuild_bitblasting=rebuild_bitblasting,
        rebuild_fpchecker=False,
        log=log
    )

    fpscan_prgm = [
        "complex_sqrt_i_neg",
        "complex_sqrt_i_pos",
        "lopes_1",
        "lopes_20",
        # "lopes_loop"
    ]

    # fpscan_report_dicts = get_report(
    #     prgm_dir=fpscan_dir,
    #     extension=".fpcore",
    #     report_root_dir=report_root_dir,
    #     timeout=timeout,
    #     N_point=0,
    #     prgm_name_list=fpscan_prgm,
    #     prgm_name_no_trigo_list=fpscan_prgm,
    #     rebuild_fpscan=rebuild_fpscan,
    #     rebuild_bitblasting=rebuild_bitblasting,
    #     rebuild_fpchecker=False,
    #     log=log
    # )

    mk_main_table(
        output_name="fpcore_main",
        bitblast_report_dict=fpcore_report_dicts["bitblast"],
        fpscan_report_dict=fpcore_report_dicts["fpscan"],
    )

    mk_summary_table(
        output_name="fpcore_summary",
        bitblast_report_dict=fpcore_report_dicts["bitblast"],
        fpscan_report_dict=fpcore_report_dicts["fpscan"],
    )

    # mk_main_table(
    #     output_name="fpscan_main",
    #     bitblast_report_dict=fpscan_report_dicts["bitblast"],
    #     fpscan_report_dict=fpscan_report_dicts["fpscan"]
    # )

    # mk_summary_table(
    #     output_name="fpscan_summary",
    #     bitblast_report_dict=fpscan_report_dicts["bitblast"],
    #     fpscan_report_dict=fpscan_report_dicts["fpscan"]
    # )